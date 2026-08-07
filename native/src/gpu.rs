//! Direct-Vulkan compute for the fused display-tone and sRGB-transfer stage.
//!
//! Only stages whose arithmetic outweighs the bytes they move belong here. The
//! tone transfer qualifies: a `pow` per channel costs the CPU 375 ms at 20 MP
//! against 77 ms on an Iris Xe including both host copies. The camera-to-sRGB
//! matrix does not, and was measured and left on the CPU — see
//! `color::cpu_transform_pixels`. A stage that only multiplies and adds leaves
//! the CPU memory-bound anyway, so a round trip through the device can only
//! lose.
//!
//! The backend is on by default; set `ORFEUS_GPU=0` to force the CPU path, or
//! `ORFEUS_GPU=discrete` to prefer a discrete adapter over an integrated one.
//! Any initialization, allocation, submission, or readback failure leaves the
//! input untouched so the renderer can run the CPU implementation, and a failed
//! initialization is remembered so later renders skip the attempt.

use std::ffi::{CStr, CString};
use std::io::Cursor;
use std::sync::{Mutex, OnceLock, TryLockError};
use std::time::Instant;

use ash::vk::Handle;
use ash::{Device, Entry, Instance, vk};

const TONE_TRANSFER_SHADER: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/tone_transfer.spv"));
const NR_YCBCR_SHADER: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/nr_ycbcr.spv"));
const NR_BILATERAL_SHADER: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/nr_bilateral.spv"));
const NR_MEDIAN_SHADER: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/nr_median.spv"));
const NR_COMBINE_SHADER: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/nr_combine.spv"));
const NR_CONV_SHADER: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/nr_conv.spv"));

/// The push-constant block declared in `shaders/stage.glsl`.
///
/// Members are four-component so std140 and std430 agree on their offsets,
/// whichever a driver assumes for push constants.
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
struct Parameters {
    counts: [u32; 4],
    offsets: [u32; 4],
    scalars: [f32; 4],
    flags: [u32; 4],
}

impl Parameters {
    fn bytes(&self) -> &[u8] {
        // A plain `repr(C)` aggregate of `u32` and `f32`, so every byte is
        // initialized and no padding is exposed.
        unsafe {
            std::slice::from_raw_parts(
                (self as *const Self).cast::<u8>(),
                std::mem::size_of::<Self>(),
            )
        }
    }
}

/// One logical buffer: a host-mapped upload buffer plus a device-local mirror,
/// or a single host-visible device-local buffer when the adapter shares memory
/// with the host.
///
/// Integrated adapters — which this renderer prefers, because these stages are
/// transfer-bound — expose `DEVICE_LOCAL | HOST_VISIBLE` heaps, so the two
/// in-device copies per dispatch are pure waste there. A 20 MP frame moves
/// 240 MB per copy, which is the single largest avoidable cost in the dispatch.
#[derive(Default)]
struct Slot {
    host: vk::Buffer,
    host_memory: vk::DeviceMemory,
    mapped: usize,
    device: vk::Buffer,
    device_memory: vk::DeviceMemory,
    capacity: vk::DeviceSize,
    unified: bool,
}

struct Context {
    _entry: Entry,
    instance: Instance,
    device: Device,
    queue: vk::Queue,
    memory_properties: vk::PhysicalDeviceMemoryProperties,
    command_pool: vk::CommandPool,
    descriptor_set_layout: vk::DescriptorSetLayout,
    pipeline_layout: vk::PipelineLayout,
    tone_transfer: vk::Pipeline,
    nr_ycbcr: vk::Pipeline,
    nr_bilateral: vk::Pipeline,
    nr_median: vk::Pipeline,
    nr_combine: vk::Pipeline,
    nr_conv: vk::Pipeline,
    descriptor_pool: vk::DescriptorPool,
    descriptor_set: vk::DescriptorSet,
    command: vk::CommandBuffer,
    fence: vk::Fence,
    input: Slot,
    max_storage_buffer_range: vk::DeviceSize,
    adapter_name: String,
    /// True when the adapter does not share the host's memory bus.
    discrete: bool,
}

// Queue submission and command-pool allocation require external synchronization.
static CONTEXT: OnceLock<Result<Mutex<Context>, String>> = OnceLock::new();

pub(crate) struct DispatchProfile {
    pub(crate) adapter_name: String,
    pub(crate) milliseconds: f64,
}

/// Whether the active adapter has its own memory rather than the host's.
///
/// Initializes the context if it is not up yet, which the GUI has already done
/// from its warm-up thread by the time any render asks.
pub(crate) fn adapter_is_discrete() -> bool {
    context()
        .ok()
        .and_then(|context| context.try_lock().ok().map(|context| context.discrete))
        .unwrap_or(false)
}

pub(crate) fn requested() -> bool {
    requested_value(std::env::var_os("ORFEUS_GPU").as_deref())
}

fn requested_value(value: Option<&std::ffi::OsStr>) -> bool {
    !value.is_some_and(|value| value == "0")
}

/// Builds the Vulkan context on a background thread.
///
/// Initialization takes long enough to be visible on the first frames, and a
/// render that arrives mid-initialization simply waits for it — waiting costs
/// far less than the CPU fallback it would otherwise take.
pub(crate) fn warm_up() {
    if !requested() || CONTEXT.get().is_some() {
        return;
    }
    std::thread::Builder::new()
        .name("orfeus-gpu-warm-up".to_string())
        .spawn(|| {
            let _ = context();
        })
        .ok();
}

fn context() -> Result<&'static Mutex<Context>, String> {
    CONTEXT
        .get_or_init(|| initialize().map(Mutex::new))
        .as_ref()
        .map_err(Clone::clone)
}

fn vk_error(operation: &str, error: vk::Result) -> String {
    format!("Vulkan {operation}: {error:?}")
}

/// Which adapter to pick when a machine offers more than one.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum AdapterPreference {
    Integrated,
    Discrete,
}

pub(crate) fn preference() -> AdapterPreference {
    preference_value(std::env::var_os("ORFEUS_GPU").as_deref())
}

fn preference_value(value: Option<&std::ffi::OsStr>) -> AdapterPreference {
    if value.is_some_and(|value| value == "discrete") {
        AdapterPreference::Discrete
    } else {
        AdapterPreference::Integrated
    }
}

/// Integrated adapters win by default. Every stage here round-trips a whole
/// image through host memory, so the work is transfer-bound rather than
/// compute-bound: on a 13700H with an RTX 2000 Ada beside its Iris Xe, the
/// same 20 MP tone-and-transfer pass measured 137 ms on the discrete card and
/// 81 ms on the integrated one, against 372 ms on the CPU. Shared memory beats
/// PCIe here. Set `ORFEUS_GPU=discrete` to override.
fn adapter_rank(kind: vk::PhysicalDeviceType, preference: AdapterPreference) -> u32 {
    let integrated_first = preference == AdapterPreference::Integrated;
    match kind {
        vk::PhysicalDeviceType::INTEGRATED_GPU if integrated_first => 0,
        vk::PhysicalDeviceType::DISCRETE_GPU if integrated_first => 1,
        vk::PhysicalDeviceType::DISCRETE_GPU => 0,
        vk::PhysicalDeviceType::INTEGRATED_GPU => 1,
        _ => 2,
    }
}

/// A software rasterizer such as lavapipe is slower than our own CPU path, so
/// it only counts as an adapter when a validation run or an explicit request
/// asks for one. Otherwise exposing every system ICD could silently downgrade
/// a machine that has no real GPU.
fn software_allowed() -> bool {
    software_allowed_value(
        std::env::var_os("ORFEUS_GPU_TEST").as_deref(),
        std::env::var_os("ORFEUS_GPU").as_deref(),
    )
}

fn software_allowed_value(
    test: Option<&std::ffi::OsStr>,
    request: Option<&std::ffi::OsStr>,
) -> bool {
    test.is_some_and(|value| value == "1") || request.is_some_and(|value| value == "software")
}

/// Set `ORFEUS_GPU_STAGING=1` to take the discrete-adapter staging path on an
/// adapter that offers unified memory, so both paths can be validated on one
/// machine.
fn staging_forced() -> bool {
    std::env::var_os("ORFEUS_GPU_STAGING").as_deref() == Some(std::ffi::OsStr::new("1"))
}

fn adapter_is_usable(kind: vk::PhysicalDeviceType, software_allowed: bool) -> bool {
    kind != vk::PhysicalDeviceType::CPU || software_allowed
}

fn adapter_kind_name(kind: vk::PhysicalDeviceType) -> &'static str {
    match kind {
        vk::PhysicalDeviceType::INTEGRATED_GPU => "integrated",
        vk::PhysicalDeviceType::DISCRETE_GPU => "discrete",
        vk::PhysicalDeviceType::VIRTUAL_GPU => "virtual",
        vk::PhysicalDeviceType::CPU => "cpu",
        _ => "other",
    }
}

unsafe fn create_pipeline(
    device: &Device,
    layout: vk::PipelineLayout,
    spirv: &[u8],
) -> Result<vk::Pipeline, String> {
    let words = ash::util::read_spv(&mut Cursor::new(spirv))
        .map_err(|error| format!("read embedded SPIR-V: {error}"))?;
    let module_info = vk::ShaderModuleCreateInfo::default().code(&words);
    let module = unsafe { device.create_shader_module(&module_info, None) }
        .map_err(|error| vk_error("shader module creation", error))?;
    let stage = vk::PipelineShaderStageCreateInfo::default()
        .stage(vk::ShaderStageFlags::COMPUTE)
        .module(module)
        .name(c"main");
    let pipeline_info = [vk::ComputePipelineCreateInfo::default()
        .stage(stage)
        .layout(layout)];
    let result =
        unsafe { device.create_compute_pipelines(vk::PipelineCache::null(), &pipeline_info, None) };
    unsafe { device.destroy_shader_module(module, None) };
    Ok(result.map_err(|(_, error)| vk_error("compute pipeline creation", error))?[0])
}

fn initialize() -> Result<Context, String> {
    let preference = preference();
    let software = software_allowed();
    unsafe {
        let entry = Entry::load().map_err(|error| format!("load Vulkan loader: {error}"))?;
        let application_name = CString::new("Orfeus").unwrap();
        let application = vk::ApplicationInfo::default()
            .application_name(&application_name)
            .application_version(1)
            .engine_name(&application_name)
            .engine_version(1)
            .api_version(vk::API_VERSION_1_1);
        let instance_info = vk::InstanceCreateInfo::default().application_info(&application);
        let instance = entry
            .create_instance(&instance_info, None)
            .map_err(|error| vk_error("instance creation", error))?;
        let devices = instance
            .enumerate_physical_devices()
            .map_err(|error| vk_error("physical-device enumeration", error))?;
        let mut candidates = Vec::new();
        for physical_device in devices {
            let properties = instance.get_physical_device_properties(physical_device);
            for (index, family) in instance
                .get_physical_device_queue_family_properties(physical_device)
                .iter()
                .enumerate()
            {
                if family.queue_flags.contains(vk::QueueFlags::COMPUTE) {
                    if !adapter_is_usable(properties.device_type, software) {
                        break;
                    }
                    let rank = adapter_rank(properties.device_type, preference);
                    candidates.push((rank, physical_device, index as u32, properties));
                    break;
                }
            }
        }
        candidates.sort_by_key(|candidate| candidate.0);
        if std::env::var_os("ORFEUS_PROFILE").is_some() {
            for (rank, _, queue_family, properties) in &candidates {
                eprintln!(
                    "orfeus-profile gpu-adapter rank={rank} queue-family={queue_family} type={} name={:?}",
                    adapter_kind_name(properties.device_type),
                    properties
                        .device_name_as_c_str()
                        .unwrap_or(c"unnamed")
                        .to_string_lossy()
                );
            }
        }
        let (_, physical_device, queue_family, properties) = candidates
            .into_iter()
            .next()
            .ok_or_else(|| "no Vulkan compute adapter".to_string())?;
        let priority = [1.0_f32];
        let queue_info = [vk::DeviceQueueCreateInfo::default()
            .queue_family_index(queue_family)
            .queue_priorities(&priority)];
        let device_info = vk::DeviceCreateInfo::default().queue_create_infos(&queue_info);
        let device = instance
            .create_device(physical_device, &device_info, None)
            .map_err(|error| vk_error("device creation", error))?;
        let queue = device.get_device_queue(queue_family, 0);
        let command_pool_info = vk::CommandPoolCreateInfo::default()
            .queue_family_index(queue_family)
            .flags(vk::CommandPoolCreateFlags::RESET_COMMAND_BUFFER);
        let command_pool = device
            .create_command_pool(&command_pool_info, None)
            .map_err(|error| vk_error("command-pool creation", error))?;
        let bindings = [vk::DescriptorSetLayoutBinding::default()
            .binding(0)
            .descriptor_type(vk::DescriptorType::STORAGE_BUFFER)
            .descriptor_count(1)
            .stage_flags(vk::ShaderStageFlags::COMPUTE)];
        let descriptor_info = vk::DescriptorSetLayoutCreateInfo::default().bindings(&bindings);
        let descriptor_set_layout = device
            .create_descriptor_set_layout(&descriptor_info, None)
            .map_err(|error| vk_error("descriptor layout creation", error))?;
        let set_layouts = [descriptor_set_layout];
        let push_constants = [vk::PushConstantRange::default()
            .stage_flags(vk::ShaderStageFlags::COMPUTE)
            .offset(0)
            .size(std::mem::size_of::<Parameters>() as u32)];
        let pipeline_layout_info = vk::PipelineLayoutCreateInfo::default()
            .set_layouts(&set_layouts)
            .push_constant_ranges(&push_constants);
        let pipeline_layout = device
            .create_pipeline_layout(&pipeline_layout_info, None)
            .map_err(|error| vk_error("pipeline layout creation", error))?;
        let tone_transfer = create_pipeline(&device, pipeline_layout, TONE_TRANSFER_SHADER)?;
        let nr_ycbcr = create_pipeline(&device, pipeline_layout, NR_YCBCR_SHADER)?;
        let nr_bilateral = create_pipeline(&device, pipeline_layout, NR_BILATERAL_SHADER)?;
        let nr_median = create_pipeline(&device, pipeline_layout, NR_MEDIAN_SHADER)?;
        let nr_combine = create_pipeline(&device, pipeline_layout, NR_COMBINE_SHADER)?;
        let nr_conv = create_pipeline(&device, pipeline_layout, NR_CONV_SHADER)?;
        let pool_size = [vk::DescriptorPoolSize::default()
            .ty(vk::DescriptorType::STORAGE_BUFFER)
            .descriptor_count(bindings.len() as u32)];
        let descriptor_pool_info = vk::DescriptorPoolCreateInfo::default()
            .max_sets(1)
            .pool_sizes(&pool_size);
        let descriptor_pool = device
            .create_descriptor_pool(&descriptor_pool_info, None)
            .map_err(|error| vk_error("descriptor pool creation", error))?;
        let layouts = [descriptor_set_layout];
        let descriptor_allocation = vk::DescriptorSetAllocateInfo::default()
            .descriptor_pool(descriptor_pool)
            .set_layouts(&layouts);
        let descriptor_set = device
            .allocate_descriptor_sets(&descriptor_allocation)
            .map_err(|error| vk_error("descriptor set allocation", error))?[0];
        let command_info = vk::CommandBufferAllocateInfo::default()
            .command_pool(command_pool)
            .level(vk::CommandBufferLevel::PRIMARY)
            .command_buffer_count(1);
        let command = device
            .allocate_command_buffers(&command_info)
            .map_err(|error| vk_error("command buffer allocation", error))?[0];
        let fence = device
            .create_fence(&vk::FenceCreateInfo::default(), None)
            .map_err(|error| vk_error("fence creation", error))?;
        let adapter_name = CStr::from_ptr(properties.device_name.as_ptr())
            .to_string_lossy()
            .into_owned();
        Ok(Context {
            _entry: entry,
            memory_properties: instance.get_physical_device_memory_properties(physical_device),
            instance,
            device,
            queue,
            command_pool,
            descriptor_set_layout,
            pipeline_layout,
            tone_transfer,
            nr_ycbcr,
            nr_bilateral,
            nr_median,
            nr_combine,
            nr_conv,
            descriptor_pool,
            descriptor_set,
            command,
            fence,
            input: Slot::default(),
            max_storage_buffer_range: properties.limits.max_storage_buffer_range.into(),
            adapter_name,
            discrete: properties.device_type == vk::PhysicalDeviceType::DISCRETE_GPU,
        })
    }
}

/// A buffer that only feeds and receives copies. Asking for storage usage it
/// never needs narrows the memory types a driver will accept, which on a
/// discrete adapter can push the mapped buffer into a small host-visible device
/// window and make readback ruinously slow.
const TRANSFER_ONLY: vk::BufferUsageFlags = vk::BufferUsageFlags::from_raw(
    vk::BufferUsageFlags::TRANSFER_SRC.as_raw() | vk::BufferUsageFlags::TRANSFER_DST.as_raw(),
);

/// A buffer the shader reads and writes, which the host may also copy.
const TRANSFER_AND_STORAGE: vk::BufferUsageFlags = vk::BufferUsageFlags::from_raw(
    TRANSFER_ONLY.as_raw() | vk::BufferUsageFlags::STORAGE_BUFFER.as_raw(),
);

unsafe fn create_buffer(
    device: &Device,
    memory_properties: &vk::PhysicalDeviceMemoryProperties,
    size: vk::DeviceSize,
    usage: vk::BufferUsageFlags,
    required_memory_flags: vk::MemoryPropertyFlags,
    preferred_memory_flags: vk::MemoryPropertyFlags,
) -> Result<(vk::Buffer, vk::DeviceMemory), String> {
    let info = vk::BufferCreateInfo::default()
        .size(size)
        .usage(usage)
        .sharing_mode(vk::SharingMode::EXCLUSIVE);
    let buffer = unsafe { device.create_buffer(&info, None) }
        .map_err(|error| vk_error("buffer creation", error))?;
    let requirements = unsafe { device.get_buffer_memory_requirements(buffer) };
    let memory_type = (0..memory_properties.memory_type_count)
        .filter(|index| {
            requirements.memory_type_bits & (1 << index) != 0
                && memory_properties.memory_types[*index as usize]
                    .property_flags
                    .contains(required_memory_flags)
        })
        .max_by_key(|index| {
            memory_properties.memory_types[*index as usize]
                .property_flags
                .contains(preferred_memory_flags)
        })
        .ok_or_else(|| "no suitable Vulkan memory type".to_string());
    let memory_type = match memory_type {
        Ok(value) => value,
        Err(error) => {
            unsafe { device.destroy_buffer(buffer, None) };
            return Err(error);
        }
    };
    let allocation = vk::MemoryAllocateInfo::default()
        .allocation_size(requirements.size)
        .memory_type_index(memory_type);
    let memory = match unsafe { device.allocate_memory(&allocation, None) } {
        Ok(memory) => memory,
        Err(error) => {
            unsafe { device.destroy_buffer(buffer, None) };
            return Err(vk_error("buffer memory allocation", error));
        }
    };
    if let Err(error) = unsafe { device.bind_buffer_memory(buffer, memory, 0) } {
        unsafe {
            device.destroy_buffer(buffer, None);
            device.free_memory(memory, None);
        }
        return Err(vk_error("buffer memory binding", error));
    }
    Ok((buffer, memory))
}

/// Buffers are grown in coarse steps so a slightly larger frame reuses the
/// current allocation, but not so coarse that a 20 MP image reserves twice the
/// memory it needs — rounding 240 MB up to a power of two would waste 16 MB
/// short of a further 100 MB per slot.
const CAPACITY_GRANULARITY: vk::DeviceSize = 4 << 20;

fn rounded_capacity(
    size: vk::DeviceSize,
    max_storage_buffer_range: vk::DeviceSize,
) -> Result<vk::DeviceSize, String> {
    if size > max_storage_buffer_range {
        return Err(format!(
            "Vulkan storage request {size} exceeds adapter limit {max_storage_buffer_range}"
        ));
    }
    let rounded = size
        .div_ceil(CAPACITY_GRANULARITY)
        .saturating_mul(CAPACITY_GRANULARITY);
    Ok(rounded.clamp(size, max_storage_buffer_range.max(size)))
}

impl Slot {
    /// The buffer the shader binds: the shared allocation when the adapter has
    /// unified memory, otherwise the device-local mirror.
    fn storage(&self) -> vk::Buffer {
        self.device
    }

    unsafe fn destroy(&mut self, device: &Device) {
        if self.capacity == 0 {
            return;
        }
        unsafe {
            device.unmap_memory(self.host_memory);
            if !self.unified {
                device.destroy_buffer(self.device, None);
                device.free_memory(self.device_memory, None);
            }
            device.destroy_buffer(self.host, None);
            device.free_memory(self.host_memory, None);
        }
        *self = Self::default();
    }

    unsafe fn ensure(
        &mut self,
        device: &Device,
        memory_properties: &vk::PhysicalDeviceMemoryProperties,
        size: vk::DeviceSize,
        max_storage_buffer_range: vk::DeviceSize,
    ) -> Result<(), String> {
        if self.capacity >= size {
            return Ok(());
        }
        let capacity = rounded_capacity(size, max_storage_buffer_range)?;
        let host_flags =
            vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT;
        // A single buffer that is both mappable and device-local removes the two
        // whole-image copies inside the device. Integrated adapters offer it;
        // discrete ones fall back to a staging pair.
        //
        // `HOST_CACHED` is required, not merely preferred: a discrete card also
        // advertises `DEVICE_LOCAL | HOST_VISIBLE` memory, but that is a window
        // into VRAM across PCIe with no host cache behind it. Mapping it makes
        // the two host copies read and write video memory a cache line at a
        // time, which measured 31 seconds for a 20 MP frame against 25 ms on
        // the CPU. Genuinely shared memory is cached on the host side.
        let unified = if staging_forced() {
            Err("staging path forced by ORFEUS_GPU_STAGING=1".to_string())
        } else {
            unsafe {
                create_buffer(
                    device,
                    memory_properties,
                    capacity,
                    TRANSFER_AND_STORAGE,
                    host_flags
                        | vk::MemoryPropertyFlags::DEVICE_LOCAL
                        | vk::MemoryPropertyFlags::HOST_CACHED,
                    vk::MemoryPropertyFlags::empty(),
                )
            }
        };
        let (shared, host, host_memory) = match unified {
            Ok((buffer, memory)) => (true, buffer, memory),
            Err(_) => {
                let (buffer, memory) = unsafe {
                    create_buffer(
                        device,
                        memory_properties,
                        capacity,
                        TRANSFER_ONLY,
                        host_flags,
                        vk::MemoryPropertyFlags::HOST_CACHED,
                    )?
                };
                (false, buffer, memory)
            }
        };
        let mut replacement = Slot {
            host,
            host_memory,
            mapped: 0,
            device: host,
            device_memory: vk::DeviceMemory::null(),
            capacity,
            unified: shared,
        };
        let outcome = (|| -> Result<(), String> {
            if !shared {
                let (buffer, memory) = unsafe {
                    create_buffer(
                        device,
                        memory_properties,
                        capacity,
                        TRANSFER_AND_STORAGE,
                        vk::MemoryPropertyFlags::DEVICE_LOCAL,
                        vk::MemoryPropertyFlags::empty(),
                    )?
                };
                replacement.device = buffer;
                replacement.device_memory = memory;
            }
            let mapped =
                unsafe { device.map_memory(host_memory, 0, capacity, vk::MemoryMapFlags::empty()) }
                    .map_err(|error| vk_error("persistent host mapping", error))?;
            replacement.mapped = mapped as usize;
            Ok(())
        })();
        if let Err(error) = outcome {
            unsafe {
                if !replacement.device_memory.is_null() {
                    device.destroy_buffer(replacement.device, None);
                    device.free_memory(replacement.device_memory, None);
                }
                device.destroy_buffer(host, None);
                device.free_memory(host_memory, None);
            }
            return Err(error);
        }
        unsafe { self.destroy(device) };
        *self = replacement;
        if std::env::var_os("ORFEUS_PROFILE").is_some() {
            eprintln!(
                "orfeus-profile gpu-slot bytes={size} capacity={capacity} memory={}",
                if shared { "unified" } else { "staged" }
            );
        }
        Ok(())
    }

    /// Copies `values` into the host-visible buffer.
    unsafe fn upload(&self, values: &[f32]) {
        unsafe {
            std::ptr::copy_nonoverlapping(
                values.as_ptr().cast::<u8>(),
                self.mapped as *mut u8,
                std::mem::size_of_val(values),
            );
        }
    }

    /// Writes `values` into the host-visible buffer starting at `offset` floats.
    unsafe fn upload_at(&self, offset: usize, values: &[f32]) {
        unsafe {
            std::ptr::copy_nonoverlapping(
                values.as_ptr().cast::<u8>(),
                (self.mapped + offset * std::mem::size_of::<f32>()) as *mut u8,
                std::mem::size_of_val(values),
            );
        }
    }

    /// Reads the host-visible buffer's leading scalars over `values`.
    unsafe fn download(&self, values: &mut [f32]) {
        unsafe {
            std::ptr::copy_nonoverlapping(
                self.mapped as *const u8,
                values.as_mut_ptr().cast::<u8>(),
                std::mem::size_of_val(values),
            );
        }
    }
}

fn byte_size(scalars: usize) -> Result<vk::DeviceSize, String> {
    vk::DeviceSize::try_from(
        scalars
            .checked_mul(std::mem::size_of::<f32>())
            .ok_or_else(|| format!("GPU byte size overflow for {scalars} scalars"))?,
    )
    .map_err(|_| format!("GPU byte size is unsupported for {scalars} scalars"))
}

#[derive(Debug, Eq, PartialEq)]
struct DispatchGroups {
    x: u32,
    y: u32,
}

fn dispatch_groups(pixel_count: usize) -> Result<DispatchGroups, String> {
    if pixel_count == 0 {
        return Err("GPU stage needs at least one pixel".to_string());
    }
    let pixel_count = u32::try_from(pixel_count)
        .map_err(|_| format!("GPU stage has unsupported pixel count {pixel_count}"))?;
    let group_count = pixel_count.div_ceil(256);
    let x = group_count.min(65_535);
    let y = group_count.div_ceil(65_535);
    if y > 65_535 {
        return Err(format!("GPU stage requires unsupported dispatch {x}x{y}"));
    }
    Ok(DispatchGroups { x, y })
}

fn locked() -> Result<std::sync::MutexGuard<'static, Context>, String> {
    match context()?.try_lock() {
        Ok(context) => Ok(context),
        Err(TryLockError::WouldBlock) => Err("Vulkan device is busy".to_string()),
        Err(TryLockError::Poisoned(_)) => Err("Vulkan context lock poisoned".to_string()),
    }
}

/// Runs the fused display-tone and sRGB-transfer stage in place.
///
/// Nothing writes to `rgb` until the dispatch has completed, so any earlier
/// failure leaves the caller's pixels intact for the CPU path. The readback
/// itself cannot fail — the fence has already been waited on — so it writes
/// straight into `rgb` rather than through a 240 MB intermediate vector.
pub(crate) fn tone_and_transfer(rgb: &mut [f32]) -> Result<DispatchProfile, String> {
    if rgb.is_empty() || !rgb.len().is_multiple_of(3) {
        return Err("GPU RGB input must contain non-empty triplets".to_string());
    }
    let pixel_count = rgb.len() / 3;
    let parameters = Parameters {
        counts: [pixel_count as u32, 3, 0, 0],
        ..Parameters::default()
    };
    let mut context = locked()?;
    let started = Instant::now();
    unsafe { dispatch(&mut context, parameters, rgb) }?;
    Ok(DispatchProfile {
        adapter_name: context.adapter_name.clone(),
        milliseconds: started.elapsed().as_secs_f64() * 1000.0,
    })
}

/// Planes the noise reduction keeps resident, laid out end to end in one buffer.
///
/// Every dispatch takes plane offsets in its push constants, so the whole filter
/// records as a single submission against a single descriptor set. Nothing is
/// rebound and nothing round-trips to the host between passes, which is the
/// whole reason this stage pays for a GPU where the camera matrix did not.
#[derive(Clone, Copy)]
struct Planes {
    rgb: u32,
    luma: u32,
    blue: u32,
    red: u32,
    /// The finer blur scale during the luma pass, then a chroma scratch plane.
    first: u32,
    /// The coarser blur scale, then the chroma blur's result.
    second: u32,
    /// Where a separable blur leaves its horizontal half.
    scratch: u32,
}

impl Planes {
    /// Total scalars: three interleaved channels plus six single-channel planes.
    const PLANE_COUNT: usize = 6;

    fn new(pixel_count: usize) -> Result<(Self, usize), String> {
        let stride = u32::try_from(pixel_count)
            .map_err(|_| format!("GPU noise reduction cannot address {pixel_count} pixels"))?;
        let total = pixel_count
            .checked_mul(3 + Self::PLANE_COUNT)
            .ok_or_else(|| "GPU noise reduction plane size overflow".to_string())?;
        let plane = |index: u32| 3 * stride + index * stride;
        Ok((
            Self {
                rgb: 0,
                luma: plane(0),
                blue: plane(1),
                red: plane(2),
                first: plane(3),
                second: plane(4),
                scratch: plane(5),
            },
            total,
        ))
    }
}

/// One recorded dispatch: which pipeline, and the push constants it reads.
struct Pass {
    pipeline: vk::Pipeline,
    parameters: Parameters,
}

/// The pipelines a noise-reduction run dispatches, so the pass list can be
/// built — and its shape tested — without a device.
#[derive(Clone, Copy, Default)]
struct NoisePipelines {
    ycbcr: vk::Pipeline,
    bilateral: vk::Pipeline,
    median: vk::Pipeline,
    combine: vk::Pipeline,
}

impl NoisePipelines {
    fn of(context: &Context) -> Self {
        Self {
            ycbcr: context.nr_ycbcr,
            bilateral: context.nr_bilateral,
            median: context.nr_median,
            combine: context.nr_combine,
        }
    }
}

/// Builds the pass list for one noise-reduction run.
struct PassBuilder {
    pipelines: NoisePipelines,
    passes: Vec<Pass>,
    counts: [u32; 4],
    planes: Planes,
}

impl PassBuilder {
    fn new(pipelines: NoisePipelines, width: usize, height: usize, planes: Planes) -> Self {
        Self {
            pipelines,
            passes: Vec::new(),
            counts: [(width * height) as u32, width as u32, height as u32, 0],
            planes,
        }
    }

    fn push(&mut self, pipeline: vk::Pipeline, parameters: Parameters) {
        self.passes.push(Pass {
            pipeline,
            parameters,
        });
    }

    fn ycbcr(&mut self, inverse: bool) {
        let planes = self.planes;
        self.push(
            self.pipelines.ycbcr,
            Parameters {
                counts: self.counts,
                offsets: [planes.rgb, planes.luma, planes.blue, planes.red],
                flags: [0, u32::from(inverse), 0, 0],
                ..Parameters::default()
            },
        );
    }

    /// A separable edge-guided blur from `source` into `target`, guided by
    /// `guide`, leaving its horizontal half in the scratch plane.
    fn blur(&mut self, source: u32, guide: u32, target: u32, step: u32) {
        let scratch = self.planes.scratch;
        let mut counts = self.counts;
        counts[3] = step;
        for (axis, from, to) in [(0, source, scratch), (1, scratch, target)] {
            self.push(
                self.pipelines.bilateral,
                Parameters {
                    counts,
                    offsets: [from, guide, to, 0],
                    flags: [axis, 0, 0, 0],
                    ..Parameters::default()
                },
            );
        }
    }

    fn median(&mut self, source: u32, target: u32) {
        self.push(
            self.pipelines.median,
            Parameters {
                counts: self.counts,
                offsets: [source, 0, target, 0],
                ..Parameters::default()
            },
        );
    }

    fn blend(&mut self, target: u32, filtered: u32, amount: f32) {
        self.push(
            self.pipelines.combine,
            Parameters {
                counts: self.counts,
                offsets: [filtered, 0, target, 0],
                scalars: [amount, 0.0, 0.0, 0.0],
                flags: [0, 0, 0, 0],
            },
        );
    }

    fn recombine_luma(&mut self, strength: f32) {
        let planes = self.planes;
        self.push(
            self.pipelines.combine,
            Parameters {
                counts: self.counts,
                offsets: [planes.first, planes.second, planes.luma, 0],
                scalars: [strength, 0.0, 0.0, 0.0],
                flags: [0, 1, 0, 0],
            },
        );
    }
}

/// The pass list implementing `render::apply_noise_reduction` on the GPU.
fn noise_reduction_passes(
    pipelines: NoisePipelines,
    width: usize,
    height: usize,
    planes: Planes,
    luma: f32,
    chroma: f32,
) -> Vec<Pass> {
    let mut builder = PassBuilder::new(pipelines, width, height, planes);
    builder.ycbcr(false);
    let luma_strength = luma.clamp(0.0, 1.0);
    if luma_strength > 0.0 {
        // Two scales of the same edge-guided blur, one twice as wide as the
        // other, so fine and coarse detail can be thresholded separately.
        builder.blur(planes.luma, planes.luma, planes.first, 1);
        builder.blur(planes.first, planes.luma, planes.second, 2);
        builder.recombine_luma(luma_strength);
    }
    let chroma_strength = chroma.clamp(0.0, 1.0);
    if chroma_strength > 0.0 {
        for channel in [planes.blue, planes.red] {
            builder.median(channel, planes.scratch);
            // The median lands in the scratch plane, which the blur below then
            // reuses, so blend it away first.
            builder.blend(channel, planes.scratch, (chroma_strength * 1.5).min(1.0));
            for (step, amount) in [
                (1, (chroma_strength * 1.8).min(1.0)),
                (2, (chroma_strength * 1.25).min(1.0)),
                (4, ((chroma_strength - 0.2) * 1.25).clamp(0.0, 1.0)),
            ] {
                if amount > 0.0 {
                    // Chroma follows the filtered luma, so edges stay put.
                    builder.blur(channel, planes.luma, planes.second, step);
                    builder.blend(channel, planes.second, amount);
                }
            }
        }
    }
    builder.ycbcr(true);
    builder.passes
}

/// Runs the whole edge-aware noise reduction on the GPU, rewriting `rgb`.
///
/// `rgb` is interleaved linear RGB. It is left untouched unless every pass
/// completed, so the caller can fall back to the CPU implementation.
pub(crate) fn noise_reduction(
    rgb: &mut [f32],
    width: usize,
    height: usize,
    luma: f32,
    chroma: f32,
) -> Result<DispatchProfile, String> {
    let pixel_count = width
        .checked_mul(height)
        .ok_or_else(|| "GPU noise reduction size overflow".to_string())?;
    if pixel_count == 0 || rgb.len() != pixel_count * 3 {
        return Err(format!(
            "GPU noise reduction needs {pixel_count} RGB triplets, got {}",
            rgb.len()
        ));
    }
    if width < 3 || height < 3 {
        return Err("GPU noise reduction needs at least three rows and columns".to_string());
    }
    let (planes, scalars) = Planes::new(pixel_count)?;
    let groups = dispatch_groups(pixel_count)?;
    let bytes = byte_size(scalars)?;
    let mut context = locked()?;
    let started = Instant::now();
    let passes = noise_reduction_passes(
        NoisePipelines::of(&context),
        width,
        height,
        planes,
        luma,
        chroma,
    );
    unsafe { record_and_run(&mut context, rgb, bytes, groups, &passes) }?;
    Ok(DispatchProfile {
        adapter_name: context.adapter_name.clone(),
        milliseconds: started.elapsed().as_secs_f64() * 1000.0,
    })
}

/// Uploads `rgb` into the plane buffer, runs every pass in one submission, and
/// reads the RGB plane back.
unsafe fn record_and_run(
    context: &mut Context,
    rgb: &mut [f32],
    bytes: vk::DeviceSize,
    groups: DispatchGroups,
    passes: &[Pass],
) -> Result<(), String> {
    let properties = context.memory_properties;
    let limit = context.max_storage_buffer_range;
    unsafe {
        context
            .input
            .ensure(&context.device, &properties, bytes, limit)
    }?;
    unsafe { context.input.upload(rgb) };
    bind_input(context);
    unsafe { begin_recording(context) }?;
    let rgb_bytes = byte_size(rgb.len())?;
    if !context.input.unified {
        // Only the RGB plane needs uploading; the rest are written before read.
        let copy = [vk::BufferCopy::default().size(rgb_bytes)];
        let barrier = [buffer_barrier(
            context.input.storage(),
            bytes,
            vk::AccessFlags::TRANSFER_WRITE,
            vk::AccessFlags::SHADER_READ | vk::AccessFlags::SHADER_WRITE,
        )];
        unsafe {
            context.device.cmd_copy_buffer(
                context.command,
                context.input.host,
                context.input.device,
                &copy,
            );
            context.device.cmd_pipeline_barrier(
                context.command,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::COMPUTE_SHADER,
                vk::DependencyFlags::empty(),
                &[],
                &barrier,
                &[],
            );
        }
    }
    unsafe {
        context.device.cmd_bind_descriptor_sets(
            context.command,
            vk::PipelineBindPoint::COMPUTE,
            context.pipeline_layout,
            0,
            &[context.descriptor_set],
            &[],
        );
    }
    // Each pass reads what its predecessor wrote, so they are serialized.
    let between = [buffer_barrier(
        context.input.storage(),
        bytes,
        vk::AccessFlags::SHADER_WRITE,
        vk::AccessFlags::SHADER_READ | vk::AccessFlags::SHADER_WRITE,
    )];
    for (index, pass) in passes.iter().enumerate() {
        if index > 0 {
            unsafe {
                context.device.cmd_pipeline_barrier(
                    context.command,
                    vk::PipelineStageFlags::COMPUTE_SHADER,
                    vk::PipelineStageFlags::COMPUTE_SHADER,
                    vk::DependencyFlags::empty(),
                    &[],
                    &between,
                    &[],
                )
            };
        }
        unsafe {
            context.device.cmd_bind_pipeline(
                context.command,
                vk::PipelineBindPoint::COMPUTE,
                pass.pipeline,
            );
            context.device.cmd_push_constants(
                context.command,
                context.pipeline_layout,
                vk::ShaderStageFlags::COMPUTE,
                0,
                pass.parameters.bytes(),
            );
            context
                .device
                .cmd_dispatch(context.command, groups.x, groups.y, 1);
        }
    }
    unsafe { finish_recording(context, rgb_bytes) }?;
    unsafe { submit_and_wait(context) }?;
    unsafe { context.input.download(rgb) };
    Ok(())
}

/// Uploads `pixels`, runs the stage over them, and reads the result back over
/// them. `pixels` is left untouched unless the dispatch completed.
unsafe fn dispatch(
    context: &mut Context,
    parameters: Parameters,
    pixels: &mut [f32],
) -> Result<(), String> {
    let groups = dispatch_groups(parameters.counts[0] as usize)?;
    let bytes = byte_size(pixels.len())?;
    let passes = [Pass {
        pipeline: context.tone_transfer,
        parameters,
    }];
    unsafe { record_and_run(context, pixels, bytes, groups, &passes) }
}

fn bind_input(context: &Context) {
    let buffer_info = [vk::DescriptorBufferInfo::default()
        .buffer(context.input.storage())
        .range(context.input.capacity)];
    let writes = [vk::WriteDescriptorSet::default()
        .dst_set(context.descriptor_set)
        .dst_binding(0)
        .descriptor_type(vk::DescriptorType::STORAGE_BUFFER)
        .buffer_info(&buffer_info)];
    unsafe { context.device.update_descriptor_sets(&writes, &[]) };
}

unsafe fn begin_recording(context: &Context) -> Result<(), String> {
    unsafe {
        context
            .device
            .reset_command_buffer(context.command, vk::CommandBufferResetFlags::empty())
    }
    .map_err(|error| vk_error("command buffer reset", error))?;
    let begin =
        vk::CommandBufferBeginInfo::default().flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT);
    unsafe { context.device.begin_command_buffer(context.command, &begin) }
        .map_err(|error| vk_error("command recording begin", error))
}

/// Makes the leading `bytes` of the storage buffer readable by the host, then
/// closes the command buffer.
unsafe fn finish_recording(context: &Context, bytes: vk::DeviceSize) -> Result<(), String> {
    let slot = &context.input;
    if slot.unified {
        let barrier = [buffer_barrier(
            slot.storage(),
            bytes,
            vk::AccessFlags::SHADER_WRITE,
            vk::AccessFlags::HOST_READ,
        )];
        unsafe {
            context.device.cmd_pipeline_barrier(
                context.command,
                vk::PipelineStageFlags::COMPUTE_SHADER,
                vk::PipelineStageFlags::HOST,
                vk::DependencyFlags::empty(),
                &[],
                &barrier,
                &[],
            )
        };
    } else {
        let barrier = [buffer_barrier(
            slot.storage(),
            bytes,
            vk::AccessFlags::SHADER_WRITE,
            vk::AccessFlags::TRANSFER_READ,
        )];
        let copy = [vk::BufferCopy::default().size(bytes)];
        unsafe {
            context.device.cmd_pipeline_barrier(
                context.command,
                vk::PipelineStageFlags::COMPUTE_SHADER,
                vk::PipelineStageFlags::TRANSFER,
                vk::DependencyFlags::empty(),
                &[],
                &barrier,
                &[],
            );
            context
                .device
                .cmd_copy_buffer(context.command, slot.device, slot.host, &copy);
        }
    }
    unsafe { context.device.end_command_buffer(context.command) }
        .map_err(|error| vk_error("command recording end", error))
}

unsafe fn submit_and_wait(context: &Context) -> Result<(), String> {
    unsafe { context.device.reset_fences(&[context.fence]) }
        .map_err(|error| vk_error("fence reset", error))?;
    let commands = [context.command];
    let submit = [vk::SubmitInfo::default().command_buffers(&commands)];
    unsafe {
        context
            .device
            .queue_submit(context.queue, &submit, context.fence)
    }
    .map_err(|error| vk_error("queue submission", error))?;
    if let Err(error) = unsafe {
        context
            .device
            .wait_for_fences(&[context.fence], true, 30_000_000_000)
    } {
        // Do not reuse resources which may still be in flight.
        let _ = unsafe { context.device.device_wait_idle() };
        return Err(vk_error("dispatch wait", error));
    }
    Ok(())
}

fn buffer_barrier(
    buffer: vk::Buffer,
    size: vk::DeviceSize,
    source: vk::AccessFlags,
    destination: vk::AccessFlags,
) -> vk::BufferMemoryBarrier<'static> {
    vk::BufferMemoryBarrier::default()
        .src_access_mask(source)
        .dst_access_mask(destination)
        .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
        .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
        .buffer(buffer)
        .size(size)
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            let _ = self.device.device_wait_idle();
            self.input.destroy(&self.device);
            self.device.destroy_fence(self.fence, None);
            self.device
                .free_command_buffers(self.command_pool, &[self.command]);
            self.device
                .destroy_descriptor_pool(self.descriptor_pool, None);
            for pipeline in [
                self.tone_transfer,
                self.nr_ycbcr,
                self.nr_bilateral,
                self.nr_median,
                self.nr_combine,
                self.nr_conv,
            ] {
                self.device.destroy_pipeline(pipeline, None);
            }
            self.device
                .destroy_pipeline_layout(self.pipeline_layout, None);
            self.device
                .destroy_descriptor_set_layout(self.descriptor_set_layout, None);
            self.device.destroy_command_pool(self.command_pool, None);
            self.device.destroy_device(None);
            self.instance.destroy_instance(None);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tone::apply_default_display_tone;

    fn cpu_stage(rgb: &mut [f32]) {
        apply_default_display_tone(rgb);
        for value in rgb {
            *value = if *value <= 0.003_130_8 {
                12.92 * *value
            } else {
                1.055 * value.max(0.0).powf(1.0 / 2.4) - 0.055
            };
        }
    }

    /// Passes a run would record, for asserting the sequence without a device.
    fn pass_count(luma: f32, chroma: f32) -> usize {
        let (planes, _) = Planes::new(64).unwrap();
        noise_reduction_passes(NoisePipelines::default(), 8, 8, planes, luma, chroma).len()
    }

    fn gpu_testing_requested() -> bool {
        std::env::var_os("ORFEUS_GPU_TEST").as_deref() == Some(std::ffi::OsStr::new("1"))
    }

    #[test]
    fn software_rasterizers_only_count_when_asked_for() {
        use std::ffi::OsStr;
        assert!(!software_allowed_value(None, None));
        assert!(!software_allowed_value(Some(OsStr::new("0")), None));
        assert!(software_allowed_value(Some(OsStr::new("1")), None));
        assert!(software_allowed_value(None, Some(OsStr::new("software"))));
        // Real adapters are always usable; a CPU device needs the opt-in.
        assert!(adapter_is_usable(
            vk::PhysicalDeviceType::INTEGRATED_GPU,
            false
        ));
        assert!(adapter_is_usable(
            vk::PhysicalDeviceType::DISCRETE_GPU,
            false
        ));
        assert!(!adapter_is_usable(vk::PhysicalDeviceType::CPU, false));
        assert!(adapter_is_usable(vk::PhysicalDeviceType::CPU, true));
    }

    #[test]
    fn integrated_adapters_win_unless_discrete_is_requested() {
        use std::ffi::OsStr;
        assert_eq!(preference_value(None), AdapterPreference::Integrated);
        assert_eq!(
            preference_value(Some(OsStr::new("discrete"))),
            AdapterPreference::Discrete
        );
        // An opt-out or anything unrecognized keeps the default preference.
        assert_eq!(
            preference_value(Some(OsStr::new("0"))),
            AdapterPreference::Integrated
        );
        for preference in [AdapterPreference::Integrated, AdapterPreference::Discrete] {
            let integrated = adapter_rank(vk::PhysicalDeviceType::INTEGRATED_GPU, preference);
            let discrete = adapter_rank(vk::PhysicalDeviceType::DISCRETE_GPU, preference);
            let other = adapter_rank(vk::PhysicalDeviceType::CPU, preference);
            assert!(other > integrated.max(discrete), "{preference:?}");
            if preference == AdapterPreference::Integrated {
                assert!(integrated < discrete, "integrated should lead");
            } else {
                assert!(discrete < integrated, "discrete should lead");
            }
        }
    }

    #[test]
    fn gpu_is_default_with_an_explicit_numeric_opt_out() {
        assert!(requested_value(None));
        assert!(!requested_value(Some(std::ffi::OsStr::new("0"))));
        assert!(requested_value(Some(std::ffi::OsStr::new("true"))));
        assert!(requested_value(Some(std::ffi::OsStr::new("1"))));
    }

    #[test]
    fn capacity_grows_in_coarse_steps_within_the_adapter_limit() {
        // Anything under one step reserves exactly one step.
        assert_eq!(rounded_capacity(1, 1 << 30), Ok(CAPACITY_GRANULARITY));
        assert_eq!(
            rounded_capacity(CAPACITY_GRANULARITY, 1 << 30),
            Ok(CAPACITY_GRANULARITY)
        );
        assert_eq!(
            rounded_capacity(CAPACITY_GRANULARITY + 1, 1 << 30),
            Ok(2 * CAPACITY_GRANULARITY)
        );
        // A request the adapter cannot serve is an error, but one it can serve
        // is never rounded past the limit.
        assert!(rounded_capacity((1 << 30) + 1, 1 << 30).is_err());
        assert_eq!(rounded_capacity(100, 100), Ok(100));
        // 20 MP of linear RGB reserves within one step of what it needs, where
        // rounding to a power of two would have reserved 256 MB.
        let twenty_megapixels = 20_000_000 * 3 * 4;
        let capacity = rounded_capacity(twenty_megapixels, u32::MAX.into()).unwrap();
        assert!(capacity >= twenty_megapixels);
        assert!(capacity - twenty_megapixels < CAPACITY_GRANULARITY);
    }

    #[test]
    fn dispatch_sizing_is_checked_without_a_gpu() {
        assert!(dispatch_groups(0).is_err());
        assert_eq!(dispatch_groups(256), Ok(DispatchGroups { x: 1, y: 1 }));
        assert_eq!(dispatch_groups(257).unwrap().x, 2);
        // Past one row of groups the dispatch spills onto further rows, which
        // the shaders account for when they rebuild the pixel index.
        let widest = dispatch_groups(u32::MAX as usize).unwrap();
        assert_eq!(widest.x, 65_535);
        assert_eq!(widest.y, 257);
        #[cfg(target_pointer_width = "64")]
        assert!(dispatch_groups(u32::MAX as usize + 1).is_err());
        assert_eq!(
            byte_size(3),
            Ok(3 * std::mem::size_of::<f32>() as vk::DeviceSize)
        );
        #[cfg(target_pointer_width = "64")]
        assert!(byte_size(usize::MAX).is_err());
    }

    #[test]
    fn the_push_constant_block_matches_the_shader_declaration() {
        // Four-component members, within the 128 bytes Vulkan guarantees.
        assert_eq!(std::mem::size_of::<Parameters>(), 4 * 16);
        assert!(std::mem::size_of::<Parameters>() <= 128);
        let parameters = Parameters {
            counts: [7, 3, 0, 0],
            ..Parameters::default()
        };
        assert_eq!(parameters.bytes().len(), std::mem::size_of::<Parameters>());
        assert_eq!(parameters.bytes()[0], 7);
    }

    #[test]
    fn rejected_dispatch_leaves_input_available_for_cpu_fallback() {
        let mut invalid = vec![0.25, 0.5];
        let original = invalid.clone();
        assert!(tone_and_transfer(&mut invalid).is_err());
        assert_eq!(invalid, original);
    }

    #[test]
    fn cpu_reference_is_fused_tone_and_transfer() {
        let mut values = [
            -0.5, 0.5, 0.25, 0.0, 0.0, 0.0, 0.18, 0.18, 0.18, 4.0, 2.0, 1.0,
        ];
        cpu_stage(&mut values);
        assert!(values.iter().all(|value| value.is_finite()));
    }

    #[test]
    fn actual_gpu_matches_cpu_when_explicitly_requested_for_testing() {
        if !gpu_testing_requested() {
            return;
        }
        let mut gpu = vec![0.0, 0.0, 0.0, 0.18, 0.18, 0.18, 4.0, 2.0, 1.0];
        let mut cpu = gpu.clone();
        cpu_stage(&mut cpu);
        tone_and_transfer(&mut gpu).expect("Vulkan dispatch");
        for (actual, expected) in gpu.into_iter().zip(cpu) {
            assert!(
                (actual - expected).abs() <= 2.0e-5,
                "{actual} != {expected}"
            );
        }
    }

    /// The noise reduction is many passes deep, so agreement is checked against
    /// the CPU implementation rather than against a hand-computed expectation.
    #[test]
    fn actual_gpu_noise_reduction_matches_cpu_when_requested_for_testing() {
        if !gpu_testing_requested() {
            return;
        }
        let (width, height) = (61, 43);
        // Structure plus a repeating high-frequency ripple, so edges and noise
        // both exercise the guided weights and the soft thresholds.
        let data: Vec<f32> = (0..width * height * 3)
            .map(|index| {
                let pixel = index / 3;
                let (x, y) = (pixel % width, pixel / width);
                let base = if x > width / 2 { 0.62 } else { 0.14 };
                let ripple = ((index % 7) as f32 - 3.0) * 0.011;
                let slope = y as f32 / height as f32 * 0.2;
                (base + ripple + slope).max(0.0)
            })
            .collect();
        for (luma, chroma) in [(0.6, 0.0), (0.0, 0.5), (0.45, 0.35), (1.0, 1.0)] {
            let mut expected = crate::render::RgbImage {
                width,
                height,
                data: data.clone(),
            };
            crate::render::cpu_noise_reduction(&mut expected, luma, chroma);
            let mut actual = data.clone();
            noise_reduction(&mut actual, width, height, luma, chroma)
                .expect("Vulkan noise reduction dispatch");
            let mut worst = 0.0_f32;
            for (actual, expected) in actual.iter().zip(&expected.data) {
                worst = worst.max((actual - expected).abs());
            }
            // The CPU path sums its taps with FMA in a different order, so the
            // two agree to single-precision rounding rather than bit for bit.
            assert!(
                worst <= 2.0e-4,
                "luma {luma} chroma {chroma} differed by {worst}"
            );
        }
    }

    #[test]
    fn noise_reduction_rejects_sizes_it_cannot_run() {
        // Too small for a 3x3 neighbourhood, and a mismatched buffer length.
        assert!(noise_reduction(&mut [0.0; 3 * 4], 2, 2, 0.5, 0.5).is_err());
        assert!(noise_reduction(&mut [0.0; 10], 4, 4, 0.5, 0.5).is_err());
        assert!(noise_reduction(&mut [], 0, 0, 0.5, 0.5).is_err());
    }

    #[test]
    fn the_plane_layout_leaves_no_overlap() {
        let (planes, total) = Planes::new(100).unwrap();
        assert_eq!(planes.rgb, 0);
        let mut starts = [
            planes.luma,
            planes.blue,
            planes.red,
            planes.first,
            planes.second,
            planes.scratch,
        ];
        starts.sort_unstable();
        // Every plane starts past the interleaved RGB and a whole plane apart.
        assert_eq!(starts[0], 300);
        for pair in starts.windows(2) {
            assert_eq!(pair[1] - pair[0], 100);
        }
        assert_eq!(total, 100 * (3 + Planes::PLANE_COUNT));
        assert_eq!(*starts.last().unwrap() as usize + 100, total);
    }

    /// The pass list is what makes this one submission rather than many, so its
    /// shape is asserted without needing a device.
    #[test]
    fn the_pass_list_follows_the_requested_strengths() {
        assert_eq!(pass_count(0.0, 0.0), 2, "conversions only");
        // Luma adds two separable blurs and one recombination.
        assert_eq!(pass_count(0.5, 0.0), 2 + 5);
        // Each chroma channel adds a median, its blend, and three blurs with
        // their blends; the widest blur only switches on above 0.2.
        assert_eq!(pass_count(0.0, 0.1), 2 + 2 * (2 + 2 * 3));
        assert_eq!(pass_count(0.0, 0.5), 2 + 2 * (2 + 3 * 3));
        assert_eq!(pass_count(0.5, 0.5), 2 + 5 + 2 * (2 + 3 * 3));
    }

    #[test]
    #[ignore = "requires a Vulkan GPU and prints machine-specific timings"]
    fn benchmark_gpu_including_transfer_against_cpu() {
        let pixels = 20_000_000;
        let source: Vec<f32> = (0..pixels * 3)
            .map(|index| 0.01 + (index % 997) as f32 / 400.0)
            .collect();
        let mut cpu = source.clone();
        let started = Instant::now();
        cpu_stage(&mut cpu);
        let cpu_ms = started.elapsed().as_secs_f64() * 1000.0;
        let mut first = source.clone();
        let first_started = Instant::now();
        let first_profile = tone_and_transfer(&mut first).expect("first Vulkan dispatch");
        let first_ms = first_started.elapsed().as_secs_f64() * 1000.0;
        let mut warm_times = Vec::new();
        let mut max_error = 0.0_f32;
        let mut adapter_name = String::new();
        for _ in 0..3 {
            let mut warm = source.clone();
            let warm_profile = tone_and_transfer(&mut warm).expect("warm Vulkan dispatch");
            warm_times.push(warm_profile.milliseconds);
            adapter_name = warm_profile.adapter_name;
            max_error = max_error.max(
                warm.iter()
                    .zip(&cpu)
                    .map(|(gpu, cpu)| (gpu - cpu).abs())
                    .fold(0.0_f32, f32::max),
            );
        }
        warm_times.sort_by(f64::total_cmp);
        eprintln!(
            "tone-transfer-benchmark pixels={pixels} cpu_ms={cpu_ms:.3} gpu_first_with_init_ms={first_ms:.3} gpu_first_dispatch_ms={:.3} gpu_warm_with_transfer_ms={warm_times:?} gpu_warm_median_ms={:.3} adapter={adapter_name:?} max_error={max_error}",
            first_profile.milliseconds, warm_times[1]
        );
        assert!(max_error <= 2.0e-5, "GPU maximum error {max_error}");
    }
}

/// One convolution layer's shape and where its parameters sit in the blob.
#[derive(Clone, Copy, Debug)]
pub(crate) struct LayerSpec {
    pub(crate) input_channels: usize,
    pub(crate) output_channels: usize,
    /// Offset of this layer's weights within the blob, in floats.
    pub(crate) weights: usize,
    /// Offset of this layer's biases within the blob, in floats.
    pub(crate) biases: usize,
}

/// Output channels one `nr_conv` workgroup accumulates; must match GROUP there.
const CONV_CHANNEL_GROUP: u32 = 8;
/// Output pixels one `nr_conv` workgroup covers per axis; must match SPAN there,
/// which is its thread count times the square each thread computes.
const CONV_SPAN: u32 = 32;

/// Where each region of the network's working buffer starts, in floats.
///
/// One allocation, because the renderer binds exactly one storage buffer and
/// addresses everything through push-constant offsets. The input patch and the
/// final output share the front of it: the last layer writes twelve planes back
/// over the thirteen it started from, so the download reads from offset zero and
/// no separate result region is needed.
struct NetworkLayout {
    ping: usize,
    pong: usize,
    weights: usize,
    total: usize,
}

impl NetworkLayout {
    fn new(patch_edge: usize, features: usize, blob: usize) -> Result<Self, String> {
        let patch = patch_edge
            .checked_mul(patch_edge)
            .ok_or_else(|| format!("GPU network patch {patch_edge} is too large"))?;
        let stack = features
            .checked_mul(patch)
            .ok_or_else(|| "GPU network feature stack overflow".to_string())?;
        // The front holds the input patch, which is narrower than a feature
        // stack, so a stack's worth of room covers both it and the result.
        let ping = stack;
        let pong = ping
            .checked_add(stack)
            .ok_or_else(|| "GPU network scratch overflow".to_string())?;
        let weights = pong
            .checked_add(stack)
            .ok_or_else(|| "GPU network scratch overflow".to_string())?;
        let total = weights
            .checked_add(blob)
            .ok_or_else(|| "GPU network buffer overflow".to_string())?;
        Ok(Self {
            ping,
            pong,
            weights,
            total,
        })
    }
}

/// The conv dispatch is genuinely three-dimensional — two spatial axes and one
/// over groups of output channels — where every other stage walks a flat pixel
/// index and so needs only a pair.
#[derive(Debug, Eq, PartialEq)]
struct ConvGroups {
    x: u32,
    y: u32,
    z: u32,
}

fn conv_groups(output_edge: usize, output_channels: usize) -> ConvGroups {
    ConvGroups {
        x: (output_edge as u32).div_ceil(CONV_SPAN),
        y: (output_edge as u32).div_ceil(CONV_SPAN),
        z: (output_channels as u32).div_ceil(CONV_CHANNEL_GROUP),
    }
}

/// Runs every convolution layer over one patch in a single submission.
///
/// `patch` arrives holding the layer-one input planes and leaves holding the
/// final output planes, both at its front. `origin` is the patch's top-left
/// corner in the global feature map, which goes negative while halo remains, and
/// `image` is that map's extent: together they tell the shader which activations
/// lie outside the real image and must return to zero, exactly as the CPU
/// reference re-zeroes them between layers.
pub(crate) fn convolve_network(
    patch: &mut [f32],
    patch_edge: usize,
    origin: (isize, isize),
    image: (usize, usize),
    blob: &[f32],
    layers: &[LayerSpec],
) -> Result<DispatchProfile, String> {
    if layers.is_empty() {
        return Err("GPU network has no layers".to_string());
    }
    let features = layers
        .iter()
        .map(|layer| layer.output_channels.max(layer.input_channels))
        .max()
        .unwrap_or(0);
    if patch_edge < 2 * layers.len() + 1 {
        return Err(format!(
            "GPU network needs a patch wider than {} halo pixels, got {patch_edge}",
            2 * layers.len()
        ));
    }
    let layout = NetworkLayout::new(patch_edge, features, blob.len())?;
    let bytes = byte_size(layout.total)?;
    let mut context = locked()?;
    let started = Instant::now();
    let properties = context.memory_properties;
    let limit = context.max_storage_buffer_range;
    {
        let Context { device, input, .. } = &mut *context;
        unsafe { input.ensure(device, &properties, bytes, limit) }?;
        unsafe {
            input.upload(patch);
            input.upload_at(layout.weights, blob);
        }
    }
    bind_input(&context);
    unsafe { begin_recording(&context) }?;

    // Weights are re-sent per patch. It is 3.4 MB against several hundred
    // milliseconds of arithmetic, so keeping them resident would buy nothing
    // and would need invalidating whenever the buffer grows.
    let uploaded = byte_size(layout.weights + blob.len())?;
    if !context.input.unified {
        let copy = [vk::BufferCopy::default().size(uploaded)];
        let barrier = [buffer_barrier(
            context.input.storage(),
            bytes,
            vk::AccessFlags::TRANSFER_WRITE,
            vk::AccessFlags::SHADER_READ | vk::AccessFlags::SHADER_WRITE,
        )];
        unsafe {
            context.device.cmd_copy_buffer(
                context.command,
                context.input.host,
                context.input.device,
                &copy,
            );
            context.device.cmd_pipeline_barrier(
                context.command,
                vk::PipelineStageFlags::TRANSFER,
                vk::PipelineStageFlags::COMPUTE_SHADER,
                vk::DependencyFlags::empty(),
                &[],
                &barrier,
                &[],
            );
        }
    }
    unsafe {
        context.device.cmd_bind_descriptor_sets(
            context.command,
            vk::PipelineBindPoint::COMPUTE,
            context.pipeline_layout,
            0,
            &[context.descriptor_set],
            &[],
        );
        context.device.cmd_bind_pipeline(
            context.command,
            vk::PipelineBindPoint::COMPUTE,
            context.nr_conv,
        );
    }
    let between = [buffer_barrier(
        context.input.storage(),
        bytes,
        vk::AccessFlags::SHADER_WRITE,
        vk::AccessFlags::SHADER_READ | vk::AccessFlags::SHADER_WRITE,
    )];
    let mut input_edge = patch_edge;
    let mut source = 0_usize;
    let last = layers.len() - 1;
    for (index, layer) in layers.iter().enumerate() {
        let output_edge = input_edge - 2;
        // Alternate between the two scratch stacks, except that the final layer
        // lands back at the front where the download expects it.
        let target = if index == last {
            0
        } else if index % 2 == 0 {
            layout.ping
        } else {
            layout.pong
        };
        let remaining_halo = (last - index) as f32;
        let parameters = Parameters {
            counts: [
                input_edge as u32,
                output_edge as u32,
                layer.input_channels as u32,
                layer.output_channels as u32,
            ],
            offsets: [
                (layout.weights + layer.weights) as u32,
                (layout.weights + layer.biases) as u32,
                source as u32,
                target as u32,
            ],
            scalars: [
                origin.0 as f32 - remaining_halo,
                origin.1 as f32 - remaining_halo,
                0.0,
                0.0,
            ],
            flags: [
                u32::from(index < last),
                image.0 as u32,
                image.1 as u32,
                0,
            ],
        };
        let groups = conv_groups(output_edge, layer.output_channels);
        unsafe {
            if index > 0 {
                context.device.cmd_pipeline_barrier(
                    context.command,
                    vk::PipelineStageFlags::COMPUTE_SHADER,
                    vk::PipelineStageFlags::COMPUTE_SHADER,
                    vk::DependencyFlags::empty(),
                    &[],
                    &between,
                    &[],
                );
            }
            context.device.cmd_push_constants(
                context.command,
                context.pipeline_layout,
                vk::ShaderStageFlags::COMPUTE,
                0,
                parameters.bytes(),
            );
            context
                .device
                .cmd_dispatch(context.command, groups.x, groups.y, groups.z);
        }
        input_edge = output_edge;
        source = target;
    }
    let result_edge = input_edge;
    let result = layers[last]
        .output_channels
        .checked_mul(result_edge * result_edge)
        .ok_or_else(|| "GPU network result overflow".to_string())?;
    unsafe { finish_recording(&context, byte_size(result)?) }?;
    unsafe { submit_and_wait(&context) }?;
    unsafe { context.input.download(&mut patch[..result]) };
    Ok(DispatchProfile {
        adapter_name: context.adapter_name.clone(),
        milliseconds: started.elapsed().as_secs_f64() * 1000.0,
    })
}
