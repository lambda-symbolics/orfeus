//! Direct-Vulkan compute for the fused display-tone and sRGB-transfer stage.
//!
//! The backend is on by default; set `ORFEUS_GPU=0` to force the CPU path, or
//! `ORFEUS_GPU=discrete` to prefer a discrete adapter over an integrated one.
//! Any initialization, allocation, submission, or readback failure leaves the
//! input untouched so the renderer can run the CPU implementation, and a
//! failed initialization is remembered so later renders skip the attempt.

use std::ffi::{CStr, CString};
use std::io::Cursor;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Mutex, OnceLock, TryLockError};
use std::time::Instant;

use ash::{Device, Entry, Instance, vk};

const SHADER: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/tone_transfer.spv"));

struct Context {
    _entry: Entry,
    instance: Instance,
    physical_device: vk::PhysicalDevice,
    device: Device,
    queue: vk::Queue,
    command_pool: vk::CommandPool,
    descriptor_set_layout: vk::DescriptorSetLayout,
    pipeline_layout: vk::PipelineLayout,
    pipeline: vk::Pipeline,
    descriptor_pool: vk::DescriptorPool,
    descriptor_set: vk::DescriptorSet,
    command: vk::CommandBuffer,
    fence: vk::Fence,
    staging: vk::Buffer,
    staging_memory: vk::DeviceMemory,
    staging_mapped: usize,
    storage: vk::Buffer,
    storage_memory: vk::DeviceMemory,
    capacity: vk::DeviceSize,
    max_storage_buffer_range: vk::DeviceSize,
    adapter_name: String,
}

// Queue submission and command-pool allocation require external synchronization.
static CONTEXT: OnceLock<Result<Mutex<Context>, String>> = OnceLock::new();
static INITIALIZING: AtomicBool = AtomicBool::new(false);

pub(crate) struct DispatchProfile {
    pub(crate) adapter_name: String,
    pub(crate) milliseconds: f64,
}

pub(crate) fn requested() -> bool {
    requested_value(std::env::var_os("ORFEUS_GPU").as_deref())
}

fn requested_value(value: Option<&std::ffi::OsStr>) -> bool {
    !value.is_some_and(|value| value == "0")
}

fn context() -> Result<&'static Mutex<Context>, String> {
    if let Some(result) = CONTEXT.get() {
        return result.as_ref().map_err(Clone::clone);
    }
    if INITIALIZING
        .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
        .is_err()
    {
        return Err("Vulkan context is initializing".to_string());
    }
    struct InitializationGuard;
    impl Drop for InitializationGuard {
        fn drop(&mut self) {
            INITIALIZING.store(false, Ordering::Release);
        }
    }
    let _guard = InitializationGuard;
    let _ = CONTEXT.set(initialize().map(Mutex::new));
    CONTEXT
        .get()
        .expect("Vulkan context initialization did not publish a result")
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

fn adapter_kind_name(kind: vk::PhysicalDeviceType) -> &'static str {
    match kind {
        vk::PhysicalDeviceType::INTEGRATED_GPU => "integrated",
        vk::PhysicalDeviceType::DISCRETE_GPU => "discrete",
        vk::PhysicalDeviceType::VIRTUAL_GPU => "virtual",
        vk::PhysicalDeviceType::CPU => "cpu",
        _ => "other",
    }
}

fn initialize() -> Result<Context, String> {
    let preference = preference();
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
        let binding = [vk::DescriptorSetLayoutBinding::default()
            .binding(0)
            .descriptor_type(vk::DescriptorType::STORAGE_BUFFER)
            .descriptor_count(1)
            .stage_flags(vk::ShaderStageFlags::COMPUTE)];
        let descriptor_info = vk::DescriptorSetLayoutCreateInfo::default().bindings(&binding);
        let descriptor_set_layout = device
            .create_descriptor_set_layout(&descriptor_info, None)
            .map_err(|error| vk_error("descriptor layout creation", error))?;
        let set_layouts = [descriptor_set_layout];
        let pipeline_layout_info =
            vk::PipelineLayoutCreateInfo::default().set_layouts(&set_layouts);
        let pipeline_layout = device
            .create_pipeline_layout(&pipeline_layout_info, None)
            .map_err(|error| vk_error("pipeline layout creation", error))?;
        let words = ash::util::read_spv(&mut Cursor::new(SHADER))
            .map_err(|error| format!("read embedded SPIR-V: {error}"))?;
        let module_info = vk::ShaderModuleCreateInfo::default().code(&words);
        let module = device
            .create_shader_module(&module_info, None)
            .map_err(|error| vk_error("shader module creation", error))?;
        let entry_name = c"main";
        let stage = vk::PipelineShaderStageCreateInfo::default()
            .stage(vk::ShaderStageFlags::COMPUTE)
            .module(module)
            .name(entry_name);
        let pipeline_info = [vk::ComputePipelineCreateInfo::default()
            .stage(stage)
            .layout(pipeline_layout)];
        let pipeline_result =
            device.create_compute_pipelines(vk::PipelineCache::null(), &pipeline_info, None);
        device.destroy_shader_module(module, None);
        let pipeline =
            pipeline_result.map_err(|(_, error)| vk_error("compute pipeline creation", error))?[0];
        let pool_size = [vk::DescriptorPoolSize::default()
            .ty(vk::DescriptorType::STORAGE_BUFFER)
            .descriptor_count(1)];
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
            instance,
            physical_device,
            device,
            queue,
            command_pool,
            descriptor_set_layout,
            pipeline_layout,
            pipeline,
            descriptor_pool,
            descriptor_set,
            command,
            fence,
            staging: vk::Buffer::null(),
            staging_memory: vk::DeviceMemory::null(),
            staging_mapped: 0,
            storage: vk::Buffer::null(),
            storage_memory: vk::DeviceMemory::null(),
            capacity: 0,
            max_storage_buffer_range: properties.limits.max_storage_buffer_range.into(),
            adapter_name,
        })
    }
}

unsafe fn create_buffer(
    context: &Context,
    size: vk::DeviceSize,
    usage: vk::BufferUsageFlags,
    required_memory_flags: vk::MemoryPropertyFlags,
    preferred_memory_flags: vk::MemoryPropertyFlags,
) -> Result<(vk::Buffer, vk::DeviceMemory), String> {
    let info = vk::BufferCreateInfo::default()
        .size(size)
        .usage(usage)
        .sharing_mode(vk::SharingMode::EXCLUSIVE);
    let buffer = unsafe { context.device.create_buffer(&info, None) }
        .map_err(|error| vk_error("buffer creation", error))?;
    let requirements = unsafe { context.device.get_buffer_memory_requirements(buffer) };
    let memory_properties = unsafe {
        context
            .instance
            .get_physical_device_memory_properties(context.physical_device)
    };
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
            unsafe { context.device.destroy_buffer(buffer, None) };
            return Err(error);
        }
    };
    let allocation = vk::MemoryAllocateInfo::default()
        .allocation_size(requirements.size)
        .memory_type_index(memory_type);
    let memory = match unsafe { context.device.allocate_memory(&allocation, None) } {
        Ok(memory) => memory,
        Err(error) => {
            unsafe { context.device.destroy_buffer(buffer, None) };
            return Err(vk_error("buffer memory allocation", error));
        }
    };
    if let Err(error) = unsafe { context.device.bind_buffer_memory(buffer, memory, 0) } {
        unsafe {
            context.device.destroy_buffer(buffer, None);
            context.device.free_memory(memory, None);
        }
        return Err(vk_error("buffer memory binding", error));
    }
    Ok((buffer, memory))
}

fn rounded_capacity(
    size: vk::DeviceSize,
    max_storage_buffer_range: vk::DeviceSize,
) -> Result<vk::DeviceSize, String> {
    if size > max_storage_buffer_range {
        return Err(format!(
            "Vulkan storage request {size} exceeds adapter limit {max_storage_buffer_range}"
        ));
    }
    let capacity = size
        .checked_next_power_of_two()
        .ok_or_else(|| format!("Vulkan storage capacity overflow for {size} bytes"))?;
    if capacity > max_storage_buffer_range {
        return Err(format!(
            "Vulkan rounded storage capacity {capacity} exceeds adapter limit {max_storage_buffer_range}"
        ));
    }
    Ok(capacity)
}

#[derive(Debug, Eq, PartialEq)]
struct DispatchSize {
    bytes: vk::DeviceSize,
    groups_x: u32,
    groups_y: u32,
}

fn checked_dispatch_size(rgb_len: usize) -> Result<DispatchSize, String> {
    if rgb_len == 0 || !rgb_len.is_multiple_of(3) {
        return Err("GPU RGB input must contain non-empty triplets".to_string());
    }
    let scalar_count = u32::try_from(rgb_len)
        .map_err(|_| format!("GPU RGB input has unsupported scalar count {rgb_len}"))?;
    let pixel_count = scalar_count / 3;
    let group_count = pixel_count.div_ceil(256);
    let groups_x = group_count.min(65_535);
    let groups_y = group_count.div_ceil(65_535);
    if groups_y > 65_535 {
        return Err(format!(
            "GPU RGB input requires unsupported dispatch {groups_x}x{groups_y}"
        ));
    }
    let bytes = vk::DeviceSize::try_from(
        rgb_len
            .checked_mul(std::mem::size_of::<f32>())
            .ok_or_else(|| format!("GPU RGB byte size overflow for {rgb_len} scalars"))?,
    )
    .map_err(|_| format!("GPU RGB byte size is unsupported for {rgb_len} scalars"))?;
    Ok(DispatchSize {
        bytes,
        groups_x,
        groups_y,
    })
}

fn ensure_capacity(context: &mut Context, size: vk::DeviceSize) -> Result<(), String> {
    let capacity = rounded_capacity(size, context.max_storage_buffer_range)?;
    if context.capacity >= size {
        return Ok(());
    }
    let host_flags = vk::MemoryPropertyFlags::HOST_VISIBLE | vk::MemoryPropertyFlags::HOST_COHERENT;
    let (staging, staging_memory) = unsafe {
        create_buffer(
            context,
            capacity,
            vk::BufferUsageFlags::TRANSFER_SRC | vk::BufferUsageFlags::TRANSFER_DST,
            host_flags,
            vk::MemoryPropertyFlags::HOST_CACHED,
        )?
    };
    let (storage, storage_memory) = match unsafe {
        create_buffer(
            context,
            capacity,
            vk::BufferUsageFlags::TRANSFER_SRC
                | vk::BufferUsageFlags::TRANSFER_DST
                | vk::BufferUsageFlags::STORAGE_BUFFER,
            vk::MemoryPropertyFlags::DEVICE_LOCAL,
            vk::MemoryPropertyFlags::empty(),
        )
    } {
        Ok(resources) => resources,
        Err(error) => {
            unsafe {
                context.device.destroy_buffer(staging, None);
                context.device.free_memory(staging_memory, None);
            }
            return Err(error);
        }
    };
    let mapped = match unsafe {
        context
            .device
            .map_memory(staging_memory, 0, capacity, vk::MemoryMapFlags::empty())
    } {
        Ok(mapped) => mapped,
        Err(error) => {
            unsafe {
                context.device.destroy_buffer(storage, None);
                context.device.free_memory(storage_memory, None);
                context.device.destroy_buffer(staging, None);
                context.device.free_memory(staging_memory, None);
            }
            return Err(vk_error("persistent staging mapping", error));
        }
    };

    unsafe {
        if context.capacity != 0 {
            context.device.unmap_memory(context.staging_memory);
            context.device.destroy_buffer(context.storage, None);
            context.device.free_memory(context.storage_memory, None);
            context.device.destroy_buffer(context.staging, None);
            context.device.free_memory(context.staging_memory, None);
        }
    }
    context.staging = staging;
    context.staging_memory = staging_memory;
    context.staging_mapped = mapped as usize;
    context.storage = storage;
    context.storage_memory = storage_memory;
    context.capacity = capacity;

    let buffer_info = [vk::DescriptorBufferInfo::default()
        .buffer(context.storage)
        .range(context.capacity)];
    let writes = [vk::WriteDescriptorSet::default()
        .dst_set(context.descriptor_set)
        .dst_binding(0)
        .descriptor_type(vk::DescriptorType::STORAGE_BUFFER)
        .buffer_info(&buffer_info)];
    unsafe { context.device.update_descriptor_sets(&writes, &[]) };
    Ok(())
}

/// Runs the fused stage and replaces `rgb` only after a successful readback.
pub(crate) fn tone_and_transfer(rgb: &mut [f32]) -> Result<DispatchProfile, String> {
    let dispatch_size = checked_dispatch_size(rgb.len())?;
    let mut context = match context()?.try_lock() {
        Ok(context) => context,
        Err(TryLockError::WouldBlock) => return Err("Vulkan device is busy".to_string()),
        Err(TryLockError::Poisoned(_)) => {
            return Err("Vulkan context lock poisoned".to_string());
        }
    };
    let started = Instant::now();
    let output = unsafe { dispatch(&mut context, rgb, dispatch_size) }?;
    rgb.copy_from_slice(&output);
    Ok(DispatchProfile {
        adapter_name: context.adapter_name.clone(),
        milliseconds: started.elapsed().as_secs_f64() * 1000.0,
    })
}

unsafe fn dispatch(
    context: &mut Context,
    rgb: &[f32],
    dispatch_size: DispatchSize,
) -> Result<Vec<f32>, String> {
    let size = dispatch_size.bytes;
    ensure_capacity(context, size)?;
    unsafe {
        std::ptr::copy_nonoverlapping(
            rgb.as_ptr().cast::<u8>(),
            context.staging_mapped as *mut u8,
            size as usize,
        )
    };

    unsafe {
        context
            .device
            .reset_command_buffer(context.command, vk::CommandBufferResetFlags::empty())
    }
    .map_err(|error| vk_error("command buffer reset", error))?;
    let begin =
        vk::CommandBufferBeginInfo::default().flags(vk::CommandBufferUsageFlags::ONE_TIME_SUBMIT);
    unsafe { context.device.begin_command_buffer(context.command, &begin) }
        .map_err(|error| vk_error("command recording begin", error))?;
    let copy = [vk::BufferCopy::default().size(size)];
    unsafe {
        context
            .device
            .cmd_copy_buffer(context.command, context.staging, context.storage, &copy)
    };
    let barrier = [vk::BufferMemoryBarrier::default()
        .src_access_mask(vk::AccessFlags::TRANSFER_WRITE)
        .dst_access_mask(vk::AccessFlags::SHADER_READ | vk::AccessFlags::SHADER_WRITE)
        .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
        .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
        .buffer(context.storage)
        .size(size)];
    unsafe {
        context.device.cmd_pipeline_barrier(
            context.command,
            vk::PipelineStageFlags::TRANSFER,
            vk::PipelineStageFlags::COMPUTE_SHADER,
            vk::DependencyFlags::empty(),
            &[],
            &barrier,
            &[],
        );
        context.device.cmd_bind_pipeline(
            context.command,
            vk::PipelineBindPoint::COMPUTE,
            context.pipeline,
        );
        context.device.cmd_bind_descriptor_sets(
            context.command,
            vk::PipelineBindPoint::COMPUTE,
            context.pipeline_layout,
            0,
            &[context.descriptor_set],
            &[],
        );
    }
    unsafe {
        context.device.cmd_dispatch(
            context.command,
            dispatch_size.groups_x,
            dispatch_size.groups_y,
            1,
        )
    };
    let barrier = [vk::BufferMemoryBarrier::default()
        .src_access_mask(vk::AccessFlags::SHADER_WRITE)
        .dst_access_mask(vk::AccessFlags::TRANSFER_READ)
        .src_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
        .dst_queue_family_index(vk::QUEUE_FAMILY_IGNORED)
        .buffer(context.storage)
        .size(size)];
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
            .cmd_copy_buffer(context.command, context.storage, context.staging, &copy);
        context.device.end_command_buffer(context.command)
    }
    .map_err(|error| vk_error("command recording end", error))?;

    unsafe { context.device.reset_fences(&[context.fence]) }
        .map_err(|error| vk_error("fence reset", error))?;
    let commands = [context.command];
    let submit = [vk::SubmitInfo::default().command_buffers(&commands)];
    if let Err(error) = unsafe {
        context
            .device
            .queue_submit(context.queue, &submit, context.fence)
    } {
        return Err(vk_error("queue submission", error));
    }
    if let Err(error) = unsafe {
        context
            .device
            .wait_for_fences(&[context.fence], true, 10_000_000_000)
    } {
        // Do not reuse resources which may still be in flight.
        let _ = unsafe { context.device.device_wait_idle() };
        return Err(vk_error("dispatch wait", error));
    }

    let mut output = Vec::<f32>::with_capacity(rgb.len());
    unsafe {
        std::ptr::copy_nonoverlapping(
            context.staging_mapped as *const u8,
            output.as_mut_ptr().cast(),
            size as usize,
        );
        output.set_len(rgb.len());
    }
    Ok(output)
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe {
            let _ = self.device.device_wait_idle();
            if self.capacity != 0 {
                self.device.unmap_memory(self.staging_memory);
                self.device.destroy_buffer(self.storage, None);
                self.device.free_memory(self.storage_memory, None);
                self.device.destroy_buffer(self.staging, None);
                self.device.free_memory(self.staging_memory, None);
            }
            self.device.destroy_fence(self.fence, None);
            self.device
                .free_command_buffers(self.command_pool, &[self.command]);
            self.device
                .destroy_descriptor_pool(self.descriptor_pool, None);
            self.device.destroy_pipeline(self.pipeline, None);
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
    fn capacity_rounding_respects_storage_buffer_range() {
        assert_eq!(rounded_capacity(64, 64), Ok(64));
        assert_eq!(rounded_capacity(33, 64), Ok(64));
        assert!(rounded_capacity(129, 128).is_err());
        assert!(rounded_capacity(65, 100).is_err());
        assert!(rounded_capacity(vk::DeviceSize::MAX, vk::DeviceSize::MAX).is_err());
    }

    #[test]
    fn dispatch_sizing_is_checked_without_a_gpu() {
        assert!(checked_dispatch_size(0).is_err());
        assert!(checked_dispatch_size(2).is_err());
        assert_eq!(
            checked_dispatch_size(256 * 3),
            Ok(DispatchSize {
                bytes: (256 * 3 * std::mem::size_of::<f32>()) as vk::DeviceSize,
                groups_x: 1,
                groups_y: 1,
            })
        );
        assert_eq!(checked_dispatch_size(257 * 3).unwrap().groups_x, 2);
        assert_eq!(
            checked_dispatch_size(u32::MAX as usize).unwrap().groups_y,
            86
        );
        #[cfg(target_pointer_width = "64")]
        assert!(checked_dispatch_size(u32::MAX as usize + 3).is_err());
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
        if std::env::var_os("ORFEUS_GPU_TEST").as_deref() != Some(std::ffi::OsStr::new("1")) {
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
