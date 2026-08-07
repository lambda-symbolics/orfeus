//! Neural noise reduction: CPU inference for the FFDNet color denoiser.
//!
//! The network is the pretrained 12-layer FFDNet from KAIR (see
//! `THIRD-PARTY.md`): a 2x pixel-unshuffle, twelve 3x3 convolutions with ReLU
//! between them over 96 feature channels, and a 2x pixel-shuffle back. A
//! uniform noise-level map conditions its strength, which is what the user's
//! single slider drives. Inference tiles the half-resolution feature space and
//! runs the tiles in parallel; inside one tile every convolution runs directly
//! as vectorizable row accumulations without scratch allocation, evaluated
//! valid-only over a halo so tile seams match the whole-image result exactly.

use rayon::prelude::*;
use std::sync::OnceLock;

use super::Error;

const WEIGHTS: &[u8] = include_bytes!("ffdnet_color.bin");
const WEIGHTS_MAGIC: &[u8; 8] = b"ORFEUSNN";
const LAYER_COUNT: usize = 12;
const FEATURES: usize = 96;
const INPUT_CHANNELS: usize = 13;
const OUTPUT_CHANNELS: usize = 12;
const KERNEL: usize = 3;
/// Fallback half-resolution output tile edge; one halo pixel per layer.
const TILE: usize = 192;
/// Tiles a frame should be cut into before their size is allowed to grow.
///
/// A machine-independent number on purpose. Choosing it from the core count
/// would make the denoised result depend on the machine that produced it: tiles
/// agree with an untiled reference to within 3e-6, which is invisible, but a
/// deliverable should not vary at all with where it was rendered.
const MIN_TILES: usize = 16;
const HALO: usize = LAYER_COUNT;
/// Scratch memory allowed for tiles in flight, across all worker threads.
///
/// Tiles are the only parallelism in the network: `convolve_valid` is serial, so
/// the number of tiles in flight is the number of cores doing work. It used to
/// be pinned at four, which left twenty of this machine's twenty-four cores
/// idle and made an 80 MP frame take five minutes. A tile is about 150 floating
/// point operations per byte it moves, so this scales with cores nearly
/// linearly and it is worth spending memory to get them.
const TILE_SCRATCH_BUDGET: usize = 2 << 30;
/// Slider position 1.0 asks the network for this training-domain sigma.
/// Sigma 30/255 already erases the worst high-ISO Micro Four Thirds noise;
/// larger values only smear detail on real photographs.
const MAX_NETWORK_SIGMA: f32 = 30.0 / 255.0;

struct ConvLayer {
    output_channels: usize,
    input_channels: usize,
    /// Row-major [output_channels x input_channels*9], matching im2col rows.
    weights: Vec<f32>,
    bias: Vec<f32>,
}

struct FfdNet {
    layers: Vec<ConvLayer>,
}

fn read_u32(bytes: &[u8], offset: &mut usize) -> Result<u32, String> {
    let end = *offset + 4;
    let value = bytes
        .get(*offset..end)
        .ok_or("truncated weight header")?
        .try_into()
        .map(u32::from_le_bytes)
        .map_err(|_| "truncated weight header")?;
    *offset = end;
    Ok(value)
}

fn read_f32_slice(bytes: &[u8], offset: &mut usize, count: usize) -> Result<Vec<f32>, String> {
    let end = *offset + count * 4;
    let raw = bytes.get(*offset..end).ok_or("truncated weight data")?;
    *offset = end;
    let values: Vec<f32> = raw
        .chunks_exact(4)
        .map(|chunk| f32::from_le_bytes(chunk.try_into().expect("chunked by four")))
        .collect();
    if values.iter().any(|value| !value.is_finite()) {
        return Err("non-finite network weight".into());
    }
    Ok(values)
}

fn parse_network(bytes: &[u8]) -> Result<FfdNet, String> {
    if bytes.len() < 16 || &bytes[..8] != WEIGHTS_MAGIC {
        return Err("embedded FFDNet weights have a wrong magic".into());
    }
    let mut offset = 8;
    if read_u32(bytes, &mut offset)? != 1 {
        return Err("unsupported FFDNet weight format version".into());
    }
    if read_u32(bytes, &mut offset)? as usize != LAYER_COUNT {
        return Err("unexpected FFDNet layer count".into());
    }
    let mut layers = Vec::with_capacity(LAYER_COUNT);
    for index in 0..LAYER_COUNT {
        let output_channels = read_u32(bytes, &mut offset)? as usize;
        let input_channels = read_u32(bytes, &mut offset)? as usize;
        let kernel = read_u32(bytes, &mut offset)? as usize;
        let expected = match index {
            0 => (FEATURES, INPUT_CHANNELS),
            11 => (OUTPUT_CHANNELS, FEATURES),
            _ => (FEATURES, FEATURES),
        };
        if (output_channels, input_channels) != expected || kernel != KERNEL {
            return Err(format!("unexpected FFDNet layer {index} shape"));
        }
        let weights = read_f32_slice(
            bytes,
            &mut offset,
            output_channels * input_channels * KERNEL * KERNEL,
        )?;
        let bias = read_f32_slice(bytes, &mut offset, output_channels)?;
        layers.push(ConvLayer {
            output_channels,
            input_channels,
            weights,
            bias,
        });
    }
    if offset != bytes.len() {
        return Err("trailing bytes after FFDNet weights".into());
    }
    Ok(FfdNet { layers })
}

fn network() -> Result<&'static FfdNet, Error> {
    static NETWORK: OnceLock<Result<FfdNet, String>> = OnceLock::new();
    NETWORK
        .get_or_init(|| parse_network(WEIGHTS))
        .as_ref()
        .map_err(|message| Error::Render(format!("neural noise reduction: {message}")))
}

fn srgb_encode(value: f32) -> f32 {
    if value <= 0.003_130_8 {
        12.92 * value
    } else {
        1.055 * value.powf(1.0 / 2.4) - 0.055
    }
}

fn srgb_decode(value: f32) -> f32 {
    if value <= 0.040_45 {
        value / 12.92
    } else {
        ((value + 0.055) / 1.055).powf(2.4)
    }
}

/// One tile's scratch space, reused across all twelve layers.
struct TileScratch {
    ping: Vec<f32>,
    pong: Vec<f32>,
    row_accumulators: Vec<f32>,
}

impl TileScratch {
    fn new(tile_size: usize) -> Self {
        let patch = tile_size + 2 * HALO;
        Self {
            ping: vec![0.0; FEATURES.max(INPUT_CHANNELS) * patch * patch],
            pong: vec![0.0; FEATURES * (patch - 2) * (patch - 2)],
            row_accumulators: vec![0.0; 3 * (patch - 2)],
        }
    }
}

/// Sweeps all input channels and kernel taps into three L1-resident output
/// accumulator rows. Every input row load feeds three fused multiply-add
/// chains, so the kernel stays arithmetic-bound instead of cache-bound.
macro_rules! accumulate_triple_body {
    ($layer:ident, $input:ident, $size:ident, $out_y:ident, $weight_rows:ident,
     $acc0:ident, $acc1:ident, $acc2:ident) => {
        let count = $acc0.len();
        let plane = $size * $size;
        assert!($acc1.len() == count && $acc2.len() == count && count + 2 <= $size);
        for in_channel in 0..$layer.input_channels {
            for tap_y in 0..KERNEL {
                let input_row = &$input[in_channel * plane + ($out_y + tap_y) * $size..][..$size];
                let offset = in_channel * KERNEL * KERNEL + tap_y * KERNEL;
                let w0 = &$weight_rows[0][offset..][..KERNEL];
                let w1 = &$weight_rows[1][offset..][..KERNEL];
                let w2 = &$weight_rows[2][offset..][..KERNEL];
                for index in 0..count {
                    let r0 = input_row[index];
                    let r1 = input_row[index + 1];
                    let r2 = input_row[index + 2];
                    $acc0[index] += w0[0] * r0 + w0[1] * r1 + w0[2] * r2;
                    $acc1[index] += w1[0] * r0 + w1[1] * r1 + w1[2] * r2;
                    $acc2[index] += w2[0] * r0 + w2[1] * r1 + w2[2] * r2;
                }
            }
        }
    };
}

/// Hand-vectorized AVX2/FMA kernel: three 8-lane accumulator chains per
/// input-row load. The crate itself still targets baseline x86-64; this runs
/// only after runtime feature detection.
#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "avx2", enable = "fma")]
#[allow(clippy::too_many_arguments)]
fn accumulate_triple_fma(
    layer: &ConvLayer,
    input: &[f32],
    size: usize,
    out_y: usize,
    weight_rows: [&[f32]; 3],
    acc0: &mut [f32],
    acc1: &mut [f32],
    acc2: &mut [f32],
) {
    use std::arch::x86_64::{_mm256_fmadd_ps, _mm256_loadu_ps, _mm256_set1_ps, _mm256_storeu_ps};
    let count = acc0.len();
    let plane = size * size;
    assert!(acc1.len() == count && acc2.len() == count && count + 2 <= size);
    for in_channel in 0..layer.input_channels {
        for tap_y in 0..KERNEL {
            let input_row = &input[in_channel * plane + (out_y + tap_y) * size..][..size];
            let offset = in_channel * KERNEL * KERNEL + tap_y * KERNEL;
            let w0 = &weight_rows[0][offset..][..KERNEL];
            let w1 = &weight_rows[1][offset..][..KERNEL];
            let w2 = &weight_rows[2][offset..][..KERNEL];
            // SAFETY: All pointer offsets stay inside the slices checked
            // above; loads/stores are the unaligned AVX variants.
            unsafe {
                let broadcast = |values: &[f32]| {
                    [
                        _mm256_set1_ps(values[0]),
                        _mm256_set1_ps(values[1]),
                        _mm256_set1_ps(values[2]),
                    ]
                };
                let wa = broadcast(w0);
                let wb = broadcast(w1);
                let wc = broadcast(w2);
                let row = input_row.as_ptr();
                let mut index = 0;
                while index + 8 <= count {
                    let r0 = _mm256_loadu_ps(row.add(index));
                    let r1 = _mm256_loadu_ps(row.add(index + 1));
                    let r2 = _mm256_loadu_ps(row.add(index + 2));
                    let mut a = _mm256_loadu_ps(acc0.as_ptr().add(index));
                    let mut b = _mm256_loadu_ps(acc1.as_ptr().add(index));
                    let mut c = _mm256_loadu_ps(acc2.as_ptr().add(index));
                    a = _mm256_fmadd_ps(wa[0], r0, a);
                    b = _mm256_fmadd_ps(wb[0], r0, b);
                    c = _mm256_fmadd_ps(wc[0], r0, c);
                    a = _mm256_fmadd_ps(wa[1], r1, a);
                    b = _mm256_fmadd_ps(wb[1], r1, b);
                    c = _mm256_fmadd_ps(wc[1], r1, c);
                    a = _mm256_fmadd_ps(wa[2], r2, a);
                    b = _mm256_fmadd_ps(wb[2], r2, b);
                    c = _mm256_fmadd_ps(wc[2], r2, c);
                    _mm256_storeu_ps(acc0.as_mut_ptr().add(index), a);
                    _mm256_storeu_ps(acc1.as_mut_ptr().add(index), b);
                    _mm256_storeu_ps(acc2.as_mut_ptr().add(index), c);
                    index += 8;
                }
                for tail in index..count {
                    let r0 = input_row[tail];
                    let r1 = input_row[tail + 1];
                    let r2 = input_row[tail + 2];
                    acc0[tail] += w0[0] * r0 + w0[1] * r1 + w0[2] * r2;
                    acc1[tail] += w1[0] * r0 + w1[1] * r1 + w1[2] * r2;
                    acc2[tail] += w2[0] * r0 + w2[1] * r1 + w2[2] * r2;
                }
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn accumulate_triple_portable(
    layer: &ConvLayer,
    input: &[f32],
    size: usize,
    out_y: usize,
    weight_rows: [&[f32]; 3],
    acc0: &mut [f32],
    acc1: &mut [f32],
    acc2: &mut [f32],
) {
    accumulate_triple_body!(layer, input, size, out_y, weight_rows, acc0, acc1, acc2);
}

pub(crate) fn fma_available() -> bool {
    #[cfg(target_arch = "x86_64")]
    {
        static AVAILABLE: OnceLock<bool> = OnceLock::new();
        *AVAILABLE.get_or_init(|| {
            std::arch::is_x86_feature_detected!("avx2")
                && std::arch::is_x86_feature_detected!("fma")
        })
    }
    #[cfg(not(target_arch = "x86_64"))]
    {
        false
    }
}

/// Valid 3x3 convolution of `input` planes of `size`^2 into `output` planes
/// of `(size-2)`^2, with fused bias and optional ReLU.
///
/// Output channels are processed three at a time: their three accumulator
/// rows stay in L1 while the complete input-channel and tap sweep streams
/// through them. No scratch memory is allocated on this path.
fn convolve_valid(
    layer: &ConvLayer,
    input: &[f32],
    size: usize,
    output: &mut [f32],
    row_accumulators: &mut [f32],
    relu: bool,
) {
    debug_assert_eq!(layer.output_channels % 3, 0);
    let output_size = size - 2;
    let output_plane = output_size * output_size;
    let use_fma = fma_available();
    let weights_per_channel = layer.input_channels * KERNEL * KERNEL;
    let (acc0, rest) = row_accumulators.split_at_mut(output_size);
    let (acc1, rest) = rest.split_at_mut(output_size);
    let (acc2, _) = rest.split_at_mut(output_size);
    for out_y in 0..output_size {
        for triple in 0..layer.output_channels / 3 {
            let channel = triple * 3;
            acc0.fill(layer.bias[channel]);
            acc1.fill(layer.bias[channel + 1]);
            acc2.fill(layer.bias[channel + 2]);
            let weight_rows = [
                &layer.weights[channel * weights_per_channel..][..weights_per_channel],
                &layer.weights[(channel + 1) * weights_per_channel..][..weights_per_channel],
                &layer.weights[(channel + 2) * weights_per_channel..][..weights_per_channel],
            ];
            #[cfg(target_arch = "x86_64")]
            if use_fma {
                // SAFETY: fma_available checked AVX2 and FMA support.
                unsafe {
                    accumulate_triple_fma(layer, input, size, out_y, weight_rows, acc0, acc1, acc2)
                };
            } else {
                accumulate_triple_portable(
                    layer,
                    input,
                    size,
                    out_y,
                    weight_rows,
                    acc0,
                    acc1,
                    acc2,
                );
            }
            #[cfg(not(target_arch = "x86_64"))]
            {
                let _ = use_fma;
                accumulate_triple_portable(
                    layer,
                    input,
                    size,
                    out_y,
                    weight_rows,
                    acc0,
                    acc1,
                    acc2,
                );
            }
            for (offset, accumulator) in [&*acc0, &*acc1, &*acc2].into_iter().enumerate() {
                let target = &mut output[(channel + offset) * output_plane + out_y * output_size..]
                    [..output_size];
                if relu {
                    for (destination, value) in target.iter_mut().zip(accumulator) {
                        *destination = value.max(0.0);
                    }
                } else {
                    target.copy_from_slice(accumulator);
                }
            }
        }
    }
}

/// Copies one input patch (with zero padding beyond the image, matching the
/// network's zero-padded convolutions at real borders) into `patch` planes.
#[allow(clippy::too_many_arguments)]
fn gather_patch(
    planes: &[f32],
    half_width: usize,
    half_height: usize,
    tile_x: usize,
    tile_y: usize,
    sigma: f32,
    patch: &mut [f32],
    patch_size: usize,
) {
    let plane = half_width * half_height;
    let patch_plane = patch_size * patch_size;
    patch[..INPUT_CHANNELS * patch_plane].fill(0.0);
    for channel in 0..OUTPUT_CHANNELS {
        for row in 0..patch_size {
            let source_y = tile_y as isize + row as isize - HALO as isize;
            if source_y < 0 || source_y >= half_height as isize {
                continue;
            }
            let source_x_start = tile_x as isize - HALO as isize;
            let clipped_start = source_x_start.max(0) as usize;
            let clipped_end =
                ((source_x_start + patch_size as isize).min(half_width as isize)).max(0) as usize;
            if clipped_end <= clipped_start {
                continue;
            }
            let target_offset = (clipped_start as isize - source_x_start) as usize;
            let source = channel * plane + source_y as usize * half_width + clipped_start;
            let target = channel * patch_plane + row * patch_size + target_offset;
            patch[target..target + clipped_end - clipped_start]
                .copy_from_slice(&planes[source..source + clipped_end - clipped_start]);
        }
    }
    let sigma_plane = OUTPUT_CHANNELS * patch_plane;
    for row in 0..patch_size {
        let source_y = tile_y as isize + row as isize - HALO as isize;
        if source_y < 0 || source_y >= half_height as isize {
            continue;
        }
        let source_x_start = tile_x as isize - HALO as isize;
        let clipped_start = source_x_start.max(0) as usize;
        let clipped_end =
            ((source_x_start + patch_size as isize).min(half_width as isize)).max(0) as usize;
        if clipped_end <= clipped_start {
            continue;
        }
        let target_offset = (clipped_start as isize - source_x_start) as usize;
        let target = sigma_plane + row * patch_size + target_offset;
        patch[target..target + clipped_end - clipped_start].fill(sigma);
    }
}

#[allow(clippy::too_many_arguments)]
fn zero_outside_global_image(
    data: &mut [f32],
    channels: usize,
    size: usize,
    origin_x: usize,
    origin_y: usize,
    remaining_halo: usize,
    image_width: usize,
    image_height: usize,
) {
    // After each valid convolution, the active patch has shed one halo pixel.
    // Activations outside the real feature-map extent must return to zero before
    // the next layer, exactly matching Conv2d padding=1 at every global border.
    let global_left = origin_x as isize - remaining_halo as isize;
    let global_top = origin_y as isize - remaining_halo as isize;
    let clamp = |value: isize| value.clamp(0, size as isize) as usize;
    let valid_x_start = clamp(-global_left);
    let valid_x_end = clamp(image_width as isize - global_left);
    let valid_y_start = clamp(-global_top);
    let valid_y_end = clamp(image_height as isize - global_top);
    let plane = size * size;

    for channel in 0..channels {
        let channel_data = &mut data[channel * plane..(channel + 1) * plane];
        channel_data[..valid_y_start * size].fill(0.0);
        channel_data[valid_y_end * size..].fill(0.0);
        for row in valid_y_start..valid_y_end {
            let row_data = &mut channel_data[row * size..(row + 1) * size];
            row_data[..valid_x_start].fill(0.0);
            row_data[valid_x_end..].fill(0.0);
        }
    }
}

/// The network's parameters packed for the GPU, with each layer's shape and the
/// offsets of its weights and biases inside the blob.
///
/// Built once. The CPU keeps its weights split per layer because its kernel
/// walks one layer at a time; the shader addresses everything through one
/// storage buffer, so the blob has to be contiguous.
struct GpuNetwork {
    blob: Vec<f32>,
    layers: Vec<super::gpu::LayerSpec>,
}

fn gpu_network() -> Result<&'static GpuNetwork, Error> {
    static PACKED: OnceLock<Result<GpuNetwork, String>> = OnceLock::new();
    PACKED
        .get_or_init(|| {
            let net = network().map_err(|error| error.to_string())?;
            let mut blob = Vec::new();
            let mut layers = Vec::with_capacity(net.layers.len());
            for layer in &net.layers {
                let weights = blob.len();
                blob.extend_from_slice(&layer.weights);
                let biases = blob.len();
                blob.extend_from_slice(&layer.bias);
                layers.push(super::gpu::LayerSpec {
                    input_channels: layer.input_channels,
                    output_channels: layer.output_channels,
                    weights,
                    biases,
                });
            }
            Ok(GpuNetwork { blob, layers })
        })
        .as_ref()
        .map_err(|error| Error::Render(error.clone()))
}

/// Whether to run the network's convolutions on the GPU.
///
/// Off by default until it has been validated on the machines that matter. Every
/// other stage Orfeus offloaded lost to the CPU because it moved as many bytes as
/// it did arithmetic; this one should not, but "should not" is not a measurement.
fn report_gpu_neural_fallback(error: &str) {
    static REPORTED: OnceLock<()> = OnceLock::new();
    REPORTED.get_or_init(|| {
        eprintln!(
            "orfeus: WARNING neural noise reduction fell back to the CPU: {error}"
        );
    });
}

/// Whether to run the network's convolutions on the GPU.
///
/// Only on an adapter with its own memory. Measured on Lukas's laptop at a
/// 1600px preview, interleaved so thermal drift shows as spread: CPU 2143, 2199,
/// 2210 ms against an RTX 2000 Ada's 893, 901, 922 ms — 2.4x. The same shader on
/// the Iris Xe measured 2270 ms against the CPU's 2044, because an integrated
/// adapter shares the memory bus it is competing for. The renderer prefers
/// integrated adapters for every other stage, all of which are transfer-bound,
/// so in practice this engages when `ORFEUS_GPU=discrete` asks for the other one.
///
/// `ORFEUS_GPU_NEURAL=1` forces it on regardless, which is how the software
/// rasterizer runs it under test.
fn gpu_requested() -> bool {
    super::gpu::requested()
        && (std::env::var_os("ORFEUS_GPU_NEURAL").is_some()
            || super::gpu::adapter_is_discrete())
}

/// Bytes of ping-pong scratch one worker needs for a tile of TILE_SIZE.
fn tile_scratch_bytes(tile_size: usize) -> usize {
    let patch = tile_size + 2 * HALO;
    (FEATURES.max(INPUT_CHANNELS) * patch * patch + FEATURES * (patch - 2) * (patch - 2))
        * std::mem::size_of::<f32>()
}

/// How many tiles to keep in flight: every core Rayon offers, unless their
/// scratch would exceed the budget.
fn parallel_tiles(tile_size: usize) -> usize {
    let affordable = TILE_SCRATCH_BUDGET / tile_scratch_bytes(tile_size).max(1);
    rayon::current_num_threads().max(1).min(affordable.max(1))
}

/// Runs every tile's twelve layers on the GPU, one submission per tile.
///
/// Sequential on purpose. The renderer holds one command buffer behind a
/// try-lock, so that a second render falls back to the CPU rather than waiting;
/// dispatching from inside the parallel tile loop meant nineteen of twenty
/// workers were told the device was busy and quietly went back to the CPU. With
/// the arithmetic on the GPU there is nothing for those workers to do anyway.
///
/// Returns an error rather than a partial result, so the caller can fall back to
/// the CPU with the input untouched.
fn infer_tiled_on_gpu(
    gpu: &GpuNetwork,
    planes: &[f32],
    half_width: usize,
    half_height: usize,
    sigma: f32,
    tile_size: usize,
) -> Result<Vec<f32>, String> {
    let half_plane = half_width * half_height;
    let patch_size = tile_size + 2 * HALO;
    let inputs = INPUT_CHANNELS * patch_size * patch_size;
    let mut denoised = vec![0.0_f32; OUTPUT_CHANNELS * half_plane];
    let mut patch = vec![0.0_f32; inputs];
    for tile_y in 0..half_height.div_ceil(tile_size) {
        for tile_x in 0..half_width.div_ceil(tile_size) {
            let origin_x = tile_x * tile_size;
            let origin_y = tile_y * tile_size;
            gather_patch(
                planes,
                half_width,
                half_height,
                origin_x,
                origin_y,
                sigma,
                &mut patch,
                patch_size,
            );
            crate::gpu::convolve_network(
                &mut patch,
                patch_size,
                (origin_x as isize, origin_y as isize),
                (half_width, half_height),
                &gpu.blob,
                &gpu.layers,
            )?;
            // The result arrives as OUTPUT_CHANNELS planes of TILE_SIZE edge at
            // the front of the patch.
            let tile_width = tile_size.min(half_width.saturating_sub(origin_x));
            let tile_height = tile_size.min(half_height.saturating_sub(origin_y));
            for channel in 0..OUTPUT_CHANNELS {
                for row in 0..tile_height {
                    let source = channel * tile_size * tile_size + row * tile_size;
                    let target =
                        channel * half_plane + (origin_y + row) * half_width + origin_x;
                    denoised[target..target + tile_width]
                        .copy_from_slice(&patch[source..source + tile_width]);
                }
            }
        }
    }
    Ok(denoised)
}

fn infer_tiled_with_tile_size(
    net: &FfdNet,
    planes: &[f32],
    half_width: usize,
    half_height: usize,
    sigma: f32,
    tile_size: usize,
) -> Vec<f32> {
    if gpu_requested()
        && let Ok(gpu) = gpu_network()
    {
        match infer_tiled_on_gpu(gpu, planes, half_width, half_height, sigma, tile_size) {
            Ok(denoised) => return denoised,
            Err(error) => report_gpu_neural_fallback(&error),
        }
    }
    let half_plane = half_width * half_height;
    let mut denoised = vec![0.0_f32; OUTPUT_CHANNELS * half_plane];
    let patch_size = tile_size + 2 * HALO;
    let tiles: Vec<(usize, usize)> = (0..half_height.div_ceil(tile_size))
        .flat_map(|tile_y| (0..half_width.div_ceil(tile_size)).map(move |tile_x| (tile_x, tile_y)))
        .collect();
    // Batched rather than one flat par_iter so the collected tile outputs of a
    // whole 80 MP frame — a second copy of the result — never exist at once.
    for tile_batch in tiles.chunks(parallel_tiles(tile_size)) {
        let tile_results: Vec<((usize, usize), Vec<f32>)> = tile_batch
            .par_iter()
            .map_init(
                || TileScratch::new(tile_size),
                |scratch, &(tile_x, tile_y)| {
                    let origin_x = tile_x * tile_size;
                    let origin_y = tile_y * tile_size;
                    gather_patch(
                        planes,
                        half_width,
                        half_height,
                        origin_x,
                        origin_y,
                        sigma,
                        &mut scratch.ping,
                        patch_size,
                    );
                    let mut size = patch_size;
                    let mut source_is_ping = true;
                    for (index, layer) in net.layers.iter().enumerate() {
                        let relu = index + 1 < net.layers.len();
                        let (input, output) = if source_is_ping {
                            (&scratch.ping, &mut scratch.pong)
                        } else {
                            (&scratch.pong, &mut scratch.ping)
                        };
                        convolve_valid(
                            layer,
                            input,
                            size,
                            output,
                            &mut scratch.row_accumulators,
                            relu,
                        );
                        size -= 2;
                        zero_outside_global_image(
                            output,
                            layer.output_channels,
                            size,
                            origin_x,
                            origin_y,
                            HALO - index - 1,
                            half_width,
                            half_height,
                        );
                        source_is_ping = !source_is_ping;
                    }
                    let result = if source_is_ping {
                        &scratch.ping
                    } else {
                        &scratch.pong
                    };
                    let tile_width = tile_size.min(half_width.saturating_sub(origin_x));
                    let tile_height = tile_size.min(half_height.saturating_sub(origin_y));
                    let mut tile_output = vec![0.0_f32; OUTPUT_CHANNELS * tile_width * tile_height];
                    for channel in 0..OUTPUT_CHANNELS {
                        for row in 0..tile_height {
                            let source = channel * size * size + row * size;
                            let target = channel * tile_width * tile_height + row * tile_width;
                            tile_output[target..target + tile_width]
                                .copy_from_slice(&result[source..source + tile_width]);
                        }
                    }
                    ((tile_x, tile_y), tile_output)
                },
            )
            .collect();
        for ((tile_x, tile_y), tile_output) in tile_results {
            let origin_x = tile_x * tile_size;
            let origin_y = tile_y * tile_size;
            let tile_width = tile_size.min(half_width.saturating_sub(origin_x));
            let tile_height = tile_size.min(half_height.saturating_sub(origin_y));
            for channel in 0..OUTPUT_CHANNELS {
                for row in 0..tile_height {
                    let source = channel * tile_width * tile_height + row * tile_width;
                    let target = channel * half_plane + (origin_y + row) * half_width + origin_x;
                    denoised[target..target + tile_width]
                        .copy_from_slice(&tile_output[source..source + tile_width]);
                }
            }
        }
    }
    denoised
}

/// The tile edge that computes the least arithmetic for this frame.
///
/// A tile always convolves a square patch of `tile + 2 * HALO`, whatever share
/// of it is inside the image, so a nominal 192 tile covering a 32-pixel sliver
/// at the right edge still does the work of a full 216x216 patch. On a 1600px
/// preview — 800x600 at half resolution — five columns of 192 spanned 960 for
/// 800 useful pixels and four rows spanned 768 for 600, so barely half the
/// arithmetic landed on real pixels. Sizes that divide the frame more evenly cut
/// that waste; the floor on tile count keeps them from growing into one tile per
/// core or fewer.
fn best_tile_size(half_width: usize, half_height: usize) -> usize {
    let candidates = (32..=320).step_by(8);
    let cost = |tile: usize| {
        let tiles = half_width.div_ceil(tile) * half_height.div_ceil(tile);
        let patch = tile + 2 * HALO;
        (tiles, tiles * patch * patch)
    };
    let divisible = candidates
        .clone()
        .map(|tile| (cost(tile), tile))
        .filter(|((tiles, _), _)| *tiles >= MIN_TILES)
        .min();
    // A frame too small to cut MIN_TILES ways takes the cheapest size outright.
    divisible
        .or_else(|| candidates.map(|tile| (cost(tile), tile)).min())
        .map_or(TILE, |((_, _), tile)| tile)
}

/// Applies FFDNet color denoising in place.
///
/// `strength` in (0, 1] maps linearly to the network's training-domain noise
/// level, up to sigma 50/255 at full strength. Pixels are processed in the
/// sRGB-encoded [0, 1] range the network was trained on; energy outside that
/// range (unclipped highlights, gamut excursions) passes through unchanged.
pub(crate) fn apply_neural_noise_reduction(
    data: &mut [f32],
    width: usize,
    height: usize,
    strength: f32,
) -> Result<(), Error> {
    if strength <= 0.0 || width < 2 || height < 2 {
        return Ok(());
    }
    let net = network()?;
    let sigma = strength.min(1.0) * MAX_NETWORK_SIGMA;

    // Replication-pad to even dimensions, then pixel-unshuffle into twelve
    // half-resolution planes of sRGB-encoded values, exactly as in training.
    let padded_width = width.next_multiple_of(2);
    let padded_height = height.next_multiple_of(2);
    let half_width = padded_width / 2;
    let half_height = padded_height / 2;
    let half_plane = half_width * half_height;
    let mut planes = vec![0.0_f32; OUTPUT_CHANNELS * half_plane];
    planes
        .par_chunks_mut(half_plane)
        .enumerate()
        .for_each(|(unshuffled, plane)| {
            let channel = unshuffled / 4;
            let sub_y = (unshuffled % 4) / 2;
            let sub_x = unshuffled % 2;
            for (index, value) in plane.iter_mut().enumerate() {
                let source_y = ((index / half_width) * 2 + sub_y).min(height - 1);
                let source_x = ((index % half_width) * 2 + sub_x).min(width - 1);
                let source = data[(source_y * width + source_x) * 3 + channel];
                *value = srgb_encode(source.clamp(0.0, 1.0));
            }
        });

    let started = std::time::Instant::now();
    let tile = best_tile_size(half_width, half_height);
    let denoised = infer_tiled_with_tile_size(net, &planes, half_width, half_height, sigma, tile);
    if std::env::var_os("ORFEUS_PROFILE").is_some() {
        eprintln!(
            "orfeus-profile stage=neural-network half={half_width}x{half_height} \
             tile={tile} tiles={} threads={} milliseconds={:.3}",
            half_width.div_ceil(tile) * half_height.div_ceil(tile),
            rayon::current_num_threads(),
            started.elapsed().as_secs_f64() * 1000.0
        );
    }

    // Pixel-shuffle back to full resolution; energy outside [0, 1] survives.
    data.par_chunks_mut(width * 3)
        .enumerate()
        .for_each(|(y, row)| {
            let sub_y = y % 2;
            let half_row = (y / 2) * half_width;
            for (x, pixel) in row.as_chunks_mut::<3>().0.iter_mut().enumerate() {
                let sub_x = x % 2;
                let half_index = half_row + x / 2;
                for (channel, value) in pixel.iter_mut().enumerate() {
                    let plane = channel * 4 + sub_y * 2 + sub_x;
                    let denoised_value =
                        srgb_decode(denoised[plane * half_plane + half_index].clamp(0.0, 1.0));
                    let out_of_range = *value - value.clamp(0.0, 1.0);
                    *value = denoised_value + out_of_range;
                }
            }
        });
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gathered_sigma_is_zero_outside_the_real_image() {
        let half_width = 3;
        let half_height = 2;
        let patch_size = TILE + 2 * HALO;
        let mut patch = vec![1.0; INPUT_CHANNELS * patch_size * patch_size];
        let planes = vec![0.0; OUTPUT_CHANNELS * half_width * half_height];
        gather_patch(
            &planes,
            half_width,
            half_height,
            0,
            0,
            0.25,
            &mut patch,
            patch_size,
        );
        let sigma = &patch[OUTPUT_CHANNELS * patch_size * patch_size..];
        assert_eq!(sigma[(HALO - 1) * patch_size + HALO], 0.0);
        assert_eq!(sigma[HALO * patch_size + HALO - 1], 0.0);
        assert_eq!(sigma[HALO * patch_size + HALO], 0.25);
        assert_eq!(sigma[(HALO + half_height) * patch_size + HALO], 0.0);
    }

    #[test]
    fn intermediate_padding_is_zeroed_at_every_global_border() {
        let size = 5;
        let mut data = vec![1.0; 2 * size * size];
        zero_outside_global_image(&mut data, 2, size, 0, 0, 2, 3, 3);
        for channel in 0..2 {
            for y in 0..size {
                for x in 0..size {
                    let expected = if x >= 2 && y >= 2 { 1.0 } else { 0.0 };
                    assert_eq!(data[channel * size * size + y * size + x], expected);
                }
            }
        }
    }

    fn infer_same_padded_reference(
        net: &FfdNet,
        planes: &[f32],
        size: usize,
        sigma: f32,
    ) -> Vec<f32> {
        let plane = size * size;
        let mut current = vec![0.0; INPUT_CHANNELS * plane];
        current[..OUTPUT_CHANNELS * plane].copy_from_slice(planes);
        current[OUTPUT_CHANNELS * plane..].fill(sigma);
        for (index, layer) in net.layers.iter().enumerate() {
            let padded_size = size + 2;
            let padded_plane = padded_size * padded_size;
            let mut padded = vec![0.0; layer.input_channels * padded_plane];
            for channel in 0..layer.input_channels {
                for row in 0..size {
                    let source = channel * plane + row * size;
                    let target = channel * padded_plane + (row + 1) * padded_size + 1;
                    padded[target..target + size].copy_from_slice(&current[source..source + size]);
                }
            }
            let mut output = vec![0.0; layer.output_channels * plane];
            let mut row_accumulators = vec![0.0; size * 3];
            convolve_valid(
                layer,
                &padded,
                padded_size,
                &mut output,
                &mut row_accumulators,
                index + 1 < net.layers.len(),
            );
            current = output;
        }
        current
    }

    #[test]
    fn tiled_inference_matches_per_layer_zero_padding_at_borders_and_seams() {
        // Every tile size has to agree with the untiled reference, because the
        // size is chosen per frame now: one that only worked for the old
        // constant would denoise some frames through a different seam pattern.
        let size = 7;
        let plane = size * size;
        let planes: Vec<f32> = (0..OUTPUT_CHANNELS * plane)
            .map(|index| (index % 17) as f32 / 19.0)
            .collect();
        let sigma = 0.07;
        let net = network().unwrap();
        let reference = infer_same_padded_reference(net, &planes, size, sigma);
        //
        // The tolerance covers summation order, not padding: a different tiling
        // accumulates the same products in a different sequence. It is two and a
        // half orders of magnitude below one 8-bit step, so a wrongly padded
        // seam — which shows up as a whole edge of visibly wrong pixels — cannot
        // hide underneath it.
        for tile_size in [1, 2, 3, 4, 6, 7, 9, 32] {
            let tiled = infer_tiled_with_tile_size(net, &planes, size, size, sigma, tile_size);
            let max_error = tiled
                .iter()
                .zip(&reference)
                .map(|(actual, expected)| (actual - expected).abs())
                .fold(0.0_f32, f32::max);
            assert!(
                max_error < 1.0e-5,
                "tile size {tile_size} had border error {max_error}"
            );
        }
    }

    #[test]
    fn gpu_convolutions_match_the_cpu_reference() {
        // Skipped unless a Vulkan device is available. Mesa's lavapipe provides
        // one on machines with no real driver:
        //   VK_DRIVER_FILES=$(nix build --no-link --print-out-paths nixpkgs#mesa)\
        //     /share/vulkan/icd.d/lvp_icd.x86_64.json \
        //   ORFEUS_GPU_TEST=1 cargo test --release gpu_convolutions -- --test-threads=1
        if std::env::var_os("ORFEUS_GPU_TEST").as_deref() != Some(std::ffi::OsStr::new("1")) {
            eprintln!("skipping: set ORFEUS_GPU_TEST=1 with a Vulkan driver");
            return;
        }
        let gpu = gpu_network().expect("the packed network");
        let net = network().unwrap();
        // Three shapes, because the shader masks work three different ways and a
        // single full workgroup in the middle of an image exercises none of them:
        //   - a tile smaller than one workgroup's output span,
        //   - a tile spanning several workgroups with a partial one at the edge,
        //   - a tile at the image corner, where the halo falls outside the real
        //     feature map and has to come back zeroed.
        for (tile, origin, image) in [
            (8_usize, (64_isize, 64_isize), (256_usize, 256_usize)),
            (40, (64, 64), (256, 256)),
            (40, (0, 0), (48, 48)),
        ] {
            let patch_size = tile + 2 * HALO;
            let inputs = INPUT_CHANNELS * patch_size * patch_size;
            let patch: Vec<f32> = (0..inputs)
                .map(|index| ((index % 97) as f32 / 97.0 - 0.5) * 0.7)
                .collect();

            let mut on_gpu = patch.clone();
            crate::gpu::convolve_network(
                &mut on_gpu[..inputs],
                patch_size,
                origin,
                image,
                &gpu.blob,
                &gpu.layers,
            )
            .expect("the network dispatches");

            // The CPU reference, layer by layer over the same patch.
            let mut scratch = TileScratch::new(tile);
            scratch.ping[..inputs].copy_from_slice(&patch);
            let mut size = patch_size;
            let mut source_is_ping = true;
            for (index, layer) in net.layers.iter().enumerate() {
                let relu = index + 1 < net.layers.len();
                let (input, output) = if source_is_ping {
                    (&scratch.ping, &mut scratch.pong)
                } else {
                    (&scratch.pong, &mut scratch.ping)
                };
                convolve_valid(
                    layer,
                    input,
                    size,
                    output,
                    &mut scratch.row_accumulators,
                    relu,
                );
                size -= 2;
                zero_outside_global_image(
                    output,
                    layer.output_channels,
                    size,
                    origin.0 as usize,
                    origin.1 as usize,
                    HALO - index - 1,
                    image.0,
                    image.1,
                );
                source_is_ping = !source_is_ping;
            }
            let reference = if source_is_ping {
                &scratch.ping
            } else {
                &scratch.pong
            };
            let count = OUTPUT_CHANNELS * size * size;
            let worst = on_gpu[..count]
                .iter()
                .zip(&reference[..count])
                .map(|(actual, expected)| (actual - expected).abs())
                .fold(0.0_f32, f32::max);
            // Two buffers of zeros would agree perfectly, so the reference has to
            // be shown to carry signal before its agreement means anything. The
            // corner case zeroes most of its output, hence only a nonzero check.
            let magnitude = reference[..count]
                .iter()
                .map(|value| value.abs())
                .fold(0.0_f32, f32::max);
            assert!(
                magnitude > 1.0e-3,
                "tile {tile} at {origin:?} produced nothing to compare ({magnitude})"
            );
            eprintln!("tile {tile} at {origin:?}: worst {worst}, magnitude {magnitude}");
            // Twelve layers of accumulation in a different order, so exact
            // equality is not on offer; this is far below one 8-bit step.
            assert!(
                worst < 2.0e-4,
                "tile {tile} at {origin:?} differed by {worst}"
            );
        }
    }

    #[test]
    #[ignore = "timing sweep, run with --ignored --nocapture"]
    fn tile_size_timing_sweep() {
        // 800x600 half-resolution planes: what a 1600px preview asks for.
        let (width, height) = (800_usize, 600_usize);
        let plane = width * height;
        let planes: Vec<f32> = (0..OUTPUT_CHANNELS * plane)
            .map(|index| (index % 251) as f32 / 251.0)
            .collect();
        let net = network().unwrap();
        eprintln!("threads={}", rayon::current_num_threads());
        eprintln!("chosen tile={}", best_tile_size(width, height));
        for tile in [64, 96, 128, 160, 192, 224, 256, 320] {
            let tiles = width.div_ceil(tile) * height.div_ceil(tile);
            let patch = tile + 2 * HALO;
            let started = std::time::Instant::now();
            let _ = infer_tiled_with_tile_size(net, &planes, width, height, 0.07, tile);
            eprintln!(
                "tile={tile:4} tiles={tiles:4} patchpx={:9} {:8.1} ms",
                tiles * patch * patch,
                started.elapsed().as_secs_f64() * 1000.0
            );
        }
    }

    #[test]
    fn the_chosen_tile_size_wastes_less_than_the_old_constant() {
        // 800x600 is a 1600px preview at half resolution, the size the panel
        // asks for most, and the case the fixed 192 handled worst.
        let waste = |tile: usize| {
            let patch = tile + 2 * HALO;
            (800_usize).div_ceil(tile) * (600_usize).div_ceil(tile) * patch * patch
        };
        let chosen = best_tile_size(800, 600);
        assert!(
            waste(chosen) < waste(TILE),
            "chose {chosen}, which computes {} patch pixels against {} for {TILE}",
            waste(chosen),
            waste(TILE)
        );
        // Enough tiles to spread across a machine's cores, and never so many
        // that halo dominates.
        let tiles = 800_usize.div_ceil(chosen) * 600_usize.div_ceil(chosen);
        assert!(tiles >= MIN_TILES, "only {tiles} tiles for {chosen}");
        // A frame smaller than one tile still works rather than dividing by zero.
        assert!(best_tile_size(4, 4) >= 1);
    }

    #[test]
    fn embedded_weights_parse_with_expected_shapes() {
        let net = parse_network(WEIGHTS).expect("embedded weights must parse");
        assert_eq!(net.layers.len(), LAYER_COUNT);
        assert_eq!(net.layers[0].input_channels, INPUT_CHANNELS);
        assert_eq!(net.layers[11].output_channels, OUTPUT_CHANNELS);
        assert!(net.layers.iter().all(|layer| {
            layer.weights.len() == layer.output_channels * layer.input_channels * KERNEL * KERNEL
                && layer.bias.len() == layer.output_channels
        }));
    }

    fn hash_noise(x: usize, y: usize, salt: usize) -> f32 {
        let hash = (x * 73 + y * 151 + x * y * 19 + salt * 7919) % 101;
        (hash as f32 / 100.0 - 0.5) * 0.12
    }

    fn noisy_gray_patch(width: usize, height: usize) -> Vec<f32> {
        let mut data = Vec::with_capacity(width * height * 3);
        for y in 0..height {
            for x in 0..width {
                for channel in 0..3 {
                    data.push(0.35 + hash_noise(x, y, channel));
                }
            }
        }
        data
    }

    fn luma_rms(data: &[f32], expected: f32) -> f32 {
        let squared: f32 = data
            .as_chunks::<3>()
            .0
            .iter()
            .map(|pixel| {
                let luma = 0.2126 * pixel[0] + 0.7152 * pixel[1] + 0.0722 * pixel[2];
                (luma - expected).powi(2)
            })
            .sum();
        (squared / (data.len() / 3) as f32).sqrt()
    }

    #[test]
    fn denoising_flattens_a_noisy_patch_and_is_deterministic() {
        let width = 96;
        let height = 64;
        let mut image = noisy_gray_patch(width, height);
        let mut second = image.clone();
        let before = luma_rms(&image, 0.35);
        apply_neural_noise_reduction(&mut image, width, height, 0.8).unwrap();
        apply_neural_noise_reduction(&mut second, width, height, 0.8).unwrap();
        let after = luma_rms(&image, 0.35);
        assert!(
            after < before * 0.45,
            "neural NR left RMS {after} of {before}"
        );
        assert_eq!(image, second, "denoising must be deterministic");
    }

    #[test]
    fn denoising_preserves_a_strong_edge() {
        let width = 96;
        let height = 64;
        let mut image = Vec::with_capacity(width * height * 3);
        for y in 0..height {
            for x in 0..width {
                let base = if x < width / 2 { 0.15 } else { 0.75 };
                for channel in 0..3 {
                    image.push(base + hash_noise(x, y, channel) * 0.5);
                }
            }
        }
        apply_neural_noise_reduction(&mut image, width, height, 0.6).unwrap();
        let row = height / 2;
        let dark = image[(row * width + width / 2 - 4) * 3];
        let bright = image[(row * width + width / 2 + 4) * 3];
        assert!(
            bright - dark > 0.4,
            "edge contrast collapsed: {dark} vs {bright}"
        );
    }

    #[test]
    fn out_of_range_energy_survives_denoising() {
        let width = 64;
        let height = 66;
        let mut image = noisy_gray_patch(width, height);
        image[0] = 3.5;
        image[1] = -0.2;
        apply_neural_noise_reduction(&mut image, width, height, 0.5).unwrap();
        assert!(image[0] > 2.5, "unclipped highlight was crushed");
        let neighbor_green = image[2 * 3 + 1];
        assert!(
            image[1] < neighbor_green - 0.1,
            "negative gamut excursion was discarded: {} vs neighbor {neighbor_green}",
            image[1]
        );
        assert!(image.iter().all(|value| value.is_finite()));
    }

    #[test]
    fn odd_dimensions_round_trip() {
        let width = 33;
        let height = 21;
        let mut image = noisy_gray_patch(width, height);
        apply_neural_noise_reduction(&mut image, width, height, 0.4).unwrap();
        assert_eq!(image.len(), width * height * 3);
        assert!(image.iter().all(|value| value.is_finite()));
    }

    #[test]
    fn zero_strength_is_an_exact_no_op() {
        let width = 16;
        let height = 16;
        let mut image = noisy_gray_patch(width, height);
        let reference = image.clone();
        apply_neural_noise_reduction(&mut image, width, height, 0.0).unwrap();
        assert_eq!(image, reference);
    }
}
