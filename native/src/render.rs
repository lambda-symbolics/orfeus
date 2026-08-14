use std::collections::HashSet;
use std::ffi::{CStr, c_char};
use std::fs::{self, File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Read, Seek, Write};
use std::os::unix::fs::MetadataExt;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Condvar, Mutex, OnceLock};
use std::time::{Instant, SystemTime, UNIX_EPOCH};

use image::codecs::tiff::TiffEncoder;
use image::{ColorType, ImageEncoder};
use lensfun::{Camera, Database, Lens, Modifier};
use rawler::decoders::{RawDecodeParams, RawLoader};
use rawler::imgop::Rect;
use rawler::imgop::develop::{Intermediate, ProcessingStep, RawDevelop};
use rawler::pixarray::Color2D;
use rawler::rawimage::{CFAConfig, RawImage, RawImageData, RawPhotometricInterpretation};
use rawler::rawsource::RawSource;
use rayon::prelude::*;
use sha2::{Digest, Sha256};

use super::Error;
use super::color::intermediate_to_linear_srgb;

pub const SETTINGS_VERSION: u32 = 3;
pub const OUTPUT_JPEG: u32 = 1;
pub const OUTPUT_TIFF: u32 = 2;
pub const FLAG_LENS_DISTORTION: u32 = 1;
pub const FLAG_LENS_TCA: u32 = 2;
pub const CACHE_NONE: u32 = 0;
pub const CACHE_USE: u32 = 1;

pub(crate) fn validate_cache_mode(cache_mode: u32) -> Result<(), Error> {
    if matches!(cache_mode, CACHE_NONE | CACHE_USE) {
        Ok(())
    } else {
        Err(Error::InvalidArgument("unsupported decode cache mode"))
    }
}
/// Full-resolution scene-linear images retained for interactive re-rendering.
/// Two slots cover the selected photograph plus its neutral Before preview.
const DECODE_CACHE_CAPACITY: usize = 2;
const KNOWN_FLAGS: u32 = FLAG_LENS_DISTORTION | FLAG_LENS_TCA;
const MAX_LUT_BYTES: u64 = 32 * 1024 * 1024;
const MAX_LUT_SIZE: usize = 65;
const LENS_MAP_ROW_STEP: usize = 16;
const SRGB_ICC: &[u8] = include_bytes!("sRGB-v4.icc");

#[repr(C)]
#[derive(Clone, Copy)]
pub struct RenderSettingsV1 {
    pub struct_size: u32,
    pub version: u32,
    pub flags: u32,
    pub output_format: u32,
    pub kelvin: f32,
    pub tint: f32,
    pub exposure_ev: f32,
    pub chroma_noise_reduction: f32,
    pub luma_noise_reduction: f32,
    pub tone_blacks: f32,
    pub tone_shadows: f32,
    pub tone_dark_mids: f32,
    pub tone_midtones: f32,
    pub tone_light_mids: f32,
    pub tone_highlights: f32,
    pub tone_whites: f32,
    pub lut_strength: f32,
    pub grain_amount: f32,
    pub grain_size: f32,
    pub grain_seed: u64,
    pub max_width: u32,
    pub max_height: u32,
    pub jpeg_quality: u32,
    pub lens_correction_strength: f32,
    pub focal_reducer: f32,
    pub lens_crop_factor: f32,
    pub lut_path: *const c_char,
    pub lens_profile_model: *const c_char,
    pub neural_noise_reduction: f32,
    /// The lens to correct for when the container names none of its own.
    /// Appended, so a caller built against an earlier struct still works.
    pub lens_name: *const c_char,
}

impl Default for RenderSettingsV1 {
    fn default() -> Self {
        Self {
            struct_size: size_of::<Self>() as u32,
            version: SETTINGS_VERSION,
            flags: KNOWN_FLAGS,
            output_format: OUTPUT_JPEG,
            kelvin: 0.0,
            tint: 0.0,
            exposure_ev: 0.0,
            chroma_noise_reduction: 0.0,
            luma_noise_reduction: 0.0,
            tone_blacks: 0.0,
            tone_shadows: 0.0,
            tone_dark_mids: 0.0,
            tone_midtones: 0.0,
            tone_light_mids: 0.0,
            tone_highlights: 0.0,
            tone_whites: 0.0,
            lut_strength: 0.0,
            grain_amount: 0.0,
            grain_size: 1.0,
            grain_seed: 0,
            max_width: 0,
            max_height: 0,
            jpeg_quality: 92,
            lens_correction_strength: 1.0,
            focal_reducer: 1.0,
            lens_crop_factor: 0.0,
            lut_path: std::ptr::null(),
            lens_profile_model: std::ptr::null(),
            neural_noise_reduction: 0.0,
            lens_name: std::ptr::null(),
        }
    }
}

/// Reads an optional caller-provided string out of a render struct.
///
/// # Safety
///
/// The pointer must be null or a NUL-terminated string that outlives the call.
pub(crate) unsafe fn borrowed_c_string<'a>(
    pointer: *const c_char,
    what: &'static str,
) -> Result<Option<&'a str>, Error> {
    if pointer.is_null() {
        return Ok(None);
    }
    let text = unsafe { CStr::from_ptr(pointer) }
        .to_str()
        .map_err(|_| Error::InvalidArgument(what))?;
    Ok((!text.is_empty()).then_some(text))
}

/// The lens to correct for: what the RAW container says, or the caller's answer
/// when it says nothing.
///
/// Rawler names the lens for an ORF but not for the DNG made from that same
/// ORF, where the description lives in a maker note it does not read. Orfeus
/// already asks ExifTool for that description, and ExifTool's decoded name is
/// the very string rawler produces from the ORF — verified on two lenses — so
/// handing it down keeps lens correction working on either container. The
/// container wins when it has an answer, so nothing about an ORF render moves.
pub(crate) fn effective_lens_name<'a>(decoded: &'a str, from_caller: Option<&'a str>) -> &'a str {
    if decoded.is_empty() {
        from_caller.unwrap_or_default()
    } else {
        decoded
    }
}

impl RenderSettingsV1 {
    fn validate(&self) -> Result<(), Error> {
        if self.struct_size < size_of::<Self>() as u32 {
            return Err(Error::InvalidArgument(
                "render settings struct is too small",
            ));
        }
        if self.version != SETTINGS_VERSION {
            return Err(Error::InvalidArgument(
                "unsupported render settings version",
            ));
        }
        if self.flags & !KNOWN_FLAGS != 0 {
            return Err(Error::InvalidArgument(
                "render settings contain unknown flag bits",
            ));
        }
        if !matches!(self.output_format, OUTPUT_JPEG | OUTPUT_TIFF) {
            return Err(Error::InvalidArgument("unsupported output format"));
        }
        for (value, name) in [
            (self.kelvin, "Kelvin"),
            (self.tint, "tint"),
            (self.exposure_ev, "exposure EV"),
            (self.chroma_noise_reduction, "chroma noise reduction"),
            (self.luma_noise_reduction, "luma noise reduction"),
            (self.neural_noise_reduction, "neural noise reduction"),
            (self.tone_blacks, "tone blacks"),
            (self.tone_shadows, "tone shadows"),
            (self.tone_dark_mids, "tone dark mids"),
            (self.tone_midtones, "tone midtones"),
            (self.tone_light_mids, "tone light mids"),
            (self.tone_highlights, "tone highlights"),
            (self.tone_whites, "tone whites"),
            (self.lut_strength, "LUT strength"),
            (self.grain_amount, "grain amount"),
            (self.grain_size, "grain size"),
            (self.lens_correction_strength, "lens correction strength"),
            (self.focal_reducer, "focal reducer"),
            (self.lens_crop_factor, "lens crop factor"),
        ] {
            if !value.is_finite() {
                return Err(Error::InvalidArgument(match name {
                    "Kelvin" => "Kelvin must be finite",
                    "tint" => "tint must be finite",
                    "exposure EV" => "exposure EV must be finite",
                    "chroma noise reduction" => "chroma noise reduction must be finite",
                    "luma noise reduction" => "luma noise reduction must be finite",
                    "neural noise reduction" => "neural noise reduction must be finite",
                    "tone blacks" => "tone blacks must be finite",
                    "tone shadows" => "tone shadows must be finite",
                    "tone dark mids" => "tone dark mids must be finite",
                    "tone midtones" => "tone midtones must be finite",
                    "tone light mids" => "tone light mids must be finite",
                    "tone highlights" => "tone highlights must be finite",
                    "tone whites" => "tone whites must be finite",
                    "LUT strength" => "LUT strength must be finite",
                    "grain amount" => "grain amount must be finite",
                    "grain size" => "grain size must be finite",
                    "lens correction strength" => "lens correction strength must be finite",
                    "focal reducer" => "focal reducer must be finite",
                    _ => "lens crop factor must be finite",
                }));
            }
        }
        if self.kelvin != 0.0 && !(2000.0..=50_000.0).contains(&self.kelvin) {
            return Err(Error::InvalidArgument("Kelvin must be zero or 2000..50000"));
        }
        if !(-20.0..=20.0).contains(&self.tint)
            || !(-10.0..=10.0).contains(&self.exposure_ev)
            || !(0.0..=1.0).contains(&self.chroma_noise_reduction)
            || !(0.0..=1.0).contains(&self.luma_noise_reduction)
            || !(0.0..=1.0).contains(&self.neural_noise_reduction)
            || !(-2.0..=2.0).contains(&self.tone_blacks)
            || !(-2.0..=2.0).contains(&self.tone_shadows)
            || !(-2.0..=2.0).contains(&self.tone_dark_mids)
            || !(-2.0..=2.0).contains(&self.tone_midtones)
            || !(-2.0..=2.0).contains(&self.tone_light_mids)
            || !(-2.0..=2.0).contains(&self.tone_highlights)
            || !(-2.0..=2.0).contains(&self.tone_whites)
            || !(0.0..=1.0).contains(&self.lut_strength)
            || !(0.0..=1.0).contains(&self.grain_amount)
            || !(0.25..=16.0).contains(&self.grain_size)
            || !(0.0..=2.0).contains(&self.lens_correction_strength)
            || !(0.1..=2.0).contains(&self.focal_reducer)
            || self.lens_crop_factor < 0.0
            || self.lens_crop_factor > 10.0
        {
            return Err(Error::InvalidArgument(
                "render setting outside supported range",
            ));
        }
        if !(1..=100).contains(&self.jpeg_quality) {
            return Err(Error::InvalidArgument("JPEG quality must be 1..100"));
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub(crate) struct RgbImage {
    pub(crate) width: usize,
    pub(crate) height: usize,
    pub(crate) data: Vec<f32>,
}

#[derive(Debug)]
pub(crate) struct CubeLut {
    size: usize,
    domain_min: [f32; 3],
    domain_max: [f32; 3],
    values: Vec<[f32; 3]>,
}

impl CubeLut {
    pub(crate) fn read(path: &Path) -> Result<Self, Error> {
        let file = File::open(path)?;
        if file.metadata()?.len() > MAX_LUT_BYTES {
            return Err(Error::Render(".cube file exceeds 32 MiB limit".into()));
        }
        Self::parse(BufReader::new(file))
    }

    fn parse(reader: impl BufRead) -> Result<Self, Error> {
        let mut size = None;
        let mut domain_min = [0.0; 3];
        let mut domain_max = [1.0; 3];
        let mut values = Vec::new();
        for (line_number, line) in reader.lines().enumerate() {
            let raw_line = line?;
            if raw_line.len() > 4096 {
                return Err(Error::Render(format!(
                    ".cube line {} is too long",
                    line_number + 1
                )));
            }
            let line = raw_line.split('#').next().unwrap_or("").trim();
            if line.is_empty() || line.starts_with("TITLE") {
                continue;
            }
            let mut fields = line.split_whitespace();
            let first = fields.next().unwrap_or("");
            match first {
                "LUT_3D_SIZE" => {
                    let parsed = fields
                        .next()
                        .ok_or_else(|| Error::Render("missing LUT size".into()))?
                        .parse::<usize>()
                        .map_err(|_| {
                            Error::Render(format!(
                                "invalid LUT_3D_SIZE on line {}",
                                line_number + 1
                            ))
                        })?;
                    if fields.next().is_some()
                        || !(2..=MAX_LUT_SIZE).contains(&parsed)
                        || size.replace(parsed).is_some()
                    {
                        return Err(Error::Render("invalid or duplicate LUT_3D_SIZE".into()));
                    }
                    values.reserve(parsed * parsed * parsed);
                }
                "DOMAIN_MIN" | "DOMAIN_MAX" => {
                    let triplet = parse_triplet_iter(&mut fields, line_number)?;
                    if first == "DOMAIN_MIN" {
                        domain_min = triplet;
                    } else {
                        domain_max = triplet;
                    }
                }
                token if token.starts_with(char::is_alphabetic) => {
                    return Err(Error::Render(format!(
                        "unsupported .cube directive {token} on line {}",
                        line_number + 1
                    )));
                }
                _ => {
                    let declared = size
                        .ok_or_else(|| Error::Render("LUT_3D_SIZE must precede entries".into()))?;
                    let mut triplet = [parse_number(first, line_number)?, 0.0, 0.0];
                    triplet[1] = fields
                        .next()
                        .ok_or_else(|| {
                            Error::Render(format!("invalid .cube line {}", line_number + 1))
                        })
                        .and_then(|v| parse_number(v, line_number))?;
                    triplet[2] = fields
                        .next()
                        .ok_or_else(|| {
                            Error::Render(format!("invalid .cube line {}", line_number + 1))
                        })
                        .and_then(|v| parse_number(v, line_number))?;
                    if fields.next().is_some() || values.len() >= declared * declared * declared {
                        return Err(Error::Render(format!(
                            "too many values on .cube line {}",
                            line_number + 1
                        )));
                    }
                    values.push(triplet);
                }
            }
        }
        let size = size.ok_or_else(|| Error::Render("missing LUT_3D_SIZE".into()))?;
        let expected = size * size * size;
        if values.len() != expected {
            return Err(Error::Render(format!(
                "LUT contains {} entries, expected {expected}",
                values.len()
            )));
        }
        if (0..3).any(|channel| domain_max[channel] <= domain_min[channel]) {
            return Err(Error::Render(
                "LUT domain maximum must exceed minimum".into(),
            ));
        }
        Ok(Self {
            size,
            domain_min,
            domain_max,
            values,
        })
    }

    fn sample(&self, rgb: [f32; 3]) -> [f32; 3] {
        let mut base = [0usize; 3];
        let mut frac = [0.0; 3];
        for channel in 0..3 {
            let normalized = ((rgb[channel] - self.domain_min[channel])
                / (self.domain_max[channel] - self.domain_min[channel]))
                .clamp(0.0, 1.0)
                * (self.size - 1) as f32;
            base[channel] = (normalized.floor() as usize).min(self.size - 2);
            frac[channel] = normalized - base[channel] as f32;
        }
        let mut out = [0.0; 3];
        for bz in 0..=1 {
            for gy in 0..=1 {
                for rx in 0..=1 {
                    let weight = (if rx == 0 { 1.0 - frac[0] } else { frac[0] })
                        * (if gy == 0 { 1.0 - frac[1] } else { frac[1] })
                        * (if bz == 0 { 1.0 - frac[2] } else { frac[2] });
                    let index = (base[2] + bz) * self.size * self.size
                        + (base[1] + gy) * self.size
                        + base[0]
                        + rx;
                    for (channel, value) in out.iter_mut().enumerate() {
                        *value += self.values[index][channel] * weight;
                    }
                }
            }
        }
        out
    }
}

fn parse_number(value: &str, line: usize) -> Result<f32, Error> {
    let value = value
        .parse::<f32>()
        .map_err(|_| Error::Render(format!("invalid number on .cube line {}", line + 1)))?;
    if value.is_finite() {
        Ok(value)
    } else {
        Err(Error::Render(format!(
            "non-finite number on .cube line {}",
            line + 1
        )))
    }
}

fn parse_triplet_iter<'a>(
    fields: &mut impl Iterator<Item = &'a str>,
    line: usize,
) -> Result<[f32; 3], Error> {
    let mut out = [0.0; 3];
    for value in &mut out {
        *value = parse_number(
            fields
                .next()
                .ok_or_else(|| Error::Render(format!("invalid .cube line {}", line + 1)))?,
            line,
        )?;
    }
    if fields.next().is_some() {
        return Err(Error::Render(format!("invalid .cube line {}", line + 1)));
    }
    Ok(out)
}

pub(crate) fn apply_exposure(image: &mut RgbImage, ev: f32) {
    if ev == 0.0 {
        return;
    }
    let multiplier = 2.0_f32.powf(ev);
    image.data.par_chunks_mut(1 << 14).for_each(|chunk| {
        for value in chunk {
            *value *= multiplier;
        }
    });
}

fn kelvin_xy(kelvin: f32) -> (f32, f32) {
    let t = kelvin as f64;
    let x = if t <= 4000.0 {
        -0.266_123_9e9 / t.powi(3) - 0.234_358e6 / t.powi(2) + 0.877_695_6e3 / t + 0.179_910
    } else {
        -3.025_846_9e9 / t.powi(3) + 2.107_037_9e6 / t.powi(2) + 0.222_634_7e3 / t + 0.240_390
    };
    let y = if t <= 2222.0 {
        -1.106_381_4 * x.powi(3) - 1.348_110_2 * x.powi(2) + 2.185_558_32 * x - 0.202_196_83
    } else if t <= 4000.0 {
        -0.954_947_6 * x.powi(3) - 1.374_185_93 * x.powi(2) + 2.091_370_15 * x - 0.167_488_67
    } else {
        3.081_758 * x.powi(3) - 5.873_386_7 * x.powi(2) + 3.751_129_97 * x - 0.370_014_83
    };
    (x as f32, y as f32)
}

fn mat_vec(matrix: [[f32; 3]; 3], value: [f32; 3]) -> [f32; 3] {
    matrix.map(|row| row[0] * value[0] + row[1] * value[1] + row[2] * value[2])
}

fn mat_mul(left: [[f32; 3]; 3], right: [[f32; 3]; 3]) -> [[f32; 3]; 3] {
    std::array::from_fn(|row| {
        std::array::from_fn(|column| {
            (0..3)
                .map(|inner| left[row][inner] * right[inner][column])
                .sum()
        })
    })
}

/// Chromatically adapts linear sRGB from the requested source white to D65.
/// Camera WB remains rawler's camera-aware default when Kelvin is zero; tint is
/// then a small working-space green/magenta adaptation around D65. A nonzero
/// Kelvin deliberately disables camera WB and adapts the camera-matrix result.
/// Adapts the image as if its neutral had been lit at KELVIN rather than at the
/// temperature the camera balanced for.
///
/// AS_SHOT names that temperature, and the whole point of it is that passing it
/// as KELVIN changes nothing: a control reading "5200 K" on a frame shot at
/// 5200 K has to render exactly what "as shot" renders, or its number is a
/// decoration. Adapting to D65 instead — which is what this did — meant every
/// temperature except 6504 K shifted the picture the moment the control was
/// touched, from a value that had nothing to do with the photograph.
pub(crate) fn apply_white_adaptation(
    image: &mut RgbImage,
    kelvin: f32,
    tint: f32,
    as_shot: Option<f32>,
) {
    if kelvin == 0.0 && tint == 0.0 {
        return;
    }
    let (x, mut y) = if kelvin == 0.0 {
        (0.3127, 0.3290)
    } else {
        kelvin_xy(kelvin)
    };
    y = (y * 2.0_f32.powf(tint * 0.025)).clamp(0.05, 0.9);
    let source_xyz = [x / y, 1.0, (1.0 - x - y) / y];
    // The frame's own neutral when it is known, and D65 when it is not, which
    // is the behaviour a file without usable balance metadata had all along.
    let d65_xyz = match as_shot {
        Some(as_shot) => {
            let (x, y) = kelvin_xy(as_shot);
            [x / y, 1.0, (1.0 - x - y) / y]
        }
        None => [0.95047, 1.0, 1.08883],
    };
    let bradford = [
        [0.8951, 0.2664, -0.1614],
        [-0.7502, 1.7135, 0.0367],
        [0.0389, -0.0685, 1.0296],
    ];
    let inverse = [
        [0.986_992_9, -0.147_054_3, 0.159_962_7],
        [0.432_305_3, 0.518_360_3, 0.049_291_2],
        [-0.008_528_7, 0.040_042_8, 0.968_486_7],
    ];
    let src_lms = mat_vec(bradford, source_xyz);
    let dst_lms = mat_vec(bradford, d65_xyz);
    let rgb_to_xyz = [
        [0.412_456_4, 0.357_576_1, 0.180_437_5],
        [0.212_672_9, 0.715_152_2, 0.072_175],
        [0.019_333_9, 0.119_192, 0.950_304_1],
    ];
    let xyz_to_rgb = [
        [3.240_454_2, -1.537_138_5, -0.498_531_4],
        [-0.969_266, 1.876_010_8, 0.041_556],
        [0.055_643_4, -0.204_025_9, 1.057_225_2],
    ];
    let scale: [[f32; 3]; 3] = std::array::from_fn(|row| {
        std::array::from_fn(|column| {
            if row == column {
                dst_lms[row] / src_lms[row]
            } else {
                0.0
            }
        })
    });
    let adaptation = mat_mul(
        xyz_to_rgb,
        mat_mul(inverse, mat_mul(scale, mat_mul(bradford, rgb_to_xyz))),
    );
    image.data.par_chunks_mut(3 * 8192).for_each(|chunk| {
        for pixel in chunk.as_chunks_mut::<3>().0 {
            *pixel = mat_vec(adaptation, *pixel);
        }
    });
}

fn tone_adjustment_ev(luminance: f32, adjustments: &[f32; 7]) -> f32 {
    if !luminance.is_finite() || luminance <= 0.0 {
        return 0.0;
    }
    let position = (luminance / 0.18).log2().clamp(-6.0, 6.0);
    let interval = ((position + 6.0) * 0.5).floor().clamp(0.0, 5.0) as usize;
    let t = ((position - (-6.0 + interval as f32 * 2.0)) * 0.5).clamp(0.0, 1.0);
    let smooth = t * t * (3.0 - 2.0 * t);
    adjustments[interval] * (1.0 - smooth) + adjustments[interval + 1] * smooth
}

pub(crate) fn apply_tonal_equalizer(image: &mut RgbImage, adjustments: [f32; 7]) {
    if adjustments.iter().all(|adjustment| *adjustment == 0.0) {
        return;
    }
    image.data.par_chunks_mut(3 * 8192).for_each(|chunk| {
        for pixel in chunk.as_chunks_mut::<3>().0 {
            let luminance = 0.212_672_9 * pixel[0] + 0.715_152_2 * pixel[1] + 0.072_175 * pixel[2];
            let gain = tone_adjustment_ev(luminance, &adjustments).exp2();
            for value in pixel.iter_mut() {
                *value *= gain;
            }
        }
    });
}

/// One bilateral tap accumulated eight pixels wide: SUM/WEIGHT accumulate
/// `kernel / (1 + (guide - center)^2 * inverse_sigma^2)` weighted values.
#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "avx2", enable = "fma")]
#[allow(clippy::too_many_arguments)]
unsafe fn bilateral_span_avx(
    output: &mut [f32],
    guide_center: *const f32,
    tap_sources: [*const f32; 5],
    tap_guides: [*const f32; 5],
) {
    use std::arch::x86_64::{
        _mm256_add_ps, _mm256_div_ps, _mm256_fmadd_ps, _mm256_fnmadd_ps, _mm256_loadu_ps,
        _mm256_max_ps, _mm256_mul_ps, _mm256_rcp_ps, _mm256_set1_ps, _mm256_setzero_ps,
        _mm256_sqrt_ps, _mm256_storeu_ps, _mm256_sub_ps,
    };
    let kernel = [1.0_f32, 4.0, 6.0, 4.0, 1.0];
    // SAFETY: The caller guarantees every pointer stays readable for
    // output.len() + 7 lanes; the loop only issues full 8-wide iterations.
    unsafe {
        let ones = _mm256_set1_ps(1.0);
        let two = _mm256_set1_ps(2.0);
        let mut x = 0;
        while x + 8 <= output.len() {
            let center = _mm256_loadu_ps(guide_center.add(x));
            let sigma = _mm256_fmadd_ps(
                _mm256_set1_ps(0.055),
                _mm256_sqrt_ps(_mm256_max_ps(center, _mm256_setzero_ps())),
                _mm256_set1_ps(0.012),
            );
            let inverse = _mm256_div_ps(ones, _mm256_mul_ps(sigma, sigma));
            let mut sum = _mm256_setzero_ps();
            let mut weight_sum = _mm256_setzero_ps();
            for tap in 0..5 {
                let guide_value = _mm256_loadu_ps(tap_guides[tap].add(x));
                let source_value = _mm256_loadu_ps(tap_sources[tap].add(x));
                let distance = _mm256_sub_ps(guide_value, center);
                let denominator = _mm256_fmadd_ps(_mm256_mul_ps(distance, distance), inverse, ones);
                // Refined reciprocal: rcp * (2 - denominator * rcp).
                let rcp = _mm256_rcp_ps(denominator);
                let refined = _mm256_mul_ps(rcp, _mm256_fnmadd_ps(denominator, rcp, two));
                let weight = _mm256_mul_ps(_mm256_set1_ps(kernel[tap]), refined);
                sum = _mm256_fmadd_ps(source_value, weight, sum);
                weight_sum = _mm256_add_ps(weight_sum, weight);
            }
            _mm256_storeu_ps(output.as_mut_ptr().add(x), _mm256_div_ps(sum, weight_sum));
            x += 8;
        }
        // The scalar tail matches the vector math via the same refinement.
        for tail in x..output.len() {
            let center = *guide_center.add(tail);
            let sigma = 0.012 + 0.055 * center.max(0.0).sqrt();
            let inverse = 1.0 / (sigma * sigma);
            let mut sum = 0.0;
            let mut weight_sum = 0.0;
            for tap in 0..5 {
                let guide_value = *tap_guides[tap].add(tail);
                let source_value = *tap_sources[tap].add(tail);
                let distance = guide_value - center;
                let denominator = 1.0 + distance * distance * inverse;
                let rcp = 1.0 / denominator;
                let weight = kernel[tap] * rcp;
                sum += source_value * weight;
                weight_sum += weight;
            }
            *output.get_unchecked_mut(tail) = sum / weight_sum;
        }
    }
}

struct BilateralPass {
    width: usize,
    row: usize,
    stride: isize,
    limit: usize,
    step: usize,
}

fn bilateral_pass_scalar(output: &mut [f32], source: &[f32], guide: &[f32], pass: BilateralPass) {
    // Taps run along either axis: stride 1 walks x, stride width walks y;
    // limit is the axis length used for clamping.
    let BilateralPass {
        width,
        row,
        stride,
        limit,
        step,
    } = pass;
    let kernel = [1.0_f32, 4.0, 6.0, 4.0, 1.0];
    for (x, value) in output.iter_mut().enumerate() {
        let center = guide[row * width + x];
        let sigma = 0.012 + 0.055 * center.max(0.0).sqrt();
        let inverse_sigma_squared = 1.0 / (sigma * sigma);
        let mut sum = 0.0;
        let mut weight_sum = 0.0;
        for (kernel_index, offset) in (-2_isize..=2).enumerate() {
            let along = if stride == 1 { x } else { row };
            let sample =
                (along as isize + offset * step as isize).clamp(0, limit as isize - 1) as usize;
            let index = if stride == 1 {
                row * width + sample
            } else {
                sample * width + x
            };
            let distance = guide[index] - center;
            let range_weight = 1.0 / (1.0 + distance * distance * inverse_sigma_squared);
            let weight = kernel[kernel_index] * range_weight;
            sum += source[index] * weight;
            weight_sum += weight;
        }
        *value = sum / weight_sum;
    }
}

/// Horizontal bilateral pass for one row into ROW-OUTPUT.
fn bilateral_row_horizontal(
    row_output: &mut [f32],
    source: &[f32],
    guide: &[f32],
    width: usize,
    y: usize,
    step: usize,
    use_avx: bool,
) {
    let reach = 2 * step;
    #[cfg(target_arch = "x86_64")]
    if use_avx && width > 2 * reach + 8 {
        let interior = width - 2 * reach;
        let row_base = y * width;
        // SAFETY: Every tap pointer covers `interior` + 7 lanes inside this
        // row because the interior excludes the clamped borders.
        unsafe {
            let taps: [*const f32; 5] =
                std::array::from_fn(|tap| source.as_ptr().add(row_base + tap * step));
            let guides: [*const f32; 5] =
                std::array::from_fn(|tap| guide.as_ptr().add(row_base + tap * step));
            bilateral_span_avx(
                &mut row_output[reach..reach + interior],
                guide.as_ptr().add(row_base + reach),
                taps,
                guides,
            );
        }
        let kernel = [1.0_f32, 4.0, 6.0, 4.0, 1.0];
        for x in (0..reach).chain(reach + interior..width) {
            let center = guide[y * width + x];
            let sigma = 0.012 + 0.055 * center.max(0.0).sqrt();
            let inverse = 1.0 / (sigma * sigma);
            let mut sum = 0.0;
            let mut weight_sum = 0.0;
            for (kernel_index, offset) in (-2_isize..=2).enumerate() {
                let sample_x =
                    (x as isize + offset * step as isize).clamp(0, width as isize - 1) as usize;
                let index = y * width + sample_x;
                let distance = guide[index] - center;
                let weight = kernel[kernel_index] / (1.0 + distance * distance * inverse);
                sum += source[index] * weight;
                weight_sum += weight;
            }
            row_output[x] = sum / weight_sum;
        }
        return;
    }
    let _ = use_avx;
    bilateral_pass_scalar(
        row_output,
        source,
        guide,
        BilateralPass {
            width,
            row: y,
            stride: 1,
            limit: width,
            step,
        },
    );
}

/// Rows processed per band; the fused band keeps the horizontal intermediate
/// resident in cache instead of round-tripping the whole plane through DRAM.
const BILATERAL_BAND_ROWS: usize = 64;

/// Blurs SOURCE along edges in GUIDE, mixing BLEND of the result back over the
/// source as it writes.
///
/// The mix is folded in here rather than run as its own pass because the row
/// has just been written and is still in cache; as a separate traversal it read
/// two planes and wrote a third for every one of the eight mixes a chroma
/// denoise performs.
#[allow(clippy::too_many_arguments)]
fn edge_guided_blur_into(
    source: &[f32],
    guide: &[f32],
    width: usize,
    height: usize,
    step: usize,
    blend: f32,
    horizontal: &mut Vec<f32>,
    output: &mut Vec<f32>,
) {
    let _ = horizontal;
    // Grow to fit without clearing first. Every element is written below, so
    // clearing would only force the whole plane to be re-zeroed on each call
    // and throw away the point of reusing the buffer.
    output.resize(source.len(), 0.0);
    let use_avx = super::nn::fma_available();
    let reach = 2 * step;
    let kernel = [1.0_f32, 4.0, 6.0, 4.0, 1.0];
    output
        .par_chunks_mut(width * BILATERAL_BAND_ROWS)
        .enumerate()
        .for_each_init(Vec::<f32>::new, |band_buffer, (band_index, band_output)| {
            let y0 = band_index * BILATERAL_BAND_ROWS;
            let band_rows = band_output.len() / width;
            let halo_lo = y0.saturating_sub(reach);
            let halo_hi = (y0 + band_rows + reach).min(height);
            let halo_rows = halo_hi - halo_lo;
            // Every row of the band is written by the horizontal pass below.
            band_buffer.resize(halo_rows * width, 0.0);
            // Fused pass one: horizontal blur for the band plus halo.
            for row in 0..halo_rows {
                let y = halo_lo + row;
                bilateral_row_horizontal(
                    &mut band_buffer[row * width..(row + 1) * width],
                    source,
                    guide,
                    width,
                    y,
                    step,
                    use_avx,
                );
            }
            // Fused pass two: vertical blur reading the cached band.
            for row in 0..band_rows {
                let y = y0 + row;
                let row_output = &mut band_output[row * width..(row + 1) * width];
                let tap_rows: [usize; 5] = std::array::from_fn(|tap| {
                    ((y as isize + (tap as isize - 2) * step as isize).clamp(0, height as isize - 1)
                        as usize)
                        - halo_lo
                });
                let mut blurred = false;
                #[cfg(target_arch = "x86_64")]
                if use_avx && width >= 8 {
                    // SAFETY: Tap rows live inside the band buffer, each
                    // a full row of `width` lanes.
                    unsafe {
                        let taps: [*const f32; 5] = std::array::from_fn(|tap| {
                            band_buffer.as_ptr().add(tap_rows[tap] * width)
                        });
                        let guides: [*const f32; 5] = std::array::from_fn(|tap| {
                            guide.as_ptr().add((tap_rows[tap] + halo_lo) * width)
                        });
                        bilateral_span_avx(row_output, guide.as_ptr().add(y * width), taps, guides);
                    }
                    blurred = true;
                }
                let _ = use_avx;
                if !blurred {
                    for (x, value) in row_output.iter_mut().enumerate() {
                        let center = guide[y * width + x];
                        let sigma = 0.012 + 0.055 * center.max(0.0).sqrt();
                        let inverse = 1.0 / (sigma * sigma);
                        let mut sum = 0.0;
                        let mut weight_sum = 0.0;
                        for (kernel_index, tap_row) in tap_rows.iter().enumerate() {
                            let guide_index = (tap_row + halo_lo) * width + x;
                            let distance = guide[guide_index] - center;
                            let weight =
                                kernel[kernel_index] / (1.0 + distance * distance * inverse);
                            sum += band_buffer[tap_row * width + x] * weight;
                            weight_sum += weight;
                        }
                        *value = sum / weight_sum;
                    }
                }
                if blend != 1.0 {
                    let source_row = &source[y * width..(y + 1) * width];
                    for (value, source) in row_output.iter_mut().zip(source_row) {
                        *value = source + (*value - source) * blend;
                    }
                }
            }
        });
}

fn median_of_9(mut samples: [f32; 9]) -> f32 {
    // Paeth's 19-exchange median network; equivalent to sorting and taking [4].
    macro_rules! exchange {
        ($a:expr, $b:expr) => {
            if samples[$b] < samples[$a] {
                samples.swap($a, $b);
            }
        };
    }
    exchange!(1, 2);
    exchange!(4, 5);
    exchange!(7, 8);
    exchange!(0, 1);
    exchange!(3, 4);
    exchange!(6, 7);
    exchange!(1, 2);
    exchange!(4, 5);
    exchange!(7, 8);
    exchange!(0, 3);
    exchange!(5, 8);
    exchange!(4, 7);
    exchange!(3, 6);
    exchange!(1, 4);
    exchange!(2, 5);
    exchange!(4, 7);
    exchange!(4, 2);
    exchange!(6, 4);
    exchange!(4, 2);
    samples[4]
}

/// Paeth's network again, eight pixels at a time.
///
/// The exchanges become min/max pairs, which for finite samples is the same
/// comparison the scalar network makes — a chroma plane holds differences of
/// developed pixels, so it is finite wherever the decode was. Nineteen vector
/// instructions replace nineteen scalar ones per pixel, and the median is the
/// second largest step of a chroma denoise.
#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "avx2")]
unsafe fn median_of_9_avx(mut samples: [std::arch::x86_64::__m256; 9]) -> std::arch::x86_64::__m256 {
    use std::arch::x86_64::{_mm256_max_ps, _mm256_min_ps};
    macro_rules! exchange {
        ($a:expr, $b:expr) => {{
            let low = _mm256_min_ps(samples[$a], samples[$b]);
            let high = _mm256_max_ps(samples[$a], samples[$b]);
            samples[$a] = low;
            samples[$b] = high;
        }};
    }
    exchange!(1, 2);
    exchange!(4, 5);
    exchange!(7, 8);
    exchange!(0, 1);
    exchange!(3, 4);
    exchange!(6, 7);
    exchange!(1, 2);
    exchange!(4, 5);
    exchange!(7, 8);
    exchange!(0, 3);
    exchange!(5, 8);
    exchange!(4, 7);
    exchange!(3, 6);
    exchange!(1, 4);
    exchange!(2, 5);
    exchange!(4, 7);
    exchange!(2, 4);
    exchange!(4, 6);
    exchange!(2, 4);
    samples[4]
}

/// One row's interior, eight pixels at a time. Columns whose neighbourhood
/// would clamp against an edge are left to the scalar path.
#[cfg(target_arch = "x86_64")]
#[target_feature(enable = "avx2")]
unsafe fn median_row_avx(
    output_row: &mut [f32],
    above: *const f32,
    center: *const f32,
    below: *const f32,
    blend: f32,
) -> usize {
    use std::arch::x86_64::{
        _mm256_add_ps, _mm256_loadu_ps, _mm256_mul_ps, _mm256_set1_ps, _mm256_storeu_ps,
        _mm256_sub_ps,
    };
    let width = output_row.len();
    if width < 10 {
        return 1;
    }
    let mixture = _mm256_set1_ps(blend);
    let mut x = 1;
    // SAFETY: Each load reads eight lanes starting at x-1, and x + 8 stays
    // below width - 1, so every tap is inside its own row.
    unsafe {
        while x + 8 < width {
            let samples = [
                _mm256_loadu_ps(above.add(x - 1)),
                _mm256_loadu_ps(above.add(x)),
                _mm256_loadu_ps(above.add(x + 1)),
                _mm256_loadu_ps(center.add(x - 1)),
                _mm256_loadu_ps(center.add(x)),
                _mm256_loadu_ps(center.add(x + 1)),
                _mm256_loadu_ps(below.add(x - 1)),
                _mm256_loadu_ps(below.add(x)),
                _mm256_loadu_ps(below.add(x + 1)),
            ];
            let own = samples[4];
            let median = median_of_9_avx(samples);
            // Multiply and add separately rather than fusing them: an FMA
            // rounds once where the scalar path rounds twice, and the two
            // paths have to agree to the bit.
            let mixed = _mm256_add_ps(
                own,
                _mm256_mul_ps(_mm256_sub_ps(median, own), mixture),
            );
            _mm256_storeu_ps(output_row.as_mut_ptr().add(x), mixed);
            x += 8;
        }
    }
    x
}

fn median_filter_3x3_into(
    source: &[f32],
    width: usize,
    height: usize,
    blend: f32,
    output: &mut Vec<f32>,
) {
    // Written in full below, so no clear; see `edge_guided_blur_into`.
    output.resize(source.len(), 0.0);
    let use_avx = super::nn::fma_available();
    output
        .par_chunks_mut(width)
        .enumerate()
        .for_each(|(y, output_row)| {
            let above = &source[y.saturating_sub(1) * width..];
            let center = &source[y * width..];
            let below = &source[(y + 1).min(height - 1) * width..];
            let mut vectorized = 1;
            #[cfg(target_arch = "x86_64")]
            if use_avx {
                // SAFETY: AVX2 was detected, and each row slice is at least
                // `width` long because `source` holds whole rows.
                vectorized = unsafe {
                    median_row_avx(
                        output_row,
                        above.as_ptr(),
                        center.as_ptr(),
                        below.as_ptr(),
                        blend,
                    )
                };
            }
            let _ = use_avx;
            let scalar = |x: usize, value: &mut f32| {
                let left = x.saturating_sub(1);
                let right = (x + 1).min(width - 1);
                let median = median_of_9([
                    above[left],
                    above[x],
                    above[right],
                    center[left],
                    center[x],
                    center[right],
                    below[left],
                    below[x],
                    below[right],
                ]);
                *value = center[x] + (median - center[x]) * blend;
            };
            // The first column, whatever the vector loop did not reach, and
            // the last column.
            scalar(0, &mut output_row[0]);
            for x in vectorized..width {
                let mut value = 0.0;
                scalar(x, &mut value);
                output_row[x] = value;
            }
        });
}

fn soft_threshold(value: f32, threshold: f32) -> f32 {
    value.signum() * (value.abs() - threshold).max(0.0)
}

/// Edge-aware noise reduction. Set `ORFEUS_GPU_NOISE=1` for the compute path.
///
/// The shaders exist, agree with this implementation, and are the largest stage
/// in an export — but they are off by default because they measured slower. At
/// 20 MP on a 13700H: CPU 354-362 ms, Iris Xe 418-479 ms, RTX 2000 348-414 ms.
///
/// The dispatch itself is competitive (176 ms on the RTX, 310 ms on the Iris Xe
/// against 355 ms of CPU). What sinks it is everything around the dispatch. Two
/// reasons, both fixable, neither fixed here:
///
/// * The CPU version is banded, so each separable blur keeps its horizontal
///   intermediate in cache. The shader writes that intermediate to a full plane
///   and reads it back, adding gigabytes of traffic across the eight blurs. A
///   shared-memory tiled blur would remove it.
/// * Uploading and downloading a 20 MP frame, and reserving the 720 MB of
///   resident planes, costs 170-240 ms per call. That vanishes once the image
///   stays on the device between stages, which is what makes this code worth
///   keeping: under a resident pipeline the dispatch times above beat the CPU
///   outright.
pub(crate) fn apply_noise_reduction(image: &mut RgbImage, luma: f32, chroma: f32) {
    if (luma == 0.0 && chroma == 0.0) || image.width < 3 || image.height < 3 {
        return;
    }
    if gpu_noise_reduction(image, luma, chroma) {
        return;
    }
    cpu_noise_reduction(image, luma, chroma)
}

fn gpu_noise_reduction(image: &mut RgbImage, luma: f32, chroma: f32) -> bool {
    if !super::gpu::requested() || !gpu_noise_reduction_requested() {
        return false;
    }
    let profiling = std::env::var_os("ORFEUS_PROFILE").is_some();
    let (width, height) = (image.width, image.height);
    let attempt = catch_unwind(AssertUnwindSafe(|| {
        super::gpu::noise_reduction(&mut image.data, width, height, luma, chroma)
    }));
    match attempt {
        Ok(Ok(dispatch)) => {
            report_gpu_status_once(
                gpu_active_notice(),
                GpuStatus::Active(&dispatch.adapter_name),
            );
            if profiling {
                eprintln!(
                    "orfeus-profile gpu-stage=noise-reduction adapter={:?} milliseconds={:.3}",
                    dispatch.adapter_name, dispatch.milliseconds
                );
            }
            true
        }
        Ok(Err(error)) => {
            report_gpu_status_once(gpu_fallback_notice(), GpuStatus::Unavailable(&error));
            if profiling {
                eprintln!("orfeus-profile gpu-stage=noise-reduction fallback=cpu error={error:?}");
            }
            false
        }
        Err(_) => {
            report_gpu_status_once(
                gpu_fallback_notice(),
                GpuStatus::Unavailable("the Vulkan backend panicked"),
            );
            if profiling {
                eprintln!("orfeus-profile gpu-stage=noise-reduction fallback=cpu error=panic");
            }
            false
        }
    }
}

fn gpu_noise_reduction_requested() -> bool {
    std::env::var_os("ORFEUS_GPU_NOISE").as_deref() == Some(std::ffi::OsStr::new("1"))
}

pub(crate) fn cpu_noise_reduction(image: &mut RgbImage, luma: f32, chroma: f32) {
    if (luma == 0.0 && chroma == 0.0) || image.width < 3 || image.height < 3 {
        return;
    }
    let profiling = std::env::var_os("ORFEUS_PROFILE").is_some();
    let mut step_started = Instant::now();
    macro_rules! profile_step {
        ($name:expr) => {
            if profiling {
                let now = Instant::now();
                eprintln!(
                    "orfeus-profile noise-step={} milliseconds={:.3}",
                    $name,
                    now.duration_since(step_started).as_secs_f64() * 1000.0
                );
                step_started = now;
            }
        };
    }
    let pixel_count = image.width * image.height;
    // Zero-filled and then overwritten, which wastes a 240 MB memset at export
    // resolution — but measurably less than the alternatives: collecting the
    // three planes through rayon's `unzip` cost 687 ms against 652 ms for this,
    // its collection machinery outweighing the memset it avoids.
    // Colour comes out of the split already halved, along with the luma that
    // guides its filtering. Building those planes at full size only to average
    // them down was two thirds of a gigabyte at 80 MP and two passes over it.
    let (width, height) = (image.width, image.height);
    let (half_width, half_height) = (width.div_ceil(2), height.div_ceil(2));
    let mut yy = vec![0.0_f32; pixel_count];
    let mut cb = vec![0.0_f32; half_width * half_height];
    let mut cr = vec![0.0_f32; half_width * half_height];
    yy.par_chunks_mut(width * 2)
        .zip(cb.par_chunks_mut(half_width))
        .zip(cr.par_chunks_mut(half_width))
        .zip(image.data.par_chunks(3 * width * 2))
        .enumerate()
        .for_each(|(band, (((yy, cb), cr), pixels))| {
            let rows = yy.len() / width;
            let pixels = pixels.as_chunks::<3>().0;
            for (x, pixel) in pixels.iter().enumerate() {
                yy[x] = 0.2126 * pixel[0] + 0.7152 * pixel[1] + 0.0722 * pixel[2];
            }
            let _ = band;
            for (x, (cb, cr)) in cb.iter_mut().zip(cr.iter_mut()).enumerate() {
                // The 2x2 block, with an odd edge repeating its last sample so
                // the average stays an average.
                let left = x * 2;
                let right = (left + 1).min(width - 1);
                let (mut blue, mut red) = (0.0, 0.0);
                for row in 0..2 {
                    let row = row.min(rows - 1);
                    for column in [left, right] {
                        let pixel = pixels[row * width + column];
                        let luma = yy[row * width + column];
                        blue += pixel[2] - luma;
                        red += pixel[0] - luma;
                    }
                }
                *cb = 0.25 * blue;
                *cr = 0.25 * red;
            }
        });

    profile_step!("split");
    let mut scratch = Vec::new();
    let mut filtered = Vec::new();
    let luma_strength = luma.clamp(0.0, 1.0);
    if luma_strength > 0.0 {
        let mut scale_one = Vec::new();
        edge_guided_blur_into(
            &yy,
            &yy,
            image.width,
            image.height,
            1,
            1.0,
            &mut scratch,
            &mut scale_one,
        );
        edge_guided_blur_into(
            &scale_one,
            &yy,
            image.width,
            image.height,
            2,
            1.0,
            &mut scratch,
            &mut filtered,
        );
        yy.par_chunks_mut(8192)
            .zip(scale_one.par_chunks(8192))
            .zip(filtered.par_chunks(8192))
            .for_each(|((values, scale_one), scale_two)| {
                for ((value, scale_one), scale_two) in
                    values.iter_mut().zip(scale_one).zip(scale_two)
                {
                    let threshold = luma_strength * (0.012 + 0.035 * value.max(0.0).sqrt());
                    let fine = soft_threshold(*value - *scale_one, threshold);
                    let coarse = soft_threshold(*scale_one - *scale_two, threshold * 0.45);
                    *value = *scale_two + coarse + fine;
                }
            });
    }

    profile_step!("luma");
    let chroma_strength = chroma.clamp(0.0, 1.0);
    if chroma_strength > 0.0 {
        // Guided by the luma this stage has already cleaned, not the raw
        // plane: a noisy guide reads its own noise as edges and keeps the
        // colour filter from crossing them.
        let mut guide = vec![0.0_f32; half_width * half_height];
        guide
            .par_chunks_mut(half_width)
            .enumerate()
            .for_each(|(y, row)| {
                let top = y * 2;
                let bottom = (top + 1).min(height - 1);
                for (x, value) in row.iter_mut().enumerate() {
                    let left = x * 2;
                    let right = (left + 1).min(width - 1);
                    *value = 0.25
                        * (yy[top * width + left]
                            + yy[top * width + right]
                            + yy[bottom * width + left]
                            + yy[bottom * width + right]);
                }
            });
        // Colour noise is coarse and colour detail is not: a sensor's chroma
        // is interpolated from a quarter of its photosites to begin with, and
        // every codec ever shipped stores it at half resolution. Denoising it
        // there costs a quarter of the work and reaches wider blotches for the
        // same filter, which is the half of this stage that was slowest.
        for small in [&mut cb, &mut cr] {
            // Each stage writes the already-mixed result into `filtered` and
            // then trades buffers with it, so nothing traverses the plane twice.
            median_filter_3x3_into(
                small,
                half_width,
                half_height,
                (chroma_strength * 1.5).min(1.0),
                &mut filtered,
            );
            std::mem::swap(small, &mut filtered);
            profile_step!("chroma-median");
            for (step, amount) in [
                (1, (chroma_strength * 1.8).min(1.0)),
                (2, (chroma_strength * 1.25).min(1.0)),
                (4, ((chroma_strength - 0.2) * 1.25).clamp(0.0, 1.0)),
            ] {
                if amount > 0.0 {
                    edge_guided_blur_into(
                        small,
                        &guide,
                        half_width,
                        half_height,
                        step,
                        amount,
                        &mut scratch,
                        &mut filtered,
                    );
                    std::mem::swap(small, &mut filtered);
                    profile_step!(format!("chroma-blur-{step}"));
                }
            }
        }
    }

    image
        .data
        .par_chunks_mut(3 * width)
        .zip(yy.par_chunks(width))
        .enumerate()
        .for_each(|(y, (pixels, yy))| {
            // Half-resolution samples sit at the centre of each 2x2 block, so
            // a full-resolution pixel lies a quarter or three quarters of the
            // way between two of them.
            let source = (y as f32 - 0.5) * 0.5;
            let top = source.floor().max(0.0) as usize;
            let bottom = (top + 1).min(half_height - 1);
            let vertical = (source - source.floor().max(0.0)).clamp(0.0, 1.0);
            for (x, (pixel, yy)) in pixels.as_chunks_mut::<3>().0.iter_mut().zip(yy).enumerate() {
                let source = (x as f32 - 0.5) * 0.5;
                let left = source.floor().max(0.0) as usize;
                let right = (left + 1).min(half_width - 1);
                let horizontal = (source - source.floor().max(0.0)).clamp(0.0, 1.0);
                let sample = |plane: &[f32]| {
                    let above = plane[top * half_width + left] * (1.0 - horizontal)
                        + plane[top * half_width + right] * horizontal;
                    let below = plane[bottom * half_width + left] * (1.0 - horizontal)
                        + plane[bottom * half_width + right] * horizontal;
                    above * (1.0 - vertical) + below * vertical
                };
                pixel[0] = *yy + sample(&cr);
                pixel[2] = *yy + sample(&cb);
                pixel[1] = (*yy - 0.2126 * pixel[0] - 0.0722 * pixel[2]) / 0.7152;
            }
        });
    profile_step!("merge");
    let _ = step_started;
}

/// Whether a sample coordinate lies inside the image.
#[inline]
pub(crate) fn inside(image: &RgbImage, x: f32, y: f32) -> bool {
    x >= 0.0 && y >= 0.0 && x <= (image.width - 1) as f32 && y <= (image.height - 1) as f32
}

/// Samples all three channels at one point.
///
/// The weights and the four addresses are computed once for the pixel instead
/// of once per channel, and the three channels of a corner are adjacent in
/// memory, so each corner is one cache line rather than three lookups.
#[inline]
pub(crate) fn bilinear_rgb(image: &RgbImage, x: f32, y: f32) -> [f32; 3] {
    if !inside(image, x, y) {
        return [0.0; 3];
    }
    let (x0, y0) = (x.floor() as usize, y.floor() as usize);
    let (x1, y1) = (
        (x0 + 1).min(image.width - 1),
        (y0 + 1).min(image.height - 1),
    );
    let (fx, fy) = (x - x0 as f32, y - y0 as f32);
    let corner = |xx: usize, yy: usize| -> &[f32] {
        let start = (yy * image.width + xx) * 3;
        &image.data[start..start + 3]
    };
    let (top_left, top_right) = (corner(x0, y0), corner(x1, y0));
    let (bottom_left, bottom_right) = (corner(x0, y1), corner(x1, y1));
    std::array::from_fn(|channel| {
        let top = top_left[channel] * (1.0 - fx) + top_right[channel] * fx;
        let bottom = bottom_left[channel] * (1.0 - fx) + bottom_right[channel] * fx;
        top * (1.0 - fy) + bottom * fy
    })
}

pub(crate) fn bilinear(image: &RgbImage, x: f32, y: f32, channel: usize) -> f32 {
    if x < 0.0 || y < 0.0 || x > (image.width - 1) as f32 || y > (image.height - 1) as f32 {
        return 0.0;
    }
    let (x0, y0) = (x.floor() as usize, y.floor() as usize);
    let (x1, y1) = (
        (x0 + 1).min(image.width - 1),
        (y0 + 1).min(image.height - 1),
    );
    let (fx, fy) = (x - x0 as f32, y - y0 as f32);
    let at = |xx, yy| image.data[(yy * image.width + xx) * 3 + channel];
    (at(x0, y0) * (1.0 - fx) + at(x1, y0) * fx) * (1.0 - fy)
        + (at(x0, y1) * (1.0 - fx) + at(x1, y1) * fx) * fy
}

fn lens_database() -> Result<&'static Database, Error> {
    static DATABASE: OnceLock<Result<Database, String>> = OnceLock::new();
    DATABASE
        .get_or_init(|| Database::load_bundled().map_err(|e| e.to_string()))
        .as_ref()
        .map_err(|e| Error::Render(format!("lensfun database: {e}")))
}

fn normalized_name(value: &str) -> String {
    value
        .chars()
        .filter(|c| c.is_ascii_alphanumeric())
        .flat_map(char::to_lowercase)
        .collect()
}

fn normalized_alias_matches<'a>(requested: &str, aliases: impl Iterator<Item = &'a str>) -> bool {
    let requested = normalized_name(requested);
    aliases.map(normalized_name).any(|candidate| {
        candidate == requested
            || (candidate.len() >= 5
                && (candidate.contains(&requested) || requested.contains(&candidate)))
    })
}

fn find_camera_profile<'a>(db: &'a Database, make: &str, model: &str) -> Option<&'a Camera> {
    db.cameras.iter().find(|camera| {
        normalized_alias_matches(
            make,
            std::iter::once(camera.maker.as_str())
                .chain(camera.maker_localized.values().map(String::as_str)),
        ) && normalized_alias_matches(
            model,
            std::iter::once(camera.model.as_str())
                .chain(camera.model_localized.values().map(String::as_str)),
        )
    })
}

fn camera_mount_compatible(db: &Database, camera: &Camera, lens: &Lens) -> bool {
    if lens.mounts.is_empty()
        || lens
            .mounts
            .iter()
            .any(|mount| mount.eq_ignore_ascii_case(&camera.mount))
    {
        return true;
    }
    db.mounts
        .iter()
        .find(|mount| mount.name.eq_ignore_ascii_case(&camera.mount))
        .is_some_and(|mount| {
            mount.compat.iter().any(|compatible| {
                lens.mounts
                    .iter()
                    .any(|lens_mount| lens_mount.eq_ignore_ascii_case(compatible))
            })
        })
}

fn lens_name_matches(lens: &Lens, requested: &str) -> bool {
    normalized_alias_matches(
        requested,
        std::iter::once(lens.model.as_str())
            .chain(lens.model_localized.values().map(String::as_str)),
    )
}

fn find_explicit_lens_profile<'a>(db: &'a Database, model: &str, focal: f32) -> Option<&'a Lens> {
    db.lenses
        .iter()
        .filter(|lens| {
            (lens.focal_min <= 0.0 || focal >= lens.focal_min - 0.1)
                && (lens.focal_max <= 0.0 || focal <= lens.focal_max + 0.1)
        })
        .filter(|lens| {
            let requested = normalized_name(model);
            std::iter::once(lens.model.as_str())
                .chain(lens.model_localized.values().map(String::as_str))
                .any(|candidate| normalized_name(candidate) == requested)
        })
        .max_by_key(|lens| {
            std::iter::once(&lens.model)
                .chain(lens.model_localized.values())
                .map(|name| normalized_name(name))
                .map(|candidate| {
                    usize::from(candidate == normalized_name(model)) * 2 + candidate.len()
                })
                .max()
                .unwrap_or_default()
        })
}

fn find_lens_profile<'a>(
    db: &'a Database,
    camera: &Camera,
    lens_name: &str,
    focal: f32,
) -> Option<&'a Lens> {
    db.lenses
        .iter()
        .filter(|lens| camera_mount_compatible(db, camera, lens))
        .filter(|lens| {
            (lens.focal_min <= 0.0 || focal >= lens.focal_min - 0.1)
                && (lens.focal_max <= 0.0 || focal <= lens.focal_max + 0.1)
        })
        .filter(|lens| lens_name_matches(lens, lens_name))
        .max_by_key(|lens| {
            std::iter::once(&lens.model)
                .chain(lens.model_localized.values())
                .map(|name| normalized_name(name))
                .map(|candidate| {
                    usize::from(candidate == normalized_name(lens_name)) * 2 + candidate.len()
                })
                .max()
                .unwrap_or_default()
        })
}

fn blend_lens_coordinate(identity: f32, corrected: f32, strength: f32) -> f32 {
    identity + (corrected - identity) * strength
}

fn lens_auto_crop_scale(valid: &[bool], width: usize, height: usize) -> f32 {
    let center_x = (width.saturating_sub(1)) as f32 * 0.5;
    let center_y = (height.saturating_sub(1)) as f32 * 0.5;
    let mut nearest_invalid_radius = 1.0_f32;
    for y in 0..height {
        for x in 0..width {
            if !valid[y * width + x] {
                let normalized_x = if center_x > 0.0 {
                    (x as f32 - center_x).abs() / center_x
                } else {
                    0.0
                };
                let normalized_y = if center_y > 0.0 {
                    (y as f32 - center_y).abs() / center_y
                } else {
                    0.0
                };
                nearest_invalid_radius = nearest_invalid_radius.min(normalized_x.max(normalized_y));
            }
        }
    }
    if nearest_invalid_radius < 1.0 {
        1.005 / nearest_invalid_radius.max(0.5)
    } else {
        1.0
    }
}

fn zoom_center(image: &mut RgbImage, scale: f32) {
    if scale <= 1.001 {
        return;
    }
    let width = image.width;
    let center_x = (image.width.saturating_sub(1)) as f32 * 0.5;
    let center_y = (image.height.saturating_sub(1)) as f32 * 0.5;
    let mut output = vec![0.0_f32; image.data.len()];
    output
        .par_chunks_mut(width * 3)
        .enumerate()
        .for_each(|(y, output_row)| {
            let source_y = center_y + (y as f32 - center_y) / scale;
            for (x, pixel) in output_row.as_chunks_mut::<3>().0.iter_mut().enumerate() {
                let source_x = center_x + (x as f32 - center_x) / scale;
                *pixel = bilinear_rgb(image, source_x, source_y);
            }
        });
    image.data = output;
}

pub(crate) struct LensCorrectionOptions<'a> {
    pub(crate) make: &'a str,
    pub(crate) model: &'a str,
    pub(crate) lens_name: &'a str,
    pub(crate) focal: f32,
    pub(crate) flags: u32,
    pub(crate) strength: f32,
    pub(crate) explicit_profile: Option<&'a str>,
    pub(crate) focal_reducer: f32,
    pub(crate) crop_factor: f32,
}

pub(crate) fn apply_lens(
    image: &mut RgbImage,
    options: &LensCorrectionOptions<'_>,
) -> Result<(), Error> {
    let make = options.make;
    let model = options.model;
    let lens_name = options.lens_name;
    let focal = options.focal;
    let flags = options.flags;
    let correction_strength = options.strength;
    let explicit_profile = options.explicit_profile;
    let focal_reducer = options.focal_reducer;
    let crop_factor = options.crop_factor;
    if flags & KNOWN_FLAGS == 0 || (flags & FLAG_LENS_TCA == 0 && correction_strength == 0.0) {
        return Ok(());
    }
    if (explicit_profile.is_none() && lens_name.is_empty()) || focal <= 0.0 {
        return Err(Error::LensProfileUnavailable(
            "RAW metadata does not identify a usable lens/focal length".into(),
        ));
    }
    let db = lens_database()?;
    let profile_focal = focal / focal_reducer;
    let camera = find_camera_profile(db, make, model);
    let (lens, base_crop_factor, display_name) = if let Some(profile) = explicit_profile {
        let lens = find_explicit_lens_profile(db, profile, profile_focal).ok_or_else(|| {
            Error::LensProfileUnavailable(format!(
                "no exact lensfun profile for adapted-lens mapping {profile}"
            ))
        })?;
        let base_crop = if crop_factor > 0.0 {
            crop_factor
        } else {
            camera.map(|matched| matched.crop_factor).ok_or_else(|| {
                Error::LensProfileUnavailable(format!(
                    "adapted-lens mapping for {profile} requires a crop factor"
                ))
            })?
        };
        (lens, base_crop, profile)
    } else {
        let camera = camera.ok_or_else(|| {
            Error::LensProfileUnavailable(format!(
                "no exact lensfun camera profile for {make} {model}"
            ))
        })?;
        let lens = find_lens_profile(db, camera, lens_name, focal).ok_or_else(|| {
            Error::LensProfileUnavailable(format!("no conservative lensfun match for {lens_name}"))
        })?;
        (lens, camera.crop_factor, lens_name)
    };
    let mut modifier = Modifier::new(
        lens,
        profile_focal,
        base_crop_factor * focal_reducer,
        image.width as u32,
        image.height as u32,
        // Lensfun's coordinate API maps corrected output pixels back into the
        // distorted source when reverse is false. Reverse=true simulates the
        // lens distortion and visibly overcorrects real RAW photographs.
        false,
    );
    let distortion = flags & FLAG_LENS_DISTORTION != 0
        && correction_strength > 0.0
        && modifier.enable_distortion_correction(lens);
    let tca = flags & FLAG_LENS_TCA != 0 && modifier.enable_tca_correction(lens);
    if !distortion && !tca {
        return Err(Error::LensProfileUnavailable(format!(
            "lensfun profile for {display_name} lacks requested calibration"
        )));
    }
    let width = image.width;
    let height = image.height;
    let row_stride = width * 3;
    // Lensfun's distortion and TCA models vary smoothly, so the maps are
    // evaluated on every LENS_MAP_ROW_STEPth row and interpolated vertically.
    // This keeps the serial modifier evaluation off the hot path; the
    // interpolation error is far below one hundredth of a pixel.
    let grid_rows = height.div_ceil(LENS_MAP_ROW_STEP) + 1;
    let mut geometry_rows = vec![0.0_f32; if distortion { grid_rows * width * 2 } else { 0 }];
    let mut subpixel_rows = vec![0.0_f32; if tca { grid_rows * width * 6 } else { 0 }];
    for grid_row in 0..grid_rows {
        let y = (grid_row * LENS_MAP_ROW_STEP) as f32;
        if distortion {
            modifier.apply_geometry_distortion(
                0.0,
                y,
                width,
                1,
                &mut geometry_rows[grid_row * width * 2..(grid_row + 1) * width * 2],
            );
        }
        if tca {
            modifier.apply_subpixel_distortion(
                0.0,
                y,
                width,
                1,
                &mut subpixel_rows[grid_row * width * 6..(grid_row + 1) * width * 6],
            );
        }
    }
    let mut valid = vec![true; width * height];
    // Resampled into a fresh buffer rather than through a copy of the image:
    // at 80 MP that copy was a gigabyte read and written for nothing.
    let mut output = vec![0.0_f32; width * height * 3];
    output
        .par_chunks_mut(row_stride)
        .zip(valid.par_chunks_mut(width))
        .enumerate()
        .for_each(|(y, (output_row, valid_row))| {
            let grid_row = y / LENS_MAP_ROW_STEP;
            let fraction = (y - grid_row * LENS_MAP_ROW_STEP) as f32 / LENS_MAP_ROW_STEP as f32;
            let lerp_rows = |rows: &[f32], stride: usize, index: usize| -> f32 {
                let low = rows[grid_row * width * stride + index];
                let high = rows[(grid_row + 1) * width * stride + index];
                low + (high - low) * fraction
            };
            for x in 0..width {
                let (gx, gy) = if distortion {
                    (
                        blend_lens_coordinate(
                            x as f32,
                            lerp_rows(&geometry_rows, 2, x * 2),
                            correction_strength,
                        ),
                        blend_lens_coordinate(
                            y as f32,
                            lerp_rows(&geometry_rows, 2, x * 2 + 1),
                            correction_strength,
                        ),
                    )
                } else {
                    (x as f32, y as f32)
                };
                if !tca {
                    // Without a chromatic correction all three channels read the
                    // same point, so they share one set of weights and land on
                    // three adjacent floats.
                    valid_row[x] &= inside(image, gx, gy);
                    output_row[x * 3..x * 3 + 3].copy_from_slice(&bilinear_rgb(image, gx, gy));
                    continue;
                }
                for channel in 0..3 {
                    let sx = lerp_rows(&subpixel_rows, 6, x * 6 + channel * 2) + gx - x as f32;
                    let sy = lerp_rows(&subpixel_rows, 6, x * 6 + channel * 2 + 1) + gy - y as f32;
                    valid_row[x] &= inside(image, sx, sy);
                    output_row[x * 3 + channel] = bilinear(image, sx, sy, channel);
                }
            }
        });
    image.data = output;
    zoom_center(
        image,
        lens_auto_crop_scale(&valid, image.width, image.height),
    );
    Ok(())
}

pub(crate) fn orient(image: RgbImage, orientation: u16) -> RgbImage {
    if orientation <= 1 || orientation > 8 {
        return image;
    }
    let swaps = orientation >= 5;
    let (width, height) = if swaps {
        (image.height, image.width)
    } else {
        (image.width, image.height)
    };
    let mut output = RgbImage {
        width,
        height,
        data: vec![0.0; width * height * 3],
    };
    output
        .data
        .par_chunks_mut(width * 3)
        .enumerate()
        .for_each(|(y, output_row)| {
            for (x, pixel) in output_row.as_chunks_mut::<3>().0.iter_mut().enumerate() {
                let (sx, sy) = match orientation {
                    2 => (image.width - 1 - x, y),
                    3 => (image.width - 1 - x, image.height - 1 - y),
                    4 => (x, image.height - 1 - y),
                    5 => (y, x),
                    6 => (y, image.height - 1 - x),
                    7 => (image.width - 1 - y, image.height - 1 - x),
                    8 => (image.width - 1 - y, x),
                    _ => (x, y),
                };
                let src = (sy * image.width + sx) * 3;
                pixel.copy_from_slice(&image.data[src..src + 3]);
            }
        });
    output
}

pub(crate) fn native_downscale_bounds(
    orientation: u16,
    max_width: u32,
    max_height: u32,
) -> (u32, u32) {
    if (5..=8).contains(&orientation) {
        (max_height, max_width)
    } else {
        (max_width, max_height)
    }
}

/// Area-averages SOURCE into the bounded size, or returns None when the
/// bounds do not require shrinking. Reading borrowed pixels lets bounded
/// renders start straight from the shared decode cache without copying it.
pub(crate) fn downscale_from(
    source: &[f32],
    source_width: usize,
    source_height: usize,
    max_width: u32,
    max_height: u32,
) -> Option<RgbImage> {
    if max_width == 0 && max_height == 0 {
        return None;
    }
    let sx = if max_width == 0 {
        1.0
    } else {
        max_width as f32 / source_width as f32
    };
    let sy = if max_height == 0 {
        1.0
    } else {
        max_height as f32 / source_height as f32
    };
    let scale = sx.min(sy).min(1.0);
    if scale >= 1.0 {
        return None;
    }
    let width = ((source_width as f32 * scale).round() as usize).max(1);
    let height = ((source_height as f32 * scale).round() as usize).max(1);
    // Minification uses exact area averaging: each output pixel integrates the
    // source cells it covers, which suppresses the aliasing that point-sampled
    // bilinear reduction produced on fine detail.
    let x_coverage = area_coverage(source_width, width);
    let y_coverage = area_coverage(source_height, height);
    let mut horizontal = vec![0.0_f32; width * source_height * 3];
    horizontal
        .par_chunks_mut(width * 3)
        .enumerate()
        .for_each(|(y, output_row)| {
            let source_row = &source[y * source_width * 3..(y + 1) * source_width * 3];
            for (x, pixel) in output_row.as_chunks_mut::<3>().0.iter_mut().enumerate() {
                let (start, ref weights) = x_coverage[x];
                let mut sum = [0.0_f32; 3];
                for (offset, weight) in weights.iter().enumerate() {
                    let source = (start + offset) * 3;
                    for channel in 0..3 {
                        sum[channel] += source_row[source + channel] * weight;
                    }
                }
                *pixel = sum;
            }
        });
    let mut output = RgbImage {
        width,
        height,
        data: vec![0.0; width * height * 3],
    };
    output
        .data
        .par_chunks_mut(width * 3)
        .enumerate()
        .for_each(|(y, output_row)| {
            let (start, ref weights) = y_coverage[y];
            for (x, pixel) in output_row.as_chunks_mut::<3>().0.iter_mut().enumerate() {
                let mut sum = [0.0_f32; 3];
                for (offset, weight) in weights.iter().enumerate() {
                    let source = ((start + offset) * width + x) * 3;
                    for channel in 0..3 {
                        sum[channel] += horizontal[source + channel] * weight;
                    }
                }
                *pixel = sum;
            }
        });
    Some(output)
}

/// Per-output-pixel source range and normalized area weights for one axis.
fn area_coverage(source: usize, target: usize) -> Vec<(usize, Vec<f32>)> {
    let ratio = source as f64 / target as f64;
    (0..target)
        .map(|index| {
            let begin = index as f64 * ratio;
            let end = ((index + 1) as f64 * ratio).min(source as f64);
            let first = (begin.floor() as usize).min(source - 1);
            let last = (end.ceil() as usize).clamp(first + 1, source);
            let mut weights = Vec::with_capacity(last - first);
            for cell in first..last {
                let cell_begin = (cell as f64).max(begin);
                let cell_end = ((cell + 1) as f64).min(end);
                weights.push((cell_end - cell_begin).max(0.0) as f32);
            }
            let total: f32 = weights.iter().sum();
            if total > 0.0 {
                for weight in &mut weights {
                    *weight /= total;
                }
            }
            (first, weights)
        })
        .collect()
}

/// The GPU status line a process prints once, so a silent CPU fallback is
/// never mistaken for hardware acceleration.
pub(crate) fn gpu_status_message(status: GpuStatus<'_>) -> String {
    match status {
        GpuStatus::Disabled => "orfeus: Vulkan compute is disabled by \
             ORFEUS_GPU=0; using the CPU path"
            .to_string(),
        GpuStatus::Unavailable(reason) => format!(
            "orfeus: WARNING Vulkan compute is unavailable, \
             falling back to the CPU path: {reason}"
        ),
        GpuStatus::Active(adapter) => {
            format!("orfeus: Vulkan compute active on {adapter}")
        }
    }
}

pub(crate) enum GpuStatus<'a> {
    Disabled,
    Unavailable(&'a str),
    Active(&'a str),
}

/// Reports STATUS on stderr the first time the process reaches it. Returns
/// true when it printed, so callers and tests can tell the once-only
/// behaviour apart from repeated frames.
pub(crate) fn report_gpu_status_once(cell: &OnceLock<()>, status: GpuStatus<'_>) -> bool {
    if cell.set(()).is_ok() {
        eprintln!("{}", gpu_status_message(status));
        true
    } else {
        false
    }
}

pub(crate) fn gpu_disabled_notice() -> &'static OnceLock<()> {
    static CELL: OnceLock<OnceLock<()>> = OnceLock::new();
    CELL.get_or_init(OnceLock::new)
}

pub(crate) fn gpu_fallback_notice() -> &'static OnceLock<()> {
    static CELL: OnceLock<OnceLock<()>> = OnceLock::new();
    CELL.get_or_init(OnceLock::new)
}

pub(crate) fn gpu_active_notice() -> &'static OnceLock<()> {
    static CELL: OnceLock<OnceLock<()>> = OnceLock::new();
    CELL.get_or_init(OnceLock::new)
}

/// Applies the default display tone and sRGB transfer, on the GPU unless
/// `ORFEUS_GPU=0` forces the CPU path. Failures
/// leave the input intact and fall back, complaining once on stderr so a
/// missing driver never degrades performance silently.
pub(crate) fn apply_display_transform(image: &mut RgbImage, profiling: bool) {
    let gpu_completed = if super::gpu::requested() {
        match catch_unwind(AssertUnwindSafe(|| {
            super::gpu::tone_and_transfer(&mut image.data)
        })) {
            Ok(Ok(dispatch)) => {
                report_gpu_status_once(
                    gpu_active_notice(),
                    GpuStatus::Active(&dispatch.adapter_name),
                );
                if profiling {
                    eprintln!(
                        "orfeus-profile gpu-stage=tone-transfer adapter={:?} milliseconds={:.3}",
                        dispatch.adapter_name, dispatch.milliseconds
                    );
                }
                true
            }
            Ok(Err(error)) => {
                report_gpu_status_once(gpu_fallback_notice(), GpuStatus::Unavailable(&error));
                if profiling {
                    eprintln!(
                        "orfeus-profile gpu-stage=tone-transfer fallback=cpu error={error:?}"
                    );
                }
                false
            }
            Err(_) => {
                report_gpu_status_once(
                    gpu_fallback_notice(),
                    GpuStatus::Unavailable("the Vulkan backend panicked"),
                );
                if profiling {
                    eprintln!("orfeus-profile gpu-stage=tone-transfer fallback=cpu error=panic");
                }
                false
            }
        }
    } else {
        report_gpu_status_once(gpu_disabled_notice(), GpuStatus::Disabled);
        false
    };
    if !gpu_completed {
        display_tone_and_transfer(&mut image.data);
    }
}

/// Tone maps scene-linear sRGB and encodes it for display in one traversal.
///
/// Both halves are point operations, and at preview resolution each pass over
/// the image costs more in memory traffic than in arithmetic, so running them
/// separately doubled the bill for no benefit.
pub(crate) fn display_tone_and_transfer(data: &mut [f32]) {
    debug_assert_eq!(data.len() % 3, 0, "RGB data must contain triplets");
    data.par_chunks_mut(3 * 8192).for_each(|chunk| {
        for pixel in chunk.as_chunks_mut::<3>().0 {
            super::tone::tone_pixel(pixel);
            for value in pixel.iter_mut() {
                *value = srgb_encode(*value);
            }
        }
    });
}

/// Writes a finished image into a caller's 8-bit RGB buffer, tone mapping and
/// encoding it on the way when it is still scene-linear.
///
/// One traversal for what used to be three. A drag tick is bounded by memory
/// traffic rather than arithmetic — eight threads already saturate this
/// machine's bandwidth on a preview-sized image — so every pass that can be
/// folded into another is worth about as much as deleting it.
pub(crate) fn write_display_bytes(image: &RgbImage, bytes: &mut [u8], scene_linear: bool) {
    debug_assert!(bytes.len() >= image.data.len());
    bytes
        .par_chunks_mut(3 * 8192)
        .zip(image.data.par_chunks(3 * 8192))
        .for_each(|(out, source)| {
            for (out, pixel) in out.as_chunks_mut::<3>().0.iter_mut().zip(source.as_chunks::<3>().0)
            {
                let mut pixel = *pixel;
                if scene_linear {
                    super::tone::tone_pixel(&mut pixel);
                    for value in pixel.iter_mut() {
                        *value = srgb_encode(*value);
                    }
                }
                for (byte, value) in out.iter_mut().zip(pixel) {
                    *byte = (value.clamp(0.0, 1.0) * 255.0 + 0.5) as u8;
                }
            }
        });
}

/// Entries in the sRGB encoding table, sampled on the square root of the
/// signal so the steep part near black is resolved as finely as the top.
const SRGB_TABLE_LAST: usize = 4096;

fn srgb_table() -> &'static [f32; SRGB_TABLE_LAST + 1] {
    static TABLE: OnceLock<Box<[f32; SRGB_TABLE_LAST + 1]>> = OnceLock::new();
    TABLE.get_or_init(|| {
        let mut table = Box::new([0.0_f32; SRGB_TABLE_LAST + 1]);
        for (index, entry) in table.iter_mut().enumerate() {
            let position = index as f32 / SRGB_TABLE_LAST as f32;
            *entry = exact_srgb_encode(position * position);
        }
        table
    })
}

fn exact_srgb_encode(value: f32) -> f32 {
    if value <= 0.003_130_8 {
        12.92 * value
    } else {
        1.055 * value.max(0.0).powf(1.0 / 2.4) - 0.055
    }
}

/// Encodes one linear-light sRGB value for display.
///
/// A `powf` per sample costs about sixty cycles, and a preview render encodes
/// nearly six million of them on every interactive tick. Reading a table on the
/// square root of the signal instead holds the error under a thousandth of an
/// 8-bit step; only the highlights above 1.0, which no display shows anyway,
/// still take the exact path.
#[inline]
pub(crate) fn srgb_encode(value: f32) -> f32 {
    if !(value > 0.0) {
        return 0.0;
    }
    if value >= 1.0 {
        return exact_srgb_encode(value);
    }
    let table = srgb_table();
    let position = value.sqrt() * SRGB_TABLE_LAST as f32;
    let index = position as usize;
    let fraction = position - index as f32;
    let next = (index + 1).min(SRGB_TABLE_LAST);
    table[index] + (table[next] - table[index]) * fraction
}

pub(crate) fn apply_lut(image: &mut RgbImage, lut: &CubeLut, strength: f32) {
    image.data.par_chunks_mut(3 * 8192).for_each(|chunk| {
        for pixel in chunk.as_chunks_mut::<3>().0 {
            let transformed = lut.sample(*pixel);
            for (value, target) in pixel.iter_mut().zip(transformed) {
                *value = (*value + (target - *value) * strength).clamp(0.0, 1.0);
            }
        }
    });
}

fn splitmix64(mut value: u64) -> u64 {
    value = value.wrapping_add(0x9e37_79b9_7f4a_7c15);
    value = (value ^ (value >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    value = (value ^ (value >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    value ^ (value >> 31)
}

pub(crate) fn apply_grain(image: &mut RgbImage, amount: f32, size: f32, seed: u64) {
    if amount == 0.0 {
        return;
    }
    let width = image.width;
    image
        .data
        .par_chunks_mut(width * 3)
        .enumerate()
        .for_each(|(y, row)| {
            let gy = (y as f32 / size).floor() as u64;
            let row_seed = seed ^ gy.wrapping_mul(0x6eed_0e9d_a4d9_4a4f);
            for (x, pixel) in row.as_chunks_mut::<3>().0.iter_mut().enumerate() {
                let gx = (x as f32 / size).floor() as u64;
                let bits = splitmix64(row_seed ^ gx.wrapping_mul(0x517c_c1b7_2722_0a95));
                let noise = ((bits >> 40) as f32 / 16_777_215.0 - 0.5) * amount * 0.16;
                let luminance = 0.2126 * pixel[0] + 0.7152 * pixel[1] + 0.0722 * pixel[2];
                let shaped = noise * (0.35 + 0.65 * (1.0 - luminance));
                for value in pixel.iter_mut() {
                    *value = (*value + shaped).clamp(0.0, 1.0);
                }
            }
        });
}

fn encode_to<W: Write + Seek>(
    image: &RgbImage,
    writer: W,
    format: u32,
    quality: u32,
) -> Result<(), Error> {
    match format {
        OUTPUT_JPEG => {
            let bytes: Vec<u8> = image
                .data
                .par_iter()
                .map(|v| (v.clamp(0.0, 1.0) * 255.0 + 0.5) as u8)
                .collect();
            let width = u16::try_from(image.width)
                .map_err(|_| Error::Render("JPEG output is wider than 65535 pixels".into()))?;
            let height = u16::try_from(image.height)
                .map_err(|_| Error::Render("JPEG output is taller than 65535 pixels".into()))?;
            let mut encoded = Vec::with_capacity(bytes.len() / 4);
            let mut encoder = jpeg_encoder::Encoder::new(&mut encoded, quality as u8);
            encoder.set_sampling_factor(jpeg_encoder::SamplingFactor::F_2_2);
            for (chunk_index, chunk) in SRGB_ICC.chunks(65_519).enumerate() {
                let chunk_count = SRGB_ICC.len().div_ceil(65_519);
                let mut segment = Vec::with_capacity(chunk.len() + 14);
                segment.extend_from_slice(b"ICC_PROFILE\0");
                segment.push(chunk_index as u8 + 1);
                segment.push(chunk_count as u8);
                segment.extend_from_slice(chunk);
                encoder
                    .add_app_segment(2, segment)
                    .map_err(|e| Error::Render(format!("JPEG ICC profile: {e}")))?;
            }
            encoder
                .encode(&bytes, width, height, jpeg_encoder::ColorType::Rgb)
                .map_err(|e| Error::Render(format!("JPEG encoding failed: {e}")))?;
            let mut writer = writer;
            return writer
                .write_all(&encoded)
                .map_err(|e| Error::Render(format!("image encoding failed: {e}")));
        }
        OUTPUT_TIFF => {
            let mut bytes = vec![0_u8; image.data.len() * 2];
            bytes
                .par_chunks_exact_mut(2)
                .zip(image.data.par_iter())
                .for_each(|(chunk, value)| {
                    chunk.copy_from_slice(
                        &((value.clamp(0.0, 1.0) * 65535.0 + 0.5) as u16).to_ne_bytes(),
                    );
                });
            let mut encoder = TiffEncoder::new(writer);
            encoder
                .set_icc_profile(SRGB_ICC.to_vec())
                .map_err(|e| Error::Render(format!("TIFF ICC profile: {e}")))?;
            encoder.write_image(
                &bytes,
                image.width as u32,
                image.height as u32,
                ColorType::Rgb16.into(),
            )
        }
        _ => unreachable!(),
    }
    .map_err(|e| Error::Render(format!("image encoding failed: {e}")))
}

pub(crate) fn same_file(input: &Path, output: &Path) -> Result<bool, Error> {
    let input_meta = fs::metadata(input)?;
    match fs::metadata(output) {
        Ok(output_meta) => {
            Ok(input_meta.dev() == output_meta.dev() && input_meta.ino() == output_meta.ino())
        }
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => Ok(false),
        Err(error) => Err(error.into()),
    }
}

fn temporary_path(output: &Path, attempt: u32) -> Result<PathBuf, Error> {
    let parent = output.parent().unwrap_or_else(|| Path::new("."));
    let name = output
        .file_name()
        .ok_or(Error::InvalidArgument("output path has no file name"))?
        .to_string_lossy();
    let nonce = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    Ok(parent.join(format!(
        ".{name}.orfeus-{}-{nonce}-{attempt}.tmp",
        std::process::id()
    )))
}

pub(crate) fn atomic_encode(
    input: &Path,
    output: &Path,
    image: &RgbImage,
    format: u32,
    quality: u32,
) -> Result<(), Error> {
    if same_file(input, output)? {
        return Err(Error::InvalidArgument(
            "input and output refer to the same file",
        ));
    }
    let mut temp = None;
    for attempt in 0..100 {
        let path = temporary_path(output, attempt)?;
        match OpenOptions::new().write(true).create_new(true).open(&path) {
            Ok(file) => {
                temp = Some((path, file));
                break;
            }
            Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {}
            Err(error) => return Err(error.into()),
        }
    }
    let (temp_path, file) = temp.ok_or_else(|| {
        Error::Io(std::io::Error::new(
            std::io::ErrorKind::AlreadyExists,
            "could not allocate output temporary file",
        ))
    })?;
    let result = (|| {
        let mut writer = BufWriter::new(file);
        encode_to(image, &mut writer, format, quality)?;
        writer.flush()?;
        let file = writer.into_inner().map_err(|e| Error::Io(e.into_error()))?;
        file.sync_all()?;
        fs::rename(&temp_path, output)?;
        File::open(output.parent().unwrap_or_else(|| Path::new(".")))?.sync_all()?;
        Ok(())
    })();
    if result.is_err() {
        let _ = fs::remove_file(&temp_path);
    }
    result
}

/// Scene-linear sRGB pixels and the source description needed to render them.
pub(crate) struct DecodedRaw {
    pub(crate) width: usize,
    pub(crate) height: usize,
    pub(crate) data: Vec<f32>,
    pub(crate) orientation: u16,
    pub(crate) make: String,
    pub(crate) model: String,
    pub(crate) lens_name: String,
    pub(crate) focal: f32,
    /// The colour temperature the camera balanced for, when it can be read.
    pub(crate) as_shot_kelvin: Option<f32>,
}

pub(crate) type DecodeCacheKey = [u8; 32];
type DecodeCacheEntries = Vec<(DecodeCacheKey, Arc<DecodedRaw>)>;

#[derive(Default)]
struct DecodeCacheState {
    entries: DecodeCacheEntries,
    loading: HashSet<DecodeCacheKey>,
}

fn decode_cache() -> &'static (Mutex<DecodeCacheState>, Condvar) {
    static CACHE: OnceLock<(Mutex<DecodeCacheState>, Condvar)> = OnceLock::new();
    CACHE.get_or_init(|| (Mutex::new(DecodeCacheState::default()), Condvar::new()))
}

pub(crate) fn neural_render_lock() -> &'static Mutex<()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
}

type ScaledSourceKey = (String, usize, usize, u32, u32, usize);
type ScaledSourceCache = Mutex<Vec<(ScaledSourceKey, Arc<RgbImage>)>>;

fn scaled_source_cache() -> &'static ScaledSourceCache {
    static CACHE: OnceLock<ScaledSourceCache> = OnceLock::new();
    CACHE.get_or_init(|| Mutex::new(Vec::new()))
}

const SCALED_SOURCE_CAPACITY: usize = 4;

/// Return the decoded source bounded to the render dimensions, caching the
/// downscale for interactive re-renders of the same photo and size.
/// The bounded source for one render, shared rather than copied.
///
/// Interactive frames that resume from a graph checkpoint never touch this
/// image at all, so handing back a handle keeps a 20 MB copy off the hot
/// path; callers that mutate it in place own it with `own_source`.
pub(crate) fn scaled_source_for_render(
    decoded: &Arc<DecodedRaw>,
    input: &Path,
    max_width: u32,
    max_height: u32,
    cache_mode: u32,
) -> Arc<RgbImage> {
    let full = |decoded: &DecodedRaw| RgbImage {
        width: decoded.width,
        height: decoded.height,
        data: decoded.data.clone(),
    };
    let bounded = max_width > 0 || max_height > 0;
    if cache_mode != CACHE_USE || !bounded {
        return Arc::new(
            match downscale_from(
                &decoded.data,
                decoded.width,
                decoded.height,
                max_width,
                max_height,
            ) {
                Some(scaled) => scaled,
                None => full(decoded),
            },
        );
    }
    let key: ScaledSourceKey = (
        input.to_string_lossy().into_owned(),
        decoded.width,
        decoded.height,
        max_width,
        max_height,
        // A re-decoded file gets a fresh Arc, invalidating stale entries.
        Arc::as_ptr(decoded) as usize,
    );
    {
        let mut cache = scaled_source_cache()
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        if let Some(position) = cache.iter().position(|(held, _)| *held == key) {
            let entry = cache.remove(position);
            let image = Arc::clone(&entry.1);
            cache.insert(0, entry);
            return image;
        }
    }
    let scaled = match downscale_from(
        &decoded.data,
        decoded.width,
        decoded.height,
        max_width,
        max_height,
    ) {
        Some(scaled) => scaled,
        None => full(decoded),
    };
    let shared = Arc::new(scaled);
    let mut cache = scaled_source_cache()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    cache.retain(|(held, _)| *held != key);
    cache.insert(0, (key, Arc::clone(&shared)));
    cache.truncate(SCALED_SOURCE_CAPACITY);
    shared
}

/// Take ownership of a shared source, copying only when it is still shared.
pub(crate) fn own_source(source: Arc<RgbImage>) -> RgbImage {
    Arc::try_unwrap(source).unwrap_or_else(|shared| (*shared).clone())
}

/// Takes the decoded image as a buffer the caller may write into.
///
/// A full-resolution render is bounded by nothing, so it works on the decode
/// itself; when the decode cache is not holding it, moving the pixels out saves
/// copying nearly a gigabyte at 80 MP, which measured 927 ms of every export.
pub(crate) fn own_decoded(decoded: Arc<DecodedRaw>) -> RgbImage {
    match Arc::try_unwrap(decoded) {
        Ok(owned) => RgbImage {
            width: owned.width,
            height: owned.height,
            data: owned.data,
        },
        Err(shared) => RgbImage {
            width: shared.width,
            height: shared.height,
            data: shared.data.clone(),
        },
    }
}

/// A file's identity as the filesystem reports it, cheap to obtain.
pub(crate) type StatIdentity = (u64, u64, u64, i64, i64);

pub(crate) fn stat_identity(input: &Path) -> Result<StatIdentity, Error> {
    let metadata = std::fs::metadata(input)?;
    Ok((
        metadata.dev(),
        metadata.ino(),
        metadata.size(),
        metadata.mtime(),
        metadata.mtime_nsec(),
    ))
}

#[cfg(test)]
pub(crate) fn lut_cache_len() -> usize {
    lut_cache()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .len()
}

const LUT_CACHE_CAPACITY: usize = 4;

/// Parsed .cube files, keyed by path and exact content digest.
///
/// A film node re-reads its LUT every time the graph executes, so dragging
/// grain used to re-parse tens of thousands of lines of text on every tick.
/// The digest means even a same-size edit with preserved timestamps takes effect.
#[allow(clippy::type_complexity)]
fn lut_cache() -> &'static Mutex<Vec<((PathBuf, DecodeCacheKey), Arc<CubeLut>)>> {
    static CACHE: OnceLock<Mutex<Vec<((PathBuf, DecodeCacheKey), Arc<CubeLut>)>>> = OnceLock::new();
    CACHE.get_or_init(|| Mutex::new(Vec::new()))
}

pub(crate) fn cached_cube_lut(path: &Path) -> Result<Arc<CubeLut>, Error> {
    let digest = file_content_digest(path)?;
    let key = (path.to_path_buf(), digest);
    {
        let mut cache = lut_cache()
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        if let Some(position) = cache.iter().position(|(held, _)| *held == key) {
            let entry = cache.remove(position);
            let lut = entry.1.clone();
            cache.insert(0, entry);
            return Ok(lut);
        }
    }
    let lut = Arc::new(CubeLut::read(path)?);
    if file_content_digest(path)? != digest {
        return Err(Error::Render(
            "film LUT changed while it was being read".into(),
        ));
    }
    let mut cache = lut_cache()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    cache.retain(|(held, _)| *held != key);
    cache.insert(0, (key, lut.clone()));
    cache.truncate(LUT_CACHE_CAPACITY);
    Ok(lut)
}

const KEY_MEMO_CAPACITY: usize = 16;

fn key_memo() -> &'static Mutex<Vec<(StatIdentity, DecodeCacheKey)>> {
    static MEMO: OnceLock<Mutex<Vec<(StatIdentity, DecodeCacheKey)>>> = OnceLock::new();
    MEMO.get_or_init(|| Mutex::new(Vec::new()))
}

/// The content hash of INPUT, memoized on its stat identity.
///
/// Hashing 20 MB of RAW is far more expensive than the render it guards
/// when the decode cache already holds the image, so an unchanged inode,
/// size, and modification time reuse the previous hash. Anything that
/// rewrites the file changes at least one of those, and callers that must
/// be certain use `file_content_digest`.
pub(crate) fn decode_cache_key(input: &Path) -> Result<DecodeCacheKey, Error> {
    let identity = stat_identity(input)?;
    {
        let mut memo = key_memo()
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        if let Some(position) = memo.iter().position(|(held, _)| *held == identity) {
            let entry = memo.remove(position);
            let key = entry.1;
            memo.push(entry);
            return Ok(key);
        }
    }
    let key = file_content_digest(input)?;
    let mut memo = key_memo()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    memo.retain(|(held, _)| *held != identity);
    if memo.len() >= KEY_MEMO_CAPACITY {
        memo.remove(0);
    }
    memo.push((identity, key));
    Ok(key)
}

pub(crate) fn file_content_digest(input: &Path) -> Result<DecodeCacheKey, Error> {
    let mut file = File::open(input)?;
    let mut hasher = Sha256::new();
    let mut buffer = [0_u8; 64 * 1024];
    loop {
        let count = file.read(&mut buffer)?;
        if count == 0 {
            break;
        }
        hasher.update(&buffer[..count]);
    }
    Ok(hasher.finalize().into())
}

/// Requested output size at or below which a preview develops at half
/// resolution.
///
/// The smallest sensor Orfeus targets is the PEN-F's 4608x3456, so a request
/// this size or smaller is always downscaling by more than the factor of two
/// that binning costs — the detail dropped was about to be resampled away.
pub(crate) const DRAFT_MAX_DIMENSION: u32 = 2048;

/// Whether a render asking for this output size should develop a draft.
///
/// Decided from the request alone rather than from the decoded dimensions,
/// because the answer has to key the decode cache before anything is decoded.
/// Set `ORFEUS_DRAFT=0` to always develop at full resolution.
pub(crate) fn draft_requested(max_width: u32, max_height: u32) -> bool {
    if std::env::var_os("ORFEUS_DRAFT").as_deref() == Some(std::ffi::OsStr::new("0")) {
        return false;
    }
    max_width > 0
        && max_height > 0
        && max_width <= DRAFT_MAX_DIMENSION
        && max_height <= DRAFT_MAX_DIMENSION
}

/// A draft decode is a different image from a full one, so it needs a different
/// identity — in this cache and in the graph prefix cache, which keys on the
/// same value.
pub(crate) fn draft_identity(key: DecodeCacheKey) -> DecodeCacheKey {
    let mut hasher = Sha256::new();
    hasher.update(key);
    hasher.update(b"orfeus-draft-v1");
    hasher.finalize().into()
}

/// Whether this frame is Olympus's eight-shot high-resolution composite.
fn olympus_high_resolution(make: &str, model: &str, width: usize, height: usize) -> bool {
    make.to_ascii_uppercase().contains("OLYMPUS")
        && model.trim().eq_ignore_ascii_case("PEN-F")
        && (width, height) == (10400, 7796)
}

/// Corrects the colour filter phase rawler reports for a pixel-shift composite.
///
/// Rawler 0.7.2 names the PEN-F's 80 MP composite RGGB, but the frame is really
/// GRBG: every 2x2 quad then reads its red and its blue from a green photosite,
/// and the image develops magenta. Adobe's converter writes GRBG in the DNG it
/// makes from the same ORF, and shifting rawler's pattern one column reproduces
/// that decode — so this is a wrong tag, not a different pixel layout.
///
/// Orfeus used to sidestep it by developing this one sensor mode through
/// LibRaw, which decoded the whole 116 MB container a second time for 1.7 s.
fn correct_pixel_shift_cfa(make: &str, model: &str, raw: &mut RawImage) {
    if !olympus_high_resolution(make, model, raw.width, raw.height) {
        return;
    }
    let RawPhotometricInterpretation::Cfa(config) = &raw.photometric else {
        return;
    };
    if !config.cfa.name.eq_ignore_ascii_case("RGGB") {
        return;
    }
    let corrected = CFAConfig::new(&config.cfa.shift(1, 0), &config.colors);
    raw.photometric = RawPhotometricInterpretation::Cfa(corrected);
}

/// How many sensor quads a draft averages into one output pixel.
///
/// The largest power of two that still leaves the long edge able to serve the
/// biggest preview a draft is ever asked for. A power of two so that one cached
/// decode covers every draft-sized request for a file — a factor tuned to each
/// requested width would re-develop the RAW whenever the window was resized.
///
/// A 20 MP frame bins 2x2 and stops there; the PEN-F's 80 MP composite bins 4x4,
/// which is the same 2592x1944 draft from four times the photosites.
fn draft_bin_factor(quad_width: usize, quad_height: usize) -> usize {
    let mut factor = 1;
    while quad_width.max(quad_height) / (factor * 2) >= DRAFT_MAX_DIMENSION as usize {
        factor *= 2;
    }
    factor
}

/// The photosite rectangle a develop covers: the camera's default crop, or the
/// active area when the file names no usable crop.
///
/// The default crop is expressed in sensor coordinates, the same frame the
/// active area is in — get this wrong and a draft preview frames differently
/// from the export it is previewing.
fn develop_roi(width: usize, height: usize, active: Option<Rect>, crop: Option<Rect>) -> Rect {
    let full = Rect::new(
        rawler::imgop::Point::new(0, 0),
        rawler::imgop::Dim2::new(width, height),
    );
    let active = active.unwrap_or(full);
    match crop {
        Some(crop)
            if crop.p.x >= active.p.x
                && crop.p.y >= active.p.y
                && crop.p.x + crop.d.w <= active.p.x + active.d.w
                && crop.p.y + crop.d.h <= active.p.y + active.d.h =>
        {
            crop
        }
        _ => active,
    }
}

/// What one sensor quad contributes: the channel each of its four corners
/// feeds, the black level subtracted there, and the span from it to white.
type QuadCorners = [(usize, f32, f32); 4];

/// Everything `bin_quads` needs that does not vary across the frame.
struct BinPlan {
    roi: Rect,
    quads_wide: usize,
    quads_high: usize,
    factor: usize,
    corners: QuadCorners,
    width: usize,
    height: usize,
}

/// Averages FACTOR x FACTOR sensor quads into each output pixel.
///
/// Reading the sensor's own integers rather than a widened copy of them is most
/// of the point: at 80 MP, `as_f32` alone allocates and fills 324 MB before any
/// arithmetic happens.
fn bin_quads<T: Copy + Into<f32> + Sync>(
    data: &[T],
    stride: usize,
    plan: &BinPlan,
) -> Vec<[f32; 3]> {
    let BinPlan {
        roi,
        quads_wide,
        quads_high,
        factor,
        corners,
        width,
        height,
    } = *plan;
    let mut output = vec![[0.0_f32; 3]; width * height];
    output
        .par_chunks_mut(width)
        .enumerate()
        .for_each(|(out_y, row)| {
            let first_quad_y = out_y * factor;
            let last_quad_y = ((out_y + 1) * factor).min(quads_high);
            for (out_x, pixel) in row.iter_mut().enumerate() {
                let first_quad_x = out_x * factor;
                let last_quad_x = ((out_x + 1) * factor).min(quads_wide);
                let mut sum = [0.0_f32; 3];
                for quad_y in first_quad_y..last_quad_y {
                    let top = (roi.p.y + quad_y * 2) * stride + roi.p.x;
                    for quad_x in first_quad_x..last_quad_x {
                        let left = top + quad_x * 2;
                        let quad = [
                            data[left],
                            data[left + 1],
                            data[left + stride],
                            data[left + stride + 1],
                        ];
                        for (corner, value) in quad.iter().enumerate() {
                            let (channel, black, span) = corners[corner];
                            // Divided, not scaled by a reciprocal, so a draft
                            // and a full develop agree to the last bit.
                            let level = ((*value).into() - black).max(0.0) / span;
                            // The two greens of a quad average into one.
                            sum[channel] += if channel == 1 { level * 0.5 } else { level };
                        }
                    }
                }
                let quads = ((last_quad_y - first_quad_y) * (last_quad_x - first_quad_x)) as f32;
                let weight = if quads > 0.0 { 1.0 / quads } else { 0.0 };
                *pixel = [sum[0] * weight, sum[1] * weight, sum[2] * weight];
            }
        });
    output
}

/// Develops a Bayer frame straight to a binned RGB image.
///
/// Each output pixel averages FACTOR x FACTOR sensor quads, and each quad
/// contributes red, the mean of its two greens, and blue — no colour is
/// interpolated across quads. Black level, white level and the crop are applied
/// in the same pass, so one traversal of the frame replaces four: rawler's
/// `as_f32` widening, its `apply_scaling`, its superpixel demosaic, and the
/// downscale that followed them. At FACTOR 4 it also writes a sixteenth of the
/// pixels the old half-resolution draft handed to every later stage.
///
/// A preview is downscaled well past this anyway, so the detail dropped was
/// about to be resampled away. Exports never use it.
fn develop_binned(raw: &RawImage, factor: usize) -> Option<Intermediate> {
    let RawPhotometricInterpretation::Cfa(config) = &raw.photometric else {
        return None;
    };
    if !config.cfa.is_rgb() || raw.cpp != 1 || factor == 0 {
        return None;
    }
    let roi = develop_roi(raw.width, raw.height, raw.active_area, raw.crop_area);
    let quads_wide = roi.d.w / 2;
    let quads_high = roi.d.h / 2;
    if quads_wide == 0 || quads_high == 0 {
        return None;
    }
    // Which channel each corner of a quad carries, and the levels that scale
    // it. Both are fixed for the whole frame: the pattern is shifted once by
    // the crop origin, and the levels repeat on the sensor's own 2x2 grid,
    // which that origin's parity locks to.
    let cfa = config.cfa.shift(roi.p.x, roi.p.y);
    let black = raw.blacklevel.as_bayer_array();
    let white = raw.whitelevel.as_bayer_array();
    let mut corners: QuadCorners = [(0, 0.0, 1.0); 4];
    for row in 0..2 {
        for column in 0..2 {
            let level = ((roi.p.y + row) & 1) * 2 + ((roi.p.x + column) & 1);
            let span = white[level] - black[level];
            corners[row * 2 + column] = (
                cfa.color_at(row, column),
                black[level],
                if span > 0.0 { span } else { f32::INFINITY },
            );
        }
    }
    let plan = BinPlan {
        roi,
        quads_wide,
        quads_high,
        factor,
        corners,
        width: quads_wide.div_ceil(factor),
        height: quads_high.div_ceil(factor),
    };
    let binned = match &raw.data {
        RawImageData::Integer(data) => bin_quads(data, raw.width, &plan),
        RawImageData::Float(data) => bin_quads(data, raw.width, &plan),
    };
    Some(Intermediate::ThreeColor(Color2D::new_with(
        binned,
        plan.width,
        plan.height,
    )))
}

/// Develops a Bayer frame at full resolution, cropped as the camera asks.
///
/// Rawler's own entry point develops the whole active area into a
/// whole-image RGB buffer, sweeps it four times, and then copies out the
/// default crop. `demosaic` runs the same algorithm over cache-sized tiles and
/// emits only the crop, which at 80 MP is the difference between about seven
/// gigabytes of memory traffic and about one.
fn develop_full(raw: &RawImage) -> Option<Intermediate> {
    let RawPhotometricInterpretation::Cfa(config) = &raw.photometric else {
        return None;
    };
    if !config.cfa.is_rgb() || raw.cpp != 1 {
        return None;
    }
    let full = Rect::new(
        rawler::imgop::Point::new(0, 0),
        rawler::imgop::Dim2::new(raw.width, raw.height),
    );
    // The whole active area may be read; only the default crop is emitted.
    let area = raw.active_area.unwrap_or(full);
    let crop = develop_roi(raw.width, raw.height, raw.active_area, raw.crop_area);
    let cfa = config.cfa.shift(area.p.x, area.p.y);
    let black = raw.blacklevel.as_bayer_array();
    let white = raw.whitelevel.as_bayer_array();
    let mut colors = [0_usize; 4];
    let mut levels = [(0.0_f32, 1.0_f32); 4];
    for row in 0..2 {
        for column in 0..2 {
            let corner = row * 2 + column;
            let level = ((area.p.y + row) & 1) * 2 + ((area.p.x + column) & 1);
            let span = white[level] - black[level];
            colors[corner] = cfa.color_at(row, column);
            levels[corner] = (black[level], if span > 0.0 { span } else { f32::INFINITY });
        }
    }
    let window = super::demosaic::Window {
        left: crop.p.x - area.p.x,
        top: crop.p.y - area.p.y,
        width: crop.d.w,
        height: crop.d.h,
    };
    let developed = match &raw.data {
        RawImageData::Integer(data) => super::demosaic::demosaic_ppg(
            &super::demosaic::BayerFrame {
                data,
                stride: raw.width,
                left: area.p.x,
                top: area.p.y,
                width: area.d.w,
                height: area.d.h,
                colors,
                levels,
            },
            window,
        ),
        RawImageData::Float(data) => super::demosaic::demosaic_ppg(
            &super::demosaic::BayerFrame {
                data,
                stride: raw.width,
                left: area.p.x,
                top: area.p.y,
                width: area.d.w,
                height: area.d.h,
                colors,
                levels,
            },
            window,
        ),
    };
    Some(Intermediate::ThreeColor(developed))
}

/// Decodes a RAW file, unwrapping a DNG's embedded original if the container
/// itself cannot be decoded.
///
/// Orfeus used to unwrap every DNG before looking at it, which wrote a 131 MB
/// temporary file and cost 489 ms on the first view of an 80 MP scan — and made
/// a DNG without an embedded original impossible to open at all. Rawler reads
/// these DNGs directly: the photosite values are identical to the original's at
/// the same coordinates. What it does not read is the maker note the lens
/// description lives in, so the caller supplies that instead.
///
/// The unwrapping stays as a fallback, in memory rather than through a file,
/// and is only paid for by a file that needs it.
pub(crate) fn decode_linear_srgb(
    input: &Path,
    draft: bool,
    profiling: bool,
) -> Result<DecodedRaw, Error> {
    let source = RawSource::new(input)?;
    let container_error = match decode_source(&source, draft, profiling) {
        Ok(decoded) => return Ok(decoded),
        Err(error) => error,
    };
    let Ok(original) = super::embedded_original(source.buf()) else {
        return Err(container_error);
    };
    if profiling {
        eprintln!(
            "orfeus-profile source=embedded-original bytes={}",
            original.len()
        );
    }
    let source = RawSource::new_from_shared_vec(Arc::new(original)).with_path(input);
    decode_source(&source, draft, profiling).map_err(|original_error| {
        // Both failed, so say so: naming only one of them sends the reader to
        // the wrong half of the file.
        Error::Render(format!(
            "{container_error}, and its embedded original: {original_error}"
        ))
    })
}

/// The colour temperature of an already-decoded frame, if one is in the cache.
///
/// The decode works this out anyway, so a photograph on screen has already
/// answered the question and the interface can have it for a hash lookup.
fn cached_as_shot_kelvin(input: &Path) -> Option<f32> {
    let key = decode_cache_key(input).ok()?;
    let (cache_lock, _) = decode_cache();
    let cache = cache_lock
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    // A preview decodes a draft and an export decodes the frame itself; either
    // knows the temperature, so take whichever is present.
    [draft_identity(key), key].iter().find_map(|wanted| {
        cache
            .entries
            .iter()
            .find(|(held, _)| held == wanted)
            .and_then(|(_, decoded)| decoded.as_shot_kelvin)
    })
}

/// The colour temperature INPUT's camera balanced for, without developing it.
///
/// Asked for on the interface thread whenever the selection changes, so it
/// decodes nothing: rawler fills in the white balance and the camera matrix
/// from the file's metadata, and a dummy image decode skips the photosites
/// entirely. Going through the ordinary decode instead would have cost two
/// seconds of full-resolution development on an 80 MP frame.
pub(crate) fn as_shot_kelvin(input: &Path) -> Result<Option<f32>, Error> {
    if let Some(kelvin) = cached_as_shot_kelvin(input) {
        return Ok(Some(kelvin));
    }
    let source = RawSource::new(input)?;
    let loader = RawLoader::new();
    let decoder = loader
        .get_decoder(&source)
        .map_err(|e| Error::Render(format!("RAW decoder: {e}")))?;
    let raw = decoder
        .raw_image(&source, &RawDecodeParams::default(), true)
        .map_err(|e| Error::Render(format!("RAW metadata: {e}")))?;
    Ok(super::color::as_shot_kelvin(&raw.color_matrix, raw.wb_coeffs))
}

fn decode_source(
    source: &RawSource,
    draft: bool,
    profiling: bool,
) -> Result<DecodedRaw, Error> {
    let mut stage_started = Instant::now();
    macro_rules! profile_stage {
        ($name:literal) => {
            if profiling {
                let now = Instant::now();
                eprintln!(
                    "orfeus-profile stage={} milliseconds={:.3}",
                    $name,
                    now.duration_since(stage_started).as_secs_f64() * 1000.0
                );
                stage_started = now;
            }
        };
    }
    let loader = RawLoader::new();
    let decoder = loader
        .get_decoder(source)
        .map_err(|e| Error::Render(format!("RAW decoder: {e}")))?;
    let params = RawDecodeParams::default();
    let metadata = decoder
        .raw_metadata(source, &params)
        .map_err(|e| Error::Render(format!("RAW metadata: {e}")))?;
    let mut raw = decoder
        .raw_image(source, &params, false)
        .map_err(|e| Error::Render(format!("RAW decode: {e}")))?;
    profile_stage!("decode");
    correct_pixel_shift_cfa(&metadata.make, &metadata.model, &mut raw);
    // Rawler performs scaling, demosaic, and cropping. Orfeus owns white
    // balance and the camera-to-sRGB transform so scene-linear highlights remain
    // unclipped through white adaptation and exposure.
    let steps = vec![
        ProcessingStep::Rescale,
        ProcessingStep::Demosaic,
        ProcessingStep::CropActiveArea,
        ProcessingStep::CropDefault,
    ];
    // A draft preview bins whole sensor quads instead of interpolating every
    // photosite; anything it cannot handle falls back to the full demosaic.
    let roi = develop_roi(raw.width, raw.height, raw.active_area, raw.crop_area);
    let factor = draft_bin_factor(roi.d.w / 2, roi.d.h / 2);
    let developed = match draft.then(|| develop_binned(&raw, factor)).flatten() {
        Some(binned) => {
            if profiling {
                eprintln!("orfeus-profile develop-path=binned factor={factor}");
            }
            binned
        }
        None => match develop_full(&raw).filter(|_| {
            // Set ORFEUS_TILED_DEMOSAIC=0 to develop through rawler instead,
            // which is how the two are compared on real photographs.
            std::env::var_os("ORFEUS_TILED_DEMOSAIC").as_deref()
                != Some(std::ffi::OsStr::new("0"))
        }) {
            Some(developed) => {
                if profiling {
                    eprintln!("orfeus-profile develop-path=tiled-ppg");
                }
                developed
            }
            None => {
                if profiling {
                    eprintln!("orfeus-profile develop-path=rawler draft={draft}");
                }
                RawDevelop { steps }
                    .develop_intermediate(&raw)
                    .map_err(|e| Error::Render(format!("RAW development: {e}")))?
            }
        },
    };
    profile_stage!("develop");
    // Start from the camera's neutral rendering for both as-shot and custom
    // temperature. Custom Kelvin is a relative chromatic adaptation around D65;
    // dropping the sensor WB coefficients leaves Bayer green dominant.
    let white_balance = raw.wb_coeffs;
    let as_shot_kelvin = super::color::as_shot_kelvin(&raw.color_matrix, white_balance);
    let linear_srgb = intermediate_to_linear_srgb(developed, &raw.color_matrix, white_balance)
        .map_err(Error::Color)?;
    profile_stage!("camera-to-srgb");
    let _ = stage_started;
    Ok(DecodedRaw {
        width: linear_srgb.width,
        height: linear_srgb.height,
        data: linear_srgb.data,
        orientation: metadata.exif.orientation.unwrap_or(1),
        make: metadata.make.clone(),
        model: metadata.model.clone(),
        lens_name: metadata
            .lens
            .as_ref()
            .map_or_else(String::new, |lens| lens.lens_name.clone()),
        focal: metadata
            .exif
            .focal_length
            .as_ref()
            .map_or(0.0, |value| value.as_f32()),
        as_shot_kelvin,
    })
}

fn decoded_for_render_with_identity_using<F>(
    input: &Path,
    cache_mode: u32,
    draft: bool,
    profiling: bool,
    decode: F,
) -> Result<(Arc<DecodedRaw>, Option<DecodeCacheKey>), Error>
where
    F: FnOnce() -> Result<DecodedRaw, Error>,
{
    if cache_mode != CACHE_USE {
        return Ok((Arc::new(decode()?), None));
    }
    // Two keys, because a draft decode of a file is a different image from its
    // full decode and must occupy its own cache entry, while the "did the file
    // move under us" check can only compare like with like. Folding the draft
    // marker into the key used for that check made every draft render fail:
    // `file_content_digest` never returns a drafted digest, so the guard below
    // always fired and the whole live-preview path errored out.
    let content_key = decode_cache_key(input)?;
    let key = if draft {
        draft_identity(content_key)
    } else {
        content_key
    };
    let (cache_lock, cache_changed) = decode_cache();
    loop {
        let mut cache = cache_lock
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        if let Some(position) = cache.entries.iter().position(|(entry, _)| *entry == key) {
            let entry = cache.entries.remove(position);
            let decoded = entry.1.clone();
            cache.entries.push(entry);
            if profiling {
                eprintln!("orfeus-profile stage=decode-cache hit=true");
            }
            return Ok((decoded, Some(key)));
        }
        if cache.loading.insert(key) {
            break;
        }
        drop(
            cache_changed
                .wait(cache)
                .unwrap_or_else(|poisoned| poisoned.into_inner()),
        );
    }

    let decode_result = catch_unwind(AssertUnwindSafe(decode));
    let result = match decode_result {
        Ok(result) => result.and_then(|decoded| {
            if file_content_digest(input)? != content_key {
                return Err(Error::Render(
                    "RAW source changed while it was being decoded".into(),
                ));
            }
            Ok(Arc::new(decoded))
        }),
        Err(payload) => {
            let mut cache = cache_lock
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner());
            cache.loading.remove(&key);
            cache_changed.notify_all();
            drop(cache);
            std::panic::resume_unwind(payload);
        }
    };
    let mut cache = cache_lock
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    cache.loading.remove(&key);
    if let Ok(decoded) = &result {
        cache.entries.retain(|(entry, _)| *entry != key);
        if cache.entries.len() >= DECODE_CACHE_CAPACITY {
            cache.entries.remove(0);
        }
        cache.entries.push((key, decoded.clone()));
    }
    cache_changed.notify_all();
    result.map(|decoded| (decoded, Some(key)))
}

fn decoded_for_render_with<F>(
    input: &Path,
    cache_mode: u32,
    draft: bool,
    profiling: bool,
    decode: F,
) -> Result<Arc<DecodedRaw>, Error>
where
    F: FnOnce() -> Result<DecodedRaw, Error>,
{
    decoded_for_render_with_identity_using(input, cache_mode, draft, profiling, decode)
        .map(|(decoded, _)| decoded)
}

/// Decodes at full quality. Colour sampling and analysis use this, because a
/// binned image reports different values than the render it is sampled for.
pub(crate) fn decoded_for_render(
    input: &Path,
    cache_mode: u32,
    profiling: bool,
) -> Result<Arc<DecodedRaw>, Error> {
    decoded_for_render_with(input, cache_mode, false, profiling, || {
        decode_linear_srgb(input, false, profiling)
    })
}

pub(crate) fn decoded_for_render_with_identity(
    input: &Path,
    cache_mode: u32,
    draft: bool,
    profiling: bool,
) -> Result<(Arc<DecodedRaw>, Option<DecodeCacheKey>), Error> {
    decoded_for_render_with_identity_using(input, cache_mode, draft, profiling, || {
        decode_linear_srgb(input, draft, profiling)
    })
}

pub fn render(
    input: &Path,
    output: &Path,
    settings: &RenderSettingsV1,
    cache_mode: u32,
) -> Result<(), Error> {
    let profiling = std::env::var_os("ORFEUS_PROFILE").is_some();
    let render_started = Instant::now();
    settings.validate()?;
    validate_cache_mode(cache_mode)?;
    if same_file(input, output)? {
        return Err(Error::InvalidArgument(
            "input and output refer to the same file",
        ));
    }
    // FFDNet retains large full-image and per-worker feature buffers. Serialize
    // neural renders before decoding so concurrent GUI preview workers cannot
    // each hold a complete RAW image while waiting for inference memory.
    let _neural_render_guard = if settings.neural_noise_reduction > 0.0 {
        Some(
            neural_render_lock()
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner()),
        )
    } else {
        None
    };
    // The flat path always writes a file, so it never develops a draft.
    let (decoded, _) = decoded_for_render_with_identity(input, cache_mode, false, profiling)?;
    let mut stage_started = Instant::now();
    macro_rules! profile_stage {
        ($name:literal) => {
            if profiling {
                let now = Instant::now();
                eprintln!(
                    "orfeus-profile stage={} milliseconds={:.3}",
                    $name,
                    now.duration_since(stage_started).as_secs_f64() * 1000.0
                );
                stage_started = now;
            }
        };
    }
    let orientation = decoded.orientation;
    let (native_max_width, native_max_height) =
        native_downscale_bounds(orientation, settings.max_width, settings.max_height);
    // SAFETY: The caller promises a null or NUL-terminated lens name.
    let named_lens = unsafe { borrowed_c_string(settings.lens_name, "lens name is not UTF-8")? };
    let as_shot_kelvin = decoded.as_shot_kelvin;
    let (make, model, lens_name, focal) = (
        decoded.make.clone(),
        decoded.model.clone(),
        effective_lens_name(&decoded.lens_name, named_lens).to_string(),
        decoded.focal,
    );
    // Downscaling first commutes with the linear white adaptation and exposure
    // gains; interactive re-renders reuse the cached bounded source.
    let mut image = if native_max_width > 0 || native_max_height > 0 {
        own_source(scaled_source_for_render(
            &decoded,
            input,
            native_max_width,
            native_max_height,
            cache_mode,
        ))
    } else {
        own_decoded(decoded)
    };
    profile_stage!("downscale");
    apply_white_adaptation(&mut image, settings.kelvin, settings.tint, as_shot_kelvin);
    apply_exposure(&mut image, settings.exposure_ev);
    profile_stage!("color");
    let explicit_profile = if settings.lens_profile_model.is_null() {
        None
    } else {
        Some(
            unsafe { CStr::from_ptr(settings.lens_profile_model) }
                .to_str()
                .map_err(|_| Error::InvalidArgument("lens profile model is not UTF-8"))?,
        )
    };
    apply_lens(
        &mut image,
        &LensCorrectionOptions {
            make: &make,
            model: &model,
            lens_name: &lens_name,
            focal,
            flags: settings.flags,
            strength: settings.lens_correction_strength,
            explicit_profile,
            focal_reducer: settings.focal_reducer,
            crop_factor: settings.lens_crop_factor,
        },
    )?;
    profile_stage!("lens");
    image = orient(image, orientation);
    profile_stage!("orient");
    // One denoiser or the other. The neural one is not a finishing pass over
    // the edge-aware one's output: it was trained on sensor noise, and handing
    // it something already smoothed asks it to model noise that is no longer
    // there while the two together erase texture neither would alone.
    if settings.neural_noise_reduction > 0.0 {
        super::nn::apply_neural_noise_reduction(
            &mut image.data,
            image.width,
            image.height,
            settings.neural_noise_reduction,
        )?;
    } else {
        apply_noise_reduction(
            &mut image,
            settings.luma_noise_reduction,
            settings.chroma_noise_reduction,
        );
    }
    profile_stage!("noise-reduction");
    apply_tonal_equalizer(
        &mut image,
        [
            settings.tone_blacks,
            settings.tone_shadows,
            settings.tone_dark_mids,
            settings.tone_midtones,
            settings.tone_light_mids,
            settings.tone_highlights,
            settings.tone_whites,
        ],
    );
    profile_stage!("tonal-equalizer");
    apply_display_transform(&mut image, profiling);
    profile_stage!("tone-transfer");
    if settings.lut_strength > 0.0 && !settings.lut_path.is_null() {
        let path = unsafe { CStr::from_ptr(settings.lut_path) }
            .to_str()
            .map_err(|_| Error::InvalidArgument("LUT path is not UTF-8"))?;
        let lut = cached_cube_lut(Path::new(path))?;
        apply_lut(&mut image, &lut, settings.lut_strength);
    }
    apply_grain(
        &mut image,
        settings.grain_amount,
        settings.grain_size,
        settings.grain_seed,
    );
    profile_stage!("lut-grain");
    atomic_encode(
        input,
        output,
        &image,
        settings.output_format,
        settings.jpeg_quality,
    )?;
    profile_stage!("encode");
    let _ = stage_started;
    if profiling {
        eprintln!(
            "orfeus-profile stage=total milliseconds={:.3}",
            render_started.elapsed().as_secs_f64() * 1000.0
        );
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    #[test]
    fn draft_is_requested_only_for_preview_sized_output() {
        // An export asks for no bound, or a bound larger than any sensor.
        assert!(!draft_requested(0, 0));
        assert!(!draft_requested(0, 1200));
        assert!(!draft_requested(6000, 4000));
        assert!(!draft_requested(DRAFT_MAX_DIMENSION + 1, 1200));
        // A preview asks for something every supported sensor dwarfs.
        assert!(draft_requested(1600, 1200));
        assert!(draft_requested(DRAFT_MAX_DIMENSION, DRAFT_MAX_DIMENSION));
        // The smallest sensor Orfeus targets is still more than twice the
        // threshold in both axes, so binning never has to upsample.
        const PEN_F_SENSOR: (u32, u32) = (4608, 3456);
        const { assert!(2 * DRAFT_MAX_DIMENSION <= PEN_F_SENSOR.0) };
        const { assert!(2 * (DRAFT_MAX_DIMENSION * 3 / 4) <= PEN_F_SENSOR.1) };
    }

    #[test]
    fn a_draft_decode_never_shares_an_identity_with_a_full_one() {
        let key: DecodeCacheKey = [7; 32];
        let other: DecodeCacheKey = [8; 32];
        assert_ne!(draft_identity(key), key);
        assert_ne!(draft_identity(key), draft_identity(other));
        // Stable, so a second preview of the same file hits the cache.
        assert_eq!(draft_identity(key), draft_identity(key));
    }

    #[test]
    fn only_the_pixel_shift_composite_has_its_colour_filter_phase_corrected() {
        assert!(olympus_high_resolution(
            "OLYMPUS CORPORATION",
            "PEN-F",
            10400,
            7796
        ));
        assert!(olympus_high_resolution(
            "Olympus Imaging Corp.",
            "pen-f",
            10400,
            7796
        ));
        // An ordinary frame from the same camera is tagged correctly, and
        // shifting it would be the very bug this corrects.
        assert!(!olympus_high_resolution(
            "OLYMPUS CORPORATION",
            "PEN-F",
            5184,
            3888
        ));
        assert!(!olympus_high_resolution("Canon", "EOS R5", 10400, 7796));
    }

    #[test]
    fn shifting_the_pattern_one_column_turns_rggb_into_grbg() {
        // The correction rests on this: what rawler calls RGGB in the 80 MP
        // composite is the same grid one column into a GRBG frame. Adobe's
        // converter writes GRBG for the same photosites.
        let rggb = rawler::CFA::new("RGGB");
        assert_eq!(rggb.shift(1, 0).name, "GRBG");
        for row in 0..2 {
            for column in 0..2 {
                assert_eq!(
                    rggb.shift(1, 0).color_at(row, column),
                    rggb.color_at(row, column + 1)
                );
            }
        }
    }

    #[test]
    fn a_draft_bins_down_to_the_largest_preview_it_will_be_asked_for() {
        // 20 MP: 2592 quads across, and halving that would no longer cover a
        // 2048 px preview, so it bins each quad on its own.
        assert_eq!(draft_bin_factor(2592, 1944), 1);
        // 80 MP: 5184 quads across bins in pairs, landing on the same 2592.
        assert_eq!(draft_bin_factor(5184, 3888), 2);
        // A hypothetical 320 MP frame keeps halving.
        assert_eq!(draft_bin_factor(10368, 7776), 4);
        // The long edge decides, so a panorama is not binned past its height.
        assert_eq!(draft_bin_factor(8192, 512), 4);
        assert_eq!(draft_bin_factor(1, 1), 1);
    }

    #[test]
    fn the_develop_area_prefers_the_default_crop_and_rejects_one_outside_it() {
        let rect = |x, y, w, h| {
            Rect::new(
                rawler::imgop::Point::new(x, y),
                rawler::imgop::Dim2::new(w, h),
            )
        };
        let active = rect(0, 0, 100, 80);
        let crop = rect(10, 10, 80, 60);
        assert_eq!(develop_roi(100, 80, Some(active), Some(crop)), crop);
        assert_eq!(develop_roi(100, 80, Some(active), None), active);
        assert_eq!(develop_roi(100, 80, None, None), rect(0, 0, 100, 80));
        // A crop reaching past the active area would read masked photosites.
        assert_eq!(
            develop_roi(100, 80, Some(rect(10, 10, 80, 60)), Some(rect(0, 0, 100, 80))),
            rect(10, 10, 80, 60)
        );
    }

    /// One quad per output pixel, so the arithmetic is visible by hand.
    #[test]
    fn binning_reads_each_quad_corner_as_the_channel_its_pattern_names() {
        let data: Vec<u16> = vec![
            100, 200, //
            300, 400, //
        ];
        // RGGB: red 100, greens 200 and 300, blue 400, black 0, white 1000.
        let corners: QuadCorners = [
            (0, 0.0, 1000.0),
            (1, 0.0, 1000.0),
            (1, 0.0, 1000.0),
            (2, 0.0, 1000.0),
        ];
        let plan = BinPlan {
            roi: Rect::new(
                rawler::imgop::Point::new(0, 0),
                rawler::imgop::Dim2::new(2, 2),
            ),
            quads_wide: 1,
            quads_high: 1,
            factor: 1,
            corners,
            width: 1,
            height: 1,
        };
        let binned = bin_quads(&data, 2, &plan);
        assert_eq!(binned, vec![[0.1, 0.25, 0.4]]);
    }

    #[test]
    fn binning_averages_whole_quads_and_clips_below_the_black_level() {
        // Four quads in a row, values 0, 1000, 2000, 3000 in every corner, with
        // a black level of 1000: the first quad clips to zero and the mean of
        // the four is (0 + 0 + 1000 + 2000) / 4 / 2000.
        let mut data = vec![0_u16; 8 * 2];
        for quad in 0..4 {
            for corner in 0..4 {
                let (row, column) = (corner / 2, corner % 2);
                data[row * 8 + quad * 2 + column] = (quad as u16) * 1000;
            }
        }
        let corners: QuadCorners = [
            (0, 1000.0, 2000.0),
            (1, 1000.0, 2000.0),
            (1, 1000.0, 2000.0),
            (2, 1000.0, 2000.0),
        ];
        let plan = BinPlan {
            roi: Rect::new(
                rawler::imgop::Point::new(0, 0),
                rawler::imgop::Dim2::new(8, 2),
            ),
            quads_wide: 4,
            quads_high: 1,
            factor: 4,
            corners,
            width: 1,
            height: 1,
        };
        let binned = bin_quads(&data, 8, &plan);
        for channel in binned[0] {
            assert!(
                (channel - 0.375).abs() < 1.0e-6,
                "expected 0.375, got {channel}"
            );
        }
    }

    #[test]
    fn a_partial_edge_pixel_averages_only_the_quads_it_covers() {
        // Three quads binned four at a time: the single output pixel must
        // divide by three, not by four, or the frame's right edge darkens.
        let mut data = vec![0_u16; 6 * 2];
        for value in data.iter_mut() {
            *value = 1000;
        }
        let corners: QuadCorners = [
            (0, 0.0, 1000.0),
            (1, 0.0, 1000.0),
            (1, 0.0, 1000.0),
            (2, 0.0, 1000.0),
        ];
        let plan = BinPlan {
            roi: Rect::new(
                rawler::imgop::Point::new(0, 0),
                rawler::imgop::Dim2::new(6, 2),
            ),
            quads_wide: 3,
            quads_high: 1,
            factor: 4,
            corners,
            width: 1,
            height: 1,
        };
        assert_eq!(bin_quads(&data, 6, &plan), vec![[1.0, 1.0, 1.0]]);
    }

    #[test]
    fn the_srgb_encoding_table_tracks_the_exact_transfer_function() {
        // The table stands in for `powf` on every displayed pixel, so its error
        // has to stay far below the 8-bit step it is quantised into.
        let mut worst: f32 = 0.0;
        for index in 0..200_001 {
            let value = index as f32 / 200_000.0 * 1.2;
            let error = (srgb_encode(value) - exact_srgb_encode(value)).abs();
            worst = worst.max(error);
        }
        assert!(
            worst < 1.0 / 255.0 / 100.0,
            "worst sRGB table error {worst} is too large"
        );
        // Anchors: black, the linear/power join, and white land exactly.
        assert_eq!(srgb_encode(0.0), 0.0);
        assert_eq!(srgb_encode(-1.0), 0.0);
        assert!((srgb_encode(1.0) - 1.0).abs() < 1.0e-6);
        // Above white the exact function still applies, unclamped.
        assert!(srgb_encode(4.0) > 1.0);
        assert_eq!(srgb_encode(4.0), exact_srgb_encode(4.0));
    }

    /// Times the display transform at preview resolution, the pass an
    /// interactive drag runs on every tick.
    #[test]
    #[ignore = "prints machine-specific timings"]
    fn benchmark_display_transform_at_preview_resolution() {
        let (width, height) = (1600, 1200);
        let source: Vec<f32> = (0..width * height * 3)
            .map(|index| (index % 997) as f32 / 700.0)
            .collect();
        for (label, transform) in [
            (
                "fused, tabled",
                (|image: &mut RgbImage| display_tone_and_transfer(&mut image.data))
                    as fn(&mut RgbImage),
            ),
            ("separate passes, powf", |image: &mut RgbImage| {
                super::super::tone::apply_default_display_tone(&mut image.data);
                image.data.par_chunks_mut(1 << 14).for_each(|chunk| {
                    for value in chunk {
                        *value = exact_srgb_encode(*value);
                    }
                });
            }),
            ("one pass, nothing but a multiply", |image: &mut RgbImage| {
                image.data.par_chunks_mut(1 << 14).for_each(|chunk| {
                    for value in chunk {
                        *value = 1.055 * *value - 0.055;
                    }
                });
            }),
        ] {
            let mut times = Vec::new();
            for _ in 0..5 {
                let mut image = RgbImage {
                    width,
                    height,
                    data: source.clone(),
                };
                let started = std::time::Instant::now();
                transform(&mut image);
                times.push(started.elapsed().as_secs_f64() * 1000.0);
                assert!(image.data.iter().all(|value| value.is_finite()));
            }
            times.sort_by(f64::total_cmp);
            eprintln!(
                "display-transform-benchmark {label:22} threads={} median={:.3} ms {times:?}",
                rayon::current_num_threads(),
                times[2]
            );
        }
    }

    /// A scene with the features a denoiser has to tell apart: flat fields at
    /// several brightnesses, hard edges, a fine texture, and a smooth ramp.
    fn denoise_test_scene(width: usize, height: usize) -> Vec<f32> {
        let mut data = vec![0.0_f32; width * height * 3];
        for y in 0..height {
            for x in 0..width {
                let (u, v) = (x as f32 / width as f32, y as f32 / height as f32);
                let band = (v * 4.0) as usize;
                let base = match band {
                    0 => 0.06,             // deep shadow, where noise shows most
                    1 => 0.20 + u * 0.30,  // a smooth ramp for banding
                    2 => {
                        // Hard edges: a bar chart the denoiser must not soften.
                        if ((x / 37) % 2) == 0 { 0.62 } else { 0.30 }
                    }
                    _ => {
                        // Fine texture at the scale noise lives at.
                        0.45 + 0.10 * (((x / 2 + y / 2) % 2) as f32 - 0.5)
                    }
                };
                let tint = [1.0, 0.94, 0.88];
                for channel in 0..3 {
                    data[(y * width + x) * 3 + channel] = base * tint[channel];
                }
            }
        }
        data
    }

    /// Photon noise plus a read floor, deterministically. Variance rises with
    /// the signal, which is what makes a fixed threshold the wrong tool.
    fn add_sensor_noise(clean: &[f32], scale: f32) -> Vec<f32> {
        let mut state = 0x2545_F491_4F6C_DD1D_u64;
        let mut next = || {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            (state >> 40) as f32 / 16_777_216.0 - 0.5
        };
        clean
            .iter()
            .map(|value| {
                // Two draws for something closer to Gaussian than a single one.
                let unit = next() + next();
                let shot = scale * (value.max(0.0).sqrt() * 0.9 + 0.25);
                value + unit * shot
            })
            .collect()
    }

    fn peak_signal_to_noise(clean: &[f32], other: &[f32]) -> f32 {
        let error: f64 = clean
            .iter()
            .zip(other)
            .map(|(clean, other)| {
                let difference = (*clean - *other) as f64;
                difference * difference
            })
            .sum::<f64>()
            / clean.len() as f64;
        if error <= 0.0 {
            return f32::INFINITY;
        }
        (10.0 * (1.0_f64 / error).log10()) as f32
    }

    /// Noise left in a flat field, separately for brightness and for colour.
    ///
    /// Measured apart because the two halves of the denoiser are separate and
    /// a reader of one number cannot tell which one moved: red carries both Y
    /// and Cr, so chroma work flatters a luma measurement taken off it.
    fn flat_field_noise(data: &[f32], width: usize, height: usize) -> (f32, f32) {
        let mut luma = (0.0_f64, 0.0_f64);
        let mut chroma = (0.0_f64, 0.0_f64);
        let mut count = 0.0_f64;
        for y in 8..height / 4 - 8 {
            for x in 8..width - 8 {
                let pixel = &data[(y * width + x) * 3..][..3];
                let y_value =
                    (0.2126 * pixel[0] + 0.7152 * pixel[1] + 0.0722 * pixel[2]) as f64;
                let cr = pixel[0] as f64 - y_value;
                luma = (luma.0 + y_value, luma.1 + y_value * y_value);
                chroma = (chroma.0 + cr, chroma.1 + cr * cr);
                count += 1.0;
            }
        }
        let deviation = |(total, squares): (f64, f64)| {
            let mean = total / count;
            ((squares / count - mean * mean).max(0.0)).sqrt() as f32
        };
        (deviation(luma), deviation(chroma))
    }

    /// Mean height of the bar-chart steps, which the clean scene sets at 0.32.
    /// A denoiser that smooths edges away loses this; one that sharpens
    /// inflates it.
    fn edge_height(data: &[f32], width: usize, height: usize) -> f32 {
        let row = height * 5 / 8;
        let mut total = 0.0_f32;
        let mut count = 0.0_f32;
        let mut boundary = 37;
        while boundary + 4 < width {
            let before = data[(row * width + boundary - 4) * 3];
            let after = data[(row * width + boundary + 4) * 3];
            total += (after - before).abs();
            count += 1.0;
            boundary += 37;
        }
        if count > 0.0 { total / count } else { 0.0 }
    }

    /// Measures what the noise reduction actually achieves, against a scene
    /// whose clean version is known.
    #[test]
    #[ignore = "prints machine-specific timings"]
    fn benchmark_noise_reduction_quality() {
        let (width, height) = (1600, 1200);
        let clean = denoise_test_scene(width, height);
        let noisy = add_sensor_noise(&clean, 0.09);
        for (label, data) in [("clean", &clean), ("noisy", &noisy)] {
            let (luma, chroma) = flat_field_noise(data, width, height);
            eprintln!(
                "noise-quality {label:22} psnr={:.2} dB  luma sigma={luma:.4}  chroma sigma={chroma:.4}  edge={:.3}",
                peak_signal_to_noise(&clean, data),
                edge_height(data, width, height)
            );
        }
        for (label, luma, chroma) in [
            // What the slider used to mean: a fifth of it for brightness.
            ("old mapping, slider 0.35", 0.2 * 0.35, 0.35),
            ("old mapping, slider 1.00", 0.2, 1.0),
            // What it means now.
            ("slider 0.35", 0.35, 0.35),
            ("slider 0.60", 0.60, 0.60),
            ("slider 1.00", 1.0, 1.0),
        ] {
            let mut image = RgbImage {
                width,
                height,
                data: noisy.clone(),
            };
            let started = std::time::Instant::now();
            cpu_noise_reduction(&mut image, luma, chroma);
            let milliseconds = started.elapsed().as_secs_f64() * 1000.0;
            let (luma_sigma, chroma_sigma) = flat_field_noise(&image.data, width, height);
            eprintln!(
                "noise-quality {label:22} psnr={:.2} dB  luma sigma={luma_sigma:.4}  chroma sigma={chroma_sigma:.4}  edge={:.3}  {milliseconds:.0} ms",
                peak_signal_to_noise(&clean, &image.data),
                edge_height(&image.data, width, height)
            );
        }
    }

    /// Times the noise reduction at export resolution, isolating it from decode
    /// and encode. Used to measure scratch-buffer handling, which is invisible
    /// in an end-to-end render's noise.
    #[test]
    #[ignore = "prints machine-specific timings"]
    fn benchmark_noise_reduction_at_export_resolution() {
        let (width, height) = (5184, 3888);
        let source: Vec<f32> = (0..width * height * 3)
            .map(|index| {
                let ripple = ((index % 11) as f32 - 5.0) * 0.008;
                (0.3 + ripple + (index % 1409) as f32 / 4000.0).max(0.0)
            })
            .collect();
        for (label, luma, chroma) in [
            ("both", 0.35, 0.35),
            ("luma only", 0.35, 0.0),
            ("chroma only", 0.0, 0.35),
        ] {
            let mut times = Vec::new();
            for _ in 0..5 {
                let mut image = RgbImage {
                    width,
                    height,
                    data: source.clone(),
                };
                let started = std::time::Instant::now();
                cpu_noise_reduction(&mut image, luma, chroma);
                times.push(started.elapsed().as_secs_f64() * 1000.0);
                assert!(image.data.iter().all(|value| value.is_finite()));
            }
            times.sort_by(f64::total_cmp);
            eprintln!(
                "noise-reduction-benchmark {label:12} pixels={} threads={} median={:.1} ms {times:?}",
                width * height,
                rayon::current_num_threads(),
                times[2]
            );
        }
    }

    use super::*;
    use image::{ImageDecoder, ImageReader};
    use std::io::Cursor;
    use std::os::unix::fs::symlink;
    use std::sync::Barrier;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::thread;

    fn identity_cube() -> CubeLut {
        CubeLut::parse(Cursor::new(
            "LUT_3D_SIZE 2\n0 0 0\n1 0 0\n0 1 0\n1 1 0\n0 0 1\n1 0 1\n0 1 1\n1 1 1\n",
        ))
        .unwrap()
    }

    #[test]
    fn gpu_fallback_complains_once_and_names_the_reason() {
        let message = gpu_status_message(GpuStatus::Unavailable("ERROR_INCOMPATIBLE_DRIVER"));
        assert!(message.contains("WARNING"), "{message}");
        assert!(message.contains("ERROR_INCOMPATIBLE_DRIVER"), "{message}");
        assert!(message.contains("CPU"), "{message}");
        // The disabled notice tells the reader how to turn Vulkan on.
        let disabled = gpu_status_message(GpuStatus::Disabled);
        assert!(disabled.contains("ORFEUS_GPU=0"), "{disabled}");
        assert!(!disabled.contains("WARNING"), "{disabled}");
        assert!(gpu_status_message(GpuStatus::Active("llvmpipe")).contains("llvmpipe"));
        // One line per process, however many frames render.
        let cell = OnceLock::new();
        assert!(report_gpu_status_once(&cell, GpuStatus::Disabled));
        assert!(!report_gpu_status_once(&cell, GpuStatus::Disabled));
        assert!(!report_gpu_status_once(&cell, GpuStatus::Disabled));
    }
    fn image() -> RgbImage {
        RgbImage {
            width: 2,
            height: 3,
            data: vec![0.25; 18],
        }
    }
    fn temp(name: &str) -> PathBuf {
        std::env::temp_dir().join(format!("orfeus-test-{}-{name}", std::process::id()))
    }

    fn test_decoded_raw() -> DecodedRaw {
        DecodedRaw {
            width: 1,
            height: 1,
            data: vec![0.5; 3],
            orientation: 1,
            make: String::new(),
            model: String::new(),
            lens_name: String::new(),
            focal: 0.0,
            as_shot_kelvin: None,
        }
    }

    #[test]
    fn decode_cache_keys_complete_content_not_metadata_or_path() {
        let first = temp("cache-key-first.raw");
        let second = temp("cache-key-second.raw");
        fs::write(&first, [1, 2, 3, 4]).unwrap();
        fs::write(&second, [1, 2, 3, 4]).unwrap();
        let original = decode_cache_key(&first).unwrap();
        assert_eq!(original, decode_cache_key(&second).unwrap());
        fs::write(&first, [4, 3, 2, 1]).unwrap();
        assert_ne!(original, decode_cache_key(&first).unwrap());
        fs::remove_file(first).unwrap();
        fs::remove_file(second).unwrap();
    }

    #[test]
    fn decode_cache_keys_are_memoized_without_rereading_the_file() {
        // The memo must not outlive a rewrite: a changed size or
        // modification time has to produce a fresh hash, or interactive
        // renders would keep grading a stale image.
        let path = temp("cache-key-memo.raw");
        fs::write(&path, [1, 2, 3, 4]).unwrap();
        let first = decode_cache_key(&path).unwrap();
        assert_eq!(first, decode_cache_key(&path).unwrap());
        assert_eq!(first, file_content_digest(&path).unwrap());
        // A rewrite of a different length changes the stat identity.
        fs::write(&path, [4, 3, 2, 1, 0]).unwrap();
        let second = decode_cache_key(&path).unwrap();
        assert_ne!(first, second);
        assert_eq!(second, file_content_digest(&path).unwrap());
        fs::remove_file(path).unwrap();
    }

    #[test]
    fn a_draft_decode_is_cached_apart_from_the_full_one_and_still_succeeds() {
        // The whole live-preview path asks for a draft, so a draft decode that
        // reports "RAW source changed while it was being decoded" takes every
        // interactive render down with it. That shipped once: the draft marker
        // was folded into the key the freshness check compared against, and
        // `file_content_digest` cannot ever produce a drafted digest.
        let input = temp("draft-identity.raw");
        fs::write(&input, b"a draft and a full decode of one file").unwrap();
        let decodes = Arc::new(AtomicUsize::new(0));
        let decode = || {
            let decodes = Arc::clone(&decodes);
            move || {
                decodes.fetch_add(1, Ordering::SeqCst);
                Ok(test_decoded_raw())
            }
        };
        let (_, draft_key) =
            decoded_for_render_with_identity_using(&input, CACHE_USE, true, false, decode())
                .expect("a draft decode must not be mistaken for a changed file");
        assert_eq!(decodes.load(Ordering::SeqCst), 1);
        // Asking again reuses the entry rather than decoding a second time.
        decoded_for_render_with_identity_using(&input, CACHE_USE, true, false, decode()).unwrap();
        assert_eq!(decodes.load(Ordering::SeqCst), 1);
        // The full decode is a different image, so it needs its own entry.
        let (_, full_key) =
            decoded_for_render_with_identity_using(&input, CACHE_USE, false, false, decode())
                .expect("a full decode of the same file must succeed too");
        assert_eq!(decodes.load(Ordering::SeqCst), 2);
        assert_ne!(draft_key, full_key);
        fs::remove_file(input).unwrap();
    }

    #[test]
    fn concurrent_decode_cache_misses_share_one_loader() {
        let input = temp("cache-concurrent.raw");
        fs::write(&input, b"unique concurrent cache source").unwrap();
        let workers = 4;
        let barrier = Arc::new(Barrier::new(workers));
        let loads = Arc::new(AtomicUsize::new(0));
        let threads: Vec<_> = (0..workers)
            .map(|_| {
                let input = input.clone();
                let barrier = barrier.clone();
                let loads = loads.clone();
                thread::spawn(move || {
                    barrier.wait();
                    decoded_for_render_with(&input, CACHE_USE, false, false, || {
                        loads.fetch_add(1, Ordering::SeqCst);
                        thread::sleep(std::time::Duration::from_millis(50));
                        Ok(test_decoded_raw())
                    })
                    .unwrap()
                })
            })
            .collect();
        let decoded: Vec<_> = threads
            .into_iter()
            .map(|worker| worker.join().unwrap())
            .collect();
        assert_eq!(loads.load(Ordering::SeqCst), 1);
        assert!(
            decoded[1..]
                .iter()
                .all(|entry| Arc::ptr_eq(&decoded[0], entry))
        );
        fs::remove_file(input).unwrap();
    }

    #[test]
    fn failed_or_changed_loads_release_decode_cache_reservations() {
        let input = temp("cache-retry.raw");
        fs::write(&input, b"first cache source").unwrap();
        let panicked = catch_unwind(AssertUnwindSafe(|| {
            let _ = decoded_for_render_with(&input, CACHE_USE, false, false, || {
                panic!("synthetic decoder panic")
            });
        }));
        assert!(panicked.is_err());
        let failed = decoded_for_render_with(&input, CACHE_USE, false, false, || {
            Err(Error::Render("synthetic decode failure".into()))
        });
        assert!(failed.is_err());
        let changed = decoded_for_render_with(&input, CACHE_USE, false, false, || {
            fs::write(&input, b"other cache source").unwrap();
            Ok(test_decoded_raw())
        });
        assert!(matches!(changed, Err(Error::Render(message)) if message.contains("changed")));
        let retried =
            decoded_for_render_with(&input, CACHE_USE, false, false, || Ok(test_decoded_raw()));
        assert!(retried.is_ok());
        fs::remove_file(input).unwrap();
    }

    #[test]
    fn high_kelvin_adaptation_warms_a_neutral_pixel() {
        let mut image = RgbImage {
            width: 1,
            height: 1,
            data: vec![0.5, 0.5, 0.5],
        };
        apply_white_adaptation(&mut image, 15_000.0, 0.0, None);
        assert!(image.data[0] > image.data[1]);
        assert!(image.data[1] > image.data[2]);
    }

    #[test]
    fn tint_extremes_move_neutral_pixels_in_opposite_directions() {
        let mut magenta = RgbImage {
            width: 1,
            height: 1,
            data: vec![0.5, 0.5, 0.5],
        };
        let mut green = magenta.clone();
        apply_white_adaptation(&mut magenta, 0.0, 20.0, None);
        apply_white_adaptation(&mut green, 0.0, -20.0, None);
        assert!(magenta.data[0] + magenta.data[2] > magenta.data[1] * 2.0);
        assert!(green.data[1] * 2.0 > green.data[0] + green.data[2]);
        assert!((magenta.data[1] - green.data[1]).abs() > 0.1);
    }

    fn flat_patch_luma_rms(image: &RgbImage, expected: f32) -> f32 {
        let squared_error = image
            .data
            .as_chunks::<3>()
            .0
            .iter()
            .map(|pixel| {
                let luma = 0.2126 * pixel[0] + 0.7152 * pixel[1] + 0.0722 * pixel[2];
                (luma - expected).powi(2)
            })
            .sum::<f32>();
        (squared_error / (image.width * image.height) as f32).sqrt()
    }

    #[test]
    fn noise_reduction_materially_smooths_a_flat_noisy_patch() {
        let mut original = RgbImage {
            width: 32,
            height: 24,
            data: Vec::new(),
        };
        for y in 0..original.height {
            for x in 0..original.width {
                let hash = ((x * 73 + y * 151 + x * y * 19) % 101) as f32 / 100.0;
                let value = 0.5 + (hash - 0.5) * 0.16;
                original.data.extend_from_slice(&[value, value, value]);
            }
        }
        let initial_rms = flat_patch_luma_rms(&original, 0.5);
        let mut moderate = original.clone();
        let mut maximum = original;
        apply_noise_reduction(&mut moderate, 0.35, 0.35);
        apply_noise_reduction(&mut maximum, 1.0, 1.0);
        let moderate_rms = flat_patch_luma_rms(&moderate, 0.5);
        let maximum_rms = flat_patch_luma_rms(&maximum, 0.5);
        assert!(
            moderate_rms < initial_rms * 0.75,
            "moderate NR RMS {moderate_rms} did not materially improve {initial_rms}"
        );
        assert!(
            maximum_rms < moderate_rms * 0.8,
            "maximum NR RMS {maximum_rms} did not improve moderate {moderate_rms}"
        );
    }

    fn flat_patch_chroma_rms(image: &RgbImage) -> f32 {
        let squared = image
            .data
            .as_chunks::<3>()
            .0
            .iter()
            .map(|pixel| {
                let yy = 0.2126 * pixel[0] + 0.7152 * pixel[1] + 0.0722 * pixel[2];
                (pixel[2] - yy).powi(2) + (pixel[0] - yy).powi(2)
            })
            .sum::<f32>();
        (squared / (image.width * image.height) as f32).sqrt()
    }

    #[test]
    fn multiscale_chroma_reduction_removes_colored_blotches() {
        let mut image = RgbImage {
            width: 35,
            height: 29,
            data: Vec::new(),
        };
        for y in 0..image.height {
            for x in 0..image.width {
                let blotch = if ((x / 5) + (y / 5)) % 2 == 0 {
                    0.09
                } else {
                    -0.09
                };
                image
                    .data
                    .extend_from_slice(&[0.3 + blotch, 0.3, 0.3 - blotch]);
            }
        }
        let before = flat_patch_chroma_rms(&image);
        apply_noise_reduction(&mut image, 0.0, 0.6);
        let after = flat_patch_chroma_rms(&image);
        assert!(
            after < before * 0.35,
            "chroma RMS {after} remained near {before}"
        );
        assert!(
            flat_patch_luma_rms(&image, 0.3) < 0.015,
            "chroma filtering changed flat-patch luminance"
        );
    }

    #[test]
    fn edge_aware_noise_reduction_suppresses_chroma_without_crossing_edges() {
        let mut image = RgbImage {
            width: 7,
            height: 5,
            data: Vec::new(),
        };
        for y in 0..image.height {
            for x in 0..image.width {
                let base = if x < 3 { 0.1 } else { 0.8 };
                let noise = if (x + y) % 2 == 0 { 0.08 } else { -0.08 };
                image
                    .data
                    .extend_from_slice(&[base + noise, base, base - noise]);
            }
        }
        let before = image.clone();
        apply_noise_reduction(&mut image, 0.5, 1.0);
        let dark_edge = image.data[(2 * image.width + 2) * 3];
        let bright_edge = image.data[(2 * image.width + 3) * 3];
        assert!(bright_edge - dark_edge > 0.5, "edge was smeared");
        let noisy = (before.data[0] - before.data[2]).abs();
        let reduced = (image.data[0] - image.data[2]).abs();
        assert!(reduced < noisy * 0.8, "chroma noise was not reduced");
    }

    #[test]
    fn zero_tonal_equalizer_is_exact_identity() {
        let mut image = RgbImage {
            width: 2,
            height: 1,
            data: vec![0.01, 0.02, 0.03, 0.2, 0.4, 0.8],
        };
        let before = image.data.clone();
        apply_tonal_equalizer(&mut image, [0.0; 7]);
        assert_eq!(image.data, before);
    }

    #[test]
    fn tonal_equalizer_applies_each_anchor_gain() {
        let anchors = [0.002_812_5, 0.011_25, 0.045, 0.18, 0.72, 2.88, 11.52];
        for (index, luminance) in anchors.into_iter().enumerate() {
            let mut adjustments = [0.0; 7];
            adjustments[index] = 1.0;
            let mut image = RgbImage {
                width: 1,
                height: 1,
                data: vec![luminance; 3],
            };
            apply_tonal_equalizer(&mut image, adjustments);
            for value in image.data {
                assert!((value - luminance * 2.0).abs() < luminance * 1.0e-5 + 1.0e-7);
            }
        }
    }

    #[test]
    fn tonal_equalizer_is_smooth_and_preserves_chromaticity() {
        let adjustments = [-1.0, -0.5, 0.0, 0.5, 1.0, 0.25, -0.25];
        let boundary = 0.18;
        let below = tone_adjustment_ev(boundary * 2.0_f32.powf(-0.001), &adjustments);
        let above = tone_adjustment_ev(boundary * 2.0_f32.powf(0.001), &adjustments);
        assert!((below - above).abs() < 0.001);

        let mut image = RgbImage {
            width: 1,
            height: 1,
            data: vec![0.09, 0.18, 0.36],
        };
        let before = image.data.clone();
        apply_tonal_equalizer(&mut image, adjustments);
        let red_gain = image.data[0] / before[0];
        let green_gain = image.data[1] / before[1];
        let blue_gain = image.data[2] / before[2];
        assert!((red_gain - green_gain).abs() < 1.0e-6);
        assert!((green_gain - blue_gain).abs() < 1.0e-6);
    }

    #[test]
    fn correction_strength_blends_identity_and_lensfun_coordinates() {
        assert_eq!(10.0, blend_lens_coordinate(10.0, 14.0, 0.0));
        assert_eq!(12.0, blend_lens_coordinate(10.0, 14.0, 0.5));
        assert_eq!(14.0, blend_lens_coordinate(10.0, 14.0, 1.0));
        assert_eq!(18.0, blend_lens_coordinate(10.0, 14.0, 2.0));
    }

    #[test]
    fn lens_auto_crop_removes_invalid_borders() {
        let mut valid = vec![true; 100 * 80];
        for y in 0..80 {
            for x in 0..100 {
                if !(5..95).contains(&x) || !(4..76).contains(&y) {
                    valid[y * 100 + x] = false;
                }
            }
        }
        let scale = lens_auto_crop_scale(&valid, 100, 80);
        assert!(scale > 1.09 && scale < 1.2, "unexpected crop scale {scale}");
        assert_eq!(1.0, lens_auto_crop_scale(&vec![true; 100 * 80], 100, 80));
    }

    #[test]
    fn matches_common_micro_four_thirds_lens_aliases() {
        let db = Database::load_bundled().unwrap();
        for (make, model, lens_name, focal) in [
            (
                "OLYMPUS CORPORATION",
                "PEN-F",
                "Leica DG Summilux 25mm F1.4 Asph.",
                25.0,
            ),
            (
                "OLYMPUS CORPORATION",
                "PEN-F",
                "Leica DG Macro-Elmarit 45mm F2.8 Asph. Mega OIS",
                45.0,
            ),
            (
                "OM Digital Solutions",
                "OM-1",
                "Olympus M.Zuiko Digital ED 12-45mm F4.0 Pro",
                25.0,
            ),
        ] {
            let camera = find_camera_profile(&db, make, model)
                .unwrap_or_else(|| panic!("camera profile missing for {make} {model}"));
            let lens = find_lens_profile(&db, camera, lens_name, focal)
                .unwrap_or_else(|| panic!("lens profile missing for {lens_name}"));
            assert!(camera_mount_compatible(&db, camera, lens));
        }
        let camera = find_camera_profile(&db, "OLYMPUS CORPORATION", "PEN-F").unwrap();
        assert!(find_lens_profile(&db, camera, "Ultron 0.7x", 28.4).is_none());
        let ultron = find_explicit_lens_profile(
            &db,
            "Voigtlander Ultron 40mm f/2 SLII Aspherical",
            28.4 / 0.71,
        )
        .expect("explicit adapted-lens profile missing");
        assert!(ultron.mounts.iter().any(|mount| mount.contains("Nikon F")));
        assert!(
            find_explicit_lens_profile(&db, "Carl Zeiss Makro-Planar T* 2/50 ZF.2", 50.0,)
                .is_none(),
            "an absent Makro-Planar 50/2 must not fall through to another Planar"
        );
    }

    #[test]
    fn vertically_interpolated_lens_maps_match_direct_evaluation() {
        let db = Database::load_bundled().unwrap();
        let camera = find_camera_profile(&db, "OM Digital Solutions", "OM-1").unwrap();
        let lens = find_lens_profile(
            &db,
            camera,
            "Olympus M.Zuiko Digital ED 12-45mm F4.0 Pro",
            12.0,
        )
        .unwrap();
        let width = 320_usize;
        let mut modifier = Modifier::new(lens, 12.0, camera.crop_factor, width as u32, 240, false);
        assert!(modifier.enable_distortion_correction(lens));
        let probe_y = 100_usize;
        let low_y = probe_y - probe_y % LENS_MAP_ROW_STEP;
        let mut direct = vec![0.0_f32; width * 2];
        let mut low = vec![0.0_f32; width * 2];
        let mut high = vec![0.0_f32; width * 2];
        assert!(modifier.apply_geometry_distortion(0.0, probe_y as f32, width, 1, &mut direct));
        assert!(modifier.apply_geometry_distortion(0.0, low_y as f32, width, 1, &mut low));
        assert!(modifier.apply_geometry_distortion(
            0.0,
            (low_y + LENS_MAP_ROW_STEP) as f32,
            width,
            1,
            &mut high
        ));
        let fraction = (probe_y - low_y) as f32 / LENS_MAP_ROW_STEP as f32;
        for index in 0..width * 2 {
            let interpolated = low[index] + (high[index] - low[index]) * fraction;
            assert!(
                (interpolated - direct[index]).abs() < 0.05,
                "lens map interpolation drifted {} pixels at {index}",
                (interpolated - direct[index]).abs()
            );
        }
    }

    #[test]
    fn area_downscale_averages_uniform_blocks_and_bounds_dimensions() {
        let mut image = RgbImage {
            width: 8,
            height: 4,
            data: Vec::new(),
        };
        for y in 0..4 {
            for x in 0..8 {
                let value = if x < 4 { 1.0 } else { 0.0 };
                let _ = y;
                image.data.extend_from_slice(&[value, value, value]);
            }
        }
        let result = downscale_from(&image.data, image.width, image.height, 4, 2).unwrap();
        assert_eq!((result.width, result.height), (4, 2));
        assert!((result.data[0] - 1.0).abs() < 1.0e-6);
        assert!((result.data[(2 * 4 - 1) * 3] - 0.0).abs() < 1.0e-6);
        let middle = result.data[3];
        assert!((middle - 1.0).abs() < 1.0e-6, "left half must stay white");
    }

    #[test]
    fn the_shot_temperature_is_the_temperature_that_changes_nothing() {
        // This is the whole contract of the control: asking for the
        // temperature a frame was shot at must render what "as shot" renders.
        // Anything else and the number on the control is decoration.
        let pixels: Vec<f32> = (0..300).map(|index| (index % 97) as f32 / 96.0).collect();
        let mut as_shot = RgbImage {
            width: 10,
            height: 10,
            data: pixels.clone(),
        };
        let mut asked_for_its_own = RgbImage {
            width: 10,
            height: 10,
            data: pixels,
        };
        apply_white_adaptation(&mut as_shot, 0.0, 0.0, Some(5200.0));
        apply_white_adaptation(&mut asked_for_its_own, 5200.0, 0.0, Some(5200.0));
        for (untouched, asked) in as_shot.data.iter().zip(&asked_for_its_own.data) {
            assert!(
                (untouched - asked).abs() < 1.0e-5,
                "{untouched} against {asked}"
            );
        }
        // And it still moves in the photographic direction: asking for a
        // warmer illuminant than the frame was shot under warms the picture.
        let mut warmer = RgbImage {
            width: 10,
            height: 10,
            data: as_shot.data.clone(),
        };
        apply_white_adaptation(&mut warmer, 8000.0, 0.0, Some(5200.0));
        let redness = |image: &RgbImage| -> f32 {
            let (mut red, mut blue) = (0.0, 0.0);
            for pixel in image.data.as_chunks::<3>().0 {
                red += pixel[0];
                blue += pixel[2];
            }
            red / blue.max(1.0e-6)
        };
        assert!(
            redness(&warmer) > redness(&as_shot),
            "a warmer illuminant did not warm the picture"
        );
    }

    #[test]
    fn median_network_matches_sorted_median() {
        let samples = [0.3_f32, -1.0, 0.5, 0.2, 0.9, -0.5, 0.0, 0.7, 0.1];
        let mut sorted = samples;
        sorted.sort_unstable_by(f32::total_cmp);
        assert_eq!(median_of_9(samples), sorted[4]);
        let ramp = [9.0_f32, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0];
        assert_eq!(median_of_9(ramp), 5.0);
    }

    #[test]
    fn the_vectorized_median_filter_matches_the_scalar_one_exactly() {
        // A median is exact, so the vector path may not differ by so much as a
        // bit; a plane wide enough to have an interior, a vector remainder and
        // both clamped edges catches a mistake in any of them.
        for width in [11_usize, 19, 24, 33, 64, 97, 1024, 4099] {
            let height = 7;
            let source: Vec<f32> = (0..width * height)
                .map(|index| {
                    // Flat runs, duplicates and both signed zeros as well as a
                    // ramp: a chroma plane is a difference, so exact zeros are
                    // its most common value.
                    match (index * 2_654_435_761_usize) % 13 {
                        0 => 0.0,
                        1 => -0.0,
                        2 => 0.25,
                        3 => -0.25,
                        4 => 4.0,
                        other => (other as f32) / 500.0 - 1.0,
                    }
                })
                .collect();
            let mut vectorized = Vec::new();
            median_filter_3x3_into(&source, width, height, 0.75, &mut vectorized);
            let mut scalar = vec![0.0_f32; source.len()];
            for y in 0..height {
                let above = &source[y.saturating_sub(1) * width..];
                let center = &source[y * width..];
                let below = &source[(y + 1).min(height - 1) * width..];
                for x in 0..width {
                    let left = x.saturating_sub(1);
                    let right = (x + 1).min(width - 1);
                    let median = median_of_9([
                        above[left],
                        above[x],
                        above[right],
                        center[left],
                        center[x],
                        center[right],
                        below[left],
                        below[x],
                        below[right],
                    ]);
                    scalar[y * width + x] = center[x] + (median - center[x]) * 0.75;
                }
            }
            assert_eq!(vectorized, scalar, "width {width}");
        }
    }

    #[test]
    fn lensfun_correction_maps_barrel_distorted_source_in_the_correct_direction() {
        let db = Database::load_bundled().unwrap();
        let camera = find_camera_profile(&db, "OM Digital Solutions", "OM-1").unwrap();
        let lens = find_lens_profile(
            &db,
            camera,
            "Olympus M.Zuiko Digital ED 12-45mm F4.0 Pro",
            12.0,
        )
        .unwrap();
        let mut modifier = Modifier::new(lens, 12.0, camera.crop_factor, 1600, 1200, false);
        assert!(modifier.enable_distortion_correction(lens));
        let mut coordinate = [0.0; 2];
        assert!(modifier.apply_geometry_distortion(0.0, 0.0, 1, 1, &mut coordinate));
        assert!(
            coordinate[0] > 0.0 && coordinate[1] > 0.0,
            "corrected output must sample inward from a barrel-distorted source: {coordinate:?}"
        );
    }

    #[test]
    fn permits_nonzero_lut_strength_without_an_active_lut() {
        let settings = RenderSettingsV1 {
            lut_strength: 1.0,
            lut_path: std::ptr::null(),
            ..RenderSettingsV1::default()
        };
        assert!(settings.validate().is_ok());
    }

    #[test]
    fn validates_unknown_flags_and_non_finite_as_invalid_argument() {
        let settings = RenderSettingsV1 {
            flags: 4,
            ..RenderSettingsV1::default()
        };
        assert!(matches!(
            settings.validate(),
            Err(Error::InvalidArgument(_))
        ));
        let settings = RenderSettingsV1 {
            tint: f32::NAN,
            ..RenderSettingsV1::default()
        };
        assert!(matches!(
            settings.validate(),
            Err(Error::InvalidArgument(_))
        ));
    }

    #[test]
    fn cube_parser_is_bounded_and_strict() {
        assert_eq!(identity_cube().values.len(), 8);
        assert!(CubeLut::parse(Cursor::new("LUT_3D_SIZE 66\n")).is_err());
        assert!(CubeLut::parse(Cursor::new("LUT_3D_SIZE 2\n0 0 0")).is_err());
    }

    #[test]
    fn bundled_film_luts_are_valid_cubes() {
        let directory = Path::new(env!("CARGO_MANIFEST_DIR")).join("../data/luts");
        let mut paths = fs::read_dir(directory)
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .filter(|path| {
                path.extension()
                    .is_some_and(|extension| extension == "cube")
            })
            .collect::<Vec<_>>();
        paths.sort();
        assert_eq!(paths.len(), 12, "unexpected bundled LUT count");
        for path in &paths {
            CubeLut::read(path)
                .unwrap_or_else(|error| panic!("invalid bundled LUT {}: {error}", path.display()));
        }
        // Repeated reads of one LUT are served from the cache, so dragging a
        // film node's grain no longer re-parses the file on every tick.
        let first = &paths[0];
        let once = cached_cube_lut(first).expect("cached bundled LUT");
        let held = lut_cache_len();
        let twice = cached_cube_lut(first).expect("cached bundled LUT");
        assert!(Arc::ptr_eq(&once, &twice), "LUT was parsed twice");
        assert_eq!(held, lut_cache_len(), "a cache hit must not add an entry");
        assert!(held <= LUT_CACHE_CAPACITY);
    }

    #[test]
    fn trilinear_identity_interpolates() {
        let sampled = identity_cube().sample([0.25, 0.5, 0.75]);
        for (actual, expected) in sampled.into_iter().zip([0.25, 0.5, 0.75]) {
            assert!((actual - expected).abs() < 1e-6);
        }
    }

    #[test]
    fn orientation_six_rotates_dimensions() {
        let result = orient(image(), 6);
        assert_eq!((result.width, result.height), (3, 2));
        assert_eq!(native_downscale_bounds(6, 1600, 1200), (1200, 1600));
        assert_eq!(native_downscale_bounds(1, 1600, 1200), (1600, 1200));
    }

    #[test]
    fn exports_real_sixteen_bit_tiff_with_icc() {
        let mut bytes = Cursor::new(Vec::new());
        encode_to(&image(), &mut bytes, OUTPUT_TIFF, 90).unwrap();
        bytes.set_position(0);
        let mut decoder = image::codecs::tiff::TiffDecoder::new(bytes).unwrap();
        assert_eq!(decoder.color_type(), ColorType::Rgb16);
        assert!(decoder.icc_profile().unwrap().is_some());
    }

    #[test]
    fn exported_jpeg_contains_icc() {
        let mut bytes = Cursor::new(Vec::new());
        encode_to(&image(), &mut bytes, OUTPUT_JPEG, 90).unwrap();
        let mut decoder = ImageReader::new(Cursor::new(bytes.into_inner()))
            .with_guessed_format()
            .unwrap()
            .into_decoder()
            .unwrap();
        assert!(decoder.icc_profile().unwrap().is_some());
    }

    #[test]
    fn atomic_export_rejects_same_symlink_and_hardlink_input() {
        let input = temp("input");
        let symlink_path = temp("symlink");
        let hardlink_path = temp("hardlink");
        fs::write(&input, b"raw photograph").unwrap();
        let _ = fs::remove_file(&symlink_path);
        let _ = fs::remove_file(&hardlink_path);
        symlink(&input, &symlink_path).unwrap();
        fs::hard_link(&input, &hardlink_path).unwrap();
        assert!(matches!(
            atomic_encode(&input, &input, &image(), OUTPUT_JPEG, 90),
            Err(Error::InvalidArgument(_))
        ));
        assert!(matches!(
            atomic_encode(&input, &symlink_path, &image(), OUTPUT_JPEG, 90),
            Err(Error::InvalidArgument(_))
        ));
        assert!(matches!(
            atomic_encode(&input, &hardlink_path, &image(), OUTPUT_JPEG, 90),
            Err(Error::InvalidArgument(_))
        ));
        assert_eq!(fs::read(&input).unwrap(), b"raw photograph");
        for path in [&symlink_path, &hardlink_path, &input] {
            let _ = fs::remove_file(path);
        }
    }

    #[test]
    fn successful_export_replaces_destination_atomically() {
        let input = temp("atomic-input");
        let output = temp("atomic-output");
        fs::write(&input, b"raw").unwrap();
        fs::write(&output, b"old").unwrap();
        atomic_encode(&input, &output, &image(), OUTPUT_JPEG, 90).unwrap();
        assert!(fs::read(&output).unwrap().starts_with(&[0xff, 0xd8]));
        let _ = fs::remove_file(input);
        let _ = fs::remove_file(output);
    }
}
