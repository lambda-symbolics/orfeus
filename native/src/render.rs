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
    /// Hand-set barrel or pincushion correction, for lenses no database
    /// describes. Positive straightens barrel, negative pincushion.
    pub lens_distortion: f32,
    /// The focal length to read the lens profile at, in millimetres, when the
    /// photograph does not say — a manual lens on a dumb adapter records
    /// nothing. Zero defers to the file.
    pub focal_length: f32,
    /// Where the colour fringing correction comes from when `FLAG_LENS_TCA` is
    /// set: 0 measures it from the frame, 1 reads the lens profile.
    pub chromatic_aberration_source: u32,
}

/// The focal length a correction is read at: what the caller stated when it
/// stated one, otherwise what the file recorded.
pub(crate) fn effective_focal(recorded: f32, stated: f32) -> f32 {
    if stated.is_finite() && stated > 0.0 {
        stated
    } else {
        recorded
    }
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
            lens_distortion: 0.0,
            focal_reducer: 1.0,
            lens_crop_factor: 0.0,
            lut_path: std::ptr::null(),
            lens_profile_model: std::ptr::null(),
            neural_noise_reduction: 0.0,
            lens_name: std::ptr::null(),
            focal_length: 0.0,
            chromatic_aberration_source: 0,
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
            (self.lens_distortion, "lens distortion"),
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
            || !(-0.5..=0.5).contains(&self.lens_distortion)
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

/// Where a luminance sits among the seven zones: 0 at black, 6 at white.
///
/// Anchored on what the pixel will *look* like rather than on how many stops it
/// is from middle grey. The stops version put its anchors at -6, -4, -2, 0, +2,
/// +4 and +6 EV, which sounds even and is not: run through the display tone
/// curve those land at 6%, 15%, 35%, **72%**, 95%, 99.3% and 99.9%. Four of the
/// seven sliders lived above 95% display, where they were indistinguishable
/// from each other and reached almost no pixels, while the one labelled
/// midtones sat at nearly three-quarters brightness. On a scanned negative it
/// was worse still: the whole inverted range lands under one stop over middle
/// grey, so only the bottom two zones could reach the picture at all and the
/// control looked broken.
///
/// Spread evenly over displayed brightness instead, each slider owns a sixth of
/// what the eye sees and the labels mean what they say.
fn tone_zone_position(luminance: f32, display_referred: bool) -> f32 {
    // Through the transfer as well as the tone curve. Display-linear is not
    // what the eye reads and not what a histogram plots: half of display-linear
    // is three-quarters of the way up the screen, so anchoring there would
    // repeat, more quietly, the bunching this replaced.
    let displayed = if display_referred {
        luminance
    } else {
        srgb_encode(super::tone::default_display_tone(luminance))
    };
    displayed.clamp(0.0, 1.0) * 6.0
}

fn tone_adjustment_ev(luminance: f32, adjustments: &[f32; 7], display_referred: bool) -> f32 {
    if !luminance.is_finite() || luminance <= 0.0 {
        return 0.0;
    }
    let position = tone_zone_position(luminance, display_referred);
    let interval = position.floor().clamp(0.0, 5.0) as usize;
    let t = (position - interval as f32).clamp(0.0, 1.0);
    let smooth = t * t * (3.0 - 2.0 * t);
    adjustments[interval] * (1.0 - smooth) + adjustments[interval + 1] * smooth
}

/// Radius of the window local detail strength is measured over.
///
/// Three pixels either side: wide enough that the estimate is not itself noise,
/// narrow enough that a small feature is not averaged into the flat ground
/// around it and smoothed away with it.
const ACTIVITY_RADIUS: usize = 3;

/// How much of the measured noise power to subtract, per unit of the slider.
///
/// The threshold this replaced could not serve both ends of the same picture:
/// set gently enough to keep texture it left a blue sky visibly grainy, and set
/// hard enough to clean the sky it flattened the texture. Subtracting a
/// *locally measured* noise power instead asks each part of the frame what it
/// contains, so the sky can be cleaned completely while the leaves beside it
/// are barely touched. Measured against the camera's own JPEG; see
/// `BENCHMARK_NOISE_REDUCTION_QUALITY`.
/// The default setting of 0.35 therefore subtracts almost exactly the noise
/// power that is there, which is the plain Wiener answer; the top of the slider
/// subtracts three times it, for frames where being clean matters more than
/// being faithful.
const ACTIVITY_STRENGTH: f32 = 2.9;

/// Grows PLANE to LEN elements, zeroing the new ones on every thread at once.
///
/// `resize` zero-fills serially, and with the allocator told to keep large
/// buffers on the heap (see `tune_allocator`) there are no fresh kernel pages
/// to lean on: a twenty-megapixel plane is an eighty-megabyte memset on one
/// core, and the denoiser used to grow five of them. Filling in parallel also
/// faults the pages in from the threads that will use them.
fn ensure_plane_len(plane: &mut Vec<f32>, len: usize) {
    if plane.len() >= len {
        plane.truncate(len);
        return;
    }
    let missing = len - plane.len();
    plane.reserve(missing);
    plane.spare_capacity_mut()[..missing]
        .par_chunks_mut(1 << 16)
        .for_each(|chunk| {
            for slot in chunk {
                slot.write(0.0);
            }
        });
    // SAFETY: Every element up to LEN was just written.
    unsafe { plane.set_len(len) };
}

/// Mean of the squares of PLANE over a square window.
///
/// The plain statement of what the denoiser's shrinkage pass computes with its
/// rolling window, and checked against it; the sharpener, with one band to
/// judge, uses it as it stands.
///
/// Separable, and each pass slides a running total rather than re-adding the
/// window at every pixel, so the window costs the same whatever its radius.
/// The vertical pass keeps one running total per column and walks a band of
/// rows downwards, which is both O(1) per pixel and in row order — a column-major
/// walk would be neither.
fn local_mean_square(
    plane: &[f32],
    output: &mut Vec<f32>,
    scratch: &mut Vec<f32>,
    width: usize,
    height: usize,
) {
    let radius = ACTIVITY_RADIUS;
    ensure_plane_len(scratch, plane.len());
    ensure_plane_len(output, plane.len());
    scratch
        .par_chunks_mut(width)
        .enumerate()
        .for_each(|(row, out)| {
            let base = row * width;
            let square = |index: usize| {
                let sample = plane[base + index];
                sample * sample
            };
            let mut total = 0.0_f32;
            let (mut first, mut past) = (0_usize, 0_usize);
            for (column, value) in out.iter_mut().enumerate() {
                let wanted_past = (column + radius + 1).min(width);
                let wanted_first = column.saturating_sub(radius);
                while past < wanted_past {
                    total += square(past);
                    past += 1;
                }
                while first < wanted_first {
                    total -= square(first);
                    first += 1;
                }
                *value = total / (past - first) as f32;
            }
        });
    // Bands of rows, each with the halo its own running totals need.
    const BAND: usize = 64;
    output
        .par_chunks_mut(width * BAND)
        .enumerate()
        .for_each_init(Vec::<f32>::new, |totals, (band, out)| {
            let top = band * BAND;
            let rows = out.len() / width;
            totals.clear();
            totals.resize(width, 0.0);
            let mut first = top.saturating_sub(radius);
            let mut past = first;
            let add = |totals: &mut Vec<f32>, row: usize| {
                for (column, total) in totals.iter_mut().enumerate() {
                    *total += scratch[row * width + column];
                }
            };
            let remove = |totals: &mut Vec<f32>, row: usize| {
                for (column, total) in totals.iter_mut().enumerate() {
                    *total -= scratch[row * width + column];
                }
            };
            for row in 0..rows {
                let centre = top + row;
                let wanted_past = (centre + radius + 1).min(height);
                let wanted_first = centre.saturating_sub(radius);
                while past < wanted_past {
                    add(totals, past);
                    past += 1;
                }
                while first < wanted_first {
                    remove(totals, first);
                    first += 1;
                }
                let count = (past - first) as f32;
                let line = &mut out[row * width..(row + 1) * width];
                for (value, total) in line.iter_mut().zip(totals.iter()) {
                    *value = *total / count;
                }
            }
        });
}

/// What fraction of a detail coefficient survives, given how much of the local
/// signal is noise.
///
/// The Wiener answer: keep the share of the local power that is not noise. In a
/// flat sky the whole of it is noise and nothing survives; in foliage the noise
/// is a small part of what is there and almost all of it does. One expression,
/// no threshold to place, and it is the same estimator the noise profile is
/// already measuring for.
#[inline]
fn activity_gain(mean_square: f32, noise_power: f32) -> f32 {
    if !(mean_square > 0.0) {
        return 0.0;
    }
    (1.0 - noise_power / mean_square).clamp(0.0, 1.0)
}

/// The brightness plane is split at these steps, finest first. Each blur
/// reaches twice its step, so the bands hold detail around one, two and four
/// pixels across. Three rather than two because the noise a demosaic leaves is
/// correlated over a pixel or two, and what a two-band split left behind in its
/// smooth residual was exactly the low, blotchy grain that read as film in a
/// blue sky.
const LUMA_BAND_STEPS: [usize; 3] = [1, 2, 4];

/// The lower quartile of a band's local power where the band holds nothing but
/// noise, as a fraction of that noise's power.
///
/// A seven-by-seven window of independent samples has forty-nine degrees of
/// freedom and its quartile sits at 0.86 of its mean; noise a demosaic has been
/// through is correlated over a pixel or two, which lowers the count and with
/// it the quartile. `BAND_NOISE_IS_MEASURED_WITHIN_A_FIFTH` checks the number.
const FLAT_QUARTILE_OF_POWER: f32 = 0.84;

/// The median of a band's local power over flat pixels, as a fraction of the
/// noise power there. Just under one, because the distribution leans right.
const FLAT_MEDIAN_OF_POWER: f32 = 0.97;

/// Noise power per brightness bin of one detail band.
#[derive(Clone, Copy, Debug, PartialEq)]
struct BandNoise {
    power: [f32; NOISE_BINS],
}

/// One brightness bin's reading of a band's noise: the power measured there,
/// the mean brightness of the pixels it was read from, and how many there were.
#[derive(Clone, Copy, Debug, Default)]
struct BinReading {
    power: f32,
    brightness: f32,
    count: usize,
}

/// How far above the fitted noise line a bin may sit and still be believed to
/// have been read from flat pixels rather than from texture.
const NOISE_ENVELOPE_TOLERANCE: f32 = 1.08;

impl BandNoise {
    /// Answers each bin with its own reading, and a bin nothing landed in with
    /// the nearest bin's.
    ///
    /// For noise that is no longer photon noise. After the denoiser has been
    /// over a frame, what is left in a flat sky is far less than what is left
    /// in foliage of the same brightness, so the residue no longer follows
    /// brightness at all and a line fitted across the bins says nothing; one
    /// bin cleaned nearly to nothing dragged the fitted line flat at its
    /// level, and a sharpener reading it took the whole sky for signal. The
    /// quartile of each bin on its own is the flat part of *that* brightness.
    fn from_readings(readings: &[BinReading; NOISE_BINS]) -> Self {
        let mut power = [0.0_f32; NOISE_BINS];
        for bin in 0..NOISE_BINS {
            let near = (0..NOISE_BINS)
                .filter(|other| readings[*other].count > 0)
                .min_by_key(|other| other.abs_diff(bin));
            power[bin] = near.map(|near| readings[near].power).unwrap_or(0.0);
        }
        BandNoise { power }
    }

    /// Fits sensor noise to what the bins read, and answers for every bin from
    /// the fit.
    ///
    /// Photon noise has a variance that is linear in scene brightness — a read
    /// floor plus a term proportional to the light — and every stage before this
    /// one scales brightness and noise together, so the line survives exposure
    /// and white balance. The line is fitted as a *lower envelope*: the one that
    /// sits at or under every bin's reading and as close to them as it can. A
    /// bin has to sit above the envelope by more than noise itself would put it
    /// to be left out, and what puts it there is texture — a brightness the
    /// frame shows only as foliage or fabric reads its own detail as noise, and
    /// with no envelope that detail would have been shrunk away as noise. On a
    /// synthetic scene whose one texture sat alone in its brightness bin, the
    /// per-bin reading alone kept 62% of it on a frame with no noise at all;
    /// the envelope keeps all of it.
    ///
    /// The bins that were read too thinly to say anything are answered from the
    /// same line, so a sparsely occupied brightness is neither shrunk on no
    /// evidence nor left untouched on none.
    fn fit(readings: &[BinReading; NOISE_BINS]) -> Self {
        let measured: Vec<&BinReading> = readings.iter().filter(|r| r.count > 0).collect();
        let mut power = [0.0_f32; NOISE_BINS];
        if measured.is_empty() {
            return BandNoise { power };
        }
        // Every line through two readings, level with one, or through the
        // origin and one; keep those under every reading, take the one that
        // hugs them closest, weighted by how many pixels each reading speaks for.
        let mut candidates: Vec<(f32, f32)> = Vec::new();
        for (i, a) in measured.iter().enumerate() {
            candidates.push((a.power, 0.0));
            if a.brightness > 1e-4 {
                candidates.push((0.0, a.power / a.brightness));
            }
            for b in &measured[i + 1..] {
                let span = b.brightness - a.brightness;
                if span.abs() > 1e-4 {
                    let slope = (b.power - a.power) / span;
                    let floor = a.power - slope * a.brightness;
                    if slope >= 0.0 && floor >= 0.0 {
                        candidates.push((floor, slope));
                    }
                }
            }
        }
        let feasible = |(floor, slope): &(f32, f32)| {
            measured
                .iter()
                .all(|r| floor + slope * r.brightness <= r.power * NOISE_ENVELOPE_TOLERANCE + 1e-12)
        };
        let closeness = |(floor, slope): &(f32, f32)| {
            measured
                .iter()
                .map(|r| (floor + slope * r.brightness) * r.count as f32)
                .sum::<f32>()
        };
        let (floor, slope) = candidates
            .into_iter()
            .filter(feasible)
            .max_by(|a, b| closeness(a).total_cmp(&closeness(b)))
            .unwrap_or((0.0, 0.0));
        for (bin, value) in power.iter_mut().enumerate() {
            let brightness = readings[bin].brightness;
            let brightness = if readings[bin].count > 0 {
                brightness
            } else {
                // The middle of the bin's own brightness range.
                let edge = (bin as f32 + 0.5) / NOISE_BINS as f32;
                edge * edge
            };
            *value = (floor + slope * brightness).max(0.0);
        }
        BandNoise { power }
    }
}

/// How far apart the pixels the noise estimate reads are, for a frame of this
/// many pixels; see `NOISE_TARGET_SAMPLES`.
fn noise_sample_stride(pixels: usize) -> usize {
    (pixels / NOISE_TARGET_SAMPLES).isqrt().max(1)
}

/// The mean square of PLANE over the activity window centred on INDEX.
///
/// What the shrinkage pass computes for every pixel with running sums, done
/// here for one pixel at a time: the noise estimate only needs it at a hundred
/// thousand sampled points, and reading forty-nine values at each of those is
/// far cheaper than a whole plane of them written out and read back.
fn window_mean_square(plane: &[f32], width: usize, height: usize, index: usize) -> f32 {
    let (row, column) = (index / width, index % width);
    let top = row.saturating_sub(ACTIVITY_RADIUS);
    let bottom = (row + ACTIVITY_RADIUS).min(height - 1);
    let left = column.saturating_sub(ACTIVITY_RADIUS);
    let right = (column + ACTIVITY_RADIUS).min(width - 1);
    let mut total = 0.0_f32;
    for y in top..=bottom {
        for value in &plane[y * width + left..=y * width + right] {
            total += value * value;
        }
    }
    total / ((bottom - top + 1) * (right - left + 1)) as f32
}

/// Measures the finest band's noise from its own local power, and names the
/// pixels it found flat.
///
/// Noise is in every pixel and texture only in some, and at a scale of one
/// pixel texture is rare besides, so the lower quartile of the band's local
/// power is a settled reading of its noise. The quarter of the samples under
/// that quartile are the flat ones: they are handed back as (index, bin) pairs
/// for the coarser bands to measure themselves over, since a pixel with no fine
/// detail is the best place to ask what a band's noise looks like undisturbed.
///
/// This replaced deriving every band's noise from one Laplacian reading through
/// the gains a *white* noise would show. Demosaiced noise is not white: it is
/// correlated over a pixel or two, so it carries more power at the coarser
/// steps than white noise would, and the derived figure came out low there — by
/// enough that the default setting left a blue sky 1.7 times as noisy as the
/// camera's own JPEG while the flat sky should have come out clean. Measuring
/// each band directly has no assumption to be wrong about.
fn fine_band_noise(
    band: &[f32],
    brightness: &[f32],
    width: usize,
    height: usize,
) -> (BandNoise, Vec<(usize, usize)>) {
    let (readings, flat) = fine_band_readings(band, brightness, width, height);
    (BandNoise::fit(&readings), flat)
}

/// The finest band's noise as each brightness bin read it, unfitted: what a
/// stage running after the denoiser has to work from.
fn fine_band_noise_by_bin(band: &[f32], brightness: &[f32], width: usize, height: usize) -> BandNoise {
    BandNoise::from_readings(&fine_band_readings(band, brightness, width, height).0)
}

/// Reads the finest band's local power at sampled pixels, bin by bin, and names
/// the flat quarter of each bin; see `fine_band_noise` for what it means.
fn fine_band_readings(
    band: &[f32],
    brightness: &[f32],
    width: usize,
    height: usize,
) -> ([BinReading; NOISE_BINS], Vec<(usize, usize)>) {
    let stride = noise_sample_stride(width * height);
    let rows: Vec<usize> = (0..height).step_by(stride).collect();
    let sampled: Vec<(f32, usize)> = rows
        .par_iter()
        .flat_map_iter(|row| {
            (0..width).step_by(stride).map(move |column| {
                let index = row * width + column;
                (window_mean_square(band, width, height, index), index)
            })
        })
        .collect();
    let mut samples: Vec<Vec<(f32, usize)>> = vec![Vec::new(); NOISE_BINS];
    for (power, index) in sampled {
        samples[noise_bin(brightness[index])].push((power, index));
    }
    let mut readings = [BinReading::default(); NOISE_BINS];
    let mut flat = Vec::new();
    for (bin, samples) in samples.iter_mut().enumerate() {
        if samples.len() < NOISE_BIN_MINIMUM {
            continue;
        }
        let rank = ((samples.len() as f32 * NOISE_QUANTILE) as usize).min(samples.len() - 1);
        let (below, quartile, _) =
            samples.select_nth_unstable_by(rank, |a, b| a.0.total_cmp(&b.0));
        let flat_here: Vec<usize> = below
            .iter()
            .chain(std::iter::once(&*quartile))
            .map(|(_, index)| *index)
            .collect();
        readings[bin] = BinReading {
            power: quartile.0 / FLAT_QUARTILE_OF_POWER,
            brightness: flat_here.iter().map(|index| brightness[*index]).sum::<f32>()
                / flat_here.len() as f32,
            count: flat_here.len(),
        };
        flat.extend(flat_here.into_iter().map(|index| (index, bin)));
    }
    (readings, flat)
}

/// Measures a coarser band's noise over the pixels the finest band found flat.
///
/// The median rather than the mean, so the few flat-looking pixels that carry
/// a soft texture the fine band could not see do not pull the figure up and
/// have that texture smoothed away.
fn coarse_band_noise(
    band: &[f32],
    brightness: &[f32],
    width: usize,
    height: usize,
    flat: &[(usize, usize)],
) -> BandNoise {
    let powers: Vec<f32> = flat
        .par_iter()
        .map(|(index, _)| window_mean_square(band, width, height, *index))
        .collect();
    let mut samples: Vec<Vec<f32>> = vec![Vec::new(); NOISE_BINS];
    let mut light = [0.0_f32; NOISE_BINS];
    for ((index, bin), power) in flat.iter().zip(powers) {
        samples[*bin].push(power);
        light[*bin] += brightness[*index];
    }
    let mut readings = [BinReading::default(); NOISE_BINS];
    for (bin, samples) in samples.iter_mut().enumerate() {
        if samples.len() < NOISE_BIN_MINIMUM / 2 {
            continue;
        }
        let rank = samples.len() / 2;
        let (_, median, _) = samples.select_nth_unstable_by(rank, f32::total_cmp);
        readings[bin] = BinReading {
            power: *median / FLAT_MEDIAN_OF_POWER,
            brightness: light[bin] / samples.len() as f32,
            count: samples.len(),
        };
    }
    BandNoise::fit(&readings)
}

/// Tile the three stacked blurs are computed in. The intermediates of a tile
/// stay in cache: 284 by 92 floats a buffer, four buffers, just over 400 KB.
const LUMA_TILE_WIDTH: usize = 256;
const LUMA_TILE_HEIGHT: usize = 64;
/// Rows and columns of neighbourhood a tile needs beyond its own: each blur
/// reaches twice its step, and the three stack.
const LUMA_TILE_HALO: usize = 2 * (LUMA_BAND_STEPS[0] + LUMA_BAND_STEPS[1] + LUMA_BAND_STEPS[2]);

/// One bilateral span, eight lanes at a time where the machine allows.
///
/// # Safety
///
/// Every pointer must stay readable for `output.len()` lanes.
unsafe fn bilateral_span(
    output: &mut [f32],
    guide_center: *const f32,
    taps: [*const f32; 5],
    guides: [*const f32; 5],
    use_avx: bool,
) {
    #[cfg(target_arch = "x86_64")]
    if use_avx {
        // SAFETY: Forwarded from the caller's guarantee.
        unsafe { bilateral_span_avx(output, guide_center, taps, guides) };
        return;
    }
    let _ = use_avx;
    let kernel = [1.0_f32, 4.0, 6.0, 4.0, 1.0];
    for (lane, value) in output.iter_mut().enumerate() {
        // SAFETY: Within the caller's readable span.
        unsafe {
            let center = *guide_center.add(lane);
            let sigma = 0.012 + 0.055 * center.max(0.0).sqrt();
            let inverse = 1.0 / (sigma * sigma);
            let mut sum = 0.0;
            let mut weight_sum = 0.0;
            for tap in 0..5 {
                let distance = *guides[tap].add(lane) - center;
                let weight = kernel[tap] * (1.0 / (1.0 + distance * distance * inverse));
                sum += *taps[tap].add(lane) * weight;
                weight_sum += weight;
            }
            *value = sum / weight_sum;
        }
    }
}

/// Where a tile's local buffers sit against the image.
///
/// Local coordinates run over the tile and its halo; `image` says which of them
/// lie inside the frame, since a tile at an edge has halo cells that do not.
struct TileGeometry {
    stride: usize,
    /// Local columns and rows that map to real pixels, as half-open ranges.
    image_columns: (usize, usize),
    image_rows: (usize, usize),
}

/// One directional bilateral pass over a region of a tile, followed by the
/// replication that stands in for clamping at the frame's edge.
///
/// The whole-plane blurs read their input at clamped coordinates, so a tap that
/// falls off the frame reads the edge pixel of *that stage's* input. A tile
/// reproduces that by computing only the cells inside the frame and copying the
/// edge cell outwards over the rest of the region: the next stage then finds the
/// edge value wherever it looks off the frame, exactly as clamping gave it.
#[allow(clippy::too_many_arguments)]
unsafe fn tile_pass(
    out: &mut [f32],
    input: &[f32],
    guide: &[f32],
    geometry: &TileGeometry,
    rows: std::ops::Range<usize>,
    columns: std::ops::Range<usize>,
    step: usize,
    horizontal: bool,
    use_avx: bool,
) {
    let stride = geometry.stride;
    let (r0, r1) = (rows.start.max(geometry.image_rows.0), rows.end.min(geometry.image_rows.1));
    let (c0, c1) = (
        columns.start.max(geometry.image_columns.0),
        columns.end.min(geometry.image_columns.1),
    );
    debug_assert!(r0 < r1 && c0 < c1);
    for row in r0..r1 {
        let base = row * stride;
        // SAFETY: Regions are laid out so every tap of every stage lies inside
        // the tile's buffers; `LUMA_TILE_HALO` is the stacked reach.
        unsafe {
            let at = |plane: &[f32], tap: usize| {
                if horizontal {
                    plane.as_ptr().add(base + c0 + tap * step - 2 * step)
                } else {
                    plane.as_ptr().add((row + tap * step - 2 * step) * stride + c0)
                }
            };
            let taps: [*const f32; 5] = std::array::from_fn(|tap| at(input, tap));
            let guides: [*const f32; 5] = std::array::from_fn(|tap| at(guide, tap));
            bilateral_span(
                &mut out[base + c0..base + c1],
                guide.as_ptr().add(base + c0),
                taps,
                guides,
                use_avx,
            );
        }
        let left = out[base + c0];
        out[base + columns.start..base + c0].fill(left);
        let right = out[base + c1 - 1];
        out[base + c1..base + columns.end].fill(right);
    }
    for row in rows.start..r0 {
        for column in columns.clone() {
            out[row * stride + column] = out[r0 * stride + column];
        }
    }
    for row in r1..rows.end {
        for column in columns.clone() {
            out[row * stride + column] = out[(r1 - 1) * stride + column];
        }
    }
}

/// A plane written by many tiles at once, each to its own cells.
#[derive(Clone, Copy)]
struct SharedPlane(*mut f32);

// SAFETY: Every tile writes a disjoint rectangle; see `split_luma_bands`.
unsafe impl Sync for SharedPlane {}
unsafe impl Send for SharedPlane {}

impl SharedPlane {
    /// # Safety
    ///
    /// INDEX must lie inside the plane and be written by this caller alone.
    #[inline]
    unsafe fn write(self, index: usize, value: f32) {
        // SAFETY: Forwarded from the caller.
        unsafe { *self.0.add(index) = value };
    }
}

/// The four planes a tile writes into.
struct BandPlanes {
    fine: SharedPlane,
    middle: SharedPlane,
    coarse: SharedPlane,
    residual: SharedPlane,
}

/// Splits PLANE into its three detail bands and the smooth residual under them.
///
/// Each blur follows edges in the original brightness, so a band never holds
/// the far side of an edge. On return PLANE holds the finest band, the two
/// vectors returned the next two, and RESIDUAL what is left.
///
/// Done in tiles rather than as three passes over the whole plane. The blurs
/// stack — the second reads the first, the third the second — and at twenty
/// megapixels each pass read two planes and wrote one, 240 MB a blur, with the
/// intermediate never in cache when it was wanted again. A tile of 256 by 64
/// copies its neighbourhood in once, runs the three blurs on four buffers that
/// fit in one core's cache, and writes the four bands straight out; the halo it
/// recomputes for its neighbours costs about forty percent more arithmetic and
/// removes two thirds of the traffic, which is what this stage was bound by.
fn split_luma_bands(
    plane: &mut Vec<f32>,
    residual: &mut Vec<f32>,
    width: usize,
    height: usize,
) -> (Vec<f32>, Vec<f32>) {
    let pixels = width * height;
    let mut fine = Vec::new();
    let mut middle = Vec::new();
    let mut coarse = Vec::new();
    ensure_plane_len(&mut fine, pixels);
    ensure_plane_len(&mut middle, pixels);
    ensure_plane_len(&mut coarse, pixels);
    ensure_plane_len(residual, pixels);
    let use_avx = super::nn::fma_available();
    let halo = LUMA_TILE_HALO;
    let (tile_width, tile_height) = (LUMA_TILE_WIDTH, LUMA_TILE_HEIGHT);
    let (stride, rows) = (tile_width + 2 * halo, tile_height + 2 * halo);
    let tiles_across = width.div_ceil(tile_width);
    let tiles_down = height.div_ceil(tile_height);
    let planes = BandPlanes {
        fine: SharedPlane(fine.as_mut_ptr()),
        middle: SharedPlane(middle.as_mut_ptr()),
        coarse: SharedPlane(coarse.as_mut_ptr()),
        residual: SharedPlane(residual.as_mut_ptr()),
    };
    let source: &[f32] = plane;
    (0..tiles_across * tiles_down)
        .into_par_iter()
        .for_each_init(
            || {
                (
                    vec![0.0_f32; stride * rows],
                    vec![0.0_f32; stride * rows],
                    vec![0.0_f32; stride * rows],
                    vec![0.0_f32; stride * rows],
                    vec![0.0_f32; tile_width],
                )
            },
            |(local, scratch, first, second, line), tile| {
                let x0 = (tile % tiles_across) * tile_width;
                let y0 = (tile / tiles_across) * tile_height;
                let own_width = tile_width.min(width - x0);
                let own_height = tile_height.min(height - y0);
                // Local column `c` is image column `x0 - halo + c`.
                let column_of = |c: usize| (x0 + c).saturating_sub(halo).min(width - 1);
                let row_of = |r: usize| (y0 + r).saturating_sub(halo).min(height - 1);
                let geometry = TileGeometry {
                    stride,
                    image_columns: (halo.saturating_sub(x0), (halo + width - x0).min(stride)),
                    image_rows: (halo.saturating_sub(y0), (halo + height - y0).min(rows)),
                };
                // The neighbourhood, clamped at the frame's edge like every tap
                // of the whole-plane blurs was.
                for r in 0..rows {
                    let image_row = row_of(r) * width;
                    let target = &mut local[r * stride..(r + 1) * stride];
                    let (c0, c1) = geometry.image_columns;
                    target[c0..c1].copy_from_slice(&source[image_row + column_of(c0)..image_row + column_of(c0) + (c1 - c0)]);
                    target[..c0].fill(source[image_row + column_of(c0)]);
                    let last = source[image_row + column_of(c1 - 1)];
                    target[c1..].fill(last);
                }
                // Each stage is computed over exactly the region the next one
                // reads: the first blur's reach in from the buffer's edge, then
                // the second's, then the third's own.
                let [one, two, four] = LUMA_BAND_STEPS;
                let (edge_one, edge_two) = (2 * one, 2 * one + 2 * two);
                // SAFETY: Region bounds keep every tap inside the buffers.
                unsafe {
                    tile_pass(scratch, local, local, &geometry, 0..rows, edge_one..stride - edge_one, one, true, use_avx);
                    tile_pass(first, scratch, local, &geometry, edge_one..rows - edge_one, edge_one..stride - edge_one, one, false, use_avx);
                    tile_pass(scratch, first, local, &geometry, edge_one..rows - edge_one, edge_two..stride - edge_two, two, true, use_avx);
                    tile_pass(second, scratch, local, &geometry, edge_two..rows - edge_two, edge_two..stride - edge_two, two, false, use_avx);
                    // The third blur's vertical pass is done a row at a time
                    // below, straight into the output.
                    tile_pass(scratch, second, local, &geometry, edge_two..rows - edge_two, halo..stride - halo, four, true, use_avx);
                }
                for r in 0..own_height {
                    let row = halo + r;
                    let base = row * stride + halo;
                    let out_row = (y0 + r) * width + x0;
                    // SAFETY: Tap rows sit within the region the third
                    // horizontal pass filled; the output pointers address this
                    // tile's own cells, which no other tile writes.
                    unsafe {
                        let taps: [*const f32; 5] = std::array::from_fn(|tap| {
                            scratch.as_ptr().add((row + tap * four - 2 * four) * stride + halo)
                        });
                        let guides: [*const f32; 5] = std::array::from_fn(|tap| {
                            local.as_ptr().add((row + tap * four - 2 * four) * stride + halo)
                        });
                        bilateral_span(&mut line[..own_width], local.as_ptr().add(base), taps, guides, use_avx);
                        for c in 0..own_width {
                            let (value, one, two, three) =
                                (local[base + c], first[base + c], second[base + c], line[c]);
                            planes.fine.write(out_row + c, value - one);
                            planes.middle.write(out_row + c, one - two);
                            planes.coarse.write(out_row + c, two - three);
                            planes.residual.write(out_row + c, three);
                        }
                    }
                }
            },
        );
    std::mem::swap(plane, &mut fine);
    (middle, coarse)
}

/// Rows a shrinkage band covers; the running window needs three more above and
/// below it.
const SHRINK_BAND_ROWS: usize = 32;

/// Rebuilds the brightness from its bands, each shrunk by how much of its local
/// power is noise, in one pass.
///
/// The three activity maps used to be written out as whole planes and read
/// back, and each band's gain was applied in its own pass — nine traversals of
/// eighty megabytes at twenty megapixels for arithmetic that fits in a few
/// rows. Here each band of rows keeps a seven-row ring of horizontal window
/// sums per detail band and rolls a vertical total down it, so a pixel's local
/// power costs a handful of adds and nothing is stored beyond the ring. OUT is
/// the residual coming in and the denoised brightness going out.
#[allow(clippy::too_many_arguments)]
fn shrink_luma_bands(
    out: &mut [f32],
    fine: &[f32],
    middle: &[f32],
    coarse: &[f32],
    noise: [BandNoise; 3],
    strength: f32,
    width: usize,
    height: usize,
) {
    let radius = ACTIVITY_RADIUS;
    let window = 2 * radius + 1;
    let bands = [fine, middle, coarse];
    out.par_chunks_mut(width * SHRINK_BAND_ROWS)
        .enumerate()
        .for_each_init(
            || (vec![0.0_f32; 3 * window * width], vec![0.0_f32; 3 * width]),
            |(ring, totals), (band_index, out)| {
                let top = band_index * SHRINK_BAND_ROWS;
                let rows = out.len() / width;
                totals.fill(0.0);
                // Horizontal window sums of one row of one band, into a ring slot.
                let row_sums = |ring: &mut [f32], band: usize, y: usize, slot: usize| {
                    let source = &bands[band][y * width..(y + 1) * width];
                    let target = &mut ring[(band * window + slot) * width..(band * window + slot + 1) * width];
                    let square = |x: usize| source[x] * source[x];
                    let mut total = 0.0_f32;
                    let (mut first, mut past) = (0_usize, 0_usize);
                    for (x, value) in target.iter_mut().enumerate() {
                        let wanted_past = (x + radius + 1).min(width);
                        let wanted_first = x.saturating_sub(radius);
                        while past < wanted_past {
                            total += square(past);
                            past += 1;
                        }
                        while first < wanted_first {
                            total -= square(first);
                            first += 1;
                        }
                        *value = total / (past - first) as f32;
                    }
                };
                // The rows the window over the band's first row reaches, rolled
                // in before any output; then one row in and one out per line.
                let mut first_row = top.saturating_sub(radius);
                let mut past_row = first_row;
                let add_row = |ring: &mut [f32], totals: &mut [f32], y: usize| {
                    for band in 0..3 {
                        let slot = y % window;
                        row_sums(ring, band, y, slot);
                        let sums = &ring[(band * window + slot) * width..(band * window + slot + 1) * width];
                        for (total, sum) in totals[band * width..(band + 1) * width].iter_mut().zip(sums) {
                            *total += sum;
                        }
                    }
                };
                let remove_row = |ring: &[f32], totals: &mut [f32], y: usize| {
                    for band in 0..3 {
                        let slot = y % window;
                        let sums = &ring[(band * window + slot) * width..(band * window + slot + 1) * width];
                        for (total, sum) in totals[band * width..(band + 1) * width].iter_mut().zip(sums) {
                            *total -= sum;
                        }
                    }
                };
                for row in 0..rows {
                    let y = top + row;
                    let wanted_past = (y + radius + 1).min(height);
                    let wanted_first = y.saturating_sub(radius);
                    // The row leaving the window goes first: the row arriving
                    // takes over its slot in the ring.
                    while first_row < wanted_first {
                        remove_row(ring, totals, first_row);
                        first_row += 1;
                    }
                    while past_row < wanted_past {
                        add_row(ring, totals, past_row);
                        past_row += 1;
                    }
                    let count = (past_row - first_row) as f32;
                    let line = &mut out[row * width..(row + 1) * width];
                    let (fine, middle, coarse) = (
                        &fine[y * width..(y + 1) * width],
                        &middle[y * width..(y + 1) * width],
                        &coarse[y * width..(y + 1) * width],
                    );
                    let (totals_fine, rest) = totals.split_at(width);
                    let (totals_middle, totals_coarse) = rest.split_at(width);
                    for x in 0..width {
                        // Binned on the smooth residual under the bands: the
                        // pixel's own value is by now a detail coefficient and
                        // says nothing about how bright anything is.
                        let bin = noise_bin(line[x]);
                        let shrink = |band: usize, total: f32, value: f32| {
                            value * activity_gain(total / count, strength * noise[band].power[bin])
                        };
                        line[x] += shrink(0, totals_fine[x], fine[x])
                            + shrink(1, totals_middle[x], middle[x])
                            + shrink(2, totals_coarse[x], coarse[x]);
                    }
                }
            },
        );
}

/// Denoises the brightness plane in place by local Wiener shrinkage of three
/// detail bands, each against the noise power measured in that band.
///
/// RESIDUAL is left holding the finest band afterwards, which the caller may
/// reuse as scratch. STRENGTH is the slider, nought to one.
fn denoise_luma(
    plane: &mut Vec<f32>,
    residual: &mut Vec<f32>,
    width: usize,
    height: usize,
    strength: f32,
) {
    let profiling = std::env::var_os("ORFEUS_PROFILE").is_some();
    let mut step_started = Instant::now();
    let mut profile_step = |name: &str| {
        if profiling {
            let now = Instant::now();
            eprintln!(
                "orfeus-profile luma-step={name} milliseconds={:.3}",
                now.duration_since(step_started).as_secs_f64() * 1000.0
            );
            step_started = now;
        }
    };
    let (middle, coarse) = split_luma_bands(plane, residual, width, height);
    profile_step("bands");
    let strength = strength * ACTIVITY_STRENGTH;
    // The finest band says where the frame is flat; the others measure there.
    let (fine_noise, flat) = fine_band_noise(plane, residual, width, height);
    let middle_noise = coarse_band_noise(&middle, residual, width, height, &flat);
    let coarse_noise = coarse_band_noise(&coarse, residual, width, height, &flat);
    profile_step("estimate");
    shrink_luma_bands(
        residual,
        plane,
        &middle,
        &coarse,
        [fine_noise, middle_noise, coarse_noise],
        strength,
        width,
        height,
    );
    std::mem::swap(plane, residual);
    profile_step("shrink");
}

/// Lifts or lowers seven bands of brightness independently.
///
/// DISPLAY_REFERRED says whether the pixels have already been through the
/// display transform, which decides where the zones sit: a graded image after a
/// film look is already the brightness it will be shown at, while a
/// scene-linear one has to be asked what it will become.
pub(crate) fn apply_tonal_equalizer(
    image: &mut RgbImage,
    adjustments: [f32; 7],
    display_referred: bool,
) {
    if adjustments.iter().all(|adjustment| *adjustment == 0.0) {
        return;
    }
    image.data.par_chunks_mut(3 * 8192).for_each(|chunk| {
        for pixel in chunk.as_chunks_mut::<3>().0 {
            let luminance = 0.212_672_9 * pixel[0] + 0.715_152_2 * pixel[1] + 0.072_175 * pixel[2];
            let gain = tone_adjustment_ev(luminance, &adjustments, display_referred).exp2();
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
    ensure_plane_len(output, source.len());
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


/// Brightness bins the frame's noise is measured in.
///
/// Photon noise grows with the signal, so one number for a whole frame either
/// leaves the shadows noisy or wipes the highlights smooth. Eight bins spaced
/// by the square root of brightness put most of the resolution where the noise
/// is worst.
const NOISE_BINS: usize = 8;

/// Which quantile is taken to be the noise floor, and what that quantile
/// equals in deviations for noise alone.
///
/// A low quantile, not the median. Noise is in every pixel; texture is in some
/// of them, and it is much larger. Taking the lower quarter therefore measures
/// the noise even where over half a region is detail, and when it is wrong it
/// is wrong towards *less* smoothing — which is the direction a denoiser should
/// err in.
const NOISE_QUANTILE: f32 = 0.25;

/// Roughly how many pixels the noise estimate looks at.
///
/// A quartile of a hundred thousand samples is a settled number, and spending
/// more on a large frame buys nothing — so the stride grows with the frame
/// rather than being fixed. Fixed was a bug: at one pixel in sixteen a
/// thumbnail-sized image offered fewer samples than a single bin needs, every
/// bin abstained, and the stage quietly did nothing at all.
const NOISE_TARGET_SAMPLES: usize = 131072;

/// Samples a brightness bin needs before it is allowed to name a noise floor.
const NOISE_BIN_MINIMUM: usize = 32;

/// How far the noise reduction reads around each pixel it writes.
///
/// The brightness chain blurs at step one and then at step two, each reaching
/// twice its step, and colour is filtered at half resolution through a median
/// and blurs at steps one, two and four — fifteen half-resolution pixels, so
/// thirty of these. Rounded up with room to spare, because this is what a
/// viewport render has to overlap its neighbours by for its interior to come
/// out identical to a whole-frame render, and being generous costs a border.
pub(crate) const NOISE_REDUCTION_REACH: usize = 48;

#[inline]
fn noise_bin(value: f32) -> usize {
    let position = value.clamp(0.0, 1.0).sqrt() * NOISE_BINS as f32;
    (position as usize).min(NOISE_BINS - 1)
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
/// Scales a denoise strength to the size the render is actually working at.
///
/// The filter's steps are counted in pixels — one, two and four — so at a
/// 1600 px preview of a 5184 px frame the very same filter reaches across three
/// times as much of the picture, while the downscale that got it there has
/// already averaged the noise away. Running it anyway does not denoise a
/// preview, it smears it.
///
/// Measured against the export downscaled to the same size, which is what a
/// preview is supposed to predict: on a 20 MP frame at 1600 px the export
/// carries a mean gradient of 357, a preview with no denoising 414, and a
/// preview denoised at the setting that shipped only 239 — the setting was
/// costing more than half the detail the export keeps. Even the gentlest
/// setting tested overshot, at 316.
///
/// So it fades out with the scale and is gone by half resolution. Half is not
/// a taste: below it a single output pixel already averages four or more
/// photosites, which is more smoothing than the filter's own finest step, and
/// everything the filter removes after that is structure. A preview then errs
/// towards showing noise the export will remove rather than mush it will not,
/// and a 1:1 view — where denoising is judged — runs at full strength and shows
/// exactly what the export does.
pub(crate) fn strength_for_scale(strength: f32, pixels: usize, full_pixels: usize) -> f32 {
    if full_pixels == 0 || pixels >= full_pixels {
        return strength;
    }
    let scale = (pixels as f32 / full_pixels as f32).sqrt();
    strength * (2.0 * scale - 1.0).clamp(0.0, 1.0)
}

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

/// The compute path is not offered at present, whatever `ORFEUS_GPU_NOISE`
/// says.
///
/// Its chroma passes implement the full-resolution chain the CPU replaced with
/// a half-resolution one, so switching it on would put a visibly different
/// picture on screen depending on an environment variable. It was never faster
/// than the CPU on the machines it was measured on, and the CPU has since
/// halved, so there is nothing to weigh against the divergence until the
/// shaders are ported. The luma half is unchanged and still checked against
/// the shaders by `actual_gpu_noise_reduction_matches_cpu_when_requested_for_testing`.
fn gpu_noise_reduction_requested() -> bool {
    false
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
        denoise_luma(&mut yy, &mut filtered, width, height, luma_strength);
    }

    profile_step!("luma");
    let chroma_strength = chroma.clamp(0.0, 1.0);
    let filtering_chroma = chroma_strength > 0.0;
    if filtering_chroma {
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

    if !filtering_chroma {
        // Colour was never touched, so it must arrive untouched: adding the
        // brightness the filter changed keeps each pixel's own Cb and Cr
        // exactly, where rebuilding from the halved planes would put the
        // colour through a resampling it never asked for.
        image
            .data
            .par_chunks_mut(3 * width)
            .zip(yy.par_chunks(width))
            .for_each(|(pixels, yy)| {
                for (pixel, luma) in pixels.as_chunks_mut::<3>().0.iter_mut().zip(yy) {
                    let before = 0.2126 * pixel[0] + 0.7152 * pixel[1] + 0.0722 * pixel[2];
                    let delta = *luma - before;
                    pixel[0] += delta;
                    pixel[2] += delta;
                    pixel[1] = (*luma - 0.2126 * pixel[0] - 0.0722 * pixel[2]) / 0.7152;
                }
            });
        profile_step!("merge");
        let _ = step_started;
        return;
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
    inside_frame(x, y, image.width, image.height)
}

#[inline]
fn inside_frame(x: f32, y: f32, width: usize, height: usize) -> bool {
    x >= 0.0 && y >= 0.0 && x <= (width - 1) as f32 && y <= (height - 1) as f32
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

/// How far a correction must zoom in before the border it could not fill
/// leaves the frame.
///
/// VALID_AT is asked about every STEPth sample in each direction. The invalid
/// region is a smooth band around the edge, so a coarse sweep finds the same
/// nearest corner as an exhaustive one to well within the half percent of
/// margin below — and a sweep is cheap only if it does not have to touch every
/// pixel, since the point of it is to run *before* the resampling pass rather
/// than force a second one.
fn auto_crop_scale<F>(width: usize, height: usize, step: usize, valid_at: F) -> f32
where
    F: Fn(usize, usize) -> bool + Sync,
{
    let center_x = (width.saturating_sub(1)) as f32 * 0.5;
    let center_y = (height.saturating_sub(1)) as f32 * 0.5;
    let nearest_invalid = (0..height.div_ceil(step))
        .into_par_iter()
        .map(|sample_row| {
            let y = sample_row * step;
            let mut nearest = 1.0_f32;
            for x in (0..width).step_by(step) {
                if valid_at(x, y) {
                    continue;
                }
                let horizontal = if center_x > 0.0 {
                    (x as f32 - center_x).abs() / center_x
                } else {
                    0.0
                };
                let vertical = if center_y > 0.0 {
                    (y as f32 - center_y).abs() / center_y
                } else {
                    0.0
                };
                nearest = nearest.min(horizontal.max(vertical));
            }
            nearest
        })
        .reduce(|| 1.0_f32, f32::min);
    if nearest_invalid < 1.0 {
        1.005 / nearest_invalid.max(0.5)
    } else {
        1.0
    }
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

/// The profile a photograph's lens resolves to, and the numbers the correction
/// is built from.
pub(crate) struct ResolvedLens<'a> {
    pub(crate) lens: &'a Lens,
    /// The focal length the profile is read at: the recorded one, undone by
    /// any focal reducer between lens and sensor.
    pub(crate) profile_focal: f32,
    /// The crop factor of the body the profile is applied on, before the
    /// reducer.
    pub(crate) base_crop_factor: f32,
    pub(crate) display_name: &'a str,
}

/// Finds the lens profile the correction would use, or says why there is none.
///
/// Shared by the correction itself and by the interface, which asks this before
/// a render so that a lens the database does not know is known not to be known
/// — rather than discovered by a render that fails and has to be run again
/// without the profile, which is what used to happen to every photograph taken
/// on a manual lens.
pub(crate) fn resolve_lens_profile<'a>(
    options: &LensCorrectionOptions<'a>,
) -> Result<ResolvedLens<'a>, Error> {
    let LensCorrectionOptions {
        make,
        model,
        lens_name,
        focal,
        explicit_profile,
        focal_reducer,
        crop_factor,
        ..
    } = *options;
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
        // The body's crop factor is the body's: when the database knows the
        // camera, its figure wins over one stored with an adapted-lens mapping.
        // A stored figure is for bodies the database has never heard of — and
        // for a while the picker stored the lens's own calibration crop there,
        // which put a full-frame Nokton's corner correction on a Four Thirds
        // frame at four times its strength and bent every straight line.
        let base_crop = camera
            .map(|matched| matched.crop_factor)
            .filter(|crop| *crop > 0.0)
            .or_else(|| (crop_factor > 0.0).then_some(crop_factor))
            .ok_or_else(|| {
                Error::LensProfileUnavailable(format!(
                    "adapted-lens mapping for {profile} requires a crop factor"
                ))
            })?;
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
    Ok(ResolvedLens {
        lens,
        profile_focal,
        base_crop_factor,
        display_name,
    })
}

/// The name a lens is shown under: its English name where the database has
/// one, otherwise its primary name.
fn lens_display_name(lens: &Lens) -> &str {
    lens.model_localized
        .get("en")
        .map(String::as_str)
        .unwrap_or(lens.model.as_str())
}

/// Which corrections a profile can offer at FOCAL on a body of CROP_FACTOR:
/// "D" for distortion, "T" for chromatic aberration, "V" for vignetting.
fn lens_calibration_letters(lens: &Lens, focal: f32, crop_factor: f32) -> String {
    let mut modifier = Modifier::new(lens, focal, crop_factor, 64, 64, false);
    let mut letters = String::new();
    if modifier.enable_distortion_correction(lens) {
        letters.push('D');
    }
    if modifier.enable_tca_correction(lens) {
        letters.push('T');
    }
    if !lens.calib_vignetting.is_empty() {
        letters.push('V');
    }
    letters
}

/// Describes the profile a photograph would be corrected with, one line of
/// tab-separated fields: primary name, display name, maker, the letters of the
/// calibrations available at its focal length, and the crop factor in use.
pub(crate) fn describe_lens_match(options: &LensCorrectionOptions<'_>) -> Result<String, Error> {
    let resolved = resolve_lens_profile(options)?;
    let crop = resolved.base_crop_factor * options.focal_reducer;
    Ok(format!(
        "{}\t{}\t{}\t{}\t{:.3}",
        resolved.lens.model,
        lens_display_name(resolved.lens),
        resolved.lens.maker,
        lens_calibration_letters(resolved.lens, resolved.profile_focal, crop),
        crop
    ))
}

/// The most profiles a search hands back; the interface shows a list, not the
/// database.
const LENS_SEARCH_LIMIT: usize = 400;

/// The words of a lens name as a search sees them: runs of letters and digits,
/// lower-cased, with everything between them dropped.
fn lens_name_words(text: &str) -> Vec<String> {
    text.split(|c: char| !c.is_ascii_alphanumeric())
        .filter(|word| !word.is_empty())
        .map(str::to_ascii_lowercase)
        .collect()
}

/// Whether one typed word is found among a lens's words.
///
/// A word of letters may sit inside a longer one — "elmarit" is in
/// "macroelmarit" once the hyphen is gone. A word with a digit in it has to
/// begin one of the lens's words: "21" should find "21mm" and not the "12-100"
/// zoom whose digits happen to run "1 2 1 0 0", which a plain substring search
/// returned as the one match for a 21mm prime.
fn lens_word_matches(word: &str, lens_words: &[String], joined: &str) -> bool {
    if word.chars().any(|c| c.is_ascii_digit()) {
        lens_words.iter().any(|candidate| candidate.starts_with(word))
    } else {
        joined.contains(word)
    }
}

/// Lists lens profiles for the interface to choose from, one per line, in the
/// fields `describe_lens_match` uses followed by the focal range and mounts.
///
/// With a QUERY, every profile whose maker or name has all of its words,
/// compared with punctuation and case set aside — "zuiko 21" finds an
/// "Olympus OM Zuiko Auto-W 21mm f/3.5" however the database punctuates it.
/// Without one, every profile a body of MAKE and MODEL can mount. Profiles the
/// body can mount come first, then those covering FOCAL, then the rest
/// alphabetically, so the likely answer is near the top whatever was typed.
pub(crate) fn search_lens_profiles(
    make: &str,
    model: &str,
    query: &str,
    focal: f32,
) -> Result<String, Error> {
    let db = lens_database()?;
    let camera = if make.is_empty() && model.is_empty() {
        None
    } else {
        find_camera_profile(db, make, model)
    };
    let words = lens_name_words(query);
    let mut matches: Vec<(bool, bool, &Lens)> = db
        .lenses
        .iter()
        .filter(|lens| {
            let names = format!(
                "{} {} {}",
                lens.maker,
                lens.model,
                lens.model_localized.values().cloned().collect::<Vec<_>>().join(" ")
            );
            let lens_words = lens_name_words(&names);
            let joined = lens_words.join("");
            words
                .iter()
                .all(|word| lens_word_matches(word, &lens_words, &joined))
        })
        .map(|lens| {
            let mountable = camera.is_none_or(|camera| camera_mount_compatible(db, camera, lens));
            let covers = focal <= 0.0
                || ((lens.focal_min <= 0.0 || focal >= lens.focal_min - 0.1)
                    && (lens.focal_max <= 0.0 || focal <= lens.focal_max + 0.1));
            (mountable, covers, lens)
        })
        .filter(|(mountable, _, _)| !words.is_empty() || *mountable)
        .collect();
    matches.sort_by(|a, b| {
        b.0.cmp(&a.0)
            .then(b.1.cmp(&a.1))
            .then_with(|| a.2.maker.to_lowercase().cmp(&b.2.maker.to_lowercase()))
            .then_with(|| lens_display_name(a.2).to_lowercase().cmp(&lens_display_name(b.2).to_lowercase()))
    });
    let crop = camera.map(|camera| camera.crop_factor).unwrap_or(0.0);
    let mut out = String::new();
    for (mountable, _, lens) in matches.into_iter().take(LENS_SEARCH_LIMIT) {
        let at_focal = if focal > 0.0 { focal } else { lens.focal_min.max(1.0) };
        let letters = lens_calibration_letters(lens, at_focal, if crop > 0.0 { crop } else { lens.crop_factor.max(1.0) });
        out.push_str(&format!(
            "{}\t{}\t{}\t{}\t{:.3}\t{}\t{}\t{}\t{}\n",
            lens.model,
            lens_display_name(lens),
            lens.maker,
            letters,
            lens.crop_factor,
            lens.focal_min,
            lens.focal_max,
            lens.mounts.join(", "),
            if mountable { 1 } else { 0 },
        ));
    }
    Ok(out)
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
    let profiling = std::env::var_os("ORFEUS_PROFILE").is_some();
    let started = Instant::now();
    let ResolvedLens {
        lens,
        profile_focal,
        base_crop_factor,
        display_name,
    } = resolve_lens_profile(options)?;
    let _ = (make, model, lens_name, focal, explicit_profile, crop_factor);
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
    if profiling {
        eprintln!("orfeus-profile lens-step=maps milliseconds={:.3}", started.elapsed().as_secs_f64() * 1000.0);
    }
    let remap_started = Instant::now();

    // Where the maps put a pixel, at any position rather than only on the
    // integer grid they were evaluated for. Distortion varies smoothly, so
    // reading between two columns and two grid rows is as accurate as the grid
    // itself; it is what lets the auto-crop zoom be folded into this pass.
    let map_at = |rows: &[f32],
                  stride: usize,
                  component: usize,
                  u: f32,
                  grid_row: usize,
                  fraction: f32| {
        let column = (u.max(0.0) as usize).min(width - 1);
        let next = (column + 1).min(width - 1);
        let across = u - column as f32;
        let at = |row: usize, column: usize| {
            rows[row * width * stride + column * stride + component]
        };
        let top = at(grid_row, column) * (1.0 - across) + at(grid_row, next) * across;
        let bottom = at(grid_row + 1, column) * (1.0 - across) + at(grid_row + 1, next) * across;
        top * (1.0 - fraction) + bottom * fraction
    };
    let grid_at = |v: f32| {
        let position = v.max(0.0) / LENS_MAP_ROW_STEP as f32;
        let grid_row = (position as usize).min(grid_rows - 2);
        (grid_row, position - grid_row as f32)
    };

    const VALIDITY_STEP: usize = 4;
    let (center_x, center_y) = (
        (width.saturating_sub(1)) as f32 * 0.5,
        (height.saturating_sub(1)) as f32 * 0.5,
    );
    let scale = auto_crop_scale(width, height, VALIDITY_STEP, |x, y| {
        let (grid_row, fraction) = grid_at(y as f32);
        let (gx, gy) = if distortion {
            (
                blend_lens_coordinate(
                    x as f32,
                    map_at(&geometry_rows, 2, 0, x as f32, grid_row, fraction),
                    correction_strength,
                ),
                blend_lens_coordinate(
                    y as f32,
                    map_at(&geometry_rows, 2, 1, x as f32, grid_row, fraction),
                    correction_strength,
                ),
            )
        } else {
            (x as f32, y as f32)
        };
        if !inside_frame(gx, gy, width, height) {
            return false;
        }
        if !tca {
            return true;
        }
        (0..3).all(|channel| {
            let sx = map_at(&subpixel_rows, 6, channel * 2, x as f32, grid_row, fraction) + gx
                - x as f32;
            let sy = map_at(&subpixel_rows, 6, channel * 2 + 1, x as f32, grid_row, fraction) + gy
                - y as f32;
            inside_frame(sx, sy, width, height)
        })
    });
    let inverse_scale = 1.0 / scale;

    // Resampled into a fresh buffer rather than through a copy of the image:
    // at 80 MP that copy was a gigabyte read and written for nothing. The
    // auto-crop zoom rides along in the same gather: as its own pass it
    // resampled the whole frame a second time — 319 ms at 80 MP to crop away
    // one percent — and resampling twice softens what one pass keeps.
    let mut output = vec![0.0_f32; width * height * 3];
    output
        .par_chunks_mut(row_stride)
        .enumerate()
        .for_each(|(y, output_row)| {
            let v = center_y + (y as f32 - center_y) * inverse_scale;
            let (grid_row, fraction) = grid_at(v);
            for x in 0..width {
                let u = center_x + (x as f32 - center_x) * inverse_scale;
                let (gx, gy) = if distortion {
                    (
                        blend_lens_coordinate(
                            u,
                            map_at(&geometry_rows, 2, 0, u, grid_row, fraction),
                            correction_strength,
                        ),
                        blend_lens_coordinate(
                            v,
                            map_at(&geometry_rows, 2, 1, u, grid_row, fraction),
                            correction_strength,
                        ),
                    )
                } else {
                    (u, v)
                };
                if !tca {
                    // Without a chromatic correction all three channels read the
                    // same point, so they share one set of weights and land on
                    // three adjacent floats.
                    output_row[x * 3..x * 3 + 3].copy_from_slice(&bilinear_rgb(image, gx, gy));
                    continue;
                }
                for channel in 0..3 {
                    let sx = map_at(&subpixel_rows, 6, channel * 2, u, grid_row, fraction) + gx - u;
                    let sy =
                        map_at(&subpixel_rows, 6, channel * 2 + 1, u, grid_row, fraction) + gy - v;
                    output_row[x * 3 + channel] = bilinear(image, sx, sy, channel);
                }
            }
        });
    image.data = output;
    if profiling {
        eprintln!(
            "orfeus-profile lens-step=remap scale={scale:.4} milliseconds={:.3}",
            remap_started.elapsed().as_secs_f64() * 1000.0
        );
    }
    Ok(())
}

/// How far the red and blue images of a frame sit from the green one, as
/// radial scale factors: positive means the channel is magnified against green.
///
/// Lateral colour is what a lens does to the size of its image at each
/// wavelength, so to first order it is one number per channel — the same model
/// a database's "linear" chromatic aberration entry uses. It is measured from
/// the photograph itself: the database was measured on somebody else's copy of
/// the lens, on another body, and on the one frame this was checked against it
/// prescribed a red shift of two and a half pixels at the corners of a frame
/// whose red sat within a fiftieth of a pixel of green — the prescription was
/// the fringe.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub(crate) struct LateralColour {
    pub(crate) red: f32,
    pub(crate) blue: f32,
}

impl LateralColour {
    pub(crate) fn is_none(&self) -> bool {
        self.red == 0.0 && self.blue == 0.0
    }
}

/// Side of the square patches lateral colour is measured on.
const COLOUR_PATCH: usize = 64;
/// How far a channel is searched for its shift, in whole pixels either way.
const COLOUR_SEARCH: usize = 3;
/// Patches kept per quadrant, so one busy corner does not speak for the frame.
const COLOUR_PATCHES_PER_QUADRANT: usize = 16;
/// Patches a measurement needs before it is believed.
const COLOUR_MIN_PATCHES: usize = 12;
/// Patches nearer the centre than this fraction of the half-diagonal are not
/// read: lateral colour grows with radius and there is nothing to see there.
const COLOUR_MIN_RADIUS: f32 = 0.35;
/// A corner shift under this many pixels is left alone: below it the
/// resampling that would move the channel costs more sharpness than the shift.
const COLOUR_NEGLIGIBLE: f32 = 0.25;
/// How uncertain the settled corner shift may be, in pixels, before the frame
/// is judged not to say: the patches' spread divided by the root of their
/// number, which is how far the median of them can be expected to be off.
const COLOUR_MAX_UNCERTAINTY: f32 = 0.12;
/// A shift with more than this much across the radial direction is not
/// lateral colour, whatever else it is.
const COLOUR_MAX_TANGENTIAL: f32 = 0.5;
/// The correlation a channel must reach with green at its best shift, and how
/// far that best must stand above the average shift, for the patch to count.
const COLOUR_MIN_CORRELATION: f32 = 0.3;
const COLOUR_MIN_PROMINENCE: f32 = 0.05;

/// Softens a square plane with a five-tap binomial in each direction, so that
/// nothing finer than about four pixels is left to compare.
///
/// Red and blue are sampled at every other photosite, so anything in them above
/// a quarter cycle per pixel is the demosaic's guess, guided by green. Below it
/// all three channels are real, and a shift read there is the lens's; the
/// guess above it is left out of the comparison rather than trusted.
fn soften_for_comparison(values: &mut [f32], size: usize) {
    let kernel = [1.0_f32 / 16.0, 4.0 / 16.0, 6.0 / 16.0, 4.0 / 16.0, 1.0 / 16.0];
    let mut rows = vec![0.0_f32; values.len()];
    for y in 0..size {
        let line = &values[y * size..(y + 1) * size];
        for x in 0..size {
            rows[y * size + x] = (0..5)
                .map(|tap| kernel[tap] * line[(x + tap).saturating_sub(2).min(size - 1)])
                .sum();
        }
    }
    for y in 0..size {
        for x in 0..size {
            values[y * size + x] = (0..5)
                .map(|tap| kernel[tap] * rows[(y + tap).saturating_sub(2).min(size - 1) * size + x])
                .sum();
        }
    }
}

/// Takes the slow light out of a square plane and scales what is left to unit
/// power, so two channels of different gain compare shape against shape.
fn high_pass_normalise(values: &mut [f32], size: usize) {
    const RADIUS: usize = 4;
    let mut blurred = vec![0.0_f32; values.len()];
    let mut rows = vec![0.0_f32; values.len()];
    for y in 0..size {
        let line = &values[y * size..(y + 1) * size];
        for x in 0..size {
            let (left, right) = (x.saturating_sub(RADIUS), (x + RADIUS).min(size - 1));
            rows[y * size + x] = line[left..=right].iter().sum::<f32>() / (right - left + 1) as f32;
        }
    }
    for y in 0..size {
        let (top, bottom) = (y.saturating_sub(RADIUS), (y + RADIUS).min(size - 1));
        for x in 0..size {
            let total: f32 = (top..=bottom).map(|yy| rows[yy * size + x]).sum();
            blurred[y * size + x] = total / (bottom - top + 1) as f32;
        }
    }
    let mut power = 0.0_f32;
    for (value, blur) in values.iter_mut().zip(&blurred) {
        *value -= blur;
        power += *value * *value;
    }
    let rms = (power / values.len() as f32).sqrt();
    if rms > 1e-7 {
        for value in values.iter_mut() {
            *value /= rms;
        }
    }
}

/// How far CHANNEL's picture in the patch at (X0, Y0) sits from green's, in
/// pixels, or None when the patch does not say.
///
/// The shift that best lays the channel over green, searched to the whole
/// pixel and then refined by the parabola through the three nearest scores. A
/// maximum on the edge of the search, a flat score surface, or a match barely
/// better than the average shift all mean the patch has nothing to say.
///
/// The score is the normalised correlation of the two windows, each divided by
/// its own energy, rather than their squared difference: the energy of a real
/// texture is not spread evenly, so a plain difference can fall by sliding the
/// window onto a quieter part of it as well as by lining the channels up.
/// Correlation asks about shape alone.
///
/// Checked against the frame itself, not against another estimate. Phase
/// correlation of demosaiced channels reports no shift whatever the lens did,
/// because red and blue are interpolated from green and so carry green's noise
/// at zero lag; the check that settles it is the position of isolated edges per
/// channel, which on a 21 mm frame this read as 2.8 px moved by 1.7 to 2.2 px
/// of red and blue at the corners, to within 0.4 px after the correction.
fn channel_shift(image: &RgbImage, x0: usize, y0: usize, channel: usize) -> Option<(f32, f32)> {
    let width = image.width;
    let patch = COLOUR_PATCH;
    let search = COLOUR_SEARCH;
    let span = patch + 2 * search;
    let plane = |ch: usize, ox: usize, oy: usize, size: usize| -> Vec<f32> {
        let mut values: Vec<f32> = (0..size * size)
            .map(|i| image.data[((oy + i / size) * width + ox + i % size) * 3 + ch])
            .collect();
        soften_for_comparison(&mut values, size);
        high_pass_normalise(&mut values, size);
        values
    };
    // Both channels are read over the same window and filtered alike; the
    // patch compared is the middle of green's. Filtering each over its own
    // extent put the box filter's edge in different places on the two, which
    // read as a quarter-pixel shift on a frame with none.
    let green = plane(1, x0 - search, y0 - search, span);
    let other = plane(channel, x0 - search, y0 - search, span);
    let steps = 2 * search + 1;
    let green_energy: f32 = (0..patch)
        .flat_map(|y| green[(y + search) * span + search..][..patch].iter())
        .map(|g| g * g)
        .sum();
    if green_energy <= 1e-9 {
        return None;
    }
    let score = |ix: usize, iy: usize| -> f32 {
        let (mut dot, mut energy) = (0.0_f32, 0.0_f32);
        for y in 0..patch {
            let row = &other[(y + iy) * span + ix..][..patch];
            let g = &green[(y + search) * span + search..][..patch];
            for (o, g) in row.iter().zip(g) {
                dot += o * g;
                energy += o * o;
            }
        }
        dot / (energy * green_energy).sqrt().max(1e-9)
    };
    let mut scores = vec![0.0_f32; steps * steps];
    let mut best = (0_usize, 0_usize, f32::NEG_INFINITY);
    for iy in 0..steps {
        for ix in 0..steps {
            let s = score(ix, iy);
            scores[iy * steps + ix] = s;
            if s > best.2 {
                best = (ix, iy, s);
            }
        }
    }
    let (ix, iy, highest) = best;
    if ix == 0 || iy == 0 || ix == steps - 1 || iy == steps - 1 || !highest.is_finite() {
        return None;
    }
    let mean = scores.iter().sum::<f32>() / scores.len() as f32;
    if highest < COLOUR_MIN_CORRELATION || highest - mean < COLOUR_MIN_PROMINENCE {
        return None;
    }
    let refine = |a: f32, b: f32, c: f32| -> Option<f32> {
        let curvature = a - 2.0 * b + c;
        (curvature < 0.0).then(|| ((a - c) / (2.0 * curvature)).clamp(-1.0, 1.0))
    };
    let at = |x: usize, y: usize| scores[y * steps + x];
    let ox = refine(at(ix - 1, iy), highest, at(ix + 1, iy))?;
    let oy = refine(at(ix, iy - 1), highest, at(ix, iy + 1))?;
    Some((
        ix as f32 - search as f32 + ox,
        iy as f32 - search as f32 + oy,
    ))
}

/// Measures the frame's lateral colour, or says it cannot.
///
/// The busiest patches of each quadrant, away from the centre, each report how
/// far red and blue sit from green; the radial part of each shift over its
/// radius is a reading of the scale, and the median of the readings is the
/// answer, provided enough patches agree closely enough for it to be one. Read
/// from a quarter of the pixels of every candidate patch and then in full from
/// sixty-four of them, so an eighty-megapixel frame costs a few tens of
/// milliseconds.
pub(crate) fn measure_lateral_colour(image: &RgbImage) -> Option<LateralColour> {
    let started = Instant::now();
    let (width, height) = (image.width, image.height);
    let patch = COLOUR_PATCH;
    let margin = COLOUR_SEARCH + 1;
    if width < 2 * (patch + margin) + 8 || height < 2 * (patch + margin) + 8 {
        return None;
    }
    let centre = ((width as f32 - 1.0) * 0.5, (height as f32 - 1.0) * 0.5);
    let half_diagonal = centre.0.hypot(centre.1);
    let columns = (width - 2 * margin) / patch;
    let rows = (height - 2 * margin) / patch;
    let candidates: Vec<(f32, usize, usize)> = (0..rows)
        .into_par_iter()
        .flat_map_iter(|row| {
            (0..columns).filter_map(move |column| {
                let (x0, y0) = (margin + column * patch, margin + row * patch);
                let (cx, cy) = (x0 as f32 + patch as f32 * 0.5, y0 as f32 + patch as f32 * 0.5);
                if (cx - centre.0).hypot(cy - centre.1) < COLOUR_MIN_RADIUS * half_diagonal {
                    return None;
                }
                let at = |x: usize, y: usize| image.data[(y * width + x) * 3 + 1];
                let mut texture = 0.0_f32;
                let mut y = y0 + 1;
                while y + 1 < y0 + patch {
                    let mut x = x0 + 1;
                    while x + 1 < x0 + patch {
                        texture += (at(x + 1, y) - at(x - 1, y)).abs()
                            + (at(x, y + 1) - at(x, y - 1)).abs();
                        x += 4;
                    }
                    y += 4;
                }
                Some((texture, x0, y0))
            })
        })
        .collect();
    let best = candidates.iter().map(|c| c.0).fold(0.0_f32, f32::max);
    if best <= 0.0 {
        return None;
    }
    let mut chosen: Vec<(f32, usize, usize)> = Vec::new();
    for quadrant in 0..4 {
        let mut here: Vec<(f32, usize, usize)> = candidates
            .iter()
            .copied()
            .filter(|(texture, x0, y0)| {
                let right = *x0 as f32 + patch as f32 * 0.5 >= centre.0;
                let below = *y0 as f32 + patch as f32 * 0.5 >= centre.1;
                usize::from(right) + 2 * usize::from(below) == quadrant && *texture >= 0.1 * best
            })
            .collect();
        here.sort_by(|a, b| b.0.total_cmp(&a.0));
        chosen.extend(here.into_iter().take(COLOUR_PATCHES_PER_QUADRANT));
    }
    // Each patch's reading of the scale, with the radius it was read at: a
    // shift measured to a tenth of a pixel is a tenth as uncertain a scale at
    // the corner as it is a third of the way out.
    let readings: Vec<(Option<(f32, f32)>, Option<(f32, f32)>)> = chosen
        .par_iter()
        .map(|&(_, x0, y0)| {
            let (cx, cy) = (x0 as f32 + patch as f32 * 0.5, y0 as f32 + patch as f32 * 0.5);
            let (dx, dy) = (cx - centre.0, cy - centre.1);
            let radius = dx.hypot(dy);
            let (ux, uy) = (dx / radius, dy / radius);
            let scale = |channel: usize| -> Option<(f32, f32)> {
                let (sx, sy) = channel_shift(image, x0, y0, channel)?;
                let radial = sx * ux + sy * uy;
                let tangential = -sx * uy + sy * ux;
                (tangential.abs() <= COLOUR_MAX_TANGENTIAL).then_some((radial / radius, radius))
            };
            (scale(0), scale(2))
        })
        .collect();
    let profiling = std::env::var_os("ORFEUS_PROFILE").is_some();
    let settle = |mut values: Vec<(f32, f32)>| -> Option<f32> {
        if values.len() < COLOUR_MIN_PATCHES {
            if profiling {
                eprintln!("orfeus-profile lateral-colour readings={} too few", values.len());
            }
            return None;
        }
        // The median weighted by radius, and how far it may be off: the
        // spread of the readings over the root of their number.
        values.sort_by(|a, b| a.0.total_cmp(&b.0));
        let total: f32 = values.iter().map(|v| v.1).sum();
        let mut running = 0.0_f32;
        let mut median = values[values.len() / 2].0;
        for (value, weight) in &values {
            running += weight;
            if running >= 0.5 * total {
                median = *value;
                break;
            }
        }
        let mut deviations: Vec<f32> = values.iter().map(|v| (v.0 - median).abs()).collect();
        deviations.sort_by(f32::total_cmp);
        let spread = deviations[deviations.len() / 2] * half_diagonal;
        let uncertainty = spread / (values.len() as f32).sqrt();
        if profiling {
            eprintln!(
                "orfeus-profile lateral-colour readings={} corner-shift={:.3} spread={:.3} uncertainty={:.3}",
                values.len(),
                median * half_diagonal,
                spread,
                uncertainty
            );
        }
        if uncertainty > COLOUR_MAX_UNCERTAINTY {
            return None;
        }
        Some(if (median * half_diagonal).abs() < COLOUR_NEGLIGIBLE { 0.0 } else { median })
    };
    let red = settle(readings.iter().filter_map(|r| r.0).collect());
    let blue = settle(readings.iter().filter_map(|r| r.1).collect());
    if std::env::var_os("ORFEUS_PROFILE").is_some() {
        eprintln!(
            "orfeus-profile lateral-colour patches={} red={:?} blue={:?} milliseconds={:.3}",
            chosen.len(),
            red,
            blue,
            started.elapsed().as_secs_f64() * 1000.0
        );
    }
    if red.is_none() && blue.is_none() {
        return None;
    }
    Some(LateralColour {
        red: red.unwrap_or(0.0),
        blue: blue.unwrap_or(0.0),
    })
}

/// Corrects barrel or pincushion distortion by a stated amount, and lateral
/// colour by measured or stated scale factors, in one resampling.
///
/// For the lenses no database describes — anything old enough, adapted, or
/// simply unlisted — where the alternative is living with bent horizons.
///
/// One term of the usual radial polynomial: a point at radius `r` from the
/// centre is gathered from `r * (1 + k r^2)`, with `r` measured against the
/// half-diagonal so that the strength means the same thing whatever the frame's
/// shape. Positive straightens barrel, negative straightens pincushion. One
/// term rather than three because a slider a photographer sets by eye against a
/// straight edge cannot usefully carry more, and the higher terms of a real
/// profile only matter once the first one is right.
///
/// Red and blue are then gathered from their own radius, scaled by the
/// measured factor, so a channel that the lens drew a fraction larger is drawn
/// back onto green. The auto-crop zoom rides along in the same gather, as it
/// does for the profile correction: resampling twice would soften what one pass
/// keeps.
pub(crate) fn apply_radial_corrections(image: &mut RgbImage, amount: f32, colour: LateralColour) {
    let (width, height) = (image.width, image.height);
    if (amount == 0.0 && colour.is_none()) || width < 2 || height < 2 {
        return;
    }
    let center_x = (width - 1) as f32 * 0.5;
    let center_y = (height - 1) as f32 * 0.5;
    let normalize = 1.0 / (center_x * center_x + center_y * center_y).sqrt();
    let channel_scale = [1.0 + colour.red, 1.0, 1.0 + colour.blue];
    let gather = |x: f32, y: f32, scale: f32| {
        let dx = (x - center_x) * normalize;
        let dy = (y - center_y) * normalize;
        let factor = (1.0 + amount * (dx * dx + dy * dy)) * scale;
        (
            center_x + (x - center_x) * factor,
            center_y + (y - center_y) * factor,
        )
    };
    const VALIDITY_STEP: usize = 4;
    let scale = auto_crop_scale(width, height, VALIDITY_STEP, |x, y| {
        channel_scale.iter().all(|scale| {
            let (sx, sy) = gather(x as f32, y as f32, *scale);
            inside_frame(sx, sy, width, height)
        })
    });
    let inverse_scale = 1.0 / scale;
    let row_stride = width * 3;
    let mut output = vec![0.0_f32; width * height * 3];
    output
        .par_chunks_mut(row_stride)
        .enumerate()
        .for_each(|(y, output_row)| {
            let v = center_y + (y as f32 - center_y) * inverse_scale;
            for x in 0..width {
                let u = center_x + (x as f32 - center_x) * inverse_scale;
                if colour.is_none() {
                    let (sx, sy) = gather(u, v, 1.0);
                    output_row[x * 3..x * 3 + 3].copy_from_slice(&bilinear_rgb(image, sx, sy));
                } else {
                    for (channel, scale) in channel_scale.iter().enumerate() {
                        let (sx, sy) = gather(u, v, *scale);
                        output_row[x * 3 + channel] = bilinear(image, sx, sy, channel);
                    }
                }
            }
        });
    image.data = output;
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

/// Mirrors the image across either axis, or both.
///
/// A permutation of the pixels, done in place: nothing is resampled and nothing
/// is lost, so flipping twice returns exactly what went in. Wanted more often
/// than it sounds for film — a negative laid on the light table emulsion side up
/// comes out mirrored, and no amount of rotating fixes a mirror.
pub(crate) fn apply_flip(image: &mut RgbImage, horizontal: bool, vertical: bool) {
    let (width, height) = (image.width, image.height);
    if horizontal {
        image.data.par_chunks_mut(width * 3).for_each(|row| {
            let pixels = row.as_chunks_mut::<3>().0;
            pixels.reverse();
        });
    }
    if vertical {
        // Split at the middle and walk the far half backwards, so each pair of
        // rows is swapped exactly once. An odd height leaves its middle row
        // unpaired, which `zip` drops on its own.
        let (top, bottom) = image.data.split_at_mut(width * 3 * (height / 2));
        top.par_chunks_mut(width * 3)
            .zip(bottom.par_chunks_mut(width * 3).rev())
            .for_each(|(row, mirrored)| row.swap_with_slice(mirrored));
    }
}

/// Where a rectangle of the unoriented frame lands once the frame is oriented.
///
/// The forward of what `map_oriented_rect` undoes, in whole pixels rather than
/// fractions, because a window render needs to know exactly — to the pixel —
/// where the part it developed belongs. SOURCE is the unoriented frame's size
/// and RECT is (left, top, width, height) within it.
pub(crate) fn orient_rect(
    orientation: u16,
    source: (usize, usize),
    rect: (usize, usize, usize, usize),
) -> (usize, usize, usize, usize) {
    let (width, height) = source;
    let (left, top, rect_width, rect_height) = rect;
    let (right, bottom) = (left + rect_width, top + rect_height);
    match orientation {
        2 => (width - right, top, rect_width, rect_height),
        3 => (width - right, height - bottom, rect_width, rect_height),
        4 => (left, height - bottom, rect_width, rect_height),
        5 => (top, left, rect_height, rect_width),
        6 => (height - bottom, left, rect_height, rect_width),
        7 => (height - bottom, width - right, rect_height, rect_width),
        8 => (top, width - right, rect_height, rect_width),
        _ => (left, top, rect_width, rect_height),
    }
}

/// Copies out one rectangle of IMAGE.
pub(crate) fn crop_rect(
    image: &RgbImage,
    left: usize,
    top: usize,
    width: usize,
    height: usize,
) -> RgbImage {
    let mut data = vec![0.0_f32; width * height * 3];
    data.par_chunks_mut(width * 3)
        .enumerate()
        .for_each(|(row, out)| {
            let start = ((top + row) * image.width + left) * 3;
            out.copy_from_slice(&image.data[start..start + width * 3]);
        });
    RgbImage {
        width,
        height,
        data,
    }
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

/// Turns a display-encoded value back into linear light.
///
/// The inverse of `exact_srgb_encode`, and used only where a control reads in
/// display terms but the pixels are scene-linear — a contrast pivot, for one.
pub(crate) fn srgb_decode(value: f32) -> f32 {
    if value <= 0.040_45 {
        value / 12.92
    } else {
        ((value + 0.055) / 1.055).max(0.0).powf(2.4)
    }
}

/// Steepens or flattens the image about a fixed tone, leaving that tone put.
///
/// A straight slope in the logarithm of the signal: `out = p * (in / p) ^ c`.
/// Written that way it is the same operator DaVinci's contrast control applies
/// in its log working space, and it has the properties a contrast control
/// wants and a subtract-multiply-add in linear light does not — it cannot drive
/// a value negative, it cannot clip a highlight, it leaves the pivot exactly
/// where it was, and doubling it twice is the same as quadrupling it once.
///
/// The pivot is stated in display terms, where a photographer can recognise it:
/// DaVinci's default of 0.435 is a little under middle grey, which is 0.46.
/// Applied per channel rather than to luminance alone, which is what makes
/// contrast also add a little saturation — again as DaVinci's does.
pub(crate) fn apply_contrast(image: &mut RgbImage, contrast: f32, pivot: f32) {
    if contrast == 1.0 {
        return;
    }
    let pivot = srgb_decode(pivot.clamp(0.01, 0.99)).max(1.0e-4);
    let inverse = 1.0 / pivot;
    image.data.par_chunks_mut(3 * 8192).for_each(|chunk| {
        for value in chunk.iter_mut() {
            if *value > 0.0 {
                *value = pivot * (*value * inverse).powf(contrast);
            }
        }
    });
}

/// Bins of the log-spaced histograms the negative inversion measures with, and
/// the signal they span in octaves: from far below any sensor's noise floor to
/// a little above full scale.
const NEGATIVE_BINS: usize = 4096;
const NEGATIVE_LOG_FLOOR: f32 = -20.0;
const NEGATIVE_LOG_CEILING: f32 = 4.0;
/// Entries in each channel's inversion table, spaced on the square root of the
/// signal so the dense end of the negative, where the positive changes fastest,
/// is resolved finest.
const NEGATIVE_LUT_SIZE: usize = 4096;
/// The fraction of the film base below which a channel is taken to be reading
/// its own noise floor rather than dye: a density of four.
const NEGATIVE_DENSITY_FLOOR: f32 = 1.0e-4;
/// Density above the frame's white over which the paper's toe takes over. A
/// print runs out of paper before a negative runs out of dye, and a scan's
/// starved blue channel runs out of photons before either; without this the
/// last stop of a dense sky came out as speckle a hundred times brighter than
/// the clouds beside it.
const NEGATIVE_SHOULDER: f32 = 0.35;
/// Share of the frame, by luminance, read as the thinnest film when no base was
/// picked: the unexposed border when there is one, the deepest shadow otherwise.
const NEGATIVE_BASE_SHARE: f32 = 0.005;
/// Share of the frame, by green density, read as the brightest tones: where the
/// white is anchored, and where the channels are made to agree.
const NEGATIVE_WHITE_SHARE: f32 = 0.01;
/// Below this fraction of the base in red, a pixel is not film. No colour
/// negative's cyan layer reaches a density of one and a half; a holder's edge,
/// a black leader or the light table's frame inside the crop does, and one such
/// strip a percent wide was enough to anchor the white to itself and print the
/// whole picture as night.
const NEGATIVE_FILM_FLOOR: f32 = 0.03;
/// How far above the frame's median density the white may be anchored. A
/// printer integrates the frame and lets a small bright sky burn out rather than
/// print a dark wood as night for its sake; half a density above the median is
/// about three stops of scene, and a frame whose brightest percent lies further
/// out than that has its white brought back to there.
const NEGATIVE_ANCHOR_REACH: f32 = 0.5;

fn negative_bin(value: f32) -> usize {
    let log = value.max(2.0_f32.powi(NEGATIVE_LOG_FLOOR as i32)).log2();
    let position = (log - NEGATIVE_LOG_FLOOR) / (NEGATIVE_LOG_CEILING - NEGATIVE_LOG_FLOOR);
    ((position * (NEGATIVE_BINS - 1) as f32).round().max(0.0) as usize).min(NEGATIVE_BINS - 1)
}

fn negative_bin_value(bin: usize) -> f32 {
    2.0_f32.powf(
        NEGATIVE_LOG_FLOOR
            + bin as f32 / (NEGATIVE_BINS - 1) as f32 * (NEGATIVE_LOG_CEILING - NEGATIVE_LOG_FLOOR),
    )
}

/// The signal below which FRACTION of HISTOGRAM's counts lie.
fn histogram_quantile(histogram: &[u32], fraction: f32) -> f32 {
    let total: u64 = histogram.iter().map(|count| *count as u64).sum();
    if total == 0 {
        return 0.0;
    }
    let target = (total as f64 * fraction as f64) as u64;
    let mut seen = 0_u64;
    for (bin, count) in histogram.iter().enumerate() {
        seen += *count as u64;
        if seen > target {
            return negative_bin_value(bin);
        }
    }
    negative_bin_value(NEGATIVE_BINS - 1)
}

fn add_histograms(mut into: Vec<u32>, from: Vec<u32>) -> Vec<u32> {
    for (a, b) in into.iter_mut().zip(&from) {
        *a += *b;
    }
    into
}

/// What the inversion measured from the frame: the film base it divides by and
/// the density of the brightest tones in each channel.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct NegativeMeasure {
    pub(crate) base: [f32; 3],
    pub(crate) white_density: [f32; 3],
    /// The green density printed as white: the brightest tones', unless they
    /// lie further above the frame's median than a printer would follow.
    pub(crate) anchor: f32,
}

/// Measures the negative: the film base when PICKED gives none, and the density
/// of the frame's brightest tones per channel.
///
/// Three passes of histograms rather than a sort: where the thinnest film lies,
/// then its colour and where the densest green lies among film, then every
/// channel over that densest green. Pixels below the film floor in red — a
/// holder's edge inside the crop — take no part in the white.
pub(crate) fn measure_negative(image: &RgbImage, picked: [f32; 3]) -> NegativeMeasure {
    let pixels = image.data.as_chunks::<3>().0;
    let luminance = |pixel: &[f32; 3]| 0.212_672_9 * pixel[0] + 0.715_152_2 * pixel[1] + 0.072_175 * pixel[2];
    let first = pixels
        .par_chunks(8192)
        .fold(
            || vec![0_u32; 4 * NEGATIVE_BINS],
            |mut histograms, chunk| {
                for pixel in chunk {
                    for (channel, value) in pixel.iter().enumerate() {
                        histograms[channel * NEGATIVE_BINS + negative_bin(*value)] += 1;
                    }
                    histograms[3 * NEGATIVE_BINS + negative_bin(luminance(pixel))] += 1;
                }
                histograms
            },
        )
        .reduce(|| vec![0_u32; 4 * NEGATIVE_BINS], add_histograms);
    let channel_histogram = |channel: usize| &first[channel * NEGATIVE_BINS..(channel + 1) * NEGATIVE_BINS];
    let bright_luminance = histogram_quantile(channel_histogram(3), 1.0 - NEGATIVE_BASE_SHARE);
    let picked_base = picked.iter().all(|value| *value > 0.0);
    // Second pass: the colour of the thinnest film when no base was picked,
    // and where the densest green lies among pixels that are film at all. The
    // film floor is judged against the picked red, or a provisional one from
    // the brightest red the frame holds; a hundredth of either is not film.
    let provisional_red = if picked_base {
        picked[0]
    } else {
        histogram_quantile(channel_histogram(0), 1.0 - NEGATIVE_BASE_SHARE)
    };
    let film_floor = provisional_red * NEGATIVE_FILM_FLOOR;
    let second = pixels
        .par_chunks(8192)
        .fold(
            || (vec![0_u32; NEGATIVE_BINS], [0.0_f64; 4]),
            |(mut histogram, mut sums), chunk| {
                for pixel in chunk {
                    if luminance(pixel) >= bright_luminance {
                        for (channel, value) in pixel.iter().enumerate() {
                            sums[channel] += *value as f64;
                        }
                        sums[3] += 1.0;
                    }
                    if pixel[0] >= film_floor {
                        histogram[negative_bin(pixel[1])] += 1;
                    }
                }
                (histogram, sums)
            },
        )
        .reduce(
            || (vec![0_u32; NEGATIVE_BINS], [0.0_f64; 4]),
            |(a, mut sa), (b, sb)| {
                for (x, y) in sa.iter_mut().zip(&sb) {
                    *x += *y;
                }
                (add_histograms(a, b), sa)
            },
        );
    let mut base = [0.0_f32; 3];
    for channel in 0..3 {
        base[channel] = if picked_base {
            picked[channel]
        } else if second.1[3] > 0.0 {
            (second.1[channel] / second.1[3]) as f32
        } else {
            histogram_quantile(channel_histogram(channel), 1.0 - NEGATIVE_BASE_SHARE)
        }
        .max(1.0e-6);
    }
    let dense_green = histogram_quantile(&second.0, NEGATIVE_WHITE_SHARE);
    let median_green = histogram_quantile(&second.0, 0.5);
    // Third pass: every channel over the densest green, film only, so the
    // white is one set of pixels chosen once rather than three chosen per
    // channel: choosing per channel let a starved blue channel's noise stand
    // in for its white and made a cloud yellow.
    let third = pixels
        .par_chunks(8192)
        .fold(
            || vec![0_u32; 3 * NEGATIVE_BINS],
            |mut histograms, chunk| {
                for pixel in chunk {
                    if pixel[0] >= film_floor && pixel[1] <= dense_green {
                        for (channel, value) in pixel.iter().enumerate() {
                            histograms[channel * NEGATIVE_BINS + negative_bin(*value)] += 1;
                        }
                    }
                }
                histograms
            },
        )
        .reduce(|| vec![0_u32; 3 * NEGATIVE_BINS], add_histograms);
    let mut white_density = [0.0_f32; 3];
    for channel in 0..3 {
        let dense = histogram_quantile(
            &third[channel * NEGATIVE_BINS..(channel + 1) * NEGATIVE_BINS],
            0.5,
        )
        .max(base[channel] * NEGATIVE_DENSITY_FLOOR);
        white_density[channel] = (base[channel] / dense).log10().max(0.0);
    }
    let median_density = (base[1] / median_green.max(base[1] * NEGATIVE_DENSITY_FLOOR))
        .log10()
        .max(0.0);
    let anchor = white_density[1].min(median_density + NEGATIVE_ANCHOR_REACH).max(0.05);
    NegativeMeasure { base, white_density, anchor }
}

/// Inverts a scanned colour negative by density, the way a print does.
///
/// The film base, PICKED from the border or measured as the thinnest film in
/// the frame, is divided out channel by channel — the orange mask is a density
/// the base carries everywhere, so dividing removes it exactly where subtracting
/// left a colour behind. What remains is dye density, and a print is ten to the
/// power of that times the paper's GAMMA: about 0.6 for the negative's own
/// contrast times 2.2 for the paper restores a scene with a little of the punch
/// a print has. The frame's brightest tones anchor white at 1.0, so gamma turns
/// the contrast about the highlights rather than sliding the exposure. With
/// BALANCE the three channels' densities at those tones are made to agree, which
/// is the per-channel white point a colourist sets by eye against a scope: a
/// camera scan through the orange mask reads the blue layer with a different
/// contrast from the red, and no single gain can neutralise both a wall and a
/// cloud.
///
/// Subtracting, the previous inversion, mapped one minus the transmission,
/// which is nearly flat over the shadows and saturates in the highlights; every
/// stop of scene the photographer meant to keep had then to be dragged back with
/// per-channel gains that clipped whatever they reached first.
pub(crate) fn apply_negative(image: &mut RgbImage, picked: [f32; 3], gamma: f32, balance: f32) {
    let measure = measure_negative(image, picked);
    apply_negative_measured(image, &measure, gamma, balance);
}

pub(crate) fn apply_negative_measured(
    image: &mut RgbImage,
    measure: &NegativeMeasure,
    gamma: f32,
    balance: f32,
) {
    // The channels are brought to green's white; white itself is printed where
    // the anchor says, which is at those tones unless they are a small bright
    // sky over a dark frame.
    let reference = measure.white_density[1].max(0.05);
    let anchor = measure.anchor;
    let mut spans = [0.0_f32; 3];
    let mut tables: [Vec<f32>; 3] = Default::default();
    for channel in 0..3 {
        let base = measure.base[channel];
        let white = measure.white_density[channel];
        let scale = if white > 0.05 {
            1.0 + balance * (reference / white - 1.0)
        } else {
            1.0
        };
        // Twice the base covers anything brighter than the film, which reads
        // as below black rather than as a fold in the table.
        spans[channel] = 2.0 * base;
        tables[channel] = (0..NEGATIVE_LUT_SIZE)
            .map(|index| {
                let root = index as f32 / (NEGATIVE_LUT_SIZE - 1) as f32;
                let signal = (root * root * spans[channel]).max(base * NEGATIVE_DENSITY_FLOOR);
                let density = (base / signal).log10() * scale;
                let density = if density > anchor {
                    anchor + NEGATIVE_SHOULDER * ((density - anchor) / NEGATIVE_SHOULDER).tanh()
                } else {
                    density
                };
                10.0_f32.powf(gamma * (density - anchor))
            })
            .collect();
    }
    let last = (NEGATIVE_LUT_SIZE - 1) as f32;
    image.data.par_chunks_mut(3 * 8192).for_each(|chunk| {
        for pixel in chunk.as_chunks_mut::<3>().0 {
            for channel in 0..3 {
                let position = (pixel[channel].max(0.0) / spans[channel]).sqrt().min(1.0) * last;
                let index = position as usize;
                let fraction = position - index as f32;
                let table = &tables[channel];
                let next = (index + 1).min(NEGATIVE_LUT_SIZE - 1);
                pixel[channel] = table[index] * (1.0 - fraction) + table[next] * fraction;
            }
        }
    });
}

/// The linear ratio between the size a render works at and the whole frame's.
///
/// One for an export. Anything that is measured in pixels — a blur radius, a
/// sharpening radius — has to be scaled by this or it means something different
/// in a preview than it does in the file that gets delivered.
pub(crate) fn scale_ratio(pixels: usize, full_pixels: usize) -> f32 {
    if full_pixels == 0 || pixels >= full_pixels {
        return 1.0;
    }
    (pixels as f32 / full_pixels as f32).sqrt()
}

/// Smallest sharpening radius worth running, in pixels.
///
/// A fitted preview is a third of the size it will be exported at, so a radius
/// scaled down with it lands under a pixel and the sharpening disappears —
/// which is honest, since downsampling destroys it anyway, but leaves the
/// control looking broken. Flooring the radius here shows a hint of the effect
/// at fit while the zoomed view, where sharpening is actually judged, shows
/// what the export will hold.
const MIN_SHARPEN_RADIUS: f32 = 0.6;

/// How far the sharpening reads around each pixel it writes.
pub(crate) const SHARPEN_MAX_REACH: usize = 16;

/// One separable Gaussian pass along the rows of a plane.
fn blur_rows(source: &[f32], output: &mut [f32], width: usize, height: usize, kernel: &[f32]) {
    let radius = (kernel.len() / 2) as isize;
    output
        .par_chunks_mut(width)
        .enumerate()
        .for_each(|(row, out)| {
            let base = row * width;
            for (column, value) in out.iter_mut().enumerate() {
                let mut sum = 0.0;
                for (index, weight) in kernel.iter().enumerate() {
                    let at = (column as isize + index as isize - radius)
                        .clamp(0, width as isize - 1) as usize;
                    sum += weight * source[base + at];
                }
                *value = sum;
            }
        });
    let _ = height;
}

/// Transposes a plane, so one row kernel serves both axes.
fn transpose_plane(source: &[f32], output: &mut [f32], width: usize, height: usize) {
    output
        .par_chunks_mut(height)
        .enumerate()
        .for_each(|(column, out)| {
            for (row, value) in out.iter_mut().enumerate() {
                *value = source[row * width + column];
            }
        });
}

fn gaussian_kernel(radius: f32) -> Vec<f32> {
    let extent = ((3.0 * radius).ceil() as usize).clamp(1, SHARPEN_MAX_REACH);
    let weights: Vec<f32> = (0..=2 * extent)
        .map(|index| {
            let offset = index as f32 - extent as f32;
            (-(offset * offset) / (2.0 * radius * radius)).exp()
        })
        .collect();
    let total: f32 = weights.iter().sum();
    weights.into_iter().map(|weight| weight / total).collect()
}

/// How far past the neighbourhood's own brightest and darkest value a
/// sharpened pixel may go, as a fraction of the distance between them.
///
/// An unsharp mask makes an edge steeper by overshooting on both sides of it,
/// and the overshoot is the halo: a bright line along the dark side of every
/// roof and a dark one along the sky. Holding each pixel to the range its
/// neighbourhood already spans keeps the steepening and takes the lines away;
/// a fifth of the range is left, because a little overshoot is what reads as
/// crisp and none reads as flat. The window is the blur's own radius, since
/// that is how far the overshoot reaches.
const SHARPEN_HALO_ALLOWANCE: f32 = 0.2;

/// Unsharp masking on brightness alone, with the noise floor kept out of it
/// and the halos held in.
///
/// Four things make this a photographic sharpener rather than a filter that
/// happens to raise contrast at edges.
///
/// It works on **brightness only**, and puts the result back as a gain on the
/// three channels, so hue and saturation come out untouched. Sharpening the
/// channels separately is what produces coloured fringes on every edge.
///
/// It works on the **square root of the signal**, not on linear light. A linear
/// unsharp mask spends most of its correction on the brightest pixels, which is
/// where a halo is most visible and least wanted.
///
/// The detail it adds back is **gated by how much of it is signal**, the way
/// the denoiser judges a band: the local power of the detail against the
/// noise power the flattest parts of the frame carry at that brightness.
/// Where the detail is at that floor nothing is added; where it is texture,
/// all of it is. THRESHOLD is how many times the floor to subtract, in halves:
/// two is the plain Wiener answer. The gate is a floor, not a judge of every
/// sky: after the denoiser the residue left in a sky is more than the residue
/// left in a smooth wall of the same brightness, so a sky still gains a little
/// grain from the mask — about a fifth in the finest band on an ISO 200
/// frame — and the default denoising is set with that in mind.
///
/// And the result is **held to what its neighbourhood already spans**, plus
/// `SHARPEN_HALO_ALLOWANCE` of it, so edges get steeper without growing the
/// light and dark lines a camera's sharpening draws along them.
pub(crate) fn apply_sharpen(image: &mut RgbImage, amount: f32, radius: f32, threshold: f32) {
    let (width, height) = (image.width, image.height);
    if amount <= 0.0 || width < 3 || height < 3 {
        return;
    }
    let radius = radius.max(MIN_SHARPEN_RADIUS);
    let luma: Vec<f32> = image
        .data
        .as_chunks::<3>()
        .0
        .par_iter()
        .map(|pixel| 0.2126 * pixel[0] + 0.7152 * pixel[1] + 0.0722 * pixel[2])
        .collect();
    let encoded: Vec<f32> = luma.par_iter().map(|value| value.max(0.0).sqrt()).collect();
    let kernel = gaussian_kernel(radius);
    let mut blurred = vec![0.0_f32; width * height];
    let mut turned = vec![0.0_f32; width * height];
    let mut turned_blur = vec![0.0_f32; width * height];
    blur_rows(&encoded, &mut blurred, width, height, &kernel);
    transpose_plane(&blurred, &mut turned, width, height);
    blur_rows(&turned, &mut turned_blur, height, width, &kernel);
    transpose_plane(&turned_blur, &mut blurred, height, width);
    // The detail band and how much of it is noise, judged as the denoiser
    // judges its bands: local power against the power flat areas carry.
    let mut detail = blurred;
    detail
        .par_chunks_mut(8192)
        .zip(encoded.par_chunks(8192))
        .for_each(|(detail, encoded)| {
            for (d, e) in detail.iter_mut().zip(encoded) {
                *d = e - *d;
            }
        });
    let noise = fine_band_noise_by_bin(&detail, &luma, width, height);
    let mut activity = Vec::new();
    let mut scratch = Vec::new();
    local_mean_square(&detail, &mut activity, &mut scratch, width, height);
    let noise_share = 0.5 * threshold;
    if std::env::var_os("ORFEUS_PROFILE").is_some() {
        let mut gated = 0_usize;
        let mut total_gain = 0.0_f64;
        for (index, (mean_square, luma)) in activity.iter().zip(&luma).enumerate() {
            if index % 97 != 0 {
                continue;
            }
            let gain = activity_gain(*mean_square, noise_share * noise.power[noise_bin(*luma)]);
            total_gain += f64::from(gain);
            gated += usize::from(gain == 0.0);
        }
        let sampled = activity.len() / 97 + 1;
        eprintln!(
            "orfeus-profile sharpen-gate mean-gain={:.3} fully-gated={:.1}% noise-power={:?}",
            total_gain / sampled as f64,
            100.0 * gated as f64 / sampled as f64,
            noise.power
        );
    }
    // The darkest and brightest value along each row within the blur's reach;
    // the column direction is folded into the pass below, a few rows at a time.
    let reach = (radius.ceil() as usize).clamp(1, 3);
    let (mut lowest, mut highest) = (turned, turned_blur);
    lowest
        .par_chunks_mut(width)
        .zip(highest.par_chunks_mut(width))
        .zip(encoded.par_chunks(width))
        .for_each(|((lowest, highest), row)| {
            for x in 0..width {
                let window = &row[x.saturating_sub(reach)..=(x + reach).min(width - 1)];
                lowest[x] = window.iter().copied().fold(f32::INFINITY, f32::min);
                highest[x] = window.iter().copied().fold(f32::NEG_INFINITY, f32::max);
            }
        });
    image
        .data
        .par_chunks_mut(3 * width)
        .enumerate()
        .for_each(|(y, pixels)| {
            let rows = y.saturating_sub(reach)..=(y + reach).min(height - 1);
            for (x, pixel) in pixels.as_chunks_mut::<3>().0.iter_mut().enumerate() {
                let index = y * width + x;
                let luma = luma[index];
                if luma <= 1.0e-5 {
                    continue;
                }
                let encoded = encoded[index];
                let detail = detail[index]
                    * activity_gain(activity[index], noise_share * noise.power[noise_bin(luma)]);
                let (mut low, mut high) = (f32::INFINITY, f32::NEG_INFINITY);
                for row in rows.clone() {
                    low = low.min(lowest[row * width + x]);
                    high = high.max(highest[row * width + x]);
                }
                let slack = SHARPEN_HALO_ALLOWANCE * (high - low);
                let sharpened = (encoded + amount * detail)
                    .clamp(low - slack, high + slack)
                    .max(0.0);
                let gain = (sharpened * sharpened) / luma;
                for channel in pixel.iter_mut() {
                    *channel *= gain;
                }
            }
        });
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

pub(crate) fn splitmix64(mut value: u64) -> u64 {
    value = value.wrapping_add(0x9e37_79b9_7f4a_7c15);
    value = (value ^ (value >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    value = (value ^ (value >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    value ^ (value >> 31)
}

/// Lays film grain down, seeded from where each pixel sits in the whole frame.
///
/// ORIGIN is the image's top-left corner within the frame it is part of, which
/// is zero for a whole-frame render and the window's corner for a render of
/// part of one. It has to be threaded through rather than assumed: the grain is
/// seeded from its coordinates, so a window that counted from its own corner
/// would get a different pattern from the frame it belongs to — the grain would
/// crawl as the view was panned, and a preview would not match its export.
pub(crate) fn apply_grain(
    image: &mut RgbImage,
    amount: f32,
    size: f32,
    seed: u64,
    origin: (usize, usize),
) {
    if amount == 0.0 {
        return;
    }
    let width = image.width;
    image
        .data
        .par_chunks_mut(width * 3)
        .enumerate()
        .for_each(|(y, row)| {
            let y = y + origin.1;
            let gy = (y as f32 / size).floor() as u64;
            let row_seed = seed ^ gy.wrapping_mul(0x6eed_0e9d_a4d9_4a4f);
            for (x, pixel) in row.as_chunks_mut::<3>().0.iter_mut().enumerate() {
                let gx = ((x + origin.0) as f32 / size).floor() as u64;
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
            let segments: Vec<(u8, Vec<u8>)> = SRGB_ICC
                .chunks(65_519)
                .enumerate()
                .map(|(chunk_index, chunk)| {
                    let chunk_count = SRGB_ICC.len().div_ceil(65_519);
                    let mut segment = Vec::with_capacity(chunk.len() + 14);
                    segment.extend_from_slice(b"ICC_PROFILE\0");
                    segment.push(chunk_index as u8 + 1);
                    segment.push(chunk_count as u8);
                    segment.extend_from_slice(chunk);
                    (2, segment)
                })
                .collect();
            let encoded = super::jpeg::encode_rgb(&bytes, image.width, image.height, quality as u8, &segments)
                .map_err(Error::Render)?;
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
    /// Pixels the frame has at full resolution, whatever size this decode is.
    ///
    /// A draft bins the sensor and a preview downscales again, and the noise
    /// reduction needs to know how far from the real thing it is working.
    pub(crate) full_pixels: usize,
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
    let full_pixels = roi.d.w * roi.d.h;
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
        full_pixels,
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
    let full_pixels = decoded.full_pixels;
    let (make, model, lens_name, focal) = (
        decoded.make.clone(),
        decoded.model.clone(),
        effective_lens_name(&decoded.lens_name, named_lens).to_string(),
        effective_focal(decoded.focal, settings.focal_length),
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
    // By hand first, then the profile — the same order the graph applies them.
    // Colour fringing is measured from the frame unless the profile was asked
    // for by name.
    let mut flags = settings.flags;
    let colour = if flags & FLAG_LENS_TCA != 0 && settings.chromatic_aberration_source == 0 {
        flags &= !FLAG_LENS_TCA;
        measure_lateral_colour(&image).unwrap_or_default()
    } else {
        LateralColour::default()
    };
    apply_radial_corrections(&mut image, settings.lens_distortion, colour);
    apply_lens(
        &mut image,
        &LensCorrectionOptions {
            make: &make,
            model: &model,
            lens_name: &lens_name,
            focal,
            flags,
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
        let pixels = image.width * image.height;
        apply_noise_reduction(
            &mut image,
            strength_for_scale(settings.luma_noise_reduction, pixels, full_pixels),
            strength_for_scale(settings.chroma_noise_reduction, pixels, full_pixels),
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
        false,
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
        (0, 0),
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
        // Swept, not one level. A denoiser is only judged by how much noise it
        // removes if it is also judged by what it does to a frame that had
        // little to remove — and a single high noise level is exactly the test
        // a fixed threshold passes while ruining base-ISO detail.
        for scale in [0.0_f32, 0.02, 0.045, 0.09] {
            let noisy = add_sensor_noise(&clean, scale);
            let (luma_sigma, chroma_sigma) = flat_field_noise(&noisy, width, height);
            eprintln!(
                "noise-quality noise={scale:.3} {:14} psnr={:.2} dB  luma sigma={luma_sigma:.4}  chroma sigma={chroma_sigma:.4}  edge={:.3}  texture={:.3}",
                "before",
                peak_signal_to_noise(&clean, &noisy),
                edge_height(&noisy, width, height),
                texture_amplitude(&noisy, width, height) / texture_amplitude(&clean, width, height)
            );
            for (label, luma, chroma) in
                [("slider 0.35", 0.35, 0.35), ("slider 1.00", 1.0, 1.0)]
            {
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
                    "noise-quality noise={scale:.3} {label:14} psnr={:.2} dB  luma sigma={luma_sigma:.4}  chroma sigma={chroma_sigma:.4}  edge={:.3}  texture={:.3}  {milliseconds:.0} ms",
                    peak_signal_to_noise(&clean, &image.data),
                    edge_height(&image.data, width, height),
                    texture_amplitude(&image.data, width, height)
                        / texture_amplitude(&clean, width, height)
                );
            }
        }
    }

    /// The amplitude of the fine checker in the scene's texture band.
    ///
    /// Detail at the scale noise lives at, which is the detail a denoiser is
    /// tempted to spend and the reason this benchmark exists. Reported as a
    /// fraction of what the clean scene had: one is every bit of it kept, zero
    /// is a frame wiped smooth.
    fn texture_amplitude(data: &[f32], width: usize, height: usize) -> f32 {
        let mut total = 0.0_f64;
        let mut count = 0_usize;
        for y in (height * 3 / 4 + 8)..(height - 8) {
            for x in 8..(width - 8) {
                let at = |x: usize, y: usize| data[(y * width + x) * 3 + 1] as f64;
                // A Laplacian of the checker's own period: maximal on the
                // checker, zero on anything smoother.
                let local = at(x, y) * 4.0 - at(x - 2, y) - at(x + 2, y) - at(x, y - 2)
                    - at(x, y + 2);
                total += local * local;
                count += 1;
            }
        }
        (total / count.max(1) as f64).sqrt() as f32
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
            full_pixels: 0,
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

    fn flat_image(width: usize, height: usize, pixel: [f32; 3]) -> RgbImage {
        RgbImage {
            width,
            height,
            data: pixel.repeat(width * height),
        }
    }

    /// A mirror is a permutation: nothing resampled, nothing lost, and doing
    /// it twice is doing nothing.
    /// Straight lines that were bent come back straight, and the centre never
    /// moves.
    #[test]
    fn manual_distortion_straightens_and_holds_the_centre() {
        let (width, height) = (161_usize, 121_usize);
        // A grid of thin lines, which is what anybody sets this slider against.
        let mut data = vec![0.0_f32; width * height * 3];
        for y in 0..height {
            for x in 0..width {
                let line = x % 40 == 0 || y % 40 == 0;
                let value = if line { 1.0 } else { 0.0 };
                for channel in 0..3 {
                    data[(y * width + x) * 3 + channel] = value;
                }
            }
        }
        let straight = RgbImage {
            width,
            height,
            data,
        };
        let at = |image: &RgbImage, x: usize, y: usize| image.data[(y * width + x) * 3];
        // Zero is exactly nothing.
        let mut untouched = straight.clone();
        apply_radial_corrections(&mut untouched, 0.0, LateralColour::default());
        assert_eq!(untouched.data, straight.data);
        for amount in [0.15_f32, -0.15] {
            let mut bent = straight.clone();
            apply_radial_corrections(&mut bent, amount, LateralColour::default());
            // The centre pixel is the fixed point of a radial map, whichever
            // way it bends.
            assert!(
                (at(&bent, width / 2, height / 2) - at(&straight, width / 2, height / 2)).abs()
                    < 1.0e-4,
                "the centre moved at {amount}"
            );
            // A radial map takes a symmetric picture to a symmetric one. This
            // is the property worth pinning: it fails the moment the centre is
            // taken as `width / 2` rather than `(width - 1) / 2`, or the radius
            // is normalised per axis instead of by the diagonal — both of which
            // bend the frame in a way no lens does.
            let worst = (0..height)
                .flat_map(|y| (0..width).map(move |x| (x, y)))
                .map(|(x, y)| {
                    (at(&bent, x, y) - at(&bent, width - 1 - x, height - 1 - y)).abs()
                })
                .fold(0.0_f32, f32::max);
            // A ten-thousandth, not a millionth: the samples either side of a
            // hard black-to-white edge are averaged with weights that round
            // differently on the two halves of the frame. A real asymmetry —
            // an off-by-one centre, a per-axis radius — shows up hundreds of
            // times larger than this.
            assert!(worst < 1.0e-4, "{amount} was not radial: {worst}");
            // And it did something.
            let moved = (0..height)
                .flat_map(|y| (0..width).map(move |x| (x, y)))
                .map(|(x, y)| (at(&bent, x, y) - at(&straight, x, y)).abs())
                .fold(0.0_f32, f32::max);
            assert!(moved > 0.25, "{amount} changed nothing: {moved}");
        }
    }

    #[test]
    fn flipping_mirrors_exactly_and_undoes_itself() {
        for (width, height) in [(7_usize, 4_usize), (4, 7), (8, 8), (1, 5), (5, 1)] {
            let original = RgbImage {
                width,
                height,
                data: (0..width * height)
                    .flat_map(|index| {
                        let value = index as f32;
                        [value, value + 0.5, value + 0.25]
                    })
                    .collect(),
            };
            let at = |image: &RgbImage, x: usize, y: usize| {
                let start = (y * width + x) * 3;
                [image.data[start], image.data[start + 1], image.data[start + 2]]
            };
            for (horizontal, vertical) in
                [(true, false), (false, true), (true, true)]
            {
                let mut image = original.clone();
                apply_flip(&mut image, horizontal, vertical);
                assert_eq!((image.width, image.height), (width, height));
                for y in 0..height {
                    for x in 0..width {
                        let source_x = if horizontal { width - 1 - x } else { x };
                        let source_y = if vertical { height - 1 - y } else { y };
                        assert_eq!(
                            at(&image, x, y),
                            at(&original, source_x, source_y),
                            "{width}x{height} h={horizontal} v={vertical} at {x},{y}"
                        );
                    }
                }
                apply_flip(&mut image, horizontal, vertical);
                assert_eq!(image.data, original.data, "flipping twice changed the image");
            }
            let mut untouched = original.clone();
            apply_flip(&mut untouched, false, false);
            assert_eq!(untouched.data, original.data);
        }
    }

    #[test]
    fn contrast_pivots_without_clipping_and_composes() {
        let pivot = 0.435_f32;
        let linear_pivot = srgb_decode(pivot);
        // The pivot is the fixed point, which is the whole promise of the
        // control: raising contrast must not shift the exposure.
        let mut image = flat_image(8, 8, [linear_pivot; 3]);
        apply_contrast(&mut image, 2.0, pivot);
        for value in &image.data {
            assert!(
                (*value - linear_pivot).abs() < 1.0e-5,
                "the pivot moved to {value}"
            );
        }
        // Identity, and no clipping either side of it.
        let ramp: Vec<f32> = (0..64).map(|index| index as f32 / 16.0).collect();
        for slope in [0.5_f32, 1.0, 2.0, 4.0] {
            let mut image = RgbImage {
                width: ramp.len(),
                height: 1,
                data: ramp.iter().flat_map(|value| [*value; 3]).collect(),
            };
            apply_contrast(&mut image, slope, pivot);
            for (before, after) in ramp.iter().zip(image.data.as_chunks::<3>().0) {
                assert!(after[0].is_finite() && after[0] >= 0.0, "{slope} produced {after:?}");
                if slope == 1.0 {
                    assert!((after[0] - before).abs() < 1.0e-6, "unity changed a value");
                } else if *before > linear_pivot {
                    assert_eq!(
                        after[0] > *before,
                        slope > 1.0,
                        "highlights moved the wrong way at slope {slope}"
                    );
                }
            }
        }
        // A slope in a logarithm composes by multiplication, which is what
        // makes two nudges of the slider behave like one bigger nudge.
        let start = flat_image(4, 4, [0.6, 0.3, 0.1]);
        let mut twice = start.clone();
        apply_contrast(&mut twice, 1.5, pivot);
        apply_contrast(&mut twice, 1.5, pivot);
        let mut once = start;
        apply_contrast(&mut once, 2.25, pivot);
        for (a, b) in twice.data.iter().zip(&once.data) {
            assert!((a - b).abs() < 1.0e-5, "{a} is not {b}");
        }
    }

    #[test]
    fn sharpening_steepens_an_edge_and_keeps_colour() {
        let (width, height) = (64, 32);
        let mut data = Vec::with_capacity(width * height * 3);
        for _ in 0..height {
            for x in 0..width {
                // A step with a soft transition, tinted so the hue can be
                // checked, at a level where the encoding has some slope.
                let level = if x < width / 2 { 0.18 } else { 0.42 };
                data.extend_from_slice(&[level, level * 0.8, level * 0.55]);
            }
        }
        let original = RgbImage {
            width,
            height,
            data,
        };
        let mut image = original.clone();
        apply_sharpen(&mut image, 0.0, 1.0, 0.0);
        assert_eq!(image.data, original.data, "no amount still changed the image");
        let mut image = original.clone();
        apply_sharpen(&mut image, 1.5, 1.2, 0.0);
        let at = |image: &RgbImage, x: usize| {
            let index = ((height / 2) * width + x) * 3;
            [image.data[index], image.data[index + 1], image.data[index + 2]]
        };
        // The dark side of the edge goes darker and the light side lighter:
        // an overshoot, which is what sharpening is.
        let middle = width / 2;
        assert!(
            at(&image, middle - 1)[0] < at(&original, middle - 1)[0],
            "the dark side of the edge did not darken"
        );
        assert!(
            at(&image, middle)[0] > at(&original, middle)[0],
            "the light side of the edge did not lighten"
        );
        // Far from the edge nothing happens, and everywhere the hue is intact.
        for x in [2_usize, width - 3] {
            let before = at(&original, x);
            let after = at(&image, x);
            assert!(
                (after[0] - before[0]).abs() < 1.0e-4,
                "a flat area moved at column {x}"
            );
        }
        for x in 0..width {
            let before = at(&original, x);
            let after = at(&image, x);
            let before_ratio = before[1] / before[0];
            let after_ratio = after[1] / after[0];
            assert!(
                (before_ratio - after_ratio).abs() < 1.0e-4,
                "column {x} changed hue: {before:?} became {after:?}"
            );
        }
    }

    /// A sharpened edge gets steeper, and gets no lines drawn along it.
    #[test]
    fn sharpening_steepens_an_edge_without_a_halo() {
        let (width, height) = (64, 16);
        let mut data = Vec::with_capacity(width * height * 3);
        for _ in 0..height {
            for x in 0..width {
                // A soft step: three pixels from dark to bright.
                let value = 0.2 + 0.5 * ((x as f32 - 30.0) / 3.0).clamp(0.0, 1.0);
                data.extend_from_slice(&[value, value, value]);
            }
        }
        let original = RgbImage { width, height, data };
        let mut sharpened = original.clone();
        apply_sharpen(&mut sharpened, 2.0, 1.5, 0.0);
        let row = |image: &RgbImage, x: usize| image.data[(8 * width + x) * 3 + 1];
        let steepest = |image: &RgbImage| {
            (1..width).map(|x| (row(image, x) - row(image, x - 1)).abs()).fold(0.0_f32, f32::max)
        };
        assert!(steepest(&sharpened) > steepest(&original) * 1.3, "the edge did not get steeper");
        let (brightest, darkest) = sharpened.data.iter().fold((0.0_f32, 1.0_f32), |(hi, lo), v| {
            (hi.max(*v), lo.min(*v))
        });
        // Unheld, an amount of two overshoots to about 1.0 and undershoots
        // below 0.1 on this edge. Held, the overshoot is a fifth of the step in
        // the square-root domain, which is under 0.09 of linear light here.
        assert!(brightest < 0.79 && darkest > 0.13, "halo: {darkest}..{brightest}");
    }

    #[test]
    fn sharpening_leaves_the_noise_floor_where_it_is() {
        let (width, height) = (96, 64);
        let mut data = Vec::with_capacity(width * height * 3);
        for y in 0..height {
            for x in 0..width {
                let bits = splitmix64((y as u64) << 32 | x as u64);
                let value = 0.3 + ((bits >> 40) as f32 / 16_777_215.0 - 0.5) * 0.03;
                data.extend_from_slice(&[value, value, value]);
            }
        }
        let noisy = RgbImage {
            width,
            height,
            data,
        };
        let deviation = |image: &RgbImage| {
            let mean: f32 = image.data.iter().sum::<f32>() / image.data.len() as f32;
            (image.data.iter().map(|v| (v - mean) * (v - mean)).sum::<f32>()
                / image.data.len() as f32)
                .sqrt()
        };
        let before = deviation(&noisy);
        let mut uncored = noisy.clone();
        apply_sharpen(&mut uncored, 1.5, 1.0, 0.0);
        let mut cored = noisy;
        apply_sharpen(&mut cored, 1.5, 1.0, 3.0);
        let uncored = deviation(&uncored);
        let cored = deviation(&cored);
        assert!(
            uncored > before * 1.2,
            "with no floor, sharpening should have amplified the grain: \
             {before} became {uncored}"
        );
        assert!(
            cored < before * 1.05,
            "with a floor, the grain should have been left alone: \
             {before} became {cored}"
        );
    }

    /// A plane of nothing but noise, at one brightness.
    fn noisy_flat_plane(width: usize, height: usize, level: f32, sigma: f32) -> Vec<f32> {
        let mut state = 0x9E37_79B9_7F4A_7C15_u64;
        let mut next = || {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            (state >> 40) as f32 / 16_777_216.0 - 0.5
        };
        (0..width * height)
            // Four uniform draws: close enough to Gaussian, variance 4/12.
            .map(|_| level + sigma * (next() + next() + next() + next()) / (1.0_f32 / 3.0).sqrt())
            .collect()
    }

    /// The tiled split has to give the bands the three whole-plane blurs give,
    /// tile seams, frame edges and odd sizes included.
    #[test]
    fn tiled_band_split_matches_the_plain_blurs() {
        for (width, height) in [(613, 389), (61, 71), (300, 64), (256, 128), (1030, 70)] {
            let mut scene: Vec<f32> = noisy_flat_plane(width, height, 0.4, 0.05);
            // Edges and a ramp, so the guide has structure to follow.
            for y in 0..height {
                for x in 0..width {
                    let ramp = x as f32 / width as f32 * 0.3;
                    let edge = if (x / 37 + y / 23) % 2 == 0 { 0.2 } else { 0.0 };
                    scene[y * width + x] += ramp + edge;
                }
            }
            let mut expected_fine = scene.clone();
            let (mut scratch, mut one, mut two, mut three) = (Vec::new(), Vec::new(), Vec::new(), Vec::new());
            edge_guided_blur_into(&scene, &scene, width, height, 1, 1.0, &mut scratch, &mut one);
            edge_guided_blur_into(&one, &scene, width, height, 2, 1.0, &mut scratch, &mut two);
            edge_guided_blur_into(&two, &scene, width, height, 4, 1.0, &mut scratch, &mut three);
            for index in 0..width * height {
                expected_fine[index] -= one[index];
                one[index] -= two[index];
                two[index] -= three[index];
            }
            let mut plane = scene.clone();
            let mut residual = Vec::new();
            let (middle, coarse) = split_luma_bands(&mut plane, &mut residual, width, height);
            let worst = |a: &[f32], b: &[f32]| {
                a.iter().zip(b).map(|(a, b)| (a - b).abs()).fold(0.0_f32, f32::max)
            };
            let differences = [
                worst(&plane, &expected_fine),
                worst(&middle, &one),
                worst(&coarse, &two),
                worst(&residual, &three),
            ];
            assert!(
                differences.iter().all(|d| *d < 2e-5),
                "{width}x{height}: bands differ by {differences:?}"
            );
        }
    }

    /// Each band's noise is read off the band itself, so the reading has to
    /// agree with the power the noise actually put there.
    #[test]
    fn band_noise_is_measured_within_a_fifth() {
        let (width, height) = (512, 384);
        let mut plane = noisy_flat_plane(width, height, 0.4, 0.02);
        let mut residual = Vec::new();
        let (middle, coarse) = split_luma_bands(&mut plane, &mut residual, width, height);
        let bin = noise_bin(0.4);
        let truth = |band: &[f32]| band.iter().map(|v| v * v).sum::<f32>() / band.len() as f32;
        let (fine, flat) = fine_band_noise(&plane, &residual, width, height);
        let readings = [
            (fine.power[bin], truth(&plane)),
            (coarse_band_noise(&middle, &residual, width, height, &flat).power[bin], truth(&middle)),
            (coarse_band_noise(&coarse, &residual, width, height, &flat).power[bin], truth(&coarse)),
        ];
        for (index, (measured, actual)) in readings.iter().enumerate() {
            let ratio = measured / actual;
            assert!(
                (0.8..=1.2).contains(&ratio),
                "band {index}: measured {measured} against actual {actual}, ratio {ratio}"
            );
        }
    }

    /// The rolling window of the shrinkage pass has to agree with the plain
    /// window it replaced, at every pixel including the edges.
    #[test]
    fn rolling_activity_matches_the_plain_window() {
        let (width, height) = (61, 71);
        let band = noisy_flat_plane(width, height, 0.5, 0.05);
        let mut activity = Vec::new();
        let mut scratch = Vec::new();
        local_mean_square(&band, &mut activity, &mut scratch, width, height);
        for index in (0..width * height).step_by(7) {
            let direct = window_mean_square(&band, width, height, index);
            assert!(
                (direct - activity[index]).abs() < 1e-6,
                "pixel {index}: window {direct} against running {}",
                activity[index]
            );
        }
        // And the fused shrinkage equals shrinking with the plane of activity.
        let zeros = vec![0.0_f32; width * height];
        let noise = BandNoise { power: [0.05 * 0.05; NOISE_BINS] };
        let mut fused = vec![0.4_f32; width * height];
        shrink_luma_bands(&mut fused, &band, &zeros, &zeros, [noise; 3], 1.0, width, height);
        for index in 0..width * height {
            let expected = 0.4 + band[index] * activity_gain(activity[index], noise.power[noise_bin(0.4)]);
            assert!(
                (fused[index] - expected).abs() < 1e-5,
                "pixel {index}: fused {} against {expected}",
                fused[index]
            );
        }
    }

    /// A frame with no noise has nothing to lose, whatever the slider says.
    ///
    /// The scene's one texture sits alone in its brightness bin, which is the
    /// case a per-bin noise reading gets wrong: it reads the texture as noise.
    /// The noise envelope across brightness is what keeps it.
    #[test]
    fn a_frame_with_no_noise_keeps_its_texture() {
        let (width, height) = (800, 600);
        let clean = denoise_test_scene(width, height);
        for strength in [0.35_f32, 1.0] {
            let mut image = RgbImage {
                width,
                height,
                data: clean.clone(),
            };
            cpu_noise_reduction(&mut image, strength, strength);
            let kept = texture_amplitude(&image.data, width, height)
                / texture_amplitude(&clean, width, height);
            assert!(kept > 0.98, "slider {strength} kept only {kept} of a clean frame's texture");
        }
    }

    #[test]
    fn noise_reduction_materially_smooths_a_flat_noisy_patch() {
        let mut original = RgbImage {
            width: 32,
            height: 24,
            data: Vec::new(),
        };
        // Real noise, from a hash that mixes. The first version of this walked
        // `(x * 73 + y * 151 + x * y * 19) % 101`, which steps by a constant
        // along each row and so is locally a straight ramp — affine, with a
        // Laplacian of exactly zero. It looked like noise and was not, and a
        // denoiser that measures the frame it is given correctly declined to
        // touch it.
        for y in 0..original.height {
            for x in 0..original.width {
                let bits = splitmix64((y as u64) << 32 | x as u64);
                let hash = (bits >> 40) as f32 / 16_777_215.0;
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
        // Stronger, but only a little. Subtracting the noise power that is
        // actually there is most of what can be done; the top of the slider
        // subtracts three times it and finds little left to take. That
        // flattening is the point of measuring rather than thresholding, so the
        // test asks for an improvement rather than for a fixed fraction.
        assert!(
            maximum_rms < moderate_rms * 0.95,
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
        apply_tonal_equalizer(&mut image, [0.0; 7], false);
        assert_eq!(image.data, before);
    }

    /// The scene-linear luminance that will be displayed at TARGET.
    fn linear_for_display(target: f32) -> f32 {
        let (mut low, mut high) = (0.0_f32, 64.0_f32);
        for _ in 0..60 {
            let middle = 0.5 * (low + high);
            if srgb_encode(super::super::tone::default_display_tone(middle)) < target {
                low = middle;
            } else {
                high = middle;
            }
        }
        0.5 * (low + high)
    }

    /// Each slider owns a sixth of displayed brightness, and moving one by a
    /// stop doubles exactly the tones it is named for.
    ///
    /// The anchors are derived from the tone curve rather than written down,
    /// because that is the claim: the zones are wherever a sixth of what the
    /// eye sees falls, not at fixed distances from middle grey.
    #[test]
    fn tonal_equalizer_applies_each_anchor_gain() {
        let anchors: Vec<f32> = (0..7)
            .map(|index| linear_for_display(index as f32 / 6.0))
            .collect();
        // Spread across the displayed range rather than bunched at the top: the
        // old anchors put four of the seven past 95% display.
        // The middle slider must land on a tone that reads as middle: about
        // half way up the screen, which is a long way under middle grey in
        // scene-linear terms once the tone curve has lifted it.
        assert!(
            anchors[3] > 0.0 && anchors[3] < 0.18,
            "the middle zone sits at {}, which is not a midtone",
            anchors[3]
        );
        for (index, luminance) in anchors.into_iter().enumerate() {
            if luminance <= 0.0 {
                continue;
            }
            let mut adjustments = [0.0; 7];
            adjustments[index] = 1.0;
            let mut image = RgbImage {
                width: 1,
                height: 1,
                data: vec![luminance; 3],
            };
            apply_tonal_equalizer(&mut image, adjustments, false);
            for value in image.data {
                assert!((value - luminance * 2.0).abs() < luminance * 1.0e-5 + 1.0e-7);
            }
        }
    }

    #[test]
    fn tonal_equalizer_is_smooth_and_preserves_chromaticity() {
        let adjustments = [-1.0, -0.5, 0.0, 0.5, 1.0, 0.25, -0.25];
        // Swept rather than sampled either side of one boundary. The zone
        // position is read through the display transfer's lookup table, whose
        // steps are the size of a two-point check's tolerance — so a sweep that
        // bounds every step at once is both stricter about a real discontinuity
        // and honest about the table.
        let mut previous = tone_adjustment_ev(1.0e-4, &adjustments, false);
        let mut largest = 0.0_f32;
        for step in 1..4000 {
            let luminance = 1.0e-4 * (12.0_f32).powf(step as f32 / 1000.0);
            let value = tone_adjustment_ev(luminance, &adjustments, false);
            largest = largest.max((value - previous).abs());
            previous = value;
        }
        assert!(
            largest < 0.02,
            "the zone blend jumped by {largest} between neighbouring tones"
        );

        let mut image = RgbImage {
            width: 1,
            height: 1,
            data: vec![0.09, 0.18, 0.36],
        };
        let before = image.data.clone();
        apply_tonal_equalizer(&mut image, adjustments, false);
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

    fn lens_options<'a>(lens_name: &'a str, focal: f32, explicit: Option<&'a str>) -> LensCorrectionOptions<'a> {
        LensCorrectionOptions {
            make: "OM Digital Solutions",
            model: "OM-1",
            lens_name,
            focal,
            flags: FLAG_LENS_DISTORTION | FLAG_LENS_TCA,
            strength: 1.0,
            explicit_profile: explicit,
            focal_reducer: 1.0,
            crop_factor: 0.0,
        }
    }

    /// The description names the profile a render would use and what it can
    /// correct, and says no the same way a render does.
    /// A full-frame lens on a Four Thirds body is corrected for the part of
    /// its image circle the sensor sees: a few pixels at most for this lens,
    /// whose moustache distortion nearly vanishes within that crop. Treating
    /// the frame as full-frame put the lens's corner correction on the sensor's
    /// corner instead and bent straight lines into pincushion.
    #[test]
    fn full_frame_profile_is_scaled_to_the_body_that_wears_it() {
        let db = lens_database().unwrap();
        let nokton = "Voigtlander Nokton 28mm F1.5 Aspherical";
        let lens = find_explicit_lens_profile(db, nokton, 28.0).unwrap();
        assert_eq!(lens.crop_factor, 1.0);
        let corner_shift = |crop: f32| {
            let mut modifier = Modifier::new(lens, 28.0, crop, 5184, 3888, false);
            assert!(modifier.enable_distortion_correction(lens));
            let mut source = [0.0_f32; 2];
            modifier.apply_geometry_distortion(0.0, 0.0, 1, 1, &mut source);
            source[0].hypot(source[1])
        };
        assert!(corner_shift(2.0) < 10.0, "{}", corner_shift(2.0));
        assert!(corner_shift(1.0) > 25.0, "{}", corner_shift(1.0));

        // The body the database knows decides the crop, whatever an
        // adapted-lens mapping stored; the stored figure serves only a body
        // the database does not know.
        let mut options = lens_options("", 28.0, Some(nokton));
        options.crop_factor = 1.0;
        assert_eq!(resolve_lens_profile(&options).unwrap().base_crop_factor, 2.0);
        options.make = "Nobody";
        options.model = "Unknown";
        assert_eq!(resolve_lens_profile(&options).unwrap().base_crop_factor, 1.0);
        options.crop_factor = 0.0;
        assert!(matches!(
            resolve_lens_profile(&options),
            Err(Error::LensProfileUnavailable(message)) if message.contains("requires a crop factor")
        ));
        // The description reports the crop the correction would use, which
        // is what the picker records for the next photograph on the lens.
        options.make = "OM Digital Solutions";
        options.model = "OM-1";
        options.crop_factor = 1.0;
        let described = describe_lens_match(&options).unwrap();
        assert_eq!(described.split('\t').last().unwrap(), "2.000");
    }

    /// A neutral six-stop ramp exposed onto film of contrast 0.6 behind an
    /// orange base, then inverted by density with the reciprocal gamma.
    fn film_ramp(base: [f32; 3], contrast: [f32; 3]) -> RgbImage {
        let width = 512;
        let height = 4;
        let mut data = Vec::with_capacity(width * height * 3);
        for _ in 0..height {
            for x in 0..width {
                let exposure = ramp_exposure(x, width);
                for channel in 0..3 {
                    data.push(base[channel] * exposure.powf(-contrast[channel]));
                }
            }
        }
        RgbImage { width, height, data }
    }

    fn ramp_exposure(x: usize, width: usize) -> f32 {
        1.0 + 63.0 * x as f32 / (width - 1) as f32
    }

    #[test]
    fn negative_inversion_recovers_the_scene_and_anchors_white() {
        let base = [0.5, 0.3, 0.2];
        let mut image = film_ramp(base, [0.6; 3]);
        apply_negative(&mut image, base, 1.0 / 0.6, 0.0);
        let pixel = |x: usize| &image.data[x * 3..x * 3 + 3];
        // Scene-linear again, up to one scale, across the ramp below white.
        let scale = pixel(64)[1] / ramp_exposure(64, 512);
        for x in (8..480).step_by(16) {
            let expected = scale * ramp_exposure(x, 512);
            for channel in 0..3 {
                let value = pixel(x)[channel];
                assert!(
                    (value / expected - 1.0).abs() < 0.02,
                    "x {x} channel {channel}: {value} against {expected}"
                );
            }
        }
        // The brightest tones sit at one; the base comes out dark.
        assert!((pixel(505)[1] - 1.0).abs() < 0.08, "{}", pixel(505)[1]);
        assert!(pixel(0)[1] < 0.03, "{}", pixel(0)[1]);
    }

    #[test]
    fn negative_balance_makes_unequal_layers_agree_at_white() {
        let base = [0.5, 0.3, 0.2];
        // The blue layer reads with nearly twice the contrast of the red.
        let contrast = [0.55, 0.65, 1.0];
        let mut plain = film_ramp(base, contrast);
        apply_negative(&mut plain, base, 2.2, 0.0);
        let mut balanced = film_ramp(base, contrast);
        apply_negative(&mut balanced, base, 2.2, 1.0);
        let at = |image: &RgbImage, x: usize| [image.data[x * 3], image.data[x * 3 + 1], image.data[x * 3 + 2]];
        let bright = at(&balanced, 470);
        assert!((bright[0] / bright[1] - 1.0).abs() < 0.05, "{bright:?}");
        assert!((bright[2] / bright[1] - 1.0).abs() < 0.05, "{bright:?}");
        let mid = at(&balanced, 256);
        assert!((mid[0] / mid[1] - 1.0).abs() < 0.05, "{mid:?}");
        assert!((mid[2] / mid[1] - 1.0).abs() < 0.05, "{mid:?}");
        // Left alone, the layers disagree by the whole of their contrast gap.
        let unbalanced = at(&plain, 256);
        assert!(unbalanced[0] / unbalanced[1] < 0.8, "{unbalanced:?}");
    }

    /// A strip of the holder inside the crop is not film: it is left out of the
    /// white, so the picture keeps the exposure the film gave it.
    #[test]
    fn negative_ignores_a_holder_edge_when_anchoring_white() {
        let base = [0.5, 0.3, 0.2];
        let mut clean = film_ramp(base, [0.6; 3]);
        let mut framed = clean.clone();
        // The holder: two percent of the frame, black in every channel.
        for y in 0..framed.height {
            for x in 0..10 {
                let offset = (y * framed.width + x) * 3;
                framed.data[offset..offset + 3].copy_from_slice(&[0.001, 0.0005, 0.0003]);
            }
        }
        apply_negative(&mut clean, base, 2.2, 1.0);
        apply_negative(&mut framed, base, 2.2, 1.0);
        for x in (16..500).step_by(32) {
            let offset = x * 3 + 1;
            assert!(
                (framed.data[offset] / clean.data[offset] - 1.0).abs() < 0.03,
                "x {x}: {} against {}",
                framed.data[offset],
                clean.data[offset]
            );
        }
        // The holder itself prints as paper white, the way a print shows it.
        assert!(framed.data[1] > 1.0);
    }

    /// A dark wood under a small bright sky: the sky is the brightest percent,
    /// but the white is not anchored on it alone, so the wood stays visible.
    #[test]
    fn negative_anchors_white_within_reach_of_the_median() {
        let base = [0.5, 0.3, 0.2];
        // Everything a stop or two above black, then two percent of the frame
        // seven stops up.
        let width = 500;
        let height = 4;
        let mut data = Vec::with_capacity(width * height * 3);
        for _ in 0..height {
            for x in 0..width {
                let exposure = if x >= 490 { 128.0 } else { 2.0 + 2.0 * x as f32 / 489.0 };
                for channel in 0..3 {
                    data.push(base[channel] * exposure.powf(-0.6));
                }
            }
        }
        let mut image = RgbImage { width, height, data };
        let measure = measure_negative(&image, base);
        // The brightest percent is the sky; the anchor stops half a density
        // above the median, about a stop of negative above the wood.
        assert!(measure.white_density[1] > 1.2, "{measure:?}");
        assert!((measure.anchor - (0.6 * 3.0_f32.log10() + 0.5)).abs() < 0.05, "{measure:?}");
        apply_negative(&mut image, base, 2.2, 1.0);
        let wood = image.data[250 * 3 + 1];
        assert!(wood > 0.05 && wood < 0.3, "{wood}");
        assert!(image.data[495 * 3 + 1] > 1.0);
    }

    #[test]
    fn negative_finds_its_own_base_in_the_border() {
        let base = [0.5, 0.3, 0.2];
        let mut framed = film_ramp(base, [0.6; 3]);
        // A border of unexposed film along one edge: two of the four rows'
        // first sixteen pixels, well over half a percent of the frame.
        for y in 0..2 {
            for x in 0..16 {
                let offset = (y * framed.width + x) * 3;
                framed.data[offset..offset + 3].copy_from_slice(&base);
            }
        }
        let measured = measure_negative(&framed, [0.0; 3]);
        for channel in 0..3 {
            assert!(
                (measured.base[channel] / base[channel] - 1.0).abs() < 0.02,
                "{:?}",
                measured.base
            );
        }
        let mut explicit = framed.clone();
        apply_negative(&mut explicit, base, 2.2, 1.0);
        apply_negative(&mut framed, [0.0; 3], 2.2, 1.0);
        let worst = framed
            .data
            .iter()
            .zip(&explicit.data)
            .map(|(a, b)| (a - b).abs())
            .fold(0.0_f32, f32::max);
        assert!(worst < 0.02, "{worst}");
    }

    #[test]
    fn lens_match_describes_the_profile_a_render_would_use() {
        let described = describe_lens_match(&lens_options(
            "Leica DG Macro-Elmarit 45mm F2.8 Asph. Mega OIS",
            45.0,
            None,
        ))
        .unwrap();
        let fields: Vec<&str> = described.split('\t').collect();
        assert_eq!(fields.len(), 5, "{described}");
        assert!(fields[0].contains("Macro-Elmarit 45mm"), "{described}");
        assert_eq!(fields[2], "Panasonic");
        assert!(fields[3].contains('D') && fields[3].contains('T'), "{described}");
        assert_eq!(fields[4], "2.000");
        let missing = describe_lens_match(&lens_options("Olympus Zuiko 21mm", 21.0, None));
        assert!(matches!(missing, Err(Error::LensProfileUnavailable(_))));
        let manual = describe_lens_match(&lens_options("", 0.0, None));
        assert!(matches!(manual, Err(Error::LensProfileUnavailable(_))));
    }

    /// A search matches every word wherever punctuation puts it, puts what the
    /// body can mount first, and lists the mountable profiles when nothing is
    /// typed.
    #[test]
    fn lens_search_finds_by_words_and_ranks_the_mountable_first() {
        let listing = search_lens_profiles("OM Digital Solutions", "OM-1", "elmarit 45", 45.0).unwrap();
        let rows: Vec<Vec<&str>> = listing.lines().map(|line| line.split('\t').collect()).collect();
        assert!(!rows.is_empty());
        for row in &rows {
            assert_eq!(row.len(), 9, "{row:?}");
            let name = normalized_name(&format!("{} {}", row[2], row[0]));
            assert!(name.contains("elmarit") && name.contains("45"), "{row:?}");
        }
        assert_eq!(rows[0][8], "1", "a Micro Four Thirds lens leads the list");
        let everything = search_lens_profiles("OM Digital Solutions", "OM-1", "", 0.0).unwrap();
        let mountable = everything.lines().count();
        assert!(mountable > 50, "{mountable} mountable profiles");
        assert!(everything.lines().all(|line| line.ends_with("\t1")));
        let anywhere = search_lens_profiles("", "", "distagon", 0.0).unwrap();
        assert!(anywhere.lines().count() >= 2);
        // Digits begin a word of the name; they are not found inside one.
        let zoom = search_lens_profiles("OM Digital Solutions", "OM-1", "zuiko 21", 21.0).unwrap();
        assert!(
            zoom.lines().all(|line| lens_name_words(line.split('\t').next().unwrap())
                .iter()
                .any(|word| word.starts_with("21"))),
            "{zoom}"
        );
        let prime = search_lens_profiles("", "", "distagon 21", 0.0).unwrap();
        assert!(prime.lines().any(|line| line.contains("21mm")), "{prime}");
    }

    /// A textured, noise-free scene: smooth enough to resample exactly,
    /// detailed enough everywhere to measure a shift against.
    fn textured_scene(width: usize, height: usize) -> RgbImage {
        let cell = 12.0_f32;
        let value = |x: usize, y: usize, channel: usize| -> f32 {
            let (fx, fy) = (x as f32 / cell, y as f32 / cell);
            let (cx, cy) = (fx.floor() as u64, fy.floor() as u64);
            let corner = |dx: u64, dy: u64| {
                (splitmix64((cx + dx) << 20 | (cy + dy) << 2 | channel as u64) >> 40) as f32
                    / 16_777_216.0
            };
            let (tx, ty) = (fx - fx.floor(), fy - fy.floor());
            let (sx, sy) = (tx * tx * (3.0 - 2.0 * tx), ty * ty * (3.0 - 2.0 * ty));
            let top = corner(0, 0) * (1.0 - sx) + corner(1, 0) * sx;
            let bottom = corner(0, 1) * (1.0 - sx) + corner(1, 1) * sx;
            0.15 + 0.6 * (top * (1.0 - sy) + bottom * sy)
        };
        let mut data = Vec::with_capacity(width * height * 3);
        for y in 0..height {
            for x in 0..width {
                // One pattern for all three channels, with a colour cast: the
                // channels have to share their detail for a shift to mean anything.
                let base = value(x, y, 7);
                data.extend_from_slice(&[base * 0.8, base, base * 1.1]);
            }
        }
        RgbImage { width, height, data }
    }

    /// Magnifies the red and blue images about the centre by the given factors,
    /// which is what a lens with lateral colour does.
    fn with_lateral_colour(image: &RgbImage, red: f32, blue: f32) -> RgbImage {
        let (width, height) = (image.width, image.height);
        let (cx, cy) = ((width - 1) as f32 * 0.5, (height - 1) as f32 * 0.5);
        let mut out = image.clone();
        for y in 0..height {
            for x in 0..width {
                for (channel, factor) in [(0, red), (2, blue)] {
                    let sx = cx + (x as f32 - cx) / (1.0 + factor);
                    let sy = cy + (y as f32 - cy) / (1.0 + factor);
                    out.data[(y * width + x) * 3 + channel] = bilinear(image, sx, sy, channel);
                }
            }
        }
        out
    }

    /// The measurement reads the scale a lens put in, and the correction takes
    /// it back out.
    #[test]
    fn lateral_colour_is_measured_and_removed() {
        let (width, height) = (1200, 900);
        let clean = textured_scene(width, height);
        let fringed = with_lateral_colour(&clean, 0.0008, -0.0005);
        let measured = measure_lateral_colour(&fringed).expect("a textured frame is measurable");
        assert!(
            (measured.red / 0.0008 - 1.0).abs() < 0.2 && (measured.blue / -0.0005 - 1.0).abs() < 0.2,
            "measured {measured:?} against red 0.0008 blue -0.0005"
        );
        let mut corrected = fringed;
        apply_radial_corrections(&mut corrected, 0.0, measured);
        let left = measure_lateral_colour(&corrected).unwrap_or_default();
        let half_diagonal = ((width as f32) * 0.5).hypot(height as f32 * 0.5);
        assert!(
            left.red.abs() * half_diagonal < 0.15 && left.blue.abs() * half_diagonal < 0.15,
            "residual {left:?} at the corner: red {} blue {} px",
            left.red * half_diagonal,
            left.blue * half_diagonal
        );
    }

    /// A frame whose channels already agree is left exactly alone, and a flat
    /// one is admitted to say nothing.
    #[test]
    fn lateral_colour_leaves_an_aligned_frame_alone() {
        let clean = textured_scene(900, 700);
        let measured = measure_lateral_colour(&clean).unwrap_or_default();
        assert!(measured.is_none(), "an aligned frame measured {measured:?}");
        let mut copy = clean.clone();
        apply_radial_corrections(&mut copy, 0.0, measured);
        assert_eq!(copy.data, clean.data);
        let flat = RgbImage { width: 400, height: 300, data: vec![0.4; 400 * 300 * 3] };
        assert!(measure_lateral_colour(&flat).is_none());
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
        let exact = auto_crop_scale(100, 80, 1, |x, y| valid[y * 100 + x]);
        assert!(exact > 1.09 && exact < 1.2, "unexpected crop scale {exact}");
        assert_eq!(1.0, auto_crop_scale(100, 80, 1, |_, _| true));

        // The production sweep looks at every fourth sample. On a border band
        // like this one — which is what a lens correction leaves — that has to
        // agree with looking at all of them, or the crop lets the border back
        // in.
        let coarse = auto_crop_scale(100, 80, 4, |x, y| valid[y * 100 + x]);
        assert!(
            (coarse - exact).abs() < 0.02,
            "coarse sweep {coarse} disagrees with {exact}"
        );
        assert!(coarse >= exact - 0.005, "coarse sweep cropped too little");
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
    fn denoising_fades_out_as_a_render_shrinks() {
        let full = 5184 * 3888;
        // A full-resolution render asks for exactly what it was given, and so
        // does anything at or above it — an export must never be weakened.
        assert_eq!(0.35, strength_for_scale(0.35, full, full));
        assert_eq!(0.35, strength_for_scale(0.35, full * 2, full));
        // A 1600 px preview of that frame is at less than a third of it, where
        // the downscale has already done more than the filter would.
        assert_eq!(0.0, strength_for_scale(0.35, 1600 * 1200, full));
        // Half resolution is the last size that asks for nothing, and a
        // three-quarter render asks for half.
        assert_eq!(0.0, strength_for_scale(0.35, full / 4, full));
        let three_quarters = strength_for_scale(1.0, (full * 9) / 16, full);
        assert!(
            (three_quarters - 0.5).abs() < 0.01,
            "three-quarter scale asked for {three_quarters}"
        );
        // A frame whose full size is unknown is taken at its word.
        assert_eq!(0.35, strength_for_scale(0.35, 1000, 0));
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
