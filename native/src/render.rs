use std::ffi::{CStr, c_char};
use std::fs::{self, File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Seek, Write};
use std::os::unix::fs::MetadataExt;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;
use std::time::{SystemTime, UNIX_EPOCH};

use image::codecs::jpeg::JpegEncoder;
use image::codecs::tiff::TiffEncoder;
use image::{ColorType, ImageEncoder};
use lensfun::{Camera, Database, Lens, Modifier};
use rawler::decoders::{RawDecodeParams, RawLoader};
use rawler::imgop::develop::{ProcessingStep, RawDevelop};
use rawler::rawsource::RawSource;

use super::Error;
use super::color::intermediate_to_linear_srgb;
use super::tone::apply_default_display_tone;

pub const SETTINGS_VERSION: u32 = 1;
pub const OUTPUT_JPEG: u32 = 1;
pub const OUTPUT_TIFF: u32 = 2;
pub const FLAG_LENS_DISTORTION: u32 = 1;
pub const FLAG_LENS_TCA: u32 = 2;
const KNOWN_FLAGS: u32 = FLAG_LENS_DISTORTION | FLAG_LENS_TCA;
const MAX_LUT_BYTES: u64 = 32 * 1024 * 1024;
const MAX_LUT_SIZE: usize = 65;
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
        }
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
struct RgbImage {
    width: usize,
    height: usize,
    data: Vec<f32>,
}

#[derive(Debug)]
struct CubeLut {
    size: usize,
    domain_min: [f32; 3],
    domain_max: [f32; 3],
    values: Vec<[f32; 3]>,
}

impl CubeLut {
    fn read(path: &Path) -> Result<Self, Error> {
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

fn apply_exposure(image: &mut RgbImage, ev: f32) {
    let multiplier = 2.0_f32.powf(ev);
    image.data.iter_mut().for_each(|value| *value *= multiplier);
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

/// Chromatically adapts linear sRGB from the requested source white to D65.
/// Camera WB remains rawler's camera-aware default when Kelvin is zero; tint is
/// then a small working-space green/magenta adaptation around D65. A nonzero
/// Kelvin deliberately disables camera WB and adapts the camera-matrix result.
fn apply_white_adaptation(image: &mut RgbImage, kelvin: f32, tint: f32) {
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
    let d65_xyz = [0.95047, 1.0, 1.08883];
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
    for pixel in image.data.as_chunks_mut::<3>().0 {
        let xyz = mat_vec(rgb_to_xyz, *pixel);
        let mut lms = mat_vec(bradford, xyz);
        for channel in 0..3 {
            lms[channel] *= dst_lms[channel] / src_lms[channel];
        }
        *pixel = mat_vec(xyz_to_rgb, mat_vec(inverse, lms));
    }
}

fn apply_noise_reduction(image: &mut RgbImage, luma: f32, chroma: f32) {
    if (luma == 0.0 && chroma == 0.0) || image.width < 3 || image.height < 3 {
        return;
    }
    let source = image.data.clone();
    let pixel_count = image.width * image.height;
    let mut ycbcr = vec![[0.0_f32; 3]; pixel_count];
    for (index, pixel) in source.as_chunks::<3>().0.iter().enumerate() {
        let yy = 0.2126 * pixel[0] + 0.7152 * pixel[1] + 0.0722 * pixel[2];
        ycbcr[index] = [yy, pixel[2] - yy, pixel[0] - yy];
    }
    let kernel = [1.0_f32, 4.0, 6.0, 4.0, 1.0];
    for y in 0..image.height {
        for x in 0..image.width {
            let center_index = y * image.width + x;
            let center = ycbcr[center_index];
            let left = ycbcr[y * image.width + x.saturating_sub(1)][0];
            let right = ycbcr[y * image.width + (x + 1).min(image.width - 1)][0];
            let above = ycbcr[y.saturating_sub(1) * image.width + x][0];
            let below = ycbcr[(y + 1).min(image.height - 1) * image.width + x][0];
            let gradient = (right - left).abs() + (below - above).abs();
            let edge_protection = 1.0 / (1.0 + gradient * 48.0);
            let shadow_boost = 1.0 + (1.0 - center[0].clamp(0.0, 1.0)) * 1.25;
            let luma_mix = (luma * 0.6 * shadow_boost * edge_protection).min(1.0);
            let chroma_mix = (chroma * shadow_boost * edge_protection).min(1.0);
            let mut filtered = [0.0_f32; 3];
            let mut weight_sum = 0.0;
            for (ky, dy) in (-2_isize..=2).enumerate() {
                let sample_y = (y as isize + dy).clamp(0, image.height as isize - 1) as usize;
                for (kx, dx) in (-2_isize..=2).enumerate() {
                    let sample_x = (x as isize + dx).clamp(0, image.width as isize - 1) as usize;
                    let weight = kernel[kx] * kernel[ky];
                    let sample = ycbcr[sample_y * image.width + sample_x];
                    for channel in 0..3 {
                        filtered[channel] += sample[channel] * weight;
                    }
                    weight_sum += weight;
                }
            }
            for value in &mut filtered {
                *value /= weight_sum;
            }
            let yy = center[0] + (filtered[0] - center[0]) * luma_mix;
            let cb = center[1] + (filtered[1] - center[1]) * chroma_mix;
            let cr = center[2] + (filtered[2] - center[2]) * chroma_mix;
            image.data[center_index * 3] = yy + cr;
            image.data[center_index * 3 + 2] = yy + cb;
            image.data[center_index * 3 + 1] = (yy
                - 0.2126 * image.data[center_index * 3]
                - 0.0722 * image.data[center_index * 3 + 2])
                / 0.7152;
        }
    }
}

fn bilinear(image: &RgbImage, x: f32, y: f32, channel: usize) -> f32 {
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
    let source = image.clone();
    let center_x = (image.width.saturating_sub(1)) as f32 * 0.5;
    let center_y = (image.height.saturating_sub(1)) as f32 * 0.5;
    for y in 0..image.height {
        let source_y = center_y + (y as f32 - center_y) / scale;
        for x in 0..image.width {
            let source_x = center_x + (x as f32 - center_x) / scale;
            for channel in 0..3 {
                image.data[(y * image.width + x) * 3 + channel] =
                    bilinear(&source, source_x, source_y, channel);
            }
        }
    }
}

struct LensCorrectionOptions<'a> {
    make: &'a str,
    model: &'a str,
    lens_name: &'a str,
    focal: f32,
    flags: u32,
    strength: f32,
    explicit_profile: Option<&'a str>,
    focal_reducer: f32,
    crop_factor: f32,
}

fn apply_lens(image: &mut RgbImage, options: &LensCorrectionOptions<'_>) -> Result<(), Error> {
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
    let source = image.clone();
    let mut geometry = vec![0.0; image.width * 2];
    let mut subpixel = vec![0.0; image.width * 6];
    let mut valid = vec![true; image.width * image.height];
    for y in 0..image.height {
        if distortion {
            modifier.apply_geometry_distortion(0.0, y as f32, image.width, 1, &mut geometry);
        }
        if tca {
            modifier.apply_subpixel_distortion(0.0, y as f32, image.width, 1, &mut subpixel);
        }
        for x in 0..image.width {
            let (gx, gy) = if distortion {
                let identity_x = x as f32;
                let identity_y = y as f32;
                (
                    blend_lens_coordinate(identity_x, geometry[x * 2], correction_strength),
                    blend_lens_coordinate(identity_y, geometry[x * 2 + 1], correction_strength),
                )
            } else {
                (x as f32, y as f32)
            };
            for channel in 0..3 {
                let (sx, sy) = if tca {
                    (
                        subpixel[x * 6 + channel * 2] + gx - x as f32,
                        subpixel[x * 6 + channel * 2 + 1] + gy - y as f32,
                    )
                } else {
                    (gx, gy)
                };
                let coordinate_valid = sx >= 0.0
                    && sy >= 0.0
                    && sx <= (image.width - 1) as f32
                    && sy <= (image.height - 1) as f32;
                valid[y * image.width + x] &= coordinate_valid;
                image.data[(y * image.width + x) * 3 + channel] =
                    bilinear(&source, sx, sy, channel);
            }
        }
    }
    zoom_center(
        image,
        lens_auto_crop_scale(&valid, image.width, image.height),
    );
    Ok(())
}

fn orient(image: RgbImage, orientation: u16) -> RgbImage {
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
    for y in 0..height {
        for x in 0..width {
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
            let dst = (y * width + x) * 3;
            let src = (sy * image.width + sx) * 3;
            output.data[dst..dst + 3].copy_from_slice(&image.data[src..src + 3]);
        }
    }
    output
}

fn native_downscale_bounds(orientation: u16, max_width: u32, max_height: u32) -> (u32, u32) {
    if (5..=8).contains(&orientation) {
        (max_height, max_width)
    } else {
        (max_width, max_height)
    }
}

fn downscale(image: RgbImage, max_width: u32, max_height: u32) -> RgbImage {
    if max_width == 0 && max_height == 0 {
        return image;
    }
    let sx = if max_width == 0 {
        1.0
    } else {
        max_width as f32 / image.width as f32
    };
    let sy = if max_height == 0 {
        1.0
    } else {
        max_height as f32 / image.height as f32
    };
    let scale = sx.min(sy).min(1.0);
    if scale >= 1.0 {
        return image;
    }
    let width = ((image.width as f32 * scale).round() as usize).max(1);
    let height = ((image.height as f32 * scale).round() as usize).max(1);
    let mut output = RgbImage {
        width,
        height,
        data: vec![0.0; width * height * 3],
    };
    for y in 0..height {
        for x in 0..width {
            for channel in 0..3 {
                output.data[(y * width + x) * 3 + channel] = bilinear(
                    &image,
                    (x as f32 + 0.5) / scale - 0.5,
                    (y as f32 + 0.5) / scale - 0.5,
                    channel,
                );
            }
        }
    }
    output
}

fn srgb_transfer(image: &mut RgbImage) {
    for value in &mut image.data {
        *value = if *value <= 0.003_130_8 {
            12.92 * *value
        } else {
            1.055 * value.max(0.0).powf(1.0 / 2.4) - 0.055
        };
    }
}

fn apply_lut(image: &mut RgbImage, lut: &CubeLut, strength: f32) {
    for pixel in image.data.as_chunks_mut::<3>().0 {
        let transformed = lut.sample(*pixel);
        for channel in 0..3 {
            pixel[channel] = (pixel[channel] + (transformed[channel] - pixel[channel]) * strength)
                .clamp(0.0, 1.0);
        }
    }
}

fn splitmix64(mut value: u64) -> u64 {
    value = value.wrapping_add(0x9e37_79b9_7f4a_7c15);
    value = (value ^ (value >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    value = (value ^ (value >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    value ^ (value >> 31)
}

fn apply_grain(image: &mut RgbImage, amount: f32, size: f32, seed: u64) {
    if amount == 0.0 {
        return;
    }
    for y in 0..image.height {
        for x in 0..image.width {
            let gx = (x as f32 / size).floor() as u64;
            let gy = (y as f32 / size).floor() as u64;
            let bits = splitmix64(
                seed ^ gx.wrapping_mul(0x517c_c1b7_2722_0a95)
                    ^ gy.wrapping_mul(0x6eed_0e9d_a4d9_4a4f),
            );
            let noise = ((bits >> 40) as f32 / 16_777_215.0 - 0.5) * amount * 0.16;
            let offset = (y * image.width + x) * 3;
            let luminance = 0.2126 * image.data[offset]
                + 0.7152 * image.data[offset + 1]
                + 0.0722 * image.data[offset + 2];
            let shaped = noise * (0.35 + 0.65 * (1.0 - luminance));
            for channel in 0..3 {
                image.data[offset + channel] =
                    (image.data[offset + channel] + shaped).clamp(0.0, 1.0);
            }
        }
    }
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
                .iter()
                .map(|v| (v.clamp(0.0, 1.0) * 255.0 + 0.5) as u8)
                .collect();
            let mut encoder = JpegEncoder::new_with_quality(writer, quality as u8);
            encoder
                .set_icc_profile(SRGB_ICC.to_vec())
                .map_err(|e| Error::Render(format!("JPEG ICC profile: {e}")))?;
            encoder.write_image(
                &bytes,
                image.width as u32,
                image.height as u32,
                ColorType::Rgb8.into(),
            )
        }
        OUTPUT_TIFF => {
            let mut bytes = Vec::with_capacity(image.data.len() * 2);
            for value in &image.data {
                bytes.extend_from_slice(
                    &((value.clamp(0.0, 1.0) * 65535.0 + 0.5) as u16).to_ne_bytes(),
                );
            }
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

fn same_file(input: &Path, output: &Path) -> Result<bool, Error> {
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

fn atomic_encode(
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

pub fn render(input: &Path, output: &Path, settings: &RenderSettingsV1) -> Result<(), Error> {
    settings.validate()?;
    if same_file(input, output)? {
        return Err(Error::InvalidArgument(
            "input and output refer to the same file",
        ));
    }
    let source = RawSource::new(input)?;
    let loader = RawLoader::new();
    let decoder = loader
        .get_decoder(&source)
        .map_err(|e| Error::Render(format!("RAW decoder: {e}")))?;
    let params = RawDecodeParams::default();
    let metadata = decoder
        .raw_metadata(&source, &params)
        .map_err(|e| Error::Render(format!("RAW metadata: {e}")))?;
    let raw = decoder
        .raw_image(&source, &params, false)
        .map_err(|e| Error::Render(format!("RAW decode: {e}")))?;
    // Rawler performs scaling, demosaic, and cropping. Orfeus owns white
    // balance and the camera-to-sRGB transform so scene-linear highlights remain
    // unclipped through white adaptation and exposure.
    let steps = vec![
        ProcessingStep::Rescale,
        ProcessingStep::Demosaic,
        ProcessingStep::CropActiveArea,
        ProcessingStep::CropDefault,
    ];
    let developed = RawDevelop { steps }
        .develop_intermediate(&raw)
        .map_err(|e| Error::Render(format!("RAW development: {e}")))?;
    // Start from the camera's neutral rendering for both as-shot and custom
    // temperature. Custom Kelvin is a relative chromatic adaptation around D65;
    // dropping the sensor WB coefficients leaves Bayer green dominant.
    let white_balance = raw.wb_coeffs;
    let linear_srgb = intermediate_to_linear_srgb(developed, &raw.color_matrix, white_balance)
        .map_err(Error::Color)?;
    let mut image = RgbImage {
        width: linear_srgb.width,
        height: linear_srgb.height,
        data: linear_srgb.data,
    };
    apply_white_adaptation(&mut image, settings.kelvin, settings.tint);
    apply_exposure(&mut image, settings.exposure_ev);
    let orientation = metadata.exif.orientation.unwrap_or(1);
    let (native_max_width, native_max_height) =
        native_downscale_bounds(orientation, settings.max_width, settings.max_height);
    image = downscale(image, native_max_width, native_max_height);
    let lens_name = metadata
        .lens
        .as_ref()
        .map_or("", |lens| lens.lens_name.as_str());
    let focal = metadata
        .exif
        .focal_length
        .as_ref()
        .map_or(0.0, |value| value.as_f32());
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
            make: &metadata.make,
            model: &metadata.model,
            lens_name,
            focal,
            flags: settings.flags,
            strength: settings.lens_correction_strength,
            explicit_profile,
            focal_reducer: settings.focal_reducer,
            crop_factor: settings.lens_crop_factor,
        },
    )?;
    image = orient(image, orientation);
    apply_noise_reduction(
        &mut image,
        settings.luma_noise_reduction,
        settings.chroma_noise_reduction,
    );
    apply_default_display_tone(&mut image.data);
    srgb_transfer(&mut image);
    if settings.lut_strength > 0.0 && !settings.lut_path.is_null() {
        let path = unsafe { CStr::from_ptr(settings.lut_path) }
            .to_str()
            .map_err(|_| Error::InvalidArgument("LUT path is not UTF-8"))?;
        apply_lut(
            &mut image,
            &CubeLut::read(Path::new(path))?,
            settings.lut_strength,
        );
    }
    apply_grain(
        &mut image,
        settings.grain_amount,
        settings.grain_size,
        settings.grain_seed,
    );
    atomic_encode(
        input,
        output,
        &image,
        settings.output_format,
        settings.jpeg_quality,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use image::{ImageDecoder, ImageReader};
    use std::io::Cursor;
    use std::os::unix::fs::symlink;

    fn identity_cube() -> CubeLut {
        CubeLut::parse(Cursor::new(
            "LUT_3D_SIZE 2\n0 0 0\n1 0 0\n0 1 0\n1 1 0\n0 0 1\n1 0 1\n0 1 1\n1 1 1\n",
        ))
        .unwrap()
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

    #[test]
    fn high_kelvin_adaptation_warms_a_neutral_pixel() {
        let mut image = RgbImage {
            width: 1,
            height: 1,
            data: vec![0.5, 0.5, 0.5],
        };
        apply_white_adaptation(&mut image, 15_000.0, 0.0);
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
        apply_white_adaptation(&mut magenta, 0.0, 20.0);
        apply_white_adaptation(&mut green, 0.0, -20.0);
        assert!(magenta.data[0] + magenta.data[2] > magenta.data[1] * 2.0);
        assert!(green.data[1] * 2.0 > green.data[0] + green.data[2]);
        assert!((magenta.data[1] - green.data[1]).abs() > 0.1);
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
