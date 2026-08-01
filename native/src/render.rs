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
use lensfun::{Database, Modifier};
use rawler::decoders::{RawDecodeParams, RawLoader};
use rawler::imgop::develop::{Intermediate, ProcessingStep, RawDevelop};
use rawler::rawsource::RawSource;

use super::Error;

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
    pub lut_path: *const c_char,
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
            lut_path: std::ptr::null(),
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
                    _ => "grain size must be finite",
                }));
            }
        }
        if self.kelvin != 0.0 && !(2000.0..=50_000.0).contains(&self.kelvin) {
            return Err(Error::InvalidArgument("Kelvin must be zero or 2000..50000"));
        }
        if !(-5.0..=5.0).contains(&self.tint)
            || !(-10.0..=10.0).contains(&self.exposure_ev)
            || !(0.0..=1.0).contains(&self.chroma_noise_reduction)
            || !(0.0..=1.0).contains(&self.luma_noise_reduction)
            || !(0.0..=1.0).contains(&self.lut_strength)
            || !(0.0..=1.0).contains(&self.grain_amount)
            || !(0.25..=16.0).contains(&self.grain_size)
        {
            return Err(Error::InvalidArgument(
                "render setting outside supported range",
            ));
        }
        if !(1..=100).contains(&self.jpeg_quality) {
            return Err(Error::InvalidArgument("JPEG quality must be 1..100"));
        }
        if self.lut_strength > 0.0 && self.lut_path.is_null() {
            return Err(Error::InvalidArgument("LUT strength requires a LUT path"));
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
    for y in 1..image.height - 1 {
        for x in 1..image.width - 1 {
            let center = (y * image.width + x) * 3;
            let center_y =
                0.2126 * source[center] + 0.7152 * source[center + 1] + 0.0722 * source[center + 2];
            let (mut sy, mut scb, mut scr, mut sw) = (0.0, 0.0, 0.0, 0.0);
            for oy in y - 1..=y + 1 {
                for ox in x - 1..=x + 1 {
                    let offset = (oy * image.width + ox) * 3;
                    let (r, g, b) = (source[offset], source[offset + 1], source[offset + 2]);
                    let yy = 0.2126 * r + 0.7152 * g + 0.0722 * b;
                    let weight = 1.0 / (1.0 + (yy - center_y).abs() * 40.0);
                    sy += yy * weight;
                    scb += (b - yy) * weight;
                    scr += (r - yy) * weight;
                    sw += weight;
                }
            }
            let yy = center_y + (sy / sw - center_y) * luma;
            let cb = (source[center + 2] - center_y)
                + (scb / sw - (source[center + 2] - center_y)) * chroma;
            let cr =
                (source[center] - center_y) + (scr / sw - (source[center] - center_y)) * chroma;
            image.data[center] = yy + cr;
            image.data[center + 2] = yy + cb;
            image.data[center + 1] =
                (yy - 0.2126 * image.data[center] - 0.0722 * image.data[center + 2]) / 0.7152;
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

fn apply_lens(
    image: &mut RgbImage,
    make: &str,
    model: &str,
    lens_name: &str,
    focal: f32,
    flags: u32,
) -> Result<(), Error> {
    if flags & KNOWN_FLAGS == 0 {
        return Ok(());
    }
    if lens_name.is_empty() || focal <= 0.0 {
        return Err(Error::LensProfileUnavailable(
            "RAW metadata does not identify a usable lens/focal length".into(),
        ));
    }
    let db = lens_database()?;
    let requested_make = normalized_name(make);
    let camera = db
        .find_cameras(Some(make), model)
        .into_iter()
        .find(|camera| {
            let candidate_make = normalized_name(&camera.maker);
            camera.model.eq_ignore_ascii_case(model)
                && (candidate_make == requested_make
                    || (candidate_make.len() >= 5
                        && (candidate_make.contains(&requested_make)
                            || requested_make.contains(&candidate_make))))
        })
        .ok_or_else(|| {
            Error::LensProfileUnavailable(format!(
                "no exact lensfun camera profile for {make} {model}"
            ))
        })?;
    let requested = normalized_name(lens_name);
    let lens = db
        .find_lenses(Some(camera), lens_name)
        .into_iter()
        .find(|lens| {
            let candidate = normalized_name(&lens.model);
            (candidate == requested
                || (candidate.len() > 8
                    && (candidate.contains(&requested) || requested.contains(&candidate))))
                && focal >= lens.focal_min - 0.1
                && focal <= lens.focal_max + 0.1
        })
        .ok_or_else(|| {
            Error::LensProfileUnavailable(format!("no conservative lensfun match for {lens_name}"))
        })?;
    let mut modifier = Modifier::new(
        lens,
        focal,
        camera.crop_factor,
        image.width as u32,
        image.height as u32,
        true,
    );
    let distortion =
        flags & FLAG_LENS_DISTORTION != 0 && modifier.enable_distortion_correction(lens);
    let tca = flags & FLAG_LENS_TCA != 0 && modifier.enable_tca_correction(lens);
    if (flags & FLAG_LENS_DISTORTION != 0 && !distortion) || (flags & FLAG_LENS_TCA != 0 && !tca) {
        return Err(Error::LensProfileUnavailable(format!(
            "lensfun profile for {lens_name} lacks requested calibration"
        )));
    }
    let source = image.clone();
    let mut geometry = vec![0.0; image.width * 2];
    let mut subpixel = vec![0.0; image.width * 6];
    for y in 0..image.height {
        if distortion {
            modifier.apply_geometry_distortion(0.0, y as f32, image.width, 1, &mut geometry);
        }
        if tca {
            modifier.apply_subpixel_distortion(0.0, y as f32, image.width, 1, &mut subpixel);
        }
        for x in 0..image.width {
            let (gx, gy) = if distortion {
                (geometry[x * 2], geometry[x * 2 + 1])
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
                image.data[(y * image.width + x) * 3 + channel] =
                    bilinear(&source, sx, sy, channel);
            }
        }
    }
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
    let mut steps = vec![
        ProcessingStep::Rescale,
        ProcessingStep::Demosaic,
        ProcessingStep::CropActiveArea,
        ProcessingStep::Calibrate,
        ProcessingStep::CropDefault,
    ];
    if settings.kelvin == 0.0 {
        steps.insert(3, ProcessingStep::WhiteBalance);
    }
    let developed = RawDevelop { steps }
        .develop_intermediate(&raw)
        .map_err(|e| Error::Render(format!("RAW development: {e}")))?;
    let pixels = match developed {
        Intermediate::ThreeColor(pixels) => pixels,
        _ => {
            return Err(Error::Render(
                "RAW development did not produce three-channel RGB".into(),
            ));
        }
    };
    let mut image = RgbImage {
        width: pixels.width,
        height: pixels.height,
        data: pixels.into_inner().into_iter().flatten().collect(),
    };
    apply_white_adaptation(&mut image, settings.kelvin, settings.tint);
    apply_exposure(&mut image, settings.exposure_ev);
    image = orient(image, metadata.exif.orientation.unwrap_or(1));
    image = downscale(image, settings.max_width, settings.max_height);
    let lens_name = metadata
        .lens
        .as_ref()
        .map_or("", |lens| lens.lens_name.as_str());
    let focal = metadata
        .exif
        .focal_length
        .as_ref()
        .map_or(0.0, |value| value.as_f32());
    apply_lens(
        &mut image,
        &metadata.make,
        &metadata.model,
        lens_name,
        focal,
        settings.flags,
    )?;
    apply_noise_reduction(
        &mut image,
        settings.luma_noise_reduction,
        settings.chroma_noise_reduction,
    );
    srgb_transfer(&mut image);
    if settings.lut_strength > 0.0 {
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
