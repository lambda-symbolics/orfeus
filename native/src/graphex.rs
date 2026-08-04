//! Executor for processing node graphs.
//!
//! A graph program arrives as a compact little-endian byte stream produced by
//! the Lisp core: filter nodes carrying one stage's parameters, and blend
//! nodes mixing two scene-linear branches by opacity. Nodes are numbered by
//! position (1..count) and may reference the decoded source as input 0. Film
//! nodes work in display space: executing one first applies the default
//! display tone and sRGB transfer to its branch, and only further film nodes
//! may consume the result. The final image is oriented and encoded exactly
//! like the flat pipeline.

use std::ffi::{CStr, c_char};
use std::path::Path;
use std::sync::{Arc, Mutex, OnceLock};

use rayon::prelude::*;

use super::Error;
use super::render::{self, DecodedRaw, LensCorrectionOptions, RgbImage};

const GRAPH_MAGIC: u32 = 0x4746_524F; // "ORFG" little-endian
const GRAPH_VERSION: u32 = 1;
const MAX_GRAPH_NODES: usize = 64;

pub const NODE_WHITE_BALANCE: u32 = 1;
pub const NODE_EXPOSURE: u32 = 2;
pub const NODE_NOISE_REDUCTION: u32 = 3;
pub const NODE_TONE: u32 = 4;
pub const NODE_OPTICS: u32 = 5;
pub const NODE_FILM: u32 = 6;
pub const NODE_BLEND: u32 = 7;
pub const NODE_COLOR_SUBTRACT: u32 = 8;
pub const NODE_CROP: u32 = 9;
pub const NODE_CURVES: u32 = 10;

/// Frame-level settings shared by every node of one graph render.
#[repr(C)]
pub struct RenderFrameV1 {
    pub struct_size: u32,
    pub version: u32,
    pub output_format: u32,
    pub max_width: u32,
    pub max_height: u32,
    pub jpeg_quality: u32,
    pub grain_seed: u64,
    pub focal_reducer: f32,
    pub lens_crop_factor: f32,
    pub lens_profile_model: *const c_char,
}

impl RenderFrameV1 {
    fn validate(&self) -> Result<(), Error> {
        if self.struct_size < size_of::<Self>() as u32 {
            return Err(Error::InvalidArgument("render frame struct is too small"));
        }
        if self.version != 1 {
            return Err(Error::InvalidArgument("unsupported render frame version"));
        }
        if !matches!(
            self.output_format,
            render::OUTPUT_JPEG | render::OUTPUT_TIFF
        ) {
            return Err(Error::InvalidArgument("unsupported output format"));
        }
        if !(1..=100).contains(&self.jpeg_quality) {
            return Err(Error::InvalidArgument("JPEG quality must be 1..100"));
        }
        if !self.focal_reducer.is_finite() || !(0.1..=2.0).contains(&self.focal_reducer) {
            return Err(Error::InvalidArgument("focal reducer must be 0.1..2"));
        }
        if !self.lens_crop_factor.is_finite() || !(0.0..=10.0).contains(&self.lens_crop_factor) {
            return Err(Error::InvalidArgument("lens crop factor must be 0..10"));
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct GraphOp {
    kind: u32,
    input_a: usize,
    input_b: usize,
    params: Vec<f32>,
    text: Option<String>,
}

struct GraphReader<'a> {
    bytes: &'a [u8],
    offset: usize,
}

impl<'a> GraphReader<'a> {
    fn u32(&mut self) -> Result<u32, Error> {
        let end = self.offset + 4;
        let value = self
            .bytes
            .get(self.offset..end)
            .ok_or(Error::InvalidArgument("truncated graph program"))?
            .try_into()
            .map(u32::from_le_bytes)
            .map_err(|_| Error::InvalidArgument("truncated graph program"))?;
        self.offset = end;
        Ok(value)
    }

    fn i32(&mut self) -> Result<i32, Error> {
        Ok(self.u32()? as i32)
    }

    fn f32(&mut self) -> Result<f32, Error> {
        Ok(f32::from_bits(self.u32()?))
    }

    fn text(&mut self) -> Result<Option<String>, Error> {
        let length = self.u32()? as usize;
        if length == 0 {
            return Ok(None);
        }
        if length > 4096 {
            return Err(Error::InvalidArgument("graph string is too long"));
        }
        let end = self.offset + length;
        let raw = self
            .bytes
            .get(self.offset..end)
            .ok_or(Error::InvalidArgument("truncated graph string"))?;
        self.offset = end;
        Ok(Some(
            std::str::from_utf8(raw)
                .map_err(|_| Error::InvalidArgument("graph string is not UTF-8"))?
                .to_owned(),
        ))
    }
}

fn expected_param_count(kind: u32) -> Result<usize, Error> {
    Ok(match kind {
        NODE_WHITE_BALANCE => 2,   // kelvin (0 = as shot), tint
        NODE_EXPOSURE => 1,        // ev
        NODE_NOISE_REDUCTION => 2, // edge-aware, neural
        NODE_TONE => 7,            // blacks..whites
        NODE_OPTICS => 3,          // distortion?, strength, tca?
        NODE_FILM => 3,            // lut strength, grain amount, grain size
        NODE_BLEND => 1,           // opacity toward input B
        NODE_COLOR_SUBTRACT => 3,  // picked color, subtracted per channel
        NODE_CROP => 5,            // left, top, width, height, angle (degrees)
        NODE_CURVES => 24,         // three channels x four (x, y) points
        _ => return Err(Error::InvalidArgument("unknown graph node kind")),
    })
}

fn validate_curve_points(params: &[f32]) -> Result<(), Error> {
    for channel in 0..3 {
        let points = &params[channel * 8..channel * 8 + 8];
        for segment in 0..3 {
            if points[segment * 2] + 1.0e-3 > points[segment * 2 + 2] {
                return Err(Error::InvalidArgument(
                    "curve points must have ascending positions",
                ));
            }
        }
    }
    Ok(())
}

const CURVE_LUT_SIZE: usize = 4096;

fn srgb_encode_value(value: f32) -> f32 {
    if value <= 0.003_130_8 {
        12.92 * value
    } else {
        1.055 * value.max(0.0).powf(1.0 / 2.4) - 0.055
    }
}

fn srgb_decode_value(value: f32) -> f32 {
    if value <= 0.040_45 {
        value / 12.92
    } else {
        ((value + 0.055) / 1.055).powf(2.4)
    }
}

/// Monotone cubic tangents (Fritsch-Carlson) for four ascending points.
fn curve_tangents(xs: &[f32; 4], ys: &[f32; 4]) -> [f32; 4] {
    let mut slopes = [0.0f32; 3];
    for segment in 0..3 {
        slopes[segment] =
            (ys[segment + 1] - ys[segment]) / (xs[segment + 1] - xs[segment]).max(1.0e-4);
    }
    let mut tangents = [0.0f32; 4];
    tangents[0] = slopes[0];
    tangents[3] = slopes[2];
    for point in 1..3 {
        tangents[point] = if slopes[point - 1] * slopes[point] <= 0.0 {
            0.0
        } else {
            let before = xs[point] - xs[point - 1];
            let after = xs[point + 1] - xs[point];
            3.0 * (before + after)
                / ((2.0 * after + before) / slopes[point - 1]
                    + (after + 2.0 * before) / slopes[point])
        };
    }
    tangents
}

fn eval_curve(xs: &[f32; 4], ys: &[f32; 4], tangents: &[f32; 4], x: f32) -> f32 {
    if x <= xs[0] {
        return ys[0];
    }
    if x >= xs[3] {
        return ys[3];
    }
    let segment = if x < xs[1] {
        0
    } else if x < xs[2] {
        1
    } else {
        2
    };
    let width = (xs[segment + 1] - xs[segment]).max(1.0e-4);
    let t = (x - xs[segment]) / width;
    let t2 = t * t;
    let t3 = t2 * t;
    let h00 = 2.0 * t3 - 3.0 * t2 + 1.0;
    let h10 = t3 - 2.0 * t2 + t;
    let h01 = -2.0 * t3 + 3.0 * t2;
    let h11 = t3 - t2;
    (h00 * ys[segment]
        + h10 * width * tangents[segment]
        + h01 * ys[segment + 1]
        + h11 * width * tangents[segment + 1])
        .clamp(0.0, 1.0)
}

/// Linear-domain LUT with square-root spacing: entry i maps the scene-linear
/// value (i / (N-1))^2 through encode -> curve -> decode, so pixels avoid the
/// per-value transfer powf.
fn curve_lut(points: &[f32]) -> Vec<f32> {
    let xs = [points[0], points[2], points[4], points[6]];
    let ys = [points[1], points[3], points[5], points[7]];
    let tangents = curve_tangents(&xs, &ys);
    (0..CURVE_LUT_SIZE)
        .map(|index| {
            let root = index as f32 / (CURVE_LUT_SIZE - 1) as f32;
            let encoded = srgb_encode_value(root * root);
            srgb_decode_value(eval_curve(&xs, &ys, &tangents, encoded))
        })
        .collect()
}

fn apply_curve_value(lut: &[f32], value: f32) -> f32 {
    let last = lut.len() - 1;
    if value <= 0.0 {
        // Out-of-gamut negatives ride along so gradients stay smooth.
        return lut[0] + value;
    }
    if value >= 1.0 {
        return lut[last] + (value - 1.0);
    }
    let position = value.sqrt() * last as f32;
    let index = position as usize;
    let fraction = position - index as f32;
    let next = (index + 1).min(last);
    lut[index] * (1.0 - fraction) + lut[next] * fraction
}

fn validate_param(kind: u32, index: usize, value: f32) -> Result<(), Error> {
    if !value.is_finite() {
        return Err(Error::InvalidArgument("graph parameter must be finite"));
    }
    let ok = match (kind, index) {
        (NODE_WHITE_BALANCE, 0) => value == 0.0 || (2000.0..=50_000.0).contains(&value),
        (NODE_WHITE_BALANCE, 1) => (-20.0..=20.0).contains(&value),
        (NODE_EXPOSURE, 0) => (-10.0..=10.0).contains(&value),
        (NODE_NOISE_REDUCTION, _) => (0.0..=1.0).contains(&value),
        (NODE_TONE, _) => (-2.0..=2.0).contains(&value),
        (NODE_OPTICS, 0) | (NODE_OPTICS, 2) => value == 0.0 || value == 1.0,
        (NODE_OPTICS, 1) => (0.0..=2.0).contains(&value),
        (NODE_FILM, 0) => (0.0..=1.0).contains(&value),
        (NODE_FILM, 1) => (0.0..=1.0).contains(&value),
        (NODE_FILM, 2) => (0.25..=16.0).contains(&value),
        (NODE_BLEND, 0) => (0.0..=1.0).contains(&value),
        (NODE_COLOR_SUBTRACT, _) => (0.0..=4.0).contains(&value),
        (NODE_CROP, 0) | (NODE_CROP, 1) => (0.0..=1.0).contains(&value),
        (NODE_CROP, 2) | (NODE_CROP, 3) => (0.05..=1.0).contains(&value),
        (NODE_CROP, 4) => (-45.0..=45.0).contains(&value),
        (NODE_CURVES, _) => (0.0..=1.0).contains(&value),
        _ => false,
    };
    if ok {
        Ok(())
    } else {
        Err(Error::InvalidArgument("graph parameter outside its range"))
    }
}

/// Parses and validates a serialized graph program.
///
/// Returns the ops in execution order together with the display-domain flags
/// implied by film nodes, enforcing the same structural rules as the Lisp
/// core: inputs reference earlier nodes only, blends stay scene-linear, optics
/// precede crop, and only film nodes may consume film output.
pub(crate) fn parse_graph(bytes: &[u8]) -> Result<Vec<GraphOp>, Error> {
    let mut reader = GraphReader { bytes, offset: 0 };
    if reader.u32()? != GRAPH_MAGIC {
        return Err(Error::InvalidArgument("graph program has a wrong magic"));
    }
    if reader.u32()? != GRAPH_VERSION {
        return Err(Error::InvalidArgument("unsupported graph program version"));
    }
    let count = reader.u32()? as usize;
    if count > MAX_GRAPH_NODES {
        return Err(Error::InvalidArgument("graph program has too many nodes"));
    }
    let mut ops = Vec::with_capacity(count);
    let mut display = vec![false; count + 1];
    let mut cropped = vec![false; count + 1];
    for index in 1..=count {
        let kind = reader.u32()?;
        let input_a = reader.i32()?;
        let input_b = reader.i32()?;
        let param_count = reader.u32()? as usize;
        if param_count != expected_param_count(kind)? {
            return Err(Error::InvalidArgument("graph node parameter count"));
        }
        let mut params = Vec::with_capacity(param_count);
        for parameter in 0..param_count {
            let value = reader.f32()?;
            validate_param(kind, parameter, value)?;
            params.push(value);
        }
        if kind == NODE_CURVES {
            validate_curve_points(&params)?;
        }
        let text = reader.text()?;
        if text.is_some() && kind != NODE_FILM {
            return Err(Error::InvalidArgument("only film nodes may carry a string"));
        }
        let check_input = |input: i32| -> Result<usize, Error> {
            if input < 0 || input as usize >= index {
                Err(Error::InvalidArgument("graph input is not upstream"))
            } else {
                Ok(input as usize)
            }
        };
        let input_a = check_input(input_a)?;
        let input_b = if kind == NODE_BLEND {
            check_input(input_b)?
        } else {
            if input_b != -1 {
                return Err(Error::InvalidArgument(
                    "only blend nodes take a second input",
                ));
            }
            0
        };
        cropped[index] = cropped[input_a] || (kind == NODE_BLEND && cropped[input_b]);
        if kind == NODE_OPTICS && cropped[input_a] {
            return Err(Error::InvalidArgument(
                "optics nodes cannot consume cropped output",
            ));
        }
        match kind {
            NODE_BLEND => {
                if display[input_a] || display[input_b] {
                    return Err(Error::InvalidArgument(
                        "blend nodes cannot consume film output",
                    ));
                }
            }
            NODE_FILM => {
                display[index] = true;
            }
            NODE_CROP => {
                // Crops keep their branch's domain and mark its geometry.
                display[index] = display[input_a];
                cropped[index] = true;
                let (left, top, width, height) = (params[0], params[1], params[2], params[3]);
                if left + width > 1.0001 || top + height > 1.0001 {
                    return Err(Error::InvalidArgument("crop rectangle leaves the frame"));
                }
            }
            _ => {
                if display[input_a] {
                    return Err(Error::InvalidArgument(
                        "only film nodes may consume film output",
                    ));
                }
            }
        }
        ops.push(GraphOp {
            kind,
            input_a,
            input_b,
            params,
            text,
        });
    }
    if reader.offset != bytes.len() {
        return Err(Error::InvalidArgument("trailing bytes after graph program"));
    }
    Ok(ops)
}

fn blend_images(base: &RgbImage, layer: &RgbImage, opacity: f32) -> Result<RgbImage, Error> {
    if base.width != layer.width || base.height != layer.height {
        return Err(Error::Render(
            "blend inputs have mismatched dimensions".into(),
        ));
    }
    let mut output = base.clone();
    output
        .data
        .par_iter_mut()
        .zip(layer.data.par_iter())
        .for_each(|(value, other)| *value += (*other - *value) * opacity);
    Ok(output)
}

/// One branch's domain during execution.
#[derive(Clone, Copy, PartialEq)]
enum Domain {
    Linear,
    Display,
}

fn to_display(image: &mut RgbImage) {
    render::apply_display_transform(image, false);
}

pub(crate) struct GraphContext<'a> {
    pub(crate) make: &'a str,
    pub(crate) model: &'a str,
    pub(crate) lens_name: &'a str,
    pub(crate) focal: f32,
    pub(crate) explicit_profile: Option<&'a str>,
    pub(crate) focal_reducer: f32,
    pub(crate) crop_factor: f32,
    pub(crate) grain_seed: u64,
    pub(crate) orientation: u16,
}

/// Maps a crop rectangle from oriented display coordinates into the
/// unoriented sensor frame the executor works in. Rectangles are normalized
/// (left, top, width, height); the mapping inverts `render::orient`.
pub(crate) fn map_oriented_rect(orientation: u16, rect: [f32; 4]) -> [f32; 4] {
    let [left, top, width, height] = rect;
    match orientation {
        2 => [1.0 - left - width, top, width, height],
        3 => [1.0 - left - width, 1.0 - top - height, width, height],
        4 => [left, 1.0 - top - height, width, height],
        5 => [top, left, height, width],
        6 => [top, 1.0 - left - width, height, width],
        7 => [1.0 - top - height, 1.0 - left - width, height, width],
        8 => [1.0 - top - height, left, height, width],
        _ => rect,
    }
}

fn crop_image(image: &RgbImage, rect: [f32; 4]) -> Result<RgbImage, Error> {
    let [left, top, width, height] = rect;
    let x0 = ((left * image.width as f32).round() as usize).min(image.width - 1);
    let y0 = ((top * image.height as f32).round() as usize).min(image.height - 1);
    let target_width = (((width) * image.width as f32).round() as usize).clamp(1, image.width - x0);
    let target_height =
        (((height) * image.height as f32).round() as usize).clamp(1, image.height - y0);
    let mut data = Vec::with_capacity(target_width * target_height * 3);
    for row in 0..target_height {
        let start = ((y0 + row) * image.width + x0) * 3;
        data.extend_from_slice(&image.data[start..start + target_width * 3]);
    }
    Ok(RgbImage {
        width: target_width,
        height: target_height,
        data,
    })
}

/// Rotates the image by SENSOR-ANGLE degrees about the rectangle's center,
/// then extracts the rectangle — one bilinear sampling pass.
fn rotate_crop_image(
    image: &RgbImage,
    rect: [f32; 4],
    sensor_angle: f32,
) -> Result<RgbImage, Error> {
    let [left, top, width, height] = rect;
    let x0 = ((left * image.width as f32).round() as usize).min(image.width - 1);
    let y0 = ((top * image.height as f32).round() as usize).min(image.height - 1);
    let target_width = ((width * image.width as f32).round() as usize).clamp(1, image.width - x0);
    let target_height =
        ((height * image.height as f32).round() as usize).clamp(1, image.height - y0);
    let x0 = x0 as f32;
    let y0 = y0 as f32;
    let center_x = x0 + target_width as f32 * 0.5;
    let center_y = y0 + target_height as f32 * 0.5;
    let radians = sensor_angle.to_radians();
    let (sin, cos) = radians.sin_cos();
    let mut output = RgbImage {
        width: target_width,
        height: target_height,
        data: vec![0.0; target_width * target_height * 3],
    };
    output
        .data
        .par_chunks_mut(target_width * 3)
        .enumerate()
        .for_each(|(row, output_row)| {
            let offset_y = y0 + row as f32 + 0.5 - center_y;
            for (column, pixel) in output_row.as_chunks_mut::<3>().0.iter_mut().enumerate() {
                let offset_x = x0 + column as f32 + 0.5 - center_x;
                // Rotating the image by theta samples the source rotated the
                // other way around the crop center.
                let source_x = center_x + cos * offset_x + sin * offset_y - 0.5;
                let source_y = center_y - sin * offset_x + cos * offset_y - 0.5;
                for (channel, value) in pixel.iter_mut().enumerate() {
                    *value = render::bilinear(image, source_x, source_y, channel);
                }
            }
        });
    Ok(output)
}

/// Executes OPS over SOURCE, returning the final display-domain image.
///
/// The source and every intermediate stays scene-linear until either a film
/// node or the end of the graph converts its branch for display.
/// A saved register entering one op boundary, for interactive resumes.
struct PrefixSnapshot {
    register: usize,
    image: RgbImage,
    domain: Domain,
    oriented: bool,
}

/// The live registers captured at op boundaries of the last interactive
/// program: dragging one node re-executes only the ops downstream of it.
/// Two checkpoints are kept: the divergence point of the latest edit, and
/// the boundary after the last expensive stage, so switching from editing
/// an expensive node to a cheap downstream one stays instant too.
struct PrefixEntry {
    ops: Vec<GraphOp>,
    checkpoints: Vec<(usize, Vec<PrefixSnapshot>)>,
}

/// Cache key: input path, decoded dimensions, bounded render dimensions.
pub(crate) type PrefixKey = (String, usize, usize, u32, u32, usize);

const PREFIX_CACHE_CAPACITY: usize = 2;
const PREFIX_SNAPSHOT_LIMIT: usize = 3;

fn prefix_cache() -> &'static Mutex<Vec<(PrefixKey, PrefixEntry)>> {
    static CACHE: OnceLock<Mutex<Vec<(PrefixKey, PrefixEntry)>>> = OnceLock::new();
    CACHE.get_or_init(|| Mutex::new(Vec::new()))
}

fn common_prefix_length(previous: &[GraphOp], current: &[GraphOp]) -> usize {
    previous
        .iter()
        .zip(current)
        .take_while(|(before, after)| before == after)
        .count()
}

/// The default checkpoint: right after the last expensive stage, so edits
/// to the cheap grading tail never pay for optics or noise reduction again.
fn default_snapshot_boundary(ops: &[GraphOp]) -> usize {
    ops.iter()
        .rposition(|op| matches!(op.kind, NODE_OPTICS | NODE_NOISE_REDUCTION))
        .map(|index| index + 1)
        .unwrap_or(0)
}

#[cfg_attr(not(test), allow(dead_code))]
pub(crate) fn execute_graph(
    ops: &[GraphOp],
    source: RgbImage,
    context: &GraphContext<'_>,
) -> Result<RgbImage, Error> {
    execute_graph_cached(ops, source, context, None)
}

pub(crate) fn execute_graph_cached(
    ops: &[GraphOp],
    source: RgbImage,
    context: &GraphContext<'_>,
    cache_key: Option<PrefixKey>,
) -> Result<RgbImage, Error> {
    let count = ops.len();
    if count == 0 {
        let mut image = render::orient(source, context.orientation);
        to_display(&mut image);
        return Ok(image);
    }
    // Interactive resume: reuse the deepest checkpoint of the previous
    // program that still lies on this program's unchanged prefix.
    let mut resume: Option<(usize, Vec<PrefixSnapshot>)> = None;
    let mut capture_points: Vec<usize> = Vec::new();
    if let Some(key) = &cache_key {
        let mut cache = prefix_cache()
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let common = if let Some(position) = cache.iter().position(|(held, _)| held == key) {
            let entry = &cache[position].1;
            let common = common_prefix_length(&entry.ops, ops);
            if let Some((boundary, snapshots)) = entry
                .checkpoints
                .iter()
                .filter(|(boundary, _)| *boundary > 0 && *boundary <= common)
                .max_by_key(|(boundary, _)| *boundary)
            {
                resume = Some((
                    *boundary,
                    snapshots
                        .iter()
                        .map(|snapshot| PrefixSnapshot {
                            register: snapshot.register,
                            image: snapshot.image.clone(),
                            domain: snapshot.domain,
                            oriented: snapshot.oriented,
                        })
                        .collect(),
                ));
            }
            let held = cache.remove(position);
            cache.insert(0, held);
            Some(common)
        } else {
            None
        };
        // Checkpoint at the edit's divergence point for same-node drags,
        // and after the last expensive stage for downstream edits.
        match common {
            Some(common) if common < count => capture_points.push(common),
            _ => {}
        }
        capture_points.push(default_snapshot_boundary(ops));
        capture_points.retain(|point| *point > 0);
        capture_points.sort_unstable();
        capture_points.dedup();
    }
    let boundary = resume.as_ref().map(|(boundary, _)| *boundary).unwrap_or(0);
    capture_points.retain(|point| *point >= boundary);
    // Remaining reader counts cover only the ops actually executed, so
    // images still move instead of cloning on their last read.
    let mut uses = vec![0_usize; count + 1];
    for op in &ops[boundary..] {
        uses[op.input_a] += 1;
        if op.kind == NODE_BLEND {
            uses[op.input_b] += 1;
        }
    }
    uses[count] += 1; // The final node feeds the output.
    let mut registers: Vec<Option<RgbImage>> = (0..=count).map(|_| None).collect();
    let mut domains = vec![Domain::Linear; count + 1];
    let mut oriented = vec![false; count + 1];
    let mut film_ordinal = 0_u64;
    match resume {
        Some((_, snapshots)) => {
            for snapshot in snapshots {
                if uses[snapshot.register] > 0 {
                    registers[snapshot.register] = Some(snapshot.image);
                    domains[snapshot.register] = snapshot.domain;
                    oriented[snapshot.register] = snapshot.oriented;
                }
            }
            if uses[0] > 0 && registers[0].is_none() {
                registers[0] = Some(source);
            }
            film_ordinal = ops[..boundary]
                .iter()
                .filter(|op| op.kind == NODE_FILM)
                .count() as u64;
        }
        None => registers[0] = Some(source),
    }
    let profiling = std::env::var_os("ORFEUS_PROFILE").is_some();
    let mut captured: Vec<(usize, Vec<PrefixSnapshot>)> = Vec::new();
    for (index, op) in ops.iter().enumerate().skip(boundary) {
        let node_started = profiling.then(std::time::Instant::now);
        if capture_points.contains(&index)
            && let Some(snapshots) = collect_snapshots(&registers, &domains, &oriented)
        {
            captured.push((index, snapshots));
        }
        let slot = index + 1;
        let take = |registers: &mut Vec<Option<RgbImage>>,
                    uses: &mut Vec<usize>,
                    from: usize|
         -> Result<RgbImage, Error> {
            let image = if uses[from] <= 1 {
                registers[from]
                    .take()
                    .ok_or(Error::Render("graph register was consumed twice".into()))?
            } else {
                registers[from]
                    .as_ref()
                    .ok_or(Error::Render("graph register was consumed twice".into()))?
                    .clone()
            };
            uses[from] -= 1;
            Ok(image)
        };
        let mut image = take(&mut registers, &mut uses, op.input_a)?;
        domains[slot] = domains[op.input_a];
        oriented[slot] = oriented[op.input_a];
        if matches!(
            op.kind,
            NODE_NOISE_REDUCTION
                | NODE_TONE
                | NODE_FILM
                | NODE_COLOR_SUBTRACT
                | NODE_CROP
                | NODE_CURVES
        ) && !oriented[slot]
        {
            image = render::orient(image, context.orientation);
            oriented[slot] = true;
        }
        match op.kind {
            NODE_WHITE_BALANCE => {
                render::apply_white_adaptation(&mut image, op.params[0], op.params[1]);
            }
            NODE_EXPOSURE => {
                render::apply_exposure(&mut image, op.params[0]);
            }
            NODE_NOISE_REDUCTION => {
                let edge = op.params[0];
                render::apply_noise_reduction(&mut image, 0.2 * edge, edge);
                super::nn::apply_neural_noise_reduction(
                    &mut image.data,
                    image.width,
                    image.height,
                    op.params[1],
                )?;
            }
            NODE_TONE => {
                let adjustments: [f32; 7] = op.params[..7]
                    .try_into()
                    .expect("parameter count was validated");
                render::apply_tonal_equalizer(&mut image, adjustments);
            }
            NODE_OPTICS => {
                let mut flags = 0;
                if op.params[0] != 0.0 {
                    flags |= render::FLAG_LENS_DISTORTION;
                }
                if op.params[2] != 0.0 {
                    flags |= render::FLAG_LENS_TCA;
                }
                if flags != 0 {
                    render::apply_lens(
                        &mut image,
                        &LensCorrectionOptions {
                            make: context.make,
                            model: context.model,
                            lens_name: context.lens_name,
                            focal: context.focal,
                            flags,
                            strength: op.params[1],
                            explicit_profile: context.explicit_profile,
                            focal_reducer: context.focal_reducer,
                            crop_factor: context.crop_factor,
                        },
                    )?;
                }
            }
            NODE_FILM => {
                if domains[slot] == Domain::Linear {
                    to_display(&mut image);
                    domains[slot] = Domain::Display;
                }
                if op.params[0] > 0.0
                    && let Some(path) = &op.text
                {
                    let lut = render::CubeLut::read(Path::new(path))?;
                    render::apply_lut(&mut image, &lut, op.params[0]);
                }
                if op.params[1] > 0.0 {
                    render::apply_grain(
                        &mut image,
                        op.params[1],
                        op.params[2],
                        context.grain_seed ^ film_ordinal.wrapping_mul(0x9e37_79b9),
                    );
                    film_ordinal += 1;
                }
            }
            NODE_BLEND => {
                let mut other = take(&mut registers, &mut uses, op.input_b)?;
                let other_oriented = oriented[op.input_b];
                if oriented[slot] != other_oriented {
                    if oriented[slot] {
                        other = render::orient(other, context.orientation);
                    } else {
                        image = render::orient(image, context.orientation);
                        oriented[slot] = true;
                    }
                }
                image = blend_images(&image, &other, op.params[0])?;
            }
            NODE_COLOR_SUBTRACT => {
                // Picked color minus pixel, per channel, deliberately
                // unclamped: this is the scene-linear negative inversion.
                let color = [op.params[0], op.params[1], op.params[2]];
                image.data.par_chunks_exact_mut(3).for_each(|pixel| {
                    for (value, base) in pixel.iter_mut().zip(color) {
                        *value = base - *value;
                    }
                });
            }
            NODE_CURVES => {
                // Per-channel monotone curves on the encoded signal: the
                // film-stock "decompression" for inverted negatives.
                let luts = [
                    curve_lut(&op.params[0..8]),
                    curve_lut(&op.params[8..16]),
                    curve_lut(&op.params[16..24]),
                ];
                image.data.par_chunks_exact_mut(3).for_each(|pixel| {
                    for (value, lut) in pixel.iter_mut().zip(&luts) {
                        *value = apply_curve_value(lut, *value);
                    }
                });
            }
            NODE_CROP => {
                let rect = [op.params[0], op.params[1], op.params[2], op.params[3]];
                let angle = op.params[4];
                if angle.abs() < 0.01 {
                    image = crop_image(&image, rect)?;
                } else {
                    image = rotate_crop_image(&image, rect, angle)?;
                }
            }
            _ => unreachable!("kinds were validated during parsing"),
        }
        if let Some(started) = node_started {
            eprintln!(
                "orfeus-profile node={} kind={} pixels={} milliseconds={:.3}",
                slot,
                op.kind,
                image.width * image.height,
                started.elapsed().as_secs_f64() * 1000.0
            );
        }
        registers[slot] = Some(image);
    }
    if capture_points.contains(&count)
        && let Some(snapshots) = collect_snapshots(&registers, &domains, &oriented)
    {
        captured.push((count, snapshots));
    }
    if let (Some(key), false) = (&cache_key, captured.is_empty()) {
        let entry = PrefixEntry {
            ops: ops.to_vec(),
            checkpoints: captured,
        };
        let mut cache = prefix_cache()
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        cache.retain(|(held, _)| held != key);
        cache.insert(0, (key.clone(), entry));
        cache.truncate(PREFIX_CACHE_CAPACITY);
    }
    let tail_started = profiling.then(std::time::Instant::now);
    let mut image = registers[count]
        .take()
        .ok_or(Error::Render("graph produced no output".into()))?;
    if !oriented[count] {
        image = render::orient(image, context.orientation);
    }
    if domains[count] == Domain::Linear {
        to_display(&mut image);
    }
    if let Some(started) = tail_started {
        eprintln!(
            "orfeus-profile graph-tail resumed-from={} milliseconds={:.3}",
            boundary,
            started.elapsed().as_secs_f64() * 1000.0
        );
    }
    Ok(image)
}

/// Clones every live register: one checkpoint's worth of resume state.
fn collect_snapshots(
    registers: &[Option<RgbImage>],
    domains: &[Domain],
    oriented: &[bool],
) -> Option<Vec<PrefixSnapshot>> {
    let mut snapshots = Vec::new();
    for (register, image) in registers.iter().enumerate() {
        if let Some(image) = image {
            snapshots.push(PrefixSnapshot {
                register,
                image: image.clone(),
                domain: domains[register],
                oriented: oriented[register],
            });
            if snapshots.len() > PREFIX_SNAPSHOT_LIMIT {
                return None; // Degenerate fan-outs are not worth the memory.
            }
        }
    }
    if snapshots.is_empty() {
        None
    } else {
        Some(snapshots)
    }
}

/// Test hook: rotate-crop with display-convention angle at orientation 1.
#[cfg(test)]
pub(crate) fn rotate_crop_for_tests(image: &RgbImage, rect: [f32; 4], angle: f32) -> RgbImage {
    if angle.abs() < 0.01 {
        crop_image(image, rect).expect("test crop")
    } else {
        rotate_crop_image(image, rect, angle).expect("test rotate crop")
    }
}

/// Renders INPUT through a serialized graph program to OUTPUT.
pub fn render_graph(
    input: &Path,
    output: &Path,
    frame: &RenderFrameV1,
    graph_bytes: &[u8],
    cache_mode: u32,
) -> Result<(), Error> {
    if render::same_file(input, output)? {
        return Err(Error::InvalidArgument(
            "input and output refer to the same file",
        ));
    }
    let image = render_graph_image(input, frame, graph_bytes, cache_mode)?;
    render::atomic_encode(
        input,
        output,
        &image,
        frame.output_format,
        frame.jpeg_quality,
    )
}

/// Renders straight into a caller-provided RGB8 buffer: the zero-copy hot
/// path for live previews, with no JPEG or file round trip.
pub fn render_graph_rgb(
    input: &Path,
    frame: &RenderFrameV1,
    graph_bytes: &[u8],
    buffer: &mut [u8],
    cache_mode: u32,
) -> Result<(usize, usize), Error> {
    let image = render_graph_image(input, frame, graph_bytes, cache_mode)?;
    let needed = image.width * image.height * 3;
    if buffer.len() < needed {
        return Err(Error::InvalidArgument("preview buffer is too small"));
    }
    buffer[..needed]
        .par_chunks_mut(1 << 16)
        .zip(image.data.par_chunks(1 << 16))
        .for_each(|(bytes, values)| {
            for (byte, value) in bytes.iter_mut().zip(values) {
                *byte = (value.clamp(0.0, 1.0) * 255.0 + 0.5) as u8;
            }
        });
    Ok((image.width, image.height))
}

fn render_graph_image(
    input: &Path,
    frame: &RenderFrameV1,
    graph_bytes: &[u8],
    cache_mode: u32,
) -> Result<RgbImage, Error> {
    frame.validate()?;
    let ops = parse_graph(graph_bytes)?;
    if !matches!(cache_mode, render::CACHE_NONE | render::CACHE_USE) {
        return Err(Error::InvalidArgument("unsupported decode cache mode"));
    }
    let profiling = std::env::var_os("ORFEUS_PROFILE").is_some();
    let needs_neural = ops
        .iter()
        .any(|op| op.kind == NODE_NOISE_REDUCTION && op.params[1] > 0.0);
    let _neural_render_guard = if needs_neural {
        Some(
            render::neural_render_lock()
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner()),
        )
    } else {
        None
    };
    let decoded: Arc<DecodedRaw> = render::decoded_for_render(input, cache_mode, profiling)?;
    let (native_max_width, native_max_height) =
        render::native_downscale_bounds(decoded.orientation, frame.max_width, frame.max_height);
    let source = render::scaled_source_for_render(
        &decoded,
        input,
        native_max_width,
        native_max_height,
        cache_mode,
    );
    let explicit_profile = if frame.lens_profile_model.is_null() {
        None
    } else {
        Some(
            unsafe { CStr::from_ptr(frame.lens_profile_model) }
                .to_str()
                .map_err(|_| Error::InvalidArgument("lens profile model is not UTF-8"))?,
        )
    };
    let context = GraphContext {
        make: &decoded.make,
        model: &decoded.model,
        lens_name: &decoded.lens_name,
        focal: decoded.focal,
        explicit_profile,
        focal_reducer: frame.focal_reducer,
        crop_factor: frame.lens_crop_factor,
        grain_seed: frame.grain_seed,
        orientation: decoded.orientation,
    };
    let bounded = native_max_width > 0 || native_max_height > 0;
    let prefix_key = (cache_mode == render::CACHE_USE && bounded).then(|| {
        (
            input.to_string_lossy().into_owned(),
            decoded.width,
            decoded.height,
            native_max_width,
            native_max_height,
            // A re-decoded file gets a fresh Arc, invalidating stale entries.
            Arc::as_ptr(&decoded) as usize,
        )
    });
    execute_graph_cached(&ops, source, &context, prefix_key)
}

#[cfg(test)]
mod tests {
    use super::*;

    pub(crate) struct GraphBuilder {
        bytes: Vec<u8>,
        count: u32,
    }

    impl GraphBuilder {
        pub(crate) fn new() -> Self {
            Self {
                bytes: Vec::new(),
                count: 0,
            }
        }

        pub(crate) fn node(
            mut self,
            kind: u32,
            input_a: i32,
            input_b: i32,
            params: &[f32],
            text: Option<&str>,
        ) -> Self {
            self.count += 1;
            self.bytes.extend_from_slice(&kind.to_le_bytes());
            self.bytes.extend_from_slice(&input_a.to_le_bytes());
            self.bytes.extend_from_slice(&input_b.to_le_bytes());
            self.bytes
                .extend_from_slice(&(params.len() as u32).to_le_bytes());
            for parameter in params {
                self.bytes
                    .extend_from_slice(&parameter.to_bits().to_le_bytes());
            }
            match text {
                Some(text) => {
                    self.bytes
                        .extend_from_slice(&(text.len() as u32).to_le_bytes());
                    self.bytes.extend_from_slice(text.as_bytes());
                }
                None => self.bytes.extend_from_slice(&0_u32.to_le_bytes()),
            }
            self
        }

        pub(crate) fn build(self) -> Vec<u8> {
            let mut result = Vec::new();
            result.extend_from_slice(&GRAPH_MAGIC.to_le_bytes());
            result.extend_from_slice(&GRAPH_VERSION.to_le_bytes());
            result.extend_from_slice(&self.count.to_le_bytes());
            result.extend_from_slice(&self.bytes);
            result
        }
    }

    fn context(seed: u64) -> GraphContext<'static> {
        GraphContext {
            make: "",
            model: "",
            lens_name: "",
            focal: 0.0,
            explicit_profile: None,
            focal_reducer: 1.0,
            crop_factor: 0.0,
            grain_seed: seed,
            orientation: 1,
        }
    }

    fn gradient_image() -> RgbImage {
        let width = 24;
        let height = 16;
        let mut data = Vec::with_capacity(width * height * 3);
        for y in 0..height {
            for x in 0..width {
                let value = 0.05 + 0.9 * (x + y * width) as f32 / (width * height) as f32;
                data.extend_from_slice(&[value, value * 0.8, value * 0.6]);
            }
        }
        RgbImage {
            width,
            height,
            data,
        }
    }

    fn max_difference(first: &RgbImage, second: &RgbImage) -> f32 {
        first
            .data
            .iter()
            .zip(&second.data)
            .map(|(a, b)| (a - b).abs())
            .fold(0.0, f32::max)
    }

    #[test]
    fn graph_chain_matches_manually_chained_stages() {
        let ops = parse_graph(
            &GraphBuilder::new()
                .node(NODE_WHITE_BALANCE, 0, -1, &[6500.0, 3.0], None)
                .node(NODE_EXPOSURE, 1, -1, &[0.5], None)
                .node(
                    NODE_TONE,
                    2,
                    -1,
                    &[0.3, 0.0, 0.0, -0.2, 0.0, 0.0, 0.1],
                    None,
                )
                .build(),
        )
        .unwrap();
        let via_graph = execute_graph(&ops, gradient_image(), &context(7)).unwrap();

        let mut reference = gradient_image();
        render::apply_white_adaptation(&mut reference, 6500.0, 3.0);
        render::apply_exposure(&mut reference, 0.5);
        render::apply_tonal_equalizer(&mut reference, [0.3, 0.0, 0.0, -0.2, 0.0, 0.0, 0.1]);
        to_display(&mut reference);
        assert!(max_difference(&via_graph, &reference) < 1.0e-6);
    }

    #[test]
    fn graph_uses_flat_pipeline_orientation_and_first_grain_seed() {
        let ops = parse_graph(
            &GraphBuilder::new()
                .node(NODE_WHITE_BALANCE, 0, -1, &[6500.0, 3.0], None)
                .node(NODE_EXPOSURE, 1, -1, &[0.5], None)
                .node(NODE_NOISE_REDUCTION, 2, -1, &[0.0, 0.0], None)
                .node(
                    NODE_TONE,
                    3,
                    -1,
                    &[0.3, 0.0, 0.0, -0.2, 0.0, 0.0, 0.1],
                    None,
                )
                .node(NODE_FILM, 4, -1, &[0.0, 0.25, 1.0], None)
                .build(),
        )
        .unwrap();
        let mut graph_context = context(17);
        graph_context.orientation = 6;
        let via_graph = execute_graph(&ops, gradient_image(), &graph_context).unwrap();

        let mut reference = gradient_image();
        render::apply_white_adaptation(&mut reference, 6500.0, 3.0);
        render::apply_exposure(&mut reference, 0.5);
        reference = render::orient(reference, 6);
        render::apply_noise_reduction(&mut reference, 0.0, 0.0);
        super::super::nn::apply_neural_noise_reduction(
            &mut reference.data,
            reference.width,
            reference.height,
            0.0,
        )
        .unwrap();
        render::apply_tonal_equalizer(&mut reference, [0.3, 0.0, 0.0, -0.2, 0.0, 0.0, 0.1]);
        to_display(&mut reference);
        render::apply_grain(&mut reference, 0.25, 1.0, 17);

        assert_eq!(via_graph.width, reference.width);
        assert_eq!(via_graph.height, reference.height);
        assert!(max_difference(&via_graph, &reference) < 1.0e-6);
    }

    #[test]
    fn first_film_seed_does_not_depend_on_node_slot() {
        let direct = parse_graph(
            &GraphBuilder::new()
                .node(NODE_FILM, 0, -1, &[0.0, 0.25, 1.0], None)
                .build(),
        )
        .unwrap();
        let preceded = parse_graph(
            &GraphBuilder::new()
                .node(NODE_EXPOSURE, 0, -1, &[0.0], None)
                .node(NODE_FILM, 1, -1, &[0.0, 0.25, 1.0], None)
                .build(),
        )
        .unwrap();
        let direct = execute_graph(&direct, gradient_image(), &context(23)).unwrap();
        let preceded = execute_graph(&preceded, gradient_image(), &context(23)).unwrap();
        assert_eq!(direct.data, preceded.data);
    }

    #[test]
    fn inert_film_does_not_advance_the_grain_seed() {
        let direct = parse_graph(
            &GraphBuilder::new()
                .node(NODE_FILM, 0, -1, &[0.0, 0.25, 1.0], None)
                .build(),
        )
        .unwrap();
        let preceded = parse_graph(
            &GraphBuilder::new()
                .node(NODE_FILM, 0, -1, &[0.0, 0.0, 1.0], None)
                .node(NODE_FILM, 1, -1, &[0.0, 0.25, 1.0], None)
                .build(),
        )
        .unwrap();
        let direct = execute_graph(&direct, gradient_image(), &context(23)).unwrap();
        let preceded = execute_graph(&preceded, gradient_image(), &context(23)).unwrap();
        assert_eq!(direct.data, preceded.data);
    }

    #[test]
    fn rotated_crop_clamps_dimensions_at_the_source_bounds() {
        let cropped = rotate_crop_image(&gradient_image(), [0.9, 0.9, 0.2, 0.2], 3.0).unwrap();
        assert_eq!((cropped.width, cropped.height), (2, 2));
    }

    #[test]
    fn repeated_nodes_of_one_kind_compose() {
        // Two consecutive +2 EV exposure nodes must equal one +4 EV node.
        let stacked = execute_graph(
            &parse_graph(
                &GraphBuilder::new()
                    .node(NODE_EXPOSURE, 0, -1, &[2.0], None)
                    .node(NODE_EXPOSURE, 1, -1, &[2.0], None)
                    .build(),
            )
            .unwrap(),
            gradient_image(),
            &context(7),
        )
        .unwrap();
        let single = execute_graph(
            &parse_graph(
                &GraphBuilder::new()
                    .node(NODE_EXPOSURE, 0, -1, &[4.0], None)
                    .build(),
            )
            .unwrap(),
            gradient_image(),
            &context(7),
        )
        .unwrap();
        assert!(max_difference(&stacked, &single) < 1.0e-6);
    }

    #[test]
    fn blend_mixes_branches_by_opacity() {
        let ops = parse_graph(
            &GraphBuilder::new()
                .node(NODE_EXPOSURE, 0, -1, &[1.0], None)
                .node(NODE_BLEND, 0, 1, &[0.25], None)
                .build(),
        )
        .unwrap();
        let blended = execute_graph(&ops, gradient_image(), &context(7)).unwrap();

        // 25% toward one stop brighter equals a uniform linear gain of 1.25.
        let mut reference = gradient_image();
        for value in &mut reference.data {
            *value *= 1.25;
        }
        to_display(&mut reference);
        assert!(max_difference(&blended, &reference) < 1.0e-5);
    }

    #[test]
    fn empty_graph_passes_the_source_to_display() {
        let via_graph = execute_graph(&[], gradient_image(), &context(1)).unwrap();
        let mut reference = gradient_image();
        to_display(&mut reference);
        assert!(max_difference(&via_graph, &reference) < 1.0e-6);
    }

    #[test]
    fn film_after_film_is_legal_but_tone_after_film_is_not() {
        assert!(
            parse_graph(
                &GraphBuilder::new()
                    .node(NODE_FILM, 0, -1, &[0.0, 0.3, 1.0], None)
                    .node(NODE_FILM, 1, -1, &[0.0, 0.1, 2.0], None)
                    .build(),
            )
            .is_ok()
        );
        assert!(matches!(
            parse_graph(
                &GraphBuilder::new()
                    .node(NODE_FILM, 0, -1, &[0.0, 0.3, 1.0], None)
                    .node(NODE_TONE, 1, -1, &[0.0; 7], None)
                    .build(),
            ),
            Err(Error::InvalidArgument(_))
        ));
        assert!(matches!(
            parse_graph(
                &GraphBuilder::new()
                    .node(NODE_FILM, 0, -1, &[0.0, 0.3, 1.0], None)
                    .node(NODE_BLEND, 0, 1, &[0.5], None)
                    .build(),
            ),
            Err(Error::InvalidArgument(_))
        ));
    }

    #[test]
    fn optics_after_crop_is_rejected() {
        assert!(matches!(
            parse_graph(
                &GraphBuilder::new()
                    .node(NODE_CROP, 0, -1, &[0.1, 0.1, 0.8, 0.8, 0.0], None)
                    .node(NODE_OPTICS, 1, -1, &[1.0, 1.0, 1.0], None)
                    .build(),
            ),
            Err(Error::InvalidArgument(_))
        ));
    }

    #[test]
    fn parser_rejects_malformed_programs() {
        assert!(parse_graph(b"junk").is_err());
        // Forward reference.
        assert!(
            parse_graph(
                &GraphBuilder::new()
                    .node(NODE_EXPOSURE, 3, -1, &[0.5], None)
                    .build(),
            )
            .is_err()
        );
        // Bad parameter count.
        assert!(
            parse_graph(
                &GraphBuilder::new()
                    .node(NODE_TONE, 0, -1, &[0.0; 3], None)
                    .build(),
            )
            .is_err()
        );
        // Out-of-range opacity.
        assert!(
            parse_graph(
                &GraphBuilder::new()
                    .node(NODE_BLEND, 0, 0, &[1.5], None)
                    .build(),
            )
            .is_err()
        );
        // Strings on non-film nodes.
        assert!(
            parse_graph(
                &GraphBuilder::new()
                    .node(NODE_EXPOSURE, 0, -1, &[0.5], Some("nope"))
                    .build(),
            )
            .is_err()
        );
        // Trailing bytes.
        let mut program = GraphBuilder::new()
            .node(NODE_EXPOSURE, 0, -1, &[0.5], None)
            .build();
        program.push(0);
        assert!(parse_graph(&program).is_err());
    }

    #[test]
    fn color_subtract_inverts_against_the_picked_base() {
        let ops = parse_graph(
            &GraphBuilder::new()
                .node(NODE_COLOR_SUBTRACT, 0, -1, &[0.9, 0.8, 0.7], None)
                .build(),
        )
        .unwrap();
        let source = gradient_image();
        let inverted = execute_graph(&ops, source.clone(), &context(7)).unwrap();
        // Verify in linear space by comparing against a manual inversion.
        let mut reference = source;
        for pixel in reference.data.as_chunks_mut::<3>().0 {
            pixel[0] = 0.9 - pixel[0];
            pixel[1] = 0.8 - pixel[1];
            pixel[2] = 0.7 - pixel[2];
        }
        to_display(&mut reference);
        assert!(max_difference(&inverted, &reference) < 1.0e-6);
    }

    #[test]
    fn crop_extracts_the_oriented_rectangle() {
        let ops = parse_graph(
            &GraphBuilder::new()
                .node(NODE_CROP, 0, -1, &[0.25, 0.25, 0.5, 0.5, 0.0], None)
                .build(),
        )
        .unwrap();
        let cropped = execute_graph(&ops, gradient_image(), &context(7)).unwrap();
        assert_eq!((cropped.width, cropped.height), (12, 8));

        // Under orientation 6 the displayed frame is the transposed sensor,
        // so a display-left crop must come from the sensor's bottom band.
        let rect = map_oriented_rect(6, [0.0, 0.0, 0.5, 1.0]);
        assert_eq!(rect, [0.0, 0.5, 1.0, 0.5]);
        let rect = map_oriented_rect(8, [0.0, 0.0, 0.5, 1.0]);
        assert_eq!(rect, [0.0, 0.0, 1.0, 0.5]);
        let rect = map_oriented_rect(3, [0.1, 0.2, 0.3, 0.4]);
        assert!((rect[0] - 0.6).abs() < 1.0e-6 && (rect[1] - 0.4).abs() < 1.0e-6);
    }

    #[test]
    fn crop_rotation_straightens_and_zero_angle_matches_the_fast_path() {
        let source = gradient_image();
        let rect = [0.25_f32, 0.25, 0.5, 0.5];
        let fast = crop_image(&source, rect).unwrap();
        let rotated_zero = rotate_crop_image(&source, rect, 0.0).unwrap();
        assert_eq!(fast.width, rotated_zero.width);
        assert_eq!(fast.height, rotated_zero.height);
        assert!(max_difference(&fast, &rotated_zero) < 1.0e-6);

        let tilted = rotate_crop_image(&source, rect, 10.0).unwrap();
        assert_eq!((tilted.width, tilted.height), (fast.width, fast.height));
        assert!(max_difference(&fast, &tilted) > 1.0e-3);
        assert!(tilted.data.iter().all(|value| value.is_finite()));
        // Opposite angles differ from each other too.
        let counter = rotate_crop_image(&source, rect, -10.0).unwrap();
        assert!(max_difference(&tilted, &counter) > 1.0e-3);
    }

    const IDENTITY_CURVE: [f32; 8] = [
        0.0,
        0.0,
        1.0 / 3.0,
        1.0 / 3.0,
        2.0 / 3.0,
        2.0 / 3.0,
        1.0,
        1.0,
    ];

    fn curves_params(red: &[f32; 8], green: &[f32; 8], blue: &[f32; 8]) -> Vec<f32> {
        let mut params = Vec::with_capacity(24);
        params.extend_from_slice(red);
        params.extend_from_slice(green);
        params.extend_from_slice(blue);
        params
    }

    #[test]
    fn identity_curves_pass_the_image_through() {
        let params = curves_params(&IDENTITY_CURVE, &IDENTITY_CURVE, &IDENTITY_CURVE);
        let ops = parse_graph(
            &GraphBuilder::new()
                .node(NODE_CURVES, 0, -1, &params, None)
                .build(),
        )
        .unwrap();
        let source = gradient_image();
        let curved = execute_graph(&ops, source.clone(), &context(7)).unwrap();
        let mut reference = source;
        to_display(&mut reference);
        assert!(max_difference(&curved, &reference) < 3.0e-3);
    }

    #[test]
    fn curve_luts_lift_midtones_and_keep_the_identity() {
        // Channel independence holds in linear space; the shared display
        // tone couples channels afterwards, so assert on the LUT itself.
        let red = [0.0, 0.0, 1.0 / 3.0, 0.55, 2.0 / 3.0, 0.85, 1.0, 1.0];
        let red_lut = curve_lut(&red);
        let identity_lut = curve_lut(&IDENTITY_CURVE);
        for step in 0..=20 {
            let value = step as f32 / 20.0;
            let unchanged = apply_curve_value(&identity_lut, value);
            assert!(
                (unchanged - value).abs() < 2.0e-3,
                "identity drift at {value}: {unchanged}"
            );
            if (0.05..=0.6).contains(&value) {
                let lifted = apply_curve_value(&red_lut, value);
                assert!(
                    lifted > unchanged + 0.01,
                    "no lift at {value}: {lifted} vs {unchanged}"
                );
            }
        }
        // Out-of-range values ride through continuously.
        assert!(apply_curve_value(&identity_lut, -0.25) < 0.0);
        assert!(apply_curve_value(&identity_lut, 1.5) > 1.4);
    }

    fn tone_program(shadows: f32) -> Vec<u8> {
        GraphBuilder::new()
            .node(NODE_EXPOSURE, 0, -1, &[0.4], None)
            .node(
                NODE_TONE,
                1,
                -1,
                &[0.0, shadows, 0.0, 0.0, 0.0, 0.0, 0.0],
                None,
            )
            .node(NODE_BLEND, 2, 0, &[0.6], None)
            .build()
    }

    #[test]
    fn prefix_resume_matches_a_cold_render_exactly() {
        let key: PrefixKey = ("prefix-test".into(), 24, 16, 24, 16, 1);
        {
            let mut cache = prefix_cache()
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner());
            cache.retain(|(held, _)| held.0 != "prefix-test");
        }
        // First render primes the checkpoint; the tone edit then resumes
        // mid-program, with the blend pulling the source across the
        // boundary. The result must be identical to an uncached render.
        let first = parse_graph(&tone_program(0.3)).unwrap();
        let second = parse_graph(&tone_program(0.7)).unwrap();
        let source = gradient_image();
        execute_graph_cached(&first, source.clone(), &context(7), Some(key.clone())).unwrap();
        let resumed =
            execute_graph_cached(&second, source.clone(), &context(7), Some(key.clone())).unwrap();
        let cold = execute_graph(&second, source.clone(), &context(7)).unwrap();
        assert_eq!(resumed.width, cold.width);
        assert_eq!(resumed.height, cold.height);
        assert!(max_difference(&resumed, &cold) == 0.0);
        // A third tweak resumes from the refreshed checkpoint.
        let third = parse_graph(&tone_program(0.9)).unwrap();
        let resumed = execute_graph_cached(&third, source.clone(), &context(7), Some(key)).unwrap();
        let cold = execute_graph(&third, source, &context(7)).unwrap();
        assert!(max_difference(&resumed, &cold) == 0.0);
    }

    #[test]
    fn prefix_cache_keys_do_not_leak_across_photos() {
        let key_a: PrefixKey = ("photo-a".into(), 24, 16, 24, 16, 1);
        let key_b: PrefixKey = ("photo-b".into(), 24, 16, 24, 16, 1);
        let ops = parse_graph(&tone_program(0.5)).unwrap();
        let bright = {
            let mut image = gradient_image();
            for value in &mut image.data {
                *value *= 2.0;
            }
            image
        };
        execute_graph_cached(&ops, gradient_image(), &context(7), Some(key_a)).unwrap();
        // Rendering photo B with its own key must not reuse A's registers.
        let edited = parse_graph(&tone_program(0.8)).unwrap();
        let via_cache =
            execute_graph_cached(&edited, bright.clone(), &context(7), Some(key_b)).unwrap();
        let cold = execute_graph(&edited, bright, &context(7)).unwrap();
        assert!(max_difference(&via_cache, &cold) == 0.0);
    }

    #[test]
    fn curves_reject_unsorted_points() {
        let bad = [0.0, 0.0, 0.6, 0.5, 0.3, 0.7, 1.0, 1.0];
        let params = curves_params(&bad, &IDENTITY_CURVE, &IDENTITY_CURVE);
        assert!(
            parse_graph(
                &GraphBuilder::new()
                    .node(NODE_CURVES, 0, -1, &params, None)
                    .build(),
            )
            .is_err()
        );
    }

    #[test]
    fn crop_keeps_film_domain_and_blend_rejects_mismatched_sizes() {
        // film -> crop -> film stays legal.
        assert!(
            parse_graph(
                &GraphBuilder::new()
                    .node(NODE_FILM, 0, -1, &[0.0, 0.2, 1.0], None)
                    .node(NODE_CROP, 1, -1, &[0.0, 0.0, 0.5, 0.5, 0.0], None)
                    .node(NODE_FILM, 2, -1, &[0.0, 0.1, 1.0], None)
                    .build(),
            )
            .is_ok()
        );
        // film -> crop -> tone must stay illegal.
        assert!(
            parse_graph(
                &GraphBuilder::new()
                    .node(NODE_FILM, 0, -1, &[0.0, 0.2, 1.0], None)
                    .node(NODE_CROP, 1, -1, &[0.0, 0.0, 0.5, 0.5, 0.0], None)
                    .node(NODE_TONE, 2, -1, &[0.0; 7], None)
                    .build(),
            )
            .is_err()
        );
        // A crop leaving the frame is rejected at parse time.
        assert!(
            parse_graph(
                &GraphBuilder::new()
                    .node(NODE_CROP, 0, -1, &[0.75, 0.0, 0.5, 1.0, 0.0], None)
                    .build(),
            )
            .is_err()
        );
        // Blending a cropped branch against the full frame errors clearly.
        let ops = parse_graph(
            &GraphBuilder::new()
                .node(NODE_CROP, 0, -1, &[0.0, 0.0, 0.5, 0.5, 0.0], None)
                .node(NODE_BLEND, 0, 1, &[0.5], None)
                .build(),
        )
        .unwrap();
        assert!(matches!(
            execute_graph(&ops, gradient_image(), &context(7)),
            Err(Error::Render(_))
        ));
    }

    #[test]
    fn film_grain_lands_in_display_space_with_per_node_seeds() {
        let ops = parse_graph(
            &GraphBuilder::new()
                .node(NODE_FILM, 0, -1, &[0.0, 0.5, 1.0], None)
                .node(NODE_FILM, 1, -1, &[0.0, 0.25, 2.0], None)
                .build(),
        )
        .unwrap();
        let grained = execute_graph(&ops, gradient_image(), &context(11)).unwrap();
        let plain = execute_graph(&[], gradient_image(), &context(11)).unwrap();
        assert!(max_difference(&grained, &plain) > 0.005);
        assert!(grained.data.iter().all(|value| (0.0..=1.0).contains(value)));

        let mut reference = gradient_image();
        to_display(&mut reference);
        render::apply_grain(&mut reference, 0.5, 1.0, 11);
        render::apply_grain(&mut reference, 0.25, 2.0, 11 ^ 0x9e37_79b9);
        assert_eq!(grained.data, reference.data);

        let repeat = execute_graph(&ops, gradient_image(), &context(11)).unwrap();
        assert_eq!(grained.data, repeat.data, "grain must stay deterministic");
    }
}
