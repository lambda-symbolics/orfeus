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
use std::time::Instant;

use rayon::prelude::*;

use super::Error;
use super::render::{self, DecodedRaw, LensCorrectionOptions, RgbImage};

const GRAPH_MAGIC: u32 = 0x4746_524F; // "ORFG" little-endian
// 3 made each curves channel variable length behind a count header;
// 4 added the rotate node.
const GRAPH_VERSION: u32 = 5;
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
pub const NODE_ROTATE: u32 = 11;
pub const NODE_CONTRAST: u32 = 12;
pub const NODE_SHARPEN: u32 = 13;

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
    /// See `FRAME_FLAG_DRAFT`. Appended, so `struct_size` keeps older callers
    /// working; anything that does not set it gets full-quality development.
    pub flags: u32,
    /// The lens to correct for when the container names none of its own.
    pub lens_name: *const c_char,
    /// The part of the frame to develop, as (left, top, width, height) in
    /// fractions of the oriented frame, or all zeros for the whole of it.
    ///
    /// A zoomed-in preview only shows a fraction of the frame, and developing
    /// the rest of it is the largest waste left in the interactive path: at four
    /// times fit, a 1400-pixel canvas was having 5600 pixels of frame denoised,
    /// tone-mapped and encoded so that 1400 of them could be looked at. Naming
    /// the region turns that back into the work it should have been.
    pub viewport: [f32; 4],
}

/// Develop at half resolution, binning each sensor quad instead of
/// interpolating a colour for every photosite.
///
/// Set by the caller rather than inferred from which entry point was used: a
/// preview is a preview whether it lands in a buffer or in a cache file, and an
/// export is an export even when it asks for a small image.
pub const FRAME_FLAG_DRAFT: u32 = 1;

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

/// How many parameters a node kind carries.
///
/// Every kind but one has a fixed count. A curves node's channels each hold
/// between two and MAX_CURVE_POINTS control points, so its length is only known
/// from the counts in its own header.
enum ParamArity {
    Exact(usize),
    Curves,
}

/// Control points per curve channel, at least the two endpoints.
const MIN_CURVE_POINTS: usize = 2;
const MAX_CURVE_POINTS: usize = 16;
const CURVE_CHANNELS: usize = 4;
const MAX_CURVE_PARAMS: usize = CURVE_CHANNELS + CURVE_CHANNELS * MAX_CURVE_POINTS * 2;

fn param_arity(kind: u32) -> Result<ParamArity, Error> {
    Ok(match kind {
        NODE_WHITE_BALANCE => ParamArity::Exact(2), // kelvin (0 = as shot), tint
        NODE_EXPOSURE => ParamArity::Exact(1),      // ev
        NODE_NOISE_REDUCTION => ParamArity::Exact(2), // edge-aware, neural
        NODE_TONE => ParamArity::Exact(7),          // blacks..whites
        NODE_OPTICS => ParamArity::Exact(3),        // distortion?, strength, tca?
        NODE_FILM => ParamArity::Exact(3),          // lut strength, grain amount, size
        NODE_BLEND => ParamArity::Exact(1),         // opacity toward input B
        NODE_COLOR_SUBTRACT => ParamArity::Exact(3), // picked colour, per channel
        NODE_CROP => ParamArity::Exact(5),          // left, top, width, height, angle
        NODE_CURVES => ParamArity::Curves,          // four counts, then their points
        NODE_ROTATE => ParamArity::Exact(1),        // quarter turns clockwise
        NODE_CONTRAST => ParamArity::Exact(2),      // slope, pivot in display terms
        NODE_SHARPEN => ParamArity::Exact(3),       // amount, radius, noise floor
        _ => return Err(Error::InvalidArgument("unknown graph node kind")),
    })
}

/// Where each channel's (x, y) pairs begin, and how many points it holds.
///
/// A curves node's parameters are four point counts followed by the points
/// themselves, red green blue luma, so that one channel can carry a film-stock
/// shape while the others stay on their two endpoints.
fn curve_channel_spans(params: &[f32]) -> Result<[(usize, usize); CURVE_CHANNELS], Error> {
    if params.len() < CURVE_CHANNELS {
        return Err(Error::InvalidArgument("curves node is missing its header"));
    }
    let mut spans = [(0_usize, 0_usize); CURVE_CHANNELS];
    let mut offset = CURVE_CHANNELS;
    for (channel, span) in spans.iter_mut().enumerate() {
        let count = params[channel];
        if count.fract() != 0.0
            || count < MIN_CURVE_POINTS as f32
            || count > MAX_CURVE_POINTS as f32
        {
            return Err(Error::InvalidArgument("curve channel point count"));
        }
        let count = count as usize;
        *span = (offset, count);
        offset += count * 2;
    }
    if offset != params.len() {
        return Err(Error::InvalidArgument(
            "curves node parameters do not match its header",
        ));
    }
    Ok(spans)
}

fn curve_channel_points(params: &[f32], channel: usize) -> &[f32] {
    // Spans were validated when the program was parsed.
    let spans = curve_channel_spans(params).expect("curves parameters were validated");
    let (offset, count) = spans[channel];
    &params[offset..offset + count * 2]
}

fn validate_curve_points(params: &[f32]) -> Result<(), Error> {
    let spans = curve_channel_spans(params)?;
    for (offset, count) in spans {
        let points = &params[offset..offset + count * 2];
        for segment in 0..count - 1 {
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

/// Monotone cubic tangents (Fritsch-Carlson) for ascending points.
fn curve_tangents(xs: &[f32], ys: &[f32]) -> Vec<f32> {
    let last = xs.len() - 1;
    let slopes: Vec<f32> = (0..last)
        .map(|segment| {
            (ys[segment + 1] - ys[segment]) / (xs[segment + 1] - xs[segment]).max(1.0e-4)
        })
        .collect();
    let mut tangents = vec![0.0f32; xs.len()];
    tangents[0] = slopes[0];
    tangents[last] = slopes[last - 1];
    for point in 1..last {
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

/// The curve at X, held flat outside its control points.
///
/// Flat extrapolation is the whole point of the endpoint handles: pulling a
/// channel's white point left from (1, 1) to (0.4, 1) means everything above
/// 0.4 reads as full scale, which is the per-channel gain move that inverts a
/// negative. Interpolating past the last point instead would keep climbing.
fn eval_curve(xs: &[f32], ys: &[f32], tangents: &[f32], x: f32) -> f32 {
    let last = xs.len() - 1;
    if x <= xs[0] {
        return ys[0];
    }
    if x >= xs[last] {
        return ys[last];
    }
    let segment = xs[..last]
        .iter()
        .rposition(|start| x >= *start)
        .unwrap_or(0);
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
    let xs: Vec<f32> = points.iter().step_by(2).copied().collect();
    let ys: Vec<f32> = points.iter().skip(1).step_by(2).copied().collect();
    let tangents = curve_tangents(&xs, &ys);
    (0..CURVE_LUT_SIZE)
        .map(|index| {
            let root = index as f32 / (CURVE_LUT_SIZE - 1) as f32;
            let encoded = srgb_encode_value(root * root);
            srgb_decode_value(eval_curve(&xs, &ys, &tangents, encoded))
        })
        .collect()
}

/// The do-nothing curve: control points on the diagonal, spanning the range.
///
/// Spanning matters because the curve is held flat outside its points. Two
/// points at (0, 0) and (0.5, 0.5) all sit on the diagonal, yet everything
/// above 0.5 is clamped to 0.5, which is very much doing something.
fn is_identity_curve(points: &[f32]) -> bool {
    let last = points.len() - 2;
    points[0].abs() < 1.0e-6
        && (points[last] - 1.0).abs() < 1.0e-6
        && points
            .chunks_exact(2)
            .all(|point| (point[0] - point[1]).abs() < 1.0e-6)
}

/// One channel's LUT with the luma curve, when it is not the identity, folded
/// in after it. Composing costs a few thousand lookups once per render instead
/// of a second curve evaluation per pixel.
fn channel_lut(points: &[f32], luma: Option<&[f32]>) -> Vec<f32> {
    let channel = curve_lut(points);
    match luma {
        None => channel,
        Some(luma) => channel
            .iter()
            .map(|value| apply_curve_value(luma, *value))
            .collect(),
    }
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
        (NODE_ROTATE, 0) => value == 0.0 || value == 1.0 || value == 2.0 || value == 3.0,
        (NODE_CONTRAST, 0) => (0.2..=4.0).contains(&value),
        (NODE_CONTRAST, 1) => (0.05..=0.95).contains(&value),
        (NODE_SHARPEN, 0) => (0.0..=3.0).contains(&value),
        (NODE_SHARPEN, 1) => (0.3..=5.0).contains(&value),
        (NODE_SHARPEN, 2) => (0.0..=8.0).contains(&value),
        // The four leading parameters are point counts, not signal levels.
        (NODE_CURVES, index) if index < CURVE_CHANNELS => {
            (MIN_CURVE_POINTS as f32..=MAX_CURVE_POINTS as f32).contains(&value)
        }
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
        match param_arity(kind)? {
            ParamArity::Exact(expected) if param_count != expected => {
                return Err(Error::InvalidArgument("graph node parameter count"));
            }
            // The header inside the parameters is what fixes a curves node's
            // length, so only the ceiling can be checked before reading them.
            ParamArity::Curves if param_count > MAX_CURVE_PARAMS => {
                return Err(Error::InvalidArgument("curves node has too many points"));
            }
            _ => {}
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
            NODE_ROTATE => {
                // A rotation is indifferent to the domain it turns, and counts
                // as reframing for the same reason a crop does: optics maps
                // against the frame it was measured on.
                display[index] = display[input_a];
                cropped[index] = true;
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
        .par_chunks_mut(1 << 14)
        .zip(layer.data.par_chunks(1 << 14))
        .for_each(|(values, others)| {
            for (value, other) in values.iter_mut().zip(others) {
                *value += (*other - *value) * opacity;
            }
        });
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
    pub(crate) as_shot_kelvin: Option<f32>,
    /// Pixels the photograph has at full resolution, so stages measured in
    /// pixels can tell how far from it this render is.
    pub(crate) full_pixels: usize,
    pub(crate) make: &'a str,
    pub(crate) model: &'a str,
    pub(crate) lens_name: &'a str,
    pub(crate) focal: f32,
    pub(crate) explicit_profile: Option<&'a str>,
    pub(crate) focal_reducer: f32,
    pub(crate) crop_factor: f32,
    pub(crate) grain_seed: u64,
    pub(crate) orientation: u16,
    /// Pixels the whole frame has at the size this render works at.
    ///
    /// Not the pixel count of whatever image a stage happens to hold. The
    /// denoiser fades out as a render shrinks, and the thing it should fade
    /// with is how much the frame was reduced — not how much of it is being
    /// looked at. Reading the image's own size made a zoomed viewport, which is
    /// small but not reduced at all, come back barely denoised; it also
    /// under-denoised any render with a crop node in it, which was a quieter
    /// version of the same mistake.
    pub(crate) scaled_pixels: usize,
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

/// Turns the image by whole quarter turns clockwise, losing nothing.
///
/// The part of orientation a crop's -45..45 degree angle cannot reach. Whole
/// turns are a pure permutation of the pixels, so unlike the crop node's
/// arbitrary angle this resamples nothing and every photosite survives.
fn quarter_turn_image(image: &RgbImage, turns: u32) -> RgbImage {
    let turns = turns % 4;
    if turns == 0 {
        return image.clone();
    }
    let swaps = turns % 2 == 1;
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
        .for_each(|(y, row)| {
            for (x, pixel) in row.as_chunks_mut::<3>().0.iter_mut().enumerate() {
                // Source of the output pixel at (x, y), reading the rotation
                // backwards: one turn clockwise sends source (sx, sy) to
                // (height - 1 - sy, sx), so the inverse is what is needed here.
                let (source_x, source_y) = match turns {
                    1 => (y, image.height - 1 - x),
                    2 => (image.width - 1 - x, image.height - 1 - y),
                    _ => (image.width - 1 - y, x),
                };
                let source = (source_y * image.width + source_x) * 3;
                pixel.copy_from_slice(&image.data[source..source + 3]);
            }
        });
    output
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
    checkpoints: Vec<(StagePoint, Vec<PrefixSnapshot>)>,
}

/// Where a resume enters the program: an op index, and how far into that op the
/// restored state already reached.
///
/// Film nodes are the one kind worth splitting. Their display transform and
/// film LUT cost far more than the grain that follows, so grain gets its own
/// stage: dragging grain resumes with the LUT result already in hand instead of
/// re-running a tone map and a trilinear 3D lookup over every pixel.
type StagePoint = (usize, u32);

const STAGE_ENTRY: u32 = 0;
const STAGE_FILM_GRAIN: u32 = 1;

/// Render context and external resources that can affect a cached checkpoint.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct PrefixContextKey {
    make: String,
    model: String,
    lens_name: String,
    focal_bits: u32,
    explicit_profile: Option<String>,
    focal_reducer_bits: u32,
    crop_factor_bits: u32,
    grain_seed: u64,
    orientation: u16,
    film_luts: Vec<(String, render::DecodeCacheKey)>,
}

/// Cache key: input identity, bounded render dimensions, and every dependency
/// that can affect an intermediate graph checkpoint.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct PrefixKey {
    input_path: String,
    decoded_width: usize,
    decoded_height: usize,
    max_width: u32,
    max_height: u32,
    source_identity: render::DecodeCacheKey,
    context: PrefixContextKey,
}

fn prefix_context_key(
    ops: &[GraphOp],
    context: &GraphContext<'_>,
) -> Result<PrefixContextKey, Error> {
    let film_luts = ops
        .iter()
        .filter(|op| op.kind == NODE_FILM && op.params[0] > 0.0)
        .filter_map(|op| op.text.as_ref())
        .map(|path| Ok((path.clone(), render::file_content_digest(Path::new(path))?)))
        .collect::<Result<Vec<_>, Error>>()?;
    Ok(PrefixContextKey {
        make: context.make.to_owned(),
        model: context.model.to_owned(),
        lens_name: context.lens_name.to_owned(),
        focal_bits: context.focal.to_bits(),
        explicit_profile: context.explicit_profile.map(str::to_owned),
        focal_reducer_bits: context.focal_reducer.to_bits(),
        crop_factor_bits: context.crop_factor.to_bits(),
        grain_seed: context.grain_seed,
        orientation: context.orientation,
        film_luts,
    })
}

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
fn default_snapshot_boundary(ops: &[GraphOp]) -> StagePoint {
    let index = ops
        .iter()
        .rposition(|op| matches!(op.kind, NODE_OPTICS | NODE_NOISE_REDUCTION))
        .map(|index| index + 1)
        .unwrap_or(0);
    (index, STAGE_ENTRY)
}

/// How much of the previous program the current one may resume from.
///
/// Ops are compared whole, with one exception: when the first difference is a
/// film node whose LUT and its strength are unchanged, only grain differs, so a
/// stage 1 checkpoint inside that node is still valid.
fn resume_limit(previous: &[GraphOp], current: &[GraphOp]) -> StagePoint {
    let common = common_prefix_length(previous, current);
    if let (Some(before), Some(after)) = (previous.get(common), current.get(common))
        && before.kind == NODE_FILM
        && after.kind == NODE_FILM
        && before.text == after.text
        && before.params[0] == after.params[0]
    {
        return (common, STAGE_FILM_GRAIN);
    }
    (common, STAGE_ENTRY)
}

/// The last film node that actually lays down grain, whose pre-grain state is
/// worth keeping. Only one is captured so a drag costs a single extra clone.
fn film_grain_capture(ops: &[GraphOp]) -> Option<StagePoint> {
    ops.iter()
        .rposition(|op| op.kind == NODE_FILM && op.params[1] > 0.0)
        .map(|index| (index, STAGE_FILM_GRAIN))
}

#[cfg(test)]
pub(crate) fn execute_graph_windowed(
    ops: &[GraphOp],
    source: RgbImage,
    context: &GraphContext<'_>,
    viewport: [f32; 4],
) -> Result<RgbImage, Error> {
    execute_graph_into(ops, Arc::new(source), context, Some(viewport), None, None)
        .map(|(_, _, image)| image.expect("no byte buffer was asked for"))
}

#[cfg_attr(not(test), allow(dead_code))]
pub(crate) fn execute_graph(
    ops: &[GraphOp],
    source: RgbImage,
    context: &GraphContext<'_>,
) -> Result<RgbImage, Error> {
    execute_graph_cached(ops, Arc::new(source), context, None)
}

pub(crate) fn execute_graph_cached(
    ops: &[GraphOp],
    source: Arc<RgbImage>,
    context: &GraphContext<'_>,
    cache_key: Option<PrefixKey>,
) -> Result<RgbImage, Error> {
    let (width, height, image) = execute_graph_into(ops, source, context, None, cache_key, None)?;
    let _ = (width, height);
    Ok(image.expect("a graph run without a byte buffer returns its image"))
}

/// Where a render of part of a frame may start working on just that part, and
/// how much overlap it needs there.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct ViewportPlan {
    /// Index of the first op that runs on the window.
    pub(crate) barrier: usize,
    /// Pixels of frame the window has to carry beyond its own edges for its
    /// interior to come out identical to a whole-frame render.
    pub(crate) halo: usize,
}

/// Decides whether and where a render bounded to a viewport can narrow.
///
/// Geometric nodes — lens correction, crop, rotate, and the blend that joins
/// two branches of possibly different geometry — are written against whole
/// frames: each reads a map defined in frame coordinates and would have to be
/// told where a window sat before it could run on one. So the narrowing happens
/// after the last of them and everything before it runs whole.
///
/// That is less of a compromise than it sounds. The geometric prefix is cheap
/// beside the tail it feeds — a remap is one pass over memory, while the
/// denoiser and the neural network are many — and the checkpoint cache keeps
/// the prefix from being re-run at all when a downstream node changes or the
/// view is merely panned. What is left to pay for is exactly the part the
/// photographer is looking at.
///
/// Returns `None` when the tail is not a simple chain, because a register that
/// bypassed the narrowing would then be read at the wrong size. Every graph the
/// interface builds is a chain past its last geometric node, so this is a
/// correctness guard rather than a path anyone travels.
pub(crate) fn plan_viewport(ops: &[GraphOp]) -> Option<ViewportPlan> {
    let barrier = ops
        .iter()
        .rposition(|op| matches!(op.kind, NODE_OPTICS | NODE_CROP | NODE_ROTATE | NODE_BLEND))
        .map_or(0, |index| index + 1);
    let mut halo = 0;
    for (index, op) in ops.iter().enumerate().skip(barrier) {
        if op.input_a != index {
            return None;
        }
        // Every other node in the tail answers for one pixel at a time, so it
        // needs no overlap at all. The two that read their neighbours are added
        // rather than maxed: a window feeds the next stage what it has, so two
        // filters in a row each want their own reach.
        halo += match op.kind {
            NODE_NOISE_REDUCTION => {
                render::NOISE_REDUCTION_REACH.max(super::nn::NETWORK_REACH)
            }
            NODE_SHARPEN => render::SHARPEN_MAX_REACH,
            _ => 0,
        };
    }
    Some(ViewportPlan { barrier, halo })
}

/// Narrows IMAGE to the part of the frame RECT names, in oriented fractions.
///
/// Returns the window, its origin in oriented pixels, and whether it came back
/// oriented. The origin is what lets the one stage that cares where it is —
/// film grain, which seeds from its coordinates — put the grain where the whole
/// frame would have had it.
///
/// The narrowing happens in the frame's own unoriented coordinates when the
/// image has not been turned yet, so that turning it is a permutation of the
/// window rather than of the whole frame. Where the window then lands is worked
/// out with `render::orient_rect` instead of by rounding the fraction twice,
/// which keeps the answer exact to the pixel.
fn crop_to_viewport(
    image: RgbImage,
    rect: [f32; 4],
    halo: usize,
    orientation: u16,
    oriented: bool,
) -> (RgbImage, (usize, usize), (usize, usize, usize, usize)) {
    let (frame_width, frame_height) = if oriented || orientation < 5 {
        (image.width, image.height)
    } else {
        (image.height, image.width)
    };
    // The window in oriented pixels, grown by the halo and clipped to the
    // frame. Rounded outwards so the requested region is always covered.
    let left = ((rect[0] * frame_width as f32).floor() as isize - halo as isize).max(0) as usize;
    let top = ((rect[1] * frame_height as f32).floor() as isize - halo as isize).max(0) as usize;
    let right = (((rect[0] + rect[2]) * frame_width as f32).ceil() as usize + halo)
        .min(frame_width);
    let bottom = (((rect[1] + rect[3]) * frame_height as f32).ceil() as usize + halo)
        .min(frame_height);
    let (width, height) = (right.saturating_sub(left), bottom.saturating_sub(top));
    // The region actually asked for, which is the window less its halo. Kept
    // separately so the halo can be trimmed off at the end: it exists to make
    // the interior come out right, not to be delivered.
    let asked = (
        (rect[0] * frame_width as f32).floor().max(0.0) as usize,
        (rect[1] * frame_height as f32).floor().max(0.0) as usize,
        ((rect[2] * frame_width as f32).ceil() as usize).max(1),
        ((rect[3] * frame_height as f32).ceil() as usize).max(1),
    );
    if width == 0 || height == 0 || (width >= frame_width && height >= frame_height) {
        return (image, (0, 0), (0, 0, frame_width, frame_height));
    }
    let inside = |placed: (usize, usize, usize, usize)| {
        let offset_x = asked.0.saturating_sub(placed.0);
        let offset_y = asked.1.saturating_sub(placed.1);
        (
            offset_x,
            offset_y,
            asked.2.min(placed.2.saturating_sub(offset_x)).max(1),
            asked.3.min(placed.3.saturating_sub(offset_y)).max(1),
        )
    };
    if oriented {
        let placed = (left, top, width, height);
        return (
            render::crop_rect(&image, left, top, width, height),
            (left, top),
            inside(placed),
        );
    }
    // Unoriented: ask `map_oriented_rect` which part of the sensor frame this
    // is, take it, then turn the window.
    let mapped = map_oriented_rect(
        orientation,
        [
            left as f32 / frame_width as f32,
            top as f32 / frame_height as f32,
            width as f32 / frame_width as f32,
            height as f32 / frame_height as f32,
        ],
    );
    let source_left = (mapped[0] * image.width as f32).round() as usize;
    let source_top = (mapped[1] * image.height as f32).round() as usize;
    let source_width = ((mapped[2] * image.width as f32).round() as usize)
        .max(1)
        .min(image.width - source_left.min(image.width - 1));
    let source_height = ((mapped[3] * image.height as f32).round() as usize)
        .max(1)
        .min(image.height - source_top.min(image.height - 1));
    let placed = render::orient_rect(
        orientation,
        (image.width, image.height),
        (source_left, source_top, source_width, source_height),
    );
    let window = render::crop_rect(&image, source_left, source_top, source_width, source_height);
    (
        render::orient(window, orientation),
        (placed.0, placed.1),
        inside(placed),
    )
}

fn execute_graph_into(
    ops: &[GraphOp],
    source: Arc<RgbImage>,
    context: &GraphContext<'_>,
    viewport: Option<[f32; 4]>,
    cache_key: Option<PrefixKey>,
    bytes: Option<&mut [u8]>,
) -> Result<(usize, usize, Option<RgbImage>), Error> {
    let count = ops.len();
    if count == 0 {
        let image = render::orient(render::own_source(source), context.orientation);
        let image = match viewport {
            Some(rect) => crop_to_viewport(image, rect, 0, context.orientation, true).0,
            None => image,
        };
        return finish(image, Domain::Linear, bytes);
    }
    // Where the window may narrow, if it may at all.
    let plan = viewport.and_then(|rect| plan_viewport(ops).map(|plan| (rect, plan)));
    let mut window_origin = (0_usize, 0_usize);
    // The requested region inside the window that was developed, so the halo
    // can come off before the result is handed back.
    let mut window_inner: Option<(usize, usize, usize, usize)> = None;
    // Interactive resume: reuse the deepest checkpoint of the previous
    // program that still lies on this program's unchanged prefix.
    let mut resume: Option<(StagePoint, Vec<PrefixSnapshot>)> = None;
    let mut capture_points: Vec<StagePoint> = Vec::new();
    if let Some(key) = &cache_key {
        let mut cache = prefix_cache()
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let limit = if let Some(position) = cache.iter().position(|(held, _)| held == key) {
            let entry = &cache[position].1;
            let limit = resume_limit(&entry.ops, ops);
            let deepest = match &plan {
                Some((_, plan)) => limit.min((plan.barrier, STAGE_ENTRY)),
                None => limit,
            };
            if let Some((point, snapshots)) = entry
                .checkpoints
                .iter()
                .filter(|(point, _)| *point > (0, STAGE_ENTRY) && *point <= deepest)
                .max_by_key(|(point, _)| *point)
            {
                resume = Some((
                    *point,
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
            Some(limit)
        } else {
            None
        };
        // Checkpoint at the edit's divergence point for same-node drags, after
        // the last expensive stage for downstream edits, and before the grain
        // of the last film node so grain drags skip its tone map and LUT.
        match limit {
            Some((common, _)) if common < count => capture_points.push((common, STAGE_ENTRY)),
            _ => {}
        }
        capture_points.push(default_snapshot_boundary(ops));
        capture_points.extend(film_grain_capture(ops));
        capture_points.retain(|point| *point > (0, STAGE_ENTRY));
        // A checkpoint has to describe a whole frame. One taken past the point
        // where the render narrows would hold a particular window, and would
        // then be handed back for a different one the moment the view was
        // panned — so the narrowing point is also the last place worth saving.
        // Nothing is lost by it: everything past that point now costs what the
        // viewport costs, which is the cheap part.
        // ...and it is also the *best* place: everything before it is the
        // geometry that does not change when the view moves, and everything
        // after it now costs what the window costs. Saving exactly there is what
        // makes a pan re-run the cheap part and nothing else.
        if let Some((_, plan)) = &plan {
            capture_points.retain(|point| *point < (plan.barrier, STAGE_ENTRY));
            capture_points.push((plan.barrier, STAGE_ENTRY));
        }
        capture_points.sort_unstable();
        capture_points.dedup();
    }
    let (boundary, entry_stage) = resume
        .as_ref()
        .map(|(point, _)| *point)
        .unwrap_or((0, STAGE_ENTRY));
    // Strictly after the resume point: the cache already holds the checkpoint
    // this run entered on, and the merge below keeps it. Re-capturing it copied
    // the whole image again on every tick of a drag that never moved off it.
    capture_points.retain(|point| *point > (boundary, entry_stage));
    // Remaining reader counts cover only the ops actually executed, so
    // images still move instead of cloning on their last read. A stage 1
    // resume re-reads its own half-finished register instead of its input.
    let mut uses = vec![0_usize; count + 1];
    for (index, op) in ops.iter().enumerate().skip(boundary) {
        if index == boundary && entry_stage != STAGE_ENTRY {
            continue; // Reads its own register, counted below.
        }
        uses[op.input_a] += 1;
        if op.kind == NODE_BLEND {
            uses[op.input_b] += 1;
        }
    }
    uses[count] += 1; // The final node feeds the output.
    if entry_stage != STAGE_ENTRY {
        // A stage 1 resume reads its own register and writes it straight back,
        // so it needs the register present without spending a reader's budget:
        // counting it as another reader would clone the whole image instead.
        uses[boundary + 1] = uses[boundary + 1].max(1);
    }
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
                registers[0] = Some(render::own_source(source));
            }
            // Only grainy film nodes advance the seed, matching the loop, and
            // a stage 1 resume has not laid down its own grain yet.
            film_ordinal = ops[..boundary]
                .iter()
                .filter(|op| op.kind == NODE_FILM && op.params[1] > 0.0)
                .count() as u64;
        }
        None => registers[0] = Some(render::own_source(source)),
    }
    let profiling = std::env::var_os("ORFEUS_PROFILE").is_some();
    let mut captured: Vec<(StagePoint, Vec<PrefixSnapshot>)> = Vec::new();
    for (index, op) in ops.iter().enumerate().skip(boundary) {
        let node_started = profiling.then(std::time::Instant::now);
        let stage = if index == boundary {
            entry_stage
        } else {
            STAGE_ENTRY
        };
        if stage == STAGE_ENTRY
            && capture_points.contains(&(index, STAGE_ENTRY))
            && let Some(snapshots) = collect_snapshots(&registers, &domains, &oriented)
        {
            captured.push(((index, STAGE_ENTRY), snapshots));
        }
        let slot = index + 1;
        // The moment the render narrows. Everything from here on works on the
        // part of the frame the viewport asked for, which is why the stages
        // that read their neighbours had a halo added to it.
        if let Some((rect, plan)) = &plan
            && index == plan.barrier
            && stage == STAGE_ENTRY
            && let Some(whole) = registers[op.input_a].take()
        {
            let (window, origin, inner) = crop_to_viewport(
                whole,
                *rect,
                plan.halo,
                context.orientation,
                oriented[op.input_a],
            );
            window_origin = origin;
            window_inner = Some(inner);
            oriented[op.input_a] = true;
            registers[op.input_a] = Some(window);
        }
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
        // A stage 1 resume already holds this op's own half-finished register,
        // together with the domain and orientation it had reached.
        let mut image = if stage == STAGE_ENTRY {
            let image = take(&mut registers, &mut uses, op.input_a)?;
            domains[slot] = domains[op.input_a];
            oriented[slot] = oriented[op.input_a];
            image
        } else {
            take(&mut registers, &mut uses, slot)?
        };
        if stage == STAGE_ENTRY
            && matches!(
                op.kind,
                NODE_NOISE_REDUCTION
                    | NODE_CONTRAST
                    | NODE_SHARPEN
                    | NODE_TONE
                    | NODE_FILM
                    | NODE_COLOR_SUBTRACT
                    | NODE_CROP
                    | NODE_ROTATE
                    | NODE_CURVES
            )
            && !oriented[slot]
        {
            image = render::orient(image, context.orientation);
            oriented[slot] = true;
        }
        match op.kind {
            NODE_WHITE_BALANCE => {
                render::apply_white_adaptation(
                    &mut image,
                    op.params[0],
                    op.params[1],
                    context.as_shot_kelvin,
                );
            }
            NODE_EXPOSURE => {
                render::apply_exposure(&mut image, op.params[0]);
            }
            NODE_NOISE_REDUCTION => {

                // One denoiser or the other, and the edge-aware one asks the
                // same of brightness as of colour. It used to get a fifth of
                // the slider for brightness, which is the noise that shows:
                // at the default setting that left grain untouched — a
                // measured drop of a tenth against a half for colour.
                let edge = render::strength_for_scale(
                    op.params[0],
                    context.scaled_pixels,
                    context.full_pixels,
                );
                let neural = op.params[1];
                if neural > 0.0 {
                    super::nn::apply_neural_noise_reduction(
                        &mut image.data,
                        image.width,
                        image.height,
                        neural,
                    )?;
                } else {
                    render::apply_noise_reduction(&mut image, edge, edge);
                }
            }
            NODE_CONTRAST => {
                render::apply_contrast(&mut image, op.params[0], op.params[1]);
            }
            NODE_SHARPEN => {
                // The radius is stated for the photograph, not for whichever
                // reduction of it is on screen, so it shrinks with the render.
                let ratio = render::scale_ratio(context.scaled_pixels, context.full_pixels);
                let profile = render::measure_noise_profile(&image);
                render::apply_sharpen(
                    &mut image,
                    op.params[0],
                    op.params[1] * ratio,
                    op.params[2],
                    &profile,
                );
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
                if stage == STAGE_ENTRY {
                    if domains[slot] == Domain::Linear {
                        to_display(&mut image);
                        domains[slot] = Domain::Display;
                    }
                    if op.params[0] > 0.0
                        && let Some(path) = &op.text
                    {
                        let lut = render::cached_cube_lut(Path::new(path))?;
                        render::apply_lut(&mut image, &lut, op.params[0]);
                    }
                    // Park the tone-mapped, LUT-applied image in its own
                    // register so the snapshot machinery can see it, then take
                    // it straight back to lay the grain down.
                    if capture_points.contains(&(index, STAGE_FILM_GRAIN))
                        && (index, STAGE_FILM_GRAIN) != (boundary, entry_stage)
                    {
                        registers[slot] = Some(image);
                        if let Some(snapshots) = collect_snapshots(&registers, &domains, &oriented)
                        {
                            captured.push(((index, STAGE_FILM_GRAIN), snapshots));
                        }
                        image = registers[slot]
                            .take()
                            .ok_or(Error::Render("film stage register vanished".into()))?;
                    }
                }
                if op.params[1] > 0.0 {
                    render::apply_grain(
                        &mut image,
                        op.params[1],
                        op.params[2],
                        context.grain_seed ^ film_ordinal.wrapping_mul(0x9e37_79b9),
                        window_origin,
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
                image.data.par_chunks_mut(3 * 8192).for_each(|chunk| {
                    for pixel in chunk.as_chunks_mut::<3>().0 {
                        for (value, base) in pixel.iter_mut().zip(color) {
                            *value = base - *value;
                        }
                    }
                });
            }
            NODE_CURVES => {
                // Per-channel monotone curves on the encoded signal, then the
                // luma curve over all three: the film-stock "decompression"
                // for inverted negatives.
                let luma_points = curve_channel_points(&op.params, 3);
                let luma = (!is_identity_curve(luma_points)).then(|| curve_lut(luma_points));
                let luts = [
                    channel_lut(curve_channel_points(&op.params, 0), luma.as_deref()),
                    channel_lut(curve_channel_points(&op.params, 1), luma.as_deref()),
                    channel_lut(curve_channel_points(&op.params, 2), luma.as_deref()),
                ];
                image.data.par_chunks_mut(3 * 8192).for_each(|chunk| {
                    for pixel in chunk.as_chunks_mut::<3>().0 {
                        for (value, lut) in pixel.iter_mut().zip(&luts) {
                            *value = apply_curve_value(lut, *value);
                        }
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
            NODE_ROTATE => {
                image = quarter_turn_image(&image, op.params[0] as u32);
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
    if capture_points.contains(&(count, STAGE_ENTRY))
        && let Some(snapshots) = collect_snapshots(&registers, &domains, &oriented)
    {
        captured.push(((count, STAGE_ENTRY), snapshots));
    }
    if let Some(key) = &cache_key {
        let mut cache = prefix_cache()
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        // Merge rather than replace. Checkpoints at or before the point this
        // render resumed from still hold the same state, so keeping them lets
        // a repeated drag reuse the one it entered on without re-cloning the
        // image every tick.
        if let Some(position) = cache.iter().position(|(held, _)| held == key) {
            let entry = &mut cache[position].1;
            entry.ops = ops.to_vec();
            entry
                .checkpoints
                .retain(|(point, _)| *point <= (boundary, entry_stage));
            for (point, snapshots) in captured {
                entry.checkpoints.retain(|(held, _)| *held != point);
                entry.checkpoints.push((point, snapshots));
            }
            if entry.checkpoints.is_empty() {
                cache.remove(position);
            } else {
                let held = cache.remove(position);
                cache.insert(0, held);
            }
        } else if !captured.is_empty() {
            let entry = PrefixEntry {
                ops: ops.to_vec(),
                checkpoints: captured,
            };
            cache.insert(0, (key.clone(), entry));
        }
        cache.truncate(PREFIX_CACHE_CAPACITY);
    }
    let tail_started = profiling.then(std::time::Instant::now);
    let mut image = registers[count]
        .take()
        .ok_or(Error::Render("graph produced no output".into()))?;
    if !oriented[count] {
        image = render::orient(image, context.orientation);
    }
    // A graph whose every node is geometric narrows here instead, after all of
    // them; there was no later op to do it before.
    if let Some((rect, plan)) = &plan
        && plan.barrier == count
    {
        let (window, _, inner) =
            crop_to_viewport(image, *rect, plan.halo, context.orientation, true);
        image = window;
        window_inner = Some(inner);
    }
    if let Some((left, top, width, height)) = window_inner
        && (width < image.width || height < image.height)
    {
        image = render::crop_rect(&image, left, top, width, height);
    }
    let result = finish(image, domains[count], bytes)?;
    if let Some(started) = tail_started {
        eprintln!(
            "orfeus-profile graph-tail resumed-from={}.{} milliseconds={:.3}",
            boundary,
            entry_stage,
            started.elapsed().as_secs_f64() * 1000.0
        );
    }
    Ok(result)
}

/// Delivers a finished graph: display-space pixels either as an image or
/// written straight into the caller's 8-bit buffer.
fn finish(
    mut image: RgbImage,
    domain: Domain,
    bytes: Option<&mut [u8]>,
) -> Result<(usize, usize, Option<RgbImage>), Error> {
    let (width, height) = (image.width, image.height);
    match bytes {
        None => {
            if domain == Domain::Linear {
                to_display(&mut image);
            }
            Ok((width, height, Some(image)))
        }
        Some(bytes) => {
            let needed = width * height * 3;
            if bytes.len() < needed {
                return Err(Error::InvalidArgument("preview buffer is too small"));
            }
            render::write_display_bytes(&image, &mut bytes[..needed], domain == Domain::Linear);
            Ok((width, height, None))
        }
    }
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
    let profiling = std::env::var_os("ORFEUS_PROFILE").is_some();
    let started = Instant::now();
    let (width, height, _) =
        render_graph_frame(input, frame, graph_bytes, cache_mode, Some(buffer))?;
    if profiling {
        eprintln!(
            "orfeus-profile frame milliseconds={:.3}",
            started.elapsed().as_secs_f64() * 1000.0
        );
    }
    Ok((width, height))
}

/// Whether a render develops a draft: only when the caller asked for one, and
/// only when the requested size is small enough that binning cannot upsample.
///
/// Both halves matter. Inferring from size alone would silently soften an export
/// that asked for 2048 pixels, legitimate for the web. Trusting the flag alone
/// would halve the resolution of a 1:1 preview, where nothing downstream
/// resamples the detail away.
fn develops_draft(flags: u32, max_width: u32, max_height: u32) -> bool {
    flags & FRAME_FLAG_DRAFT != 0 && render::draft_requested(max_width, max_height)
}

/// Fills the decode cache for a frame that is about to be rendered.
pub fn prewarm_decode(
    input: &Path,
    flags: u32,
    max_width: u32,
    max_height: u32,
    cache_mode: u32,
) -> Result<(), Error> {
    if !matches!(cache_mode, render::CACHE_NONE | render::CACHE_USE) {
        return Err(Error::InvalidArgument("unsupported decode cache mode"));
    }
    let profiling = std::env::var_os("ORFEUS_PROFILE").is_some();
    let draft = develops_draft(flags, max_width, max_height);
    render::decoded_for_render_with_identity(input, cache_mode, draft, profiling)?;
    Ok(())
}

fn render_graph_image(
    input: &Path,
    frame: &RenderFrameV1,
    graph_bytes: &[u8],
    cache_mode: u32,
) -> Result<RgbImage, Error> {
    let (_, _, image) = render_graph_frame(input, frame, graph_bytes, cache_mode, None)?;
    Ok(image.expect("a frame rendered without a byte buffer returns its image"))
}

fn render_graph_frame(
    input: &Path,
    frame: &RenderFrameV1,
    graph_bytes: &[u8],
    cache_mode: u32,
    bytes: Option<&mut [u8]>,
) -> Result<(usize, usize, Option<RgbImage>), Error> {
    frame.validate()?;
    let ops = parse_graph(graph_bytes)?;
    if !matches!(cache_mode, render::CACHE_NONE | render::CACHE_USE) {
        return Err(Error::InvalidArgument("unsupported decode cache mode"));
    }
    let profiling = std::env::var_os("ORFEUS_PROFILE").is_some();
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
    // A live preview develops a draft: each sensor quad becomes one pixel
    // rather than interpolating a colour for every photosite.
    let draft = develops_draft(frame.flags, frame.max_width, frame.max_height);
    let (decoded, source_identity): (Arc<DecodedRaw>, Option<render::DecodeCacheKey>) =
        render::decoded_for_render_with_identity(input, cache_mode, draft, profiling)?;
    profile_stage!("decoded-source");
    let (native_max_width, native_max_height) =
        render::native_downscale_bounds(decoded.orientation, frame.max_width, frame.max_height);
    let bounded = native_max_width > 0 || native_max_height > 0;
    // SAFETY: The caller promises a null or NUL-terminated lens name.
    let named_lens =
        unsafe { render::borrowed_c_string(frame.lens_name, "lens name is not UTF-8")? };
    let (make, model, lens_name, focal, orientation) = (
        decoded.make.clone(),
        decoded.model.clone(),
        render::effective_lens_name(&decoded.lens_name, named_lens).to_string(),
        decoded.focal,
        decoded.orientation,
    );
    let as_shot_kelvin = decoded.as_shot_kelvin;
    let (decoded_width, decoded_height) = (decoded.width, decoded.height);
    let full_pixels = decoded.full_pixels;
    // A full-resolution render works on the whole decode, so it takes the
    // buffer rather than copying it — nearly a gigabyte at 80 MP.
    let source = if bounded {
        render::scaled_source_for_render(
            &decoded,
            input,
            native_max_width,
            native_max_height,
            cache_mode,
        )
    } else {
        Arc::new(render::own_decoded(decoded))
    };
    profile_stage!("scaled-source");
    let explicit_profile = if frame.lens_profile_model.is_null() {
        None
    } else {
        Some(
            unsafe { CStr::from_ptr(frame.lens_profile_model) }
                .to_str()
                .map_err(|_| Error::InvalidArgument("lens profile model is not UTF-8"))?,
        )
    };
    // Zeros, a degenerate rectangle, or one that already covers the frame all
    // mean the whole frame: a caller that does not know about viewports leaves
    // the field zeroed, and one that is fitted to the window asks for all of it.
    let viewport = {
        let [left, top, width, height] = frame.viewport;
        let covers = left <= 0.0 && top <= 0.0 && width >= 1.0 && height >= 1.0;
        (width > 0.0 && height > 0.0 && !covers).then_some([
            left.clamp(0.0, 1.0),
            top.clamp(0.0, 1.0),
            width.clamp(0.0, 1.0),
            height.clamp(0.0, 1.0),
        ])
    };
    let scaled_pixels = source.width * source.height;
    let context = GraphContext {
        as_shot_kelvin,
        full_pixels,
        scaled_pixels,
        make: &make,
        model: &model,
        lens_name: &lens_name,
        focal,
        explicit_profile,
        focal_reducer: frame.focal_reducer,
        crop_factor: frame.lens_crop_factor,
        grain_seed: frame.grain_seed,
        orientation,
    };
    let prefix_key = if cache_mode == render::CACHE_USE && bounded {
        Some(PrefixKey {
            input_path: input.to_string_lossy().into_owned(),
            decoded_width,
            decoded_height,
            max_width: native_max_width,
            max_height: native_max_height,
            source_identity: source_identity
                .expect("decode cache identity is present when cache mode is CACHE_USE"),
            context: prefix_context_key(&ops, &context)?,
        })
    } else {
        None
    };
    profile_stage!("prefix-key");
    let _ = stage_started;
    execute_graph_into(&ops, source, &context, viewport, prefix_key, bytes)
}

#[cfg(test)]
mod tests {
    #[test]
    fn only_a_render_that_asked_for_it_develops_a_draft() {
        // An export at a web-sized bound is still an export: halving its
        // resolution to save time would quietly ship a softer image.
        for (width, height) in [(0, 0), (1600, 1200), (2048, 2048), (6000, 4000)] {
            assert!(
                !develops_draft(0, width, height),
                "{width}x{height} drafted without being asked"
            );
        }
        assert!(develops_draft(FRAME_FLAG_DRAFT, 1600, 1200));
        // A preview asking for full resolution — a 1:1 zoom — wants the real
        // demosaic, since nothing downstream will resample the detail away.
        assert!(!develops_draft(FRAME_FLAG_DRAFT, 0, 0));
        assert!(!develops_draft(FRAME_FLAG_DRAFT, 6000, 4000));
        // Unknown flags must not turn drafting on.
        assert!(!develops_draft(FRAME_FLAG_DRAFT << 1, 1600, 1200));
    }

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
            scaled_pixels: 0,
            make: "",
            model: "",
            lens_name: "",
            focal: 0.0,
            explicit_profile: None,
            focal_reducer: 1.0,
            crop_factor: 0.0,
            grain_seed: seed,
            orientation: 1,
            as_shot_kelvin: None,
            full_pixels: 0,
        }
    }

    fn test_prefix_key(name: &str, context: &GraphContext<'_>) -> PrefixKey {
        PrefixKey {
            input_path: name.into(),
            decoded_width: 24,
            decoded_height: 16,
            max_width: 24,
            max_height: 16,
            source_identity: [0; 32],
            context: prefix_context_key(&[], context).unwrap(),
        }
    }

    #[test]
    fn prefix_context_key_tracks_context_and_lut_identity() {
        let base = GraphContext {
            scaled_pixels: 0,
            make: "Olympus",
            model: "PEN-F",
            lens_name: "M.Zuiko",
            focal: 25.0,
            explicit_profile: Some("profile"),
            focal_reducer: 0.71,
            crop_factor: 2.0,
            grain_seed: 7,
            orientation: 6,
            as_shot_kelvin: None,
            full_pixels: 0,
        };
        let key = prefix_context_key(&[], &base).unwrap();
        assert_eq!(key.make, "Olympus");
        assert_eq!(key.model, "PEN-F");
        assert_eq!(key.lens_name, "M.Zuiko");
        assert_eq!(key.focal_bits, 25.0_f32.to_bits());
        assert_eq!(key.explicit_profile.as_deref(), Some("profile"));
        assert_eq!(key.focal_reducer_bits, 0.71_f32.to_bits());
        assert_eq!(key.crop_factor_bits, 2.0_f32.to_bits());
        assert_eq!(key.grain_seed, 7);
        assert_eq!(key.orientation, 6);
        assert_ne!(
            key,
            prefix_context_key(
                &[],
                &GraphContext {
                    grain_seed: 8,
                    ..base
                }
            )
            .unwrap()
        );

        let path = std::env::temp_dir().join(format!(
            "orfeus-prefix-lut-{}-{}.cube",
            std::process::id(),
            std::thread::current().name().unwrap_or("test")
        ));
        std::fs::write(&path, "LUT_3D_SIZE 2\n").unwrap();
        let path_text = path.to_string_lossy().into_owned();
        let ops = parse_graph(
            &GraphBuilder::new()
                .node(NODE_FILM, 0, -1, &[1.0, 0.0, 1.0], Some(&path_text))
                .build(),
        )
        .unwrap();
        let before = prefix_context_key(&ops, &context(7)).unwrap();
        std::fs::write(&path, "LUT_3D_SIZE 3\n").unwrap();
        let after = prefix_context_key(&ops, &context(7)).unwrap();
        std::fs::remove_file(path).unwrap();
        assert_ne!(before, after);
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

    /// A frame with real noise in it, big enough that a window has an interior.
    fn noisy_scene(width: usize, height: usize) -> RgbImage {
        let mut data = Vec::with_capacity(width * height * 3);
        for y in 0..height {
            for x in 0..width {
                // Structure at several scales, so the denoiser has both edges
                // to keep and flat ground to clean.
                let base = 0.18
                    + 0.5 * (x as f32 / width as f32)
                    + if (x / 23 + y / 19) % 2 == 0 { 0.12 } else { 0.0 };
                let bits = render::splitmix64((y as u64) << 32 | x as u64);
                let noise = ((bits >> 40) as f32 / 16_777_215.0 - 0.5) * 0.05;
                let value = (base + noise).clamp(0.0, 1.0);
                data.extend_from_slice(&[value, value * 0.93, value * 0.86]);
            }
        }
        RgbImage {
            width,
            height,
            data,
        }
    }

    /// The whole point of rendering a viewport: it has to produce what the
    /// whole-frame render would have produced there.
    ///
    /// Run twice over. Without the denoiser every stage is either pointwise or
    /// the film grain, so the window has to agree to the last bit — that is
    /// what pins the geometry, and in particular that the grain was taught
    /// where the window sits instead of seeding from its own corner. With the
    /// denoiser a small difference is expected and is not a bug: it measures
    /// the noise of the image it is handed, so a window measures a window and
    /// arrives at a slightly different threshold. Bounding that separately is
    /// what keeps the first assertion able to be exact.
    #[test]
    fn a_windowed_render_matches_the_whole_one() {
        let pointwise = parse_graph(
            &GraphBuilder::new()
                .node(NODE_EXPOSURE, 0, -1, &[0.4], None)
                .node(NODE_TONE, 1, -1, &[0.2, 0.1, 0.0, -0.1, 0.0, 0.1, 0.0], None)
                .node(NODE_FILM, 2, -1, &[0.0, 0.4, 2.0], None)
                .build(),
        )
        .unwrap();
        let denoised = parse_graph(
            &GraphBuilder::new()
                .node(NODE_EXPOSURE, 0, -1, &[0.4], None)
                .node(NODE_NOISE_REDUCTION, 1, -1, &[0.6, 0.0], None)
                .node(NODE_TONE, 2, -1, &[0.2, 0.1, 0.0, -0.1, 0.0, 0.1, 0.0], None)
                .node(NODE_FILM, 3, -1, &[0.0, 0.4, 2.0], None)
                .build(),
        )
        .unwrap();
        // The plain case and two quarter turns, which is what an Olympus frame
        // actually does and where a rectangle is easiest to map to the wrong
        // corner.
        for orientation in [1_u16, 6, 8] {
            let mut graph_context = context(11);
            graph_context.orientation = orientation;
            for (label, ops, tolerance) in [
                ("pointwise", &pointwise, 1.0e-6_f32),
                // Measured: the worst pixel moves by 0.0019, which is half of
                // one eight-bit level, and only where a corner window sees a
                // narrower range of brightness than the frame does.
                ("denoised", &denoised, 0.003),
            ] {
                let whole = execute_graph(ops, noisy_scene(320, 240), &graph_context).unwrap();
                for rect in [
                    [0.25_f32, 0.25, 0.5, 0.5],
                    [0.0, 0.0, 0.4, 0.4],
                    [0.55, 0.6, 0.45, 0.4],
                ] {
                    let window =
                        execute_graph_windowed(ops, noisy_scene(320, 240), &graph_context, rect)
                            .unwrap();
                    let left = (rect[0] * whole.width as f32).floor() as usize;
                    let top = (rect[1] * whole.height as f32).floor() as usize;
                    let expected = render::crop_rect(
                        &whole,
                        left,
                        top,
                        window.width.min(whole.width - left),
                        window.height.min(whole.height - top),
                    );
                    assert_eq!(
                        (window.width, window.height),
                        (expected.width, expected.height),
                        "{label} at orientation {orientation} rect {rect:?} came back \
                         the wrong size"
                    );
                    let difference = max_difference(&window, &expected);
                    assert!(
                        difference < tolerance,
                        "{label} at orientation {orientation} rect {rect:?} differs \
                         by {difference}"
                    );
                }
            }
        }
    }

    /// Where the narrowing may happen, and where it may not.
    #[test]
    fn a_viewport_narrows_after_the_last_geometric_node() {
        let plan = |ops: &[u32]| {
            let mut builder = GraphBuilder::new();
            for (index, kind) in ops.iter().enumerate() {
                let params: &[f32] = match *kind {
                    NODE_TONE => &[0.0; 7],
                    NODE_FILM => &[0.0, 0.0, 1.0],
                    NODE_NOISE_REDUCTION => &[0.5, 0.0],
                    NODE_CROP => &[0.1, 0.1, 0.8, 0.8, 0.0],
                    NODE_OPTICS => &[1.0, 1.0, 0.0],
                    _ => &[0.0],
                };
                builder = builder.node(*kind, index as i32, -1, params, None);
            }
            let bytes = builder.build();
            plan_viewport(&parse_graph(&bytes).unwrap())
        };
        // Nothing geometric: the whole program runs on the window.
        assert_eq!(
            plan(&[NODE_EXPOSURE, NODE_TONE]).unwrap().barrier,
            0,
            "a program with no geometry still rendered the whole frame"
        );
        // Lens correction reads a map in frame coordinates, so it goes first
        // and the narrowing follows it.
        assert_eq!(plan(&[NODE_OPTICS, NODE_NOISE_REDUCTION]).unwrap().barrier, 1);
        assert_eq!(
            plan(&[NODE_EXPOSURE, NODE_CROP, NODE_TONE]).unwrap().barrier,
            2
        );
        // A geometric node last leaves nothing to narrow before, which is
        // handled after the loop rather than being refused.
        let tail = plan(&[NODE_NOISE_REDUCTION, NODE_CROP]).unwrap();
        assert_eq!(tail.barrier, 2);
        assert_eq!(tail.halo, 0, "a halo was reserved for an empty tail");
        // Only the stages that read their neighbours ask for overlap, and two
        // of them ask twice.
        assert_eq!(plan(&[NODE_EXPOSURE, NODE_TONE]).unwrap().halo, 0);
        let one = plan(&[NODE_NOISE_REDUCTION]).unwrap().halo;
        assert!(one >= render::NOISE_REDUCTION_REACH);
        assert_eq!(plan(&[NODE_NOISE_REDUCTION, NODE_NOISE_REDUCTION]).unwrap().halo, 2 * one);
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
        render::apply_white_adaptation(&mut reference, 6500.0, 3.0, None);
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
        render::apply_white_adaptation(&mut reference, 6500.0, 3.0, None);
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
        render::apply_grain(&mut reference, 0.25, 1.0, 17, (0, 0));

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

    /// The two-point identity the panel now starts every channel on.
    const ENDPOINTS_CURVE: [f32; 4] = [0.0, 0.0, 1.0, 1.0];

    fn curves_params(red: &[f32], green: &[f32], blue: &[f32]) -> Vec<f32> {
        luma_curves_params(red, green, blue, &IDENTITY_CURVE)
    }

    /// Pack four channels, each any length, behind the count header.
    fn luma_curves_params(red: &[f32], green: &[f32], blue: &[f32], luma: &[f32]) -> Vec<f32> {
        let channels = [red, green, blue, luma];
        let mut params: Vec<f32> = channels
            .iter()
            .map(|points| (points.len() / 2) as f32)
            .collect();
        for points in channels {
            params.extend_from_slice(points);
        }
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

    #[test]
    fn the_luma_curve_composes_over_every_channel() {
        // A luma lift raises all three channels; folding it into the channel
        // LUTs must match evaluating the two curves in sequence.
        let lift = [0.0, 0.0, 1.0 / 3.0, 0.5, 2.0 / 3.0, 0.8, 1.0, 1.0];
        let red = [0.0, 0.0, 1.0 / 3.0, 0.25, 2.0 / 3.0, 0.6, 1.0, 1.0];
        let luma_lut = curve_lut(&lift);
        let composed = channel_lut(&red, Some(&luma_lut));
        let plain = channel_lut(&red, None);
        for step in 1..20 {
            let value = step as f32 / 20.0;
            let sequential = apply_curve_value(&luma_lut, apply_curve_value(&plain, value));
            let folded = apply_curve_value(&composed, value);
            assert!(
                (folded - sequential).abs() < 3.0e-3,
                "composition drift at {value}: {folded} vs {sequential}"
            );
            assert!(
                folded > apply_curve_value(&plain, value),
                "luma lift missing at {value}"
            );
        }
        assert!(is_identity_curve(&IDENTITY_CURVE));
        assert!(!is_identity_curve(&lift));
    }

    #[test]
    fn a_white_point_pulled_left_gains_the_channel_and_clips_above_it() {
        // The negative-scan move: drag the top-right point in, so everything
        // above it reaches full output.
        let gained = [0.0, 0.0, 0.25, 0.4, 0.5, 0.8, 0.7, 1.0];
        let ops = parse_graph(
            &GraphBuilder::new()
                .node(
                    NODE_CURVES,
                    0,
                    -1,
                    &luma_curves_params(&gained, &IDENTITY_CURVE, &IDENTITY_CURVE, &IDENTITY_CURVE),
                    None,
                )
                .build(),
        )
        .expect("a curve whose white point moved left is a legal program");
        assert_eq!(ops.len(), 1);
        let lut = curve_lut(&gained);
        let encoded_white = srgb_decode_value(0.7);
        assert!(
            apply_curve_value(&lut, encoded_white) > 0.98,
            "the moved white point should reach full output"
        );
        assert!(
            apply_curve_value(&lut, srgb_decode_value(0.85)) >= 1.0,
            "input above the white point stays clipped"
        );
        assert!(apply_curve_value(&lut, srgb_decode_value(0.2)) > srgb_decode_value(0.2));
    }

    #[test]
    fn quarter_turns_rotate_clockwise_and_lose_nothing() {
        // A 3x2 frame whose pixels carry their own coordinates, so the
        // permutation can be read off rather than inferred.
        let source = RgbImage {
            width: 3,
            height: 2,
            data: (0..6)
                .flat_map(|index| {
                    let (x, y) = (index % 3, index / 3);
                    [x as f32, y as f32, 0.0]
                })
                .collect(),
        };
        let at = |image: &RgbImage, x: usize, y: usize| {
            let base = (y * image.width + x) * 3;
            (image.data[base], image.data[base + 1])
        };
        assert_eq!(quarter_turn_image(&source, 0).data, source.data);

        let once = quarter_turn_image(&source, 1);
        assert_eq!((once.width, once.height), (2, 3));
        // One turn clockwise sends the top-left corner to the top-right.
        assert_eq!(at(&once, once.width - 1, 0), (0.0, 0.0));
        assert_eq!(at(&once, 0, 0), (0.0, 1.0));
        assert_eq!(at(&once, 0, once.height - 1), (2.0, 1.0));

        let twice = quarter_turn_image(&source, 2);
        assert_eq!((twice.width, twice.height), (3, 2));
        assert_eq!(at(&twice, 0, 0), (2.0, 1.0));

        // Four turns are the identity, and three undo one.
        let mut round = source.clone();
        for _ in 0..4 {
            round = quarter_turn_image(&round, 1);
        }
        assert_eq!(round.data, source.data);
        assert_eq!(quarter_turn_image(&once, 3).data, source.data);
        // Whole turns permute rather than resample: every pixel survives.
        let mut sorted: Vec<u32> = once.data.iter().map(|v| v.to_bits()).collect();
        let mut expected: Vec<u32> = source.data.iter().map(|v| v.to_bits()).collect();
        sorted.sort_unstable();
        expected.sort_unstable();
        assert_eq!(sorted, expected);
    }

    #[test]
    fn a_rotate_node_may_follow_film_but_optics_may_not_follow_it() {
        // Rotation reframes, so it counts as geometry: optics maps against the
        // frame it was measured on. It is domain-agnostic like a crop.
        let program = GraphBuilder::new()
            .node(NODE_FILM, 0, -1, &[0.0, 0.0, 1.0], None)
            .node(NODE_ROTATE, 1, -1, &[1.0], None)
            .build();
        assert!(parse_graph(&program).is_ok(), "rotate may follow film");
        let refused = GraphBuilder::new()
            .node(NODE_ROTATE, 0, -1, &[1.0], None)
            .node(NODE_OPTICS, 1, -1, &[1.0, 1.0, 0.0], None)
            .build();
        assert!(
            parse_graph(&refused).is_err(),
            "optics must not follow a rotation"
        );
        // Only whole quarter turns are legal.
        for turns in [-1.0_f32, 0.5, 4.0] {
            let program = GraphBuilder::new()
                .node(NODE_ROTATE, 0, -1, &[turns], None)
                .build();
            assert!(parse_graph(&program).is_err(), "accepted {turns} turns");
        }
    }

    #[test]
    fn two_endpoints_are_the_identity_and_channels_may_differ_in_length() {
        // The panel starts every channel on its two endpoints, the way Resolve's
        // custom curves do, and only the channel being shaped grows points. A
        // program mixing lengths is therefore the normal case, not an edge one.
        assert!(is_identity_curve(&ENDPOINTS_CURVE));
        let shaped = [0.0, 0.0, 0.3, 0.45, 0.62, 0.7, 1.0, 1.0];
        let params = luma_curves_params(
            &shaped,
            &ENDPOINTS_CURVE,
            &ENDPOINTS_CURVE,
            &ENDPOINTS_CURVE,
        );
        let ops = parse_graph(
            &GraphBuilder::new()
                .node(NODE_CURVES, 0, -1, &params, None)
                .build(),
        )
        .expect("channels of different lengths are a legal program");
        assert_eq!(curve_channel_points(&ops[0].params, 0), shaped);
        assert_eq!(curve_channel_points(&ops[0].params, 2), ENDPOINTS_CURVE);
        // Channel independence belongs to the node, not to the frame: the
        // display transform downstream maps a shared luminance and desaturates
        // what would clip, so lifting red legitimately moves green on screen.
        // The property worth pinning is that the untouched channels' lookups
        // stay the identity while the shaped one lifts.
        let shaped_lut = channel_lut(curve_channel_points(&ops[0].params, 0), None);
        let flat_lut = channel_lut(curve_channel_points(&ops[0].params, 1), None);
        assert!(apply_curve_value(&shaped_lut, srgb_decode_value(0.3)) > srgb_decode_value(0.3));
        for encoded in [0.1, 0.25, 0.5, 0.75, 0.9] {
            let value = srgb_decode_value(encoded);
            assert!(
                (apply_curve_value(&flat_lut, value) - value).abs() < 3.0e-3,
                "an endpoints-only channel did not pass its input through"
            );
        }
        // And the shaped channel does reach the pixels.
        let source = gradient_image();
        let curved = execute_graph(&ops, source.clone(), &context(7)).unwrap();
        let mut reference = source;
        to_display(&mut reference);
        assert!(max_difference(&curved, &reference) > 1.0e-2);
    }

    #[test]
    fn a_two_point_white_drag_gains_the_channel_without_touching_the_shadows() {
        // Exactly the gesture the reference panel is built around: grab the
        // top-right handle and pull it left. Nothing else about the curve moves,
        // so the shadows must stay put while the highlights reach full scale.
        let gained = [0.0, 0.0, 0.55, 1.0];
        let lut = curve_lut(&gained);
        assert!(apply_curve_value(&lut, srgb_decode_value(0.55)) > 0.98);
        assert!(apply_curve_value(&lut, srgb_decode_value(0.8)) >= 1.0);
        assert!(apply_curve_value(&lut, 0.0).abs() < 1.0e-6);
        let midtone = apply_curve_value(&lut, srgb_decode_value(0.3));
        assert!(
            midtone > srgb_decode_value(0.3),
            "pulling the white point in has to raise everything below it"
        );
    }

    #[test]
    fn curves_headers_that_do_not_match_their_points_are_rejected() {
        // The header is the only thing that says where a channel's points end,
        // so a program whose counts disagree with its length has to be refused
        // rather than read off the end of one channel into the next.
        let mut params = luma_curves_params(
            &ENDPOINTS_CURVE,
            &ENDPOINTS_CURVE,
            &ENDPOINTS_CURVE,
            &ENDPOINTS_CURVE,
        );
        params[0] = 3.0;
        assert!(curve_channel_spans(&params).is_err());
        params[0] = 1.0; // Fewer than the two endpoints.
        assert!(curve_channel_spans(&params).is_err());
        params[0] = 2.5; // Not a whole number of points.
        assert!(curve_channel_spans(&params).is_err());
        params[0] = (MAX_CURVE_POINTS + 1) as f32;
        assert!(curve_channel_spans(&params).is_err());
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

    fn grain_program(grain: f32) -> Vec<u8> {
        GraphBuilder::new()
            .node(NODE_EXPOSURE, 0, -1, &[0.4], None)
            .node(NODE_FILM, 1, -1, &[0.0, grain, 1.0], None)
            .build()
    }

    #[test]
    fn a_grain_edit_resumes_inside_the_film_node() {
        let key = test_prefix_key("grain-stage-test", &context(7));
        {
            let mut cache = prefix_cache()
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner());
            cache.retain(|(held, _)| held.input_path != "grain-stage-test");
        }
        let first = parse_graph(&grain_program(0.3)).unwrap();
        let second = parse_graph(&grain_program(0.7)).unwrap();
        // Only grain differs, so the reusable point is inside the film node
        // rather than at its entry.
        assert_eq!(resume_limit(&first, &second), (1, STAGE_FILM_GRAIN));
        assert_eq!(film_grain_capture(&second), Some((1, STAGE_FILM_GRAIN)));
        let source = gradient_image();
        execute_graph_cached(
            &first,
            Arc::new(source.clone()),
            &context(7),
            Some(key.clone()),
        )
        .unwrap();
        let resumed = execute_graph_cached(
            &second,
            Arc::new(source.clone()),
            &context(7),
            Some(key.clone()),
        )
        .unwrap();
        let cold = execute_graph(&second, source.clone(), &context(7)).unwrap();
        assert_eq!(max_difference(&resumed, &cold), 0.0);
        // Changing the LUT strength invalidates the stage: the film node's
        // transform has to run again from its entry.
        let strengthened = parse_graph(
            &GraphBuilder::new()
                .node(NODE_EXPOSURE, 0, -1, &[0.4], None)
                .node(NODE_FILM, 1, -1, &[0.5, 0.7, 1.0], None)
                .build(),
        )
        .unwrap();
        assert_eq!(resume_limit(&second, &strengthened), (1, STAGE_ENTRY));
        let resumed = execute_graph_cached(
            &strengthened,
            Arc::new(source.clone()),
            &context(7),
            Some(key),
        )
        .unwrap();
        let cold = execute_graph(&strengthened, source, &context(7)).unwrap();
        assert_eq!(max_difference(&resumed, &cold), 0.0);
    }

    #[test]
    fn prefix_resume_matches_a_cold_render_exactly() {
        let key = test_prefix_key("prefix-test", &context(7));
        {
            let mut cache = prefix_cache()
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner());
            cache.retain(|(held, _)| held.input_path != "prefix-test");
        }
        // First render primes the checkpoint; the tone edit then resumes
        // mid-program, with the blend pulling the source across the
        // boundary. The result must be identical to an uncached render.
        let first = parse_graph(&tone_program(0.3)).unwrap();
        let second = parse_graph(&tone_program(0.7)).unwrap();
        let source = gradient_image();
        execute_graph_cached(
            &first,
            Arc::new(source.clone()),
            &context(7),
            Some(key.clone()),
        )
        .unwrap();
        let resumed = execute_graph_cached(
            &second,
            Arc::new(source.clone()),
            &context(7),
            Some(key.clone()),
        )
        .unwrap();
        let cold = execute_graph(&second, source.clone(), &context(7)).unwrap();
        assert_eq!(resumed.width, cold.width);
        assert_eq!(resumed.height, cold.height);
        assert!(max_difference(&resumed, &cold) == 0.0);
        // A third tweak resumes from the refreshed checkpoint.
        let third = parse_graph(&tone_program(0.9)).unwrap();
        let resumed =
            execute_graph_cached(&third, Arc::new(source.clone()), &context(7), Some(key)).unwrap();
        let cold = execute_graph(&third, source, &context(7)).unwrap();
        assert!(max_difference(&resumed, &cold) == 0.0);
    }

    #[test]
    fn prefix_cache_keys_do_not_leak_across_photos() {
        let key_a = test_prefix_key("photo-a", &context(7));
        let key_b = test_prefix_key("photo-b", &context(7));
        let ops = parse_graph(&tone_program(0.5)).unwrap();
        let bright = {
            let mut image = gradient_image();
            for value in &mut image.data {
                *value *= 2.0;
            }
            image
        };
        execute_graph_cached(&ops, Arc::new(gradient_image()), &context(7), Some(key_a)).unwrap();
        // Rendering photo B with its own key must not reuse A's registers.
        let edited = parse_graph(&tone_program(0.8)).unwrap();
        let via_cache =
            execute_graph_cached(&edited, Arc::new(bright.clone()), &context(7), Some(key_b))
                .unwrap();
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
        render::apply_grain(&mut reference, 0.5, 1.0, 11, (0, 0));
        render::apply_grain(&mut reference, 0.25, 2.0, 11 ^ 0x9e37_79b9, (0, 0));
        assert_eq!(grained.data, reference.data);

        let repeat = execute_graph(&ops, gradient_image(), &context(11)).unwrap();
        assert_eq!(grained.data, repeat.data, "grain must stay deterministic");
    }
}
