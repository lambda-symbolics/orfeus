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
use std::sync::Arc;

use rayon::prelude::*;

use super::Error;
use super::render::{
    self, DecodedRaw, LensCorrectionOptions, RgbImage,
};

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
        if !matches!(self.output_format, render::OUTPUT_JPEG | render::OUTPUT_TIFF) {
            return Err(Error::InvalidArgument("unsupported output format"));
        }
        if !(1..=100).contains(&self.jpeg_quality) {
            return Err(Error::InvalidArgument("JPEG quality must be 1..100"));
        }
        if !self.focal_reducer.is_finite() || !(0.1..=2.0).contains(&self.focal_reducer) {
            return Err(Error::InvalidArgument("focal reducer must be 0.1..2"));
        }
        if !self.lens_crop_factor.is_finite()
            || !(0.0..=10.0).contains(&self.lens_crop_factor)
        {
            return Err(Error::InvalidArgument("lens crop factor must be 0..10"));
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
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
        NODE_WHITE_BALANCE => 2, // kelvin (0 = as shot), tint
        NODE_EXPOSURE => 1,      // ev
        NODE_NOISE_REDUCTION => 2, // edge-aware, neural
        NODE_TONE => 7,          // blacks..whites
        NODE_OPTICS => 3,        // distortion?, strength, tca?
        NODE_FILM => 3,          // lut strength, grain amount, grain size
        NODE_BLEND => 1,         // opacity toward input B
        NODE_COLOR_SUBTRACT => 3, // picked color, subtracted per channel
        NODE_CROP => 5,          // left, top, width, height, angle (degrees)
        _ => return Err(Error::InvalidArgument("unknown graph node kind")),
    })
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
/// core: inputs reference earlier nodes only, blends stay scene-linear, and
/// only film nodes may consume film output.
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
        let text = reader.text()?;
        if text.is_some() && kind != NODE_FILM {
            return Err(Error::InvalidArgument(
                "only film nodes may carry a string",
            ));
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
                // Crops keep their branch's domain.
                display[index] = display[input_a];
                let (left, top, width, height) =
                    (params[0], params[1], params[2], params[3]);
                if left + width > 1.0001 || top + height > 1.0001 {
                    return Err(Error::InvalidArgument(
                        "crop rectangle leaves the frame",
                    ));
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
    let target_width = (((width) * image.width as f32).round() as usize)
        .clamp(1, image.width - x0);
    let target_height = (((height) * image.height as f32).round() as usize)
        .clamp(1, image.height - y0);
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
    let x0 = (left * image.width as f32).round().max(0.0);
    let y0 = (top * image.height as f32).round().max(0.0);
    let target_width = ((width * image.width as f32).round() as usize)
        .clamp(1, image.width);
    let target_height = ((height * image.height as f32).round() as usize)
        .clamp(1, image.height);
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
            for (column, pixel) in
                output_row.as_chunks_mut::<3>().0.iter_mut().enumerate()
            {
                let offset_x = x0 + column as f32 + 0.5 - center_x;
                // Rotating the image by theta samples the source rotated the
                // other way around the crop center.
                let source_x =
                    center_x + cos * offset_x + sin * offset_y - 0.5;
                let source_y =
                    center_y - sin * offset_x + cos * offset_y - 0.5;
                for (channel, value) in pixel.iter_mut().enumerate() {
                    *value = render::bilinear(
                        image, source_x, source_y, channel,
                    );
                }
            }
        });
    Ok(output)
}

/// Executes OPS over SOURCE, returning the final display-domain image.
///
/// The source and every intermediate stays scene-linear until either a film
/// node or the end of the graph converts its branch for display.
pub(crate) fn execute_graph(
    ops: &[GraphOp],
    source: RgbImage,
    context: &GraphContext<'_>,
) -> Result<RgbImage, Error> {
    let count = ops.len();
    // Each register's remaining reader count, so images move instead of
    // cloning whenever an input is consumed for the last time.
    let mut uses = vec![0_usize; count + 1];
    for op in ops {
        uses[op.input_a] += 1;
        if op.kind == NODE_BLEND {
            uses[op.input_b] += 1;
        }
    }
    uses[count] += 1; // The final node feeds the output.
    if count == 0 {
        let mut image = source;
        to_display(&mut image);
        return Ok(image);
    }
    let mut registers: Vec<Option<RgbImage>> = (0..=count).map(|_| None).collect();
    let mut domains = vec![Domain::Linear; count + 1];
    registers[0] = Some(source);
    for (index, op) in ops.iter().enumerate() {
        let slot = index + 1;
        let take = |registers: &mut Vec<Option<RgbImage>>, uses: &mut Vec<usize>, from: usize|
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
                if op.params[0] > 0.0 {
                    if let Some(path) = &op.text {
                        let lut = render::CubeLut::read(Path::new(path))?;
                        render::apply_lut(&mut image, &lut, op.params[0]);
                    }
                }
                render::apply_grain(
                    &mut image,
                    op.params[1],
                    op.params[2],
                    context.grain_seed ^ (slot as u64).wrapping_mul(0x9e37_79b9),
                );
            }
            NODE_BLEND => {
                let other = take(&mut registers, &mut uses, op.input_b)?;
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
            NODE_CROP => {
                let rect = map_oriented_rect(
                    context.orientation,
                    [op.params[0], op.params[1], op.params[2], op.params[3]],
                );
                let angle = op.params[4];
                if angle.abs() < 0.01 {
                    image = crop_image(&image, rect)?;
                } else {
                    // Mirrored orientations conjugate the rotation, flipping
                    // its direction in sensor space.
                    let sensor_angle =
                        if matches!(context.orientation, 2 | 4 | 5 | 7) {
                            -angle
                        } else {
                            angle
                        };
                    image = rotate_crop_image(&image, rect, sensor_angle)?;
                }
            }
            _ => unreachable!("kinds were validated during parsing"),
        }
        registers[slot] = Some(image);
    }
    let mut image = registers[count]
        .take()
        .ok_or(Error::Render("graph produced no output".into()))?;
    if domains[count] == Domain::Linear {
        to_display(&mut image);
    }
    Ok(image)
}

/// Test hook: rotate-crop with display-convention angle at orientation 1.
#[cfg(test)]
pub(crate) fn rotate_crop_for_tests(
    image: &RgbImage,
    rect: [f32; 4],
    angle: f32,
) -> RgbImage {
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
    frame.validate()?;
    let ops = parse_graph(graph_bytes)?;
    if !matches!(cache_mode, render::CACHE_NONE | render::CACHE_USE) {
        return Err(Error::InvalidArgument("unsupported decode cache mode"));
    }
    if render::same_file(input, output)? {
        return Err(Error::InvalidArgument(
            "input and output refer to the same file",
        ));
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
    let (native_max_width, native_max_height) = render::native_downscale_bounds(
        decoded.orientation,
        frame.max_width,
        frame.max_height,
    );
    let source = match render::downscale_from(
        &decoded.data,
        decoded.width,
        decoded.height,
        native_max_width,
        native_max_height,
    ) {
        Some(scaled) => scaled,
        None => RgbImage {
            width: decoded.width,
            height: decoded.height,
            data: decoded.data.clone(),
        },
    };
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
    let image = execute_graph(&ops, source, &context)?;
    let image = render::orient(image, decoded.orientation);
    render::atomic_encode(
        input,
        output,
        &image,
        frame.output_format,
        frame.jpeg_quality,
    )
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
                self.bytes.extend_from_slice(&parameter.to_bits().to_le_bytes());
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
                .node(NODE_TONE, 2, -1, &[0.3, 0.0, 0.0, -0.2, 0.0, 0.0, 0.1], None)
                .build(),
        )
        .unwrap();
        let via_graph = execute_graph(&ops, gradient_image(), &context(7)).unwrap();

        let mut reference = gradient_image();
        render::apply_white_adaptation(&mut reference, 6500.0, 3.0);
        render::apply_exposure(&mut reference, 0.5);
        render::apply_tonal_equalizer(
            &mut reference,
            [0.3, 0.0, 0.0, -0.2, 0.0, 0.0, 0.1],
        );
        to_display(&mut reference);
        assert!(max_difference(&via_graph, &reference) < 1.0e-6);
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
        assert!(parse_graph(
            &GraphBuilder::new()
                .node(NODE_FILM, 0, -1, &[0.0, 0.3, 1.0], None)
                .node(NODE_FILM, 1, -1, &[0.0, 0.1, 2.0], None)
                .build(),
        )
        .is_ok());
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
    fn parser_rejects_malformed_programs() {
        assert!(parse_graph(b"junk").is_err());
        // Forward reference.
        assert!(parse_graph(
            &GraphBuilder::new()
                .node(NODE_EXPOSURE, 3, -1, &[0.5], None)
                .build(),
        )
        .is_err());
        // Bad parameter count.
        assert!(parse_graph(
            &GraphBuilder::new()
                .node(NODE_TONE, 0, -1, &[0.0; 3], None)
                .build(),
        )
        .is_err());
        // Out-of-range opacity.
        assert!(parse_graph(
            &GraphBuilder::new()
                .node(NODE_BLEND, 0, 0, &[1.5], None)
                .build(),
        )
        .is_err());
        // Strings on non-film nodes.
        assert!(parse_graph(
            &GraphBuilder::new()
                .node(NODE_EXPOSURE, 0, -1, &[0.5], Some("nope"))
                .build(),
        )
        .is_err());
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

    #[test]
    fn crop_keeps_film_domain_and_blend_rejects_mismatched_sizes() {
        // film -> crop -> film stays legal.
        assert!(parse_graph(
            &GraphBuilder::new()
                .node(NODE_FILM, 0, -1, &[0.0, 0.2, 1.0], None)
                .node(NODE_CROP, 1, -1, &[0.0, 0.0, 0.5, 0.5, 0.0], None)
                .node(NODE_FILM, 2, -1, &[0.0, 0.1, 1.0], None)
                .build(),
        )
        .is_ok());
        // film -> crop -> tone must stay illegal.
        assert!(parse_graph(
            &GraphBuilder::new()
                .node(NODE_FILM, 0, -1, &[0.0, 0.2, 1.0], None)
                .node(NODE_CROP, 1, -1, &[0.0, 0.0, 0.5, 0.5, 0.0], None)
                .node(NODE_TONE, 2, -1, &[0.0; 7], None)
                .build(),
        )
        .is_err());
        // A crop leaving the frame is rejected at parse time.
        assert!(parse_graph(
            &GraphBuilder::new()
                .node(NODE_CROP, 0, -1, &[0.75, 0.0, 0.5, 1.0, 0.0], None)
                .build(),
        )
        .is_err());
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
                .build(),
        )
        .unwrap();
        let grained = execute_graph(&ops, gradient_image(), &context(11)).unwrap();
        let plain = execute_graph(&[], gradient_image(), &context(11)).unwrap();
        assert!(max_difference(&grained, &plain) > 0.005);
        assert!(grained.data.iter().all(|value| (0.0..=1.0).contains(value)));

        let repeat = execute_graph(&ops, gradient_image(), &context(11)).unwrap();
        assert_eq!(grained.data, repeat.data, "grain must stay deterministic");
    }
}
