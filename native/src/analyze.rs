//! Scene analysis for interactive frontends: negative frame detection and
//! linear color sampling, both served from the decode cache.

use std::path::Path;

use super::Error;
use super::render::{self, DecodedRaw, RgbImage};

const ANALYSIS_EDGE: u32 = 512;
const BORDER_RING_FRACTION: f32 = 0.03;
const BASE_LIKENESS_THRESHOLD: f32 = 0.3;
const BORDER_ROW_FRACTION: f32 = 0.55;
const MINIMUM_FRAME_FRACTION: f32 = 0.2;
const FRAME_INSET_FRACTION: f32 = 0.015;

/// A detected negative frame: the crop rectangle in oriented normalized
/// coordinates and the film-base color in scene-linear values.
pub(crate) struct NegativeFrame {
    pub(crate) rect: [f32; 4],
    pub(crate) base: [f32; 3],
}

/// Maps a normalized rectangle from the unoriented sensor frame into display
/// (oriented) coordinates; the inverse of `graphex`'s oriented-to-sensor map.
fn map_unoriented_rect(orientation: u16, rect: [f32; 4]) -> [f32; 4] {
    let [left, top, width, height] = rect;
    match orientation {
        2 => [1.0 - left - width, top, width, height],
        3 => [1.0 - left - width, 1.0 - top - height, width, height],
        4 => [left, 1.0 - top - height, width, height],
        5 => [top, left, height, width],
        6 => [1.0 - top - height, left, height, width],
        7 => [1.0 - top - height, 1.0 - left - width, height, width],
        8 => [top, 1.0 - left - width, height, width],
        _ => rect,
    }
}

fn channel_median(values: &mut [f32]) -> f32 {
    if values.is_empty() {
        return 0.0;
    }
    let middle = values.len() / 2;
    *values
        .select_nth_unstable_by(middle, f32::total_cmp)
        .1
}

fn analysis_image(decoded: &DecodedRaw) -> RgbImage {
    match render::downscale_from(
        &decoded.data,
        decoded.width,
        decoded.height,
        ANALYSIS_EDGE,
        ANALYSIS_EDGE,
    ) {
        Some(scaled) => scaled,
        None => RgbImage {
            width: decoded.width,
            height: decoded.height,
            data: decoded.data.clone(),
        },
    }
}

fn border_base_color(image: &RgbImage) -> [f32; 3] {
    let ring_x = ((image.width as f32 * BORDER_RING_FRACTION) as usize).max(2);
    let ring_y = ((image.height as f32 * BORDER_RING_FRACTION) as usize).max(2);
    let mut channels: [Vec<f32>; 3] = [Vec::new(), Vec::new(), Vec::new()];
    for y in 0..image.height {
        for x in 0..image.width {
            let on_ring = y < ring_y
                || y >= image.height - ring_y
                || x < ring_x
                || x >= image.width - ring_x;
            if on_ring {
                let offset = (y * image.width + x) * 3;
                for channel in 0..3 {
                    channels[channel].push(image.data[offset + channel]);
                }
            }
        }
    }
    [
        channel_median(&mut channels[0]),
        channel_median(&mut channels[1]),
        channel_median(&mut channels[2]),
    ]
}

fn base_like(pixel: &[f32], base: &[f32; 3]) -> bool {
    (0..3).all(|channel| {
        (pixel[channel] - base[channel]).abs() / base[channel].max(0.02)
            < BASE_LIKENESS_THRESHOLD
    })
}

/// Returns the longest run of content (non-border) lines, or None.
fn content_span(border_flags: &[bool]) -> Option<(usize, usize)> {
    let mut best: Option<(usize, usize)> = None;
    let mut start = None;
    for (index, border) in border_flags.iter().enumerate() {
        match (border, start) {
            (false, None) => start = Some(index),
            (true, Some(begin)) => {
                let length = index - begin;
                if best.map_or(true, |(_, best_length)| length > best_length) {
                    best = Some((begin, length));
                }
                start = None;
            }
            _ => {}
        }
    }
    if let Some(begin) = start {
        let length = border_flags.len() - begin;
        if best.map_or(true, |(_, best_length)| length > best_length) {
            best = Some((begin, length));
        }
    }
    best
}

/// Detects the central exposed tile of a scanned or photographed negative.
///
/// The film base color comes from the border ring; rows and columns
/// dominated by base-like pixels count as border, and the largest remaining
/// span becomes the frame, inset slightly to hide the frame edge. Returns a
/// full-frame rectangle when no plausible tile is found, so callers can
/// always apply the result.
pub(crate) fn analyze_negative_frame(decoded: &DecodedRaw) -> NegativeFrame {
    let image = analysis_image(decoded);
    let base = border_base_color(&image);
    let row_border: Vec<bool> = (0..image.height)
        .map(|y| {
            let row = &image.data[y * image.width * 3..(y + 1) * image.width * 3];
            let base_like_count = row
                .chunks_exact(3)
                .filter(|pixel| base_like(pixel, &base))
                .count();
            base_like_count as f32 / image.width as f32 > BORDER_ROW_FRACTION
        })
        .collect();
    let column_border: Vec<bool> = (0..image.width)
        .map(|x| {
            let base_like_count = (0..image.height)
                .filter(|y| {
                    let offset = (y * image.width + x) * 3;
                    base_like(&image.data[offset..offset + 3], &base)
                })
                .count();
            base_like_count as f32 / image.height as f32 > BORDER_ROW_FRACTION
        })
        .collect();
    let rows = content_span(&row_border);
    let columns = content_span(&column_border);
    let rect = match (rows, columns) {
        (Some((row_start, row_length)), Some((column_start, column_length)))
            if row_length as f32 / image.height as f32 > MINIMUM_FRAME_FRACTION
                && column_length as f32 / image.width as f32
                    > MINIMUM_FRAME_FRACTION =>
        {
            let inset_x = FRAME_INSET_FRACTION;
            let inset_y = FRAME_INSET_FRACTION;
            let left = column_start as f32 / image.width as f32 + inset_x;
            let top = row_start as f32 / image.height as f32 + inset_y;
            let width = (column_length as f32 / image.width as f32
                - 2.0 * inset_x)
                .max(0.05);
            let height = (row_length as f32 / image.height as f32
                - 2.0 * inset_y)
                .max(0.05);
            [
                left.clamp(0.0, 0.95),
                top.clamp(0.0, 0.95),
                width.min(1.0 - left.clamp(0.0, 0.95)),
                height.min(1.0 - top.clamp(0.0, 0.95)),
            ]
        }
        _ => [0.0, 0.0, 1.0, 1.0],
    };
    NegativeFrame {
        rect: map_unoriented_rect(decoded.orientation, rect),
        base,
    }
}

/// Averages the scene-linear color around an oriented normalized point.
pub(crate) fn sample_linear(
    decoded: &DecodedRaw,
    x: f32,
    y: f32,
    radius: f32,
) -> Result<[f32; 3], Error> {
    if !(0.0..=1.0).contains(&x) || !(0.0..=1.0).contains(&y) {
        return Err(Error::InvalidArgument(
            "sample coordinates must be within 0..1",
        ));
    }
    if !(0.0..=0.25).contains(&radius) {
        return Err(Error::InvalidArgument("sample radius must be within 0..0.25"));
    }
    // Map the oriented point into the sensor frame via the rect mapping.
    let point =
        super::graphex::map_oriented_rect(decoded.orientation, [x, y, 0.0, 0.0]);
    let center_x = (point[0] * decoded.width as f32) as isize;
    let center_y = (point[1] * decoded.height as f32) as isize;
    let pixel_radius =
        ((radius * decoded.width.min(decoded.height) as f32) as isize).max(0);
    let mut sum = [0.0_f64; 3];
    let mut count = 0_u64;
    for sample_y in center_y - pixel_radius..=center_y + pixel_radius {
        if sample_y < 0 || sample_y >= decoded.height as isize {
            continue;
        }
        for sample_x in center_x - pixel_radius..=center_x + pixel_radius {
            if sample_x < 0 || sample_x >= decoded.width as isize {
                continue;
            }
            let offset =
                (sample_y as usize * decoded.width + sample_x as usize) * 3;
            for channel in 0..3 {
                sum[channel] += decoded.data[offset + channel] as f64;
            }
            count += 1;
        }
    }
    if count == 0 {
        return Err(Error::InvalidArgument("sample point is outside the image"));
    }
    Ok([
        (sum[0] / count as f64) as f32,
        (sum[1] / count as f64) as f32,
        (sum[2] / count as f64) as f32,
    ])
}

/// Runs negative-frame analysis for a RAW file through the decode cache.
pub(crate) fn analyze_negative_frame_file(
    input: &Path,
    cache_mode: u32,
) -> Result<NegativeFrame, Error> {
    let decoded = render::decoded_for_render(input, cache_mode, false)?;
    Ok(analyze_negative_frame(&decoded))
}

/// Samples linear color for a RAW file through the decode cache.
pub(crate) fn sample_linear_file(
    input: &Path,
    cache_mode: u32,
    x: f32,
    y: f32,
    radius: f32,
) -> Result<[f32; 3], Error> {
    let decoded = render::decoded_for_render(input, cache_mode, false)?;
    sample_linear(&decoded, x, y, radius)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn synthetic_negative(orientation: u16) -> DecodedRaw {
        // A bright orange film base with a dark exposed tile from 25%..75%
        // horizontally and 30%..80% vertically.
        let width = 200;
        let height = 120;
        let mut data = Vec::with_capacity(width * height * 3);
        for y in 0..height {
            for x in 0..width {
                let inside = (50..150).contains(&x) && (36..96).contains(&y);
                if inside {
                    data.extend_from_slice(&[0.12, 0.2, 0.25]);
                } else {
                    data.extend_from_slice(&[0.82, 0.51, 0.33]);
                }
            }
        }
        DecodedRaw {
            width,
            height,
            data,
            orientation,
            make: String::new(),
            model: String::new(),
            lens_name: String::new(),
            focal: 0.0,
        }
    }

    #[test]
    fn detects_the_central_tile_and_base_color() {
        let frame = analyze_negative_frame(&synthetic_negative(1));
        let [left, top, width, height] = frame.rect;
        assert!((left - 0.25).abs() < 0.05, "left {left}");
        assert!((top - 0.30).abs() < 0.06, "top {top}");
        assert!((width - 0.50).abs() < 0.08, "width {width}");
        assert!((height - 0.50).abs() < 0.09, "height {height}");
        assert!((frame.base[0] - 0.82).abs() < 0.02);
        assert!((frame.base[1] - 0.51).abs() < 0.02);
        assert!((frame.base[2] - 0.33).abs() < 0.02);
    }

    #[test]
    fn oriented_detection_transposes_the_rectangle() {
        let frame = analyze_negative_frame(&synthetic_negative(6));
        let [left, top, width, height] = frame.rect;
        // Orientation 6 swaps the axes for display.
        assert!((width - 0.50).abs() < 0.09, "width {width}");
        assert!((height - 0.50).abs() < 0.08, "height {height}");
        assert!((top - 0.25).abs() < 0.05, "top {top}");
        let _ = left;
    }

    #[test]
    fn uniform_images_fall_back_to_the_full_frame() {
        let mut uniform = synthetic_negative(1);
        for value in &mut uniform.data {
            *value = 0.5;
        }
        let frame = analyze_negative_frame(&uniform);
        assert_eq!(frame.rect, [0.0, 0.0, 1.0, 1.0]);
    }

    #[test]
    fn linear_sampling_averages_the_requested_patch() {
        let negative = synthetic_negative(1);
        let center = sample_linear(&negative, 0.5, 0.5, 0.02).unwrap();
        assert!((center[0] - 0.12).abs() < 1.0e-4);
        let border = sample_linear(&negative, 0.02, 0.02, 0.01).unwrap();
        assert!((border[0] - 0.82).abs() < 1.0e-4);
        assert!(sample_linear(&negative, 1.5, 0.5, 0.01).is_err());
    }
}
