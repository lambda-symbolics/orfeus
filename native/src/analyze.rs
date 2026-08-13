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
/// coordinates, the film-base color in scene-linear values, and the gentle
/// tilt (degrees, display convention) that straightens the frame.
pub(crate) struct NegativeFrame {
    pub(crate) rect: [f32; 4],
    pub(crate) base: [f32; 3],
    pub(crate) angle: f32,
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
    *values.select_nth_unstable_by(middle, f32::total_cmp).1
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
            let on_ring =
                y < ring_y || y >= image.height - ring_y || x < ring_x || x >= image.width - ring_x;
            if on_ring {
                let offset = (y * image.width + x) * 3;
                for (channel, values) in channels.iter_mut().enumerate() {
                    values.push(image.data[offset + channel]);
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
        (pixel[channel] - base[channel]).abs() / base[channel].max(0.02) < BASE_LIKENESS_THRESHOLD
    })
}

/// Returns the content (non-border) run to treat as the frame, or None.
///
/// Film carriers like the JJC set expose extra windows near an edge (a strip
/// of film edge with sprockets), so the run containing the axis midpoint wins
/// over a longer run elsewhere; the longest run is only a fallback.
fn content_span(border_flags: &[bool]) -> Option<(usize, usize)> {
    let mut runs: Vec<(usize, usize)> = Vec::new();
    let mut start = None;
    for (index, border) in border_flags.iter().enumerate() {
        match (border, start) {
            (false, None) => start = Some(index),
            (true, Some(begin)) => {
                runs.push((begin, index - begin));
                start = None;
            }
            _ => {}
        }
    }
    if let Some(begin) = start {
        runs.push((begin, border_flags.len() - begin));
    }
    let midpoint = border_flags.len() / 2;
    runs.iter()
        .copied()
        .find(|(begin, length)| *begin <= midpoint && midpoint < begin + length)
        .or_else(|| runs.into_iter().max_by_key(|(_, length)| *length))
}

/// Estimates the frame tilt from the left and right content edges.
///
/// Fits the first and last base-to-content transitions across the frame's
/// inner rows and converts the mean slope into the display-space angle that
/// straightens the frame through a crop node. Returns 0 when the evidence is
/// weak, keeping flimsy-carrier detection conservative.
fn estimate_frame_angle(
    image: &RgbImage,
    base: &[f32; 3],
    rows: (usize, usize),
    columns: (usize, usize),
) -> f32 {
    let (row_start, row_length) = rows;
    let (column_start, column_length) = columns;
    let inset = (row_length / 10).max(2);
    let probe_start = row_start + inset;
    let probe_end = (row_start + row_length).saturating_sub(inset);
    if probe_end <= probe_start + 8 {
        return 0.0;
    }
    let margin = (column_length / 4).max(4);
    let mut points_left: Vec<(f32, f32)> = Vec::new();
    let mut points_right: Vec<(f32, f32)> = Vec::new();
    for y in probe_start..probe_end {
        let row = &image.data[y * image.width * 3..(y + 1) * image.width * 3];
        let scan_from = column_start.saturating_sub(margin);
        let scan_to = (column_start + margin).min(image.width);
        for x in scan_from..scan_to {
            if !base_like(&row[x * 3..x * 3 + 3], base) {
                points_left.push((y as f32, x as f32));
                break;
            }
        }
        let right_edge = column_start + column_length;
        let scan_from = right_edge.saturating_sub(margin);
        let scan_to = (right_edge + margin).min(image.width);
        for x in (scan_from..scan_to).rev() {
            if !base_like(&row[x * 3..x * 3 + 3], base) {
                points_right.push((y as f32, x as f32));
                break;
            }
        }
    }
    let slope = |points: &[(f32, f32)]| -> Option<f32> {
        if points.len() < 8 {
            return None;
        }
        let count = points.len() as f32;
        let mean_y = points.iter().map(|(y, _)| y).sum::<f32>() / count;
        let mean_x = points.iter().map(|(_, x)| x).sum::<f32>() / count;
        let mut numerator = 0.0;
        let mut denominator = 0.0;
        for (y, x) in points {
            numerator += (y - mean_y) * (x - mean_x);
            denominator += (y - mean_y) * (y - mean_y);
        }
        if denominator < 1.0 {
            None
        } else {
            Some(numerator / denominator)
        }
    };
    let slopes: Vec<f32> = [slope(&points_left), slope(&points_right)]
        .into_iter()
        .flatten()
        .collect();
    if slopes.is_empty() {
        return 0.0;
    }
    // Edges of one rigid frame must agree; disagreement means bad evidence.
    if slopes.len() == 2 && (slopes[0] - slopes[1]).abs() > 0.03 {
        return 0.0;
    }
    let mean = slopes.iter().sum::<f32>() / slopes.len() as f32;
    let angle = mean.atan().to_degrees().clamp(-7.0, 7.0);
    if angle.abs() < 0.3 { 0.0 } else { angle }
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
    let (rect, sensor_angle) = match (rows, columns) {
        (Some((row_start, row_length)), Some((column_start, column_length)))
            if row_length as f32 / image.height as f32 > MINIMUM_FRAME_FRACTION
                && column_length as f32 / image.width as f32 > MINIMUM_FRAME_FRACTION =>
        {
            let angle = estimate_frame_angle(
                &image,
                &base,
                (row_start, row_length),
                (column_start, column_length),
            );
            let inset_x = FRAME_INSET_FRACTION;
            let inset_y = FRAME_INSET_FRACTION;
            let left = column_start as f32 / image.width as f32 + inset_x;
            let top = row_start as f32 / image.height as f32 + inset_y;
            let width = (column_length as f32 / image.width as f32 - 2.0 * inset_x).max(0.05);
            let height = (row_length as f32 / image.height as f32 - 2.0 * inset_y).max(0.05);
            (
                [
                    left.clamp(0.0, 0.95),
                    top.clamp(0.0, 0.95),
                    width.min(1.0 - left.clamp(0.0, 0.95)),
                    height.min(1.0 - top.clamp(0.0, 0.95)),
                ],
                angle,
            )
        }
        _ => ([0.0, 0.0, 1.0, 1.0], 0.0),
    };
    // Mirrored orientations flip the visual rotation direction.
    let display_angle = if matches!(decoded.orientation, 2 | 4 | 5 | 7) {
        -sensor_angle
    } else {
        sensor_angle
    };
    NegativeFrame {
        rect: map_unoriented_rect(decoded.orientation, rect),
        base,
        angle: display_angle,
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
        return Err(Error::InvalidArgument(
            "sample radius must be within 0..0.25",
        ));
    }
    // Map the oriented point into the sensor frame via the rect mapping.
    let point = super::graphex::map_oriented_rect(decoded.orientation, [x, y, 0.0, 0.0]);
    let center_x = (point[0] * decoded.width.saturating_sub(1) as f32).round() as isize;
    let center_y = (point[1] * decoded.height.saturating_sub(1) as f32).round() as isize;
    let pixel_radius = ((radius * decoded.width.min(decoded.height) as f32) as isize).max(0);
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
            let offset = (sample_y as usize * decoded.width + sample_x as usize) * 3;
            for (channel, total) in sum.iter_mut().enumerate() {
                *total += decoded.data[offset + channel] as f64;
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
    render::validate_cache_mode(cache_mode)?;
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
    render::validate_cache_mode(cache_mode)?;
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
            as_shot_kelvin: None,
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

    fn jjc_carrier_negative() -> DecodedRaw {
        // The JJC-style holder: the main tile centered, plus a thin film-edge
        // window near the top that must not confuse frame detection.
        let mut negative = synthetic_negative(1);
        let width = negative.width;
        for y in 4..14 {
            for x in 30..170 {
                let offset = (y * width + x) * 3;
                negative.data[offset] = 0.2;
                negative.data[offset + 1] = 0.28;
                negative.data[offset + 2] = 0.3;
            }
        }
        negative
    }

    #[test]
    fn film_edge_window_does_not_hijack_the_frame() {
        let frame = analyze_negative_frame(&jjc_carrier_negative());
        let [left, top, width, height] = frame.rect;
        assert!(top > 0.2, "detection latched onto the top window: {top}");
        assert!((left - 0.25).abs() < 0.05, "left {left}");
        assert!((width - 0.50).abs() < 0.08, "width {width}");
        assert!((height - 0.50).abs() < 0.09, "height {height}");
        assert_eq!(frame.angle, 0.0);
    }

    fn tilted_negative(degrees: f32) -> DecodedRaw {
        let width = 400;
        let height = 240;
        let (sin, cos) = degrees.to_radians().sin_cos();
        let (center_x, center_y) = (width as f32 / 2.0, height as f32 / 2.0);
        let (half_width, half_height) = (100.0_f32, 60.0_f32);
        let mut data = Vec::with_capacity(width * height * 3);
        for y in 0..height {
            for x in 0..width {
                // Rotate the point back by the tilt; inside the upright
                // rectangle means inside the tilted frame.
                let dx = x as f32 - center_x;
                let dy = y as f32 - center_y;
                let local_x = cos * dx + sin * dy;
                let local_y = -sin * dx + cos * dy;
                let inside = local_x.abs() < half_width && local_y.abs() < half_height;
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
            orientation: 1,
            make: String::new(),
            model: String::new(),
            lens_name: String::new(),
            focal: 0.0,
            as_shot_kelvin: None,
        }
    }

    #[test]
    fn gentle_tilt_is_detected_and_straightens_the_crop() {
        let tilt = 3.0_f32;
        let negative = tilted_negative(tilt);
        let frame = analyze_negative_frame(&negative);
        assert!(frame.angle.abs() > 1.0, "no tilt detected: {}", frame.angle);
        // Applying the crop with the reported angle must straighten the
        // frame: probe points just inside each mid-edge must be tile-colored.
        let image = RgbImage {
            width: negative.width,
            height: negative.height,
            data: negative.data.clone(),
        };
        let cropped = super::super::graphex::rotate_crop_for_tests(&image, frame.rect, frame.angle);
        let probe = |x: usize, y: usize| -> f32 { cropped.data[(y * cropped.width + x) * 3] };
        let inset = 6;
        let tile = 0.12_f32;
        for (x, y) in [
            (cropped.width / 2, inset),
            (cropped.width / 2, cropped.height - 1 - inset),
            (inset, cropped.height / 2),
            (cropped.width - 1 - inset, cropped.height / 2),
        ] {
            assert!(
                (probe(x, y) - tile).abs() < 0.05,
                "edge probe at {x},{y} saw {}, frame is still tilted",
                probe(x, y)
            );
        }
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

    #[test]
    fn normalized_sample_endpoints_address_edge_pixels() {
        let mut negative = synthetic_negative(1);
        let last = (negative.width * negative.height - 1) * 3;
        negative.data[last..last + 3].copy_from_slice(&[0.1, 0.2, 0.3]);
        assert_eq!(
            sample_linear(&negative, 1.0, 1.0, 0.0).unwrap(),
            [0.1, 0.2, 0.3]
        );

        negative.data[0..3].copy_from_slice(&[0.4, 0.5, 0.6]);
        assert_eq!(
            sample_linear(&negative, 0.0, 0.0, 0.0).unwrap(),
            [0.4, 0.5, 0.6]
        );
    }

    #[test]
    fn file_analysis_rejects_unknown_cache_modes_before_decoding() {
        let missing = Path::new("definitely-missing-analysis-input.orf");
        assert!(matches!(
            analyze_negative_frame_file(missing, 99),
            Err(Error::InvalidArgument("unsupported decode cache mode"))
        ));
        assert!(matches!(
            sample_linear_file(missing, 99, 0.5, 0.5, 0.0),
            Err(Error::InvalidArgument("unsupported decode cache mode"))
        ));
    }
}
