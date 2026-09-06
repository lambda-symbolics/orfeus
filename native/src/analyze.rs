//! Scene analysis for interactive frontends: negative frame detection,
//! levelling by the picture's own straight edges, and linear color sampling,
//! all served from the decode cache.

use std::path::Path;

use super::Error;
use super::render::{self, DecodedRaw, RgbImage};

const ANALYSIS_EDGE: u32 = 512;
const BORDER_RING_FRACTION: f32 = 0.03;
const BASE_LIKENESS_THRESHOLD: f32 = 0.3;
const BORDER_ROW_FRACTION: f32 = 0.55;
const MINIMUM_FRAME_FRACTION: f32 = 0.2;
const FRAME_INSET_FRACTION: f32 = 0.015;

/// How far from level a straight edge may lie and still be taken for one, in
/// degrees. Past this a tilt is a composition, not a slip of the hands.
const LEVEL_REACH: f32 = 10.0;
/// The angle bin in degrees: the camera's own gauge reads to a tenth.
const LEVEL_STEP: f32 = 0.1;
/// Width of a line's distance-from-centre bin, in analysis pixels.
const LEVEL_RHO_BIN: f32 = 2.0;
/// The share of pixels that count as edges: the strongest gradients only.
const LEVEL_EDGE_FRACTION: f32 = 0.08;
/// Long edge of the image the level is read from. Twice the negative
/// analysis: a tenth of a degree along a 500-pixel edge is under a pixel.
const LEVEL_ANALYSIS_EDGE: u32 = 1024;
/// How many of the strongest lines at one angle speak for it. One line is a
/// fluke; a wall, a floor and a door frame agreeing are a level.
const LEVEL_LINES_PER_ANGLE: usize = 3;
/// Bins scoring at least this share of the peak belong to the peak: the
/// distance bins are coarse enough that a long edge tops out over a few
/// neighbouring angles, and the middle of that plateau is the answer.
const LEVEL_PLATEAU_SHARE: f32 = 0.8;
/// A winner this close to the edge of the reach says the tilt lies beyond it,
/// not that it was found.
const LEVEL_EDGE_MARGIN: f32 = 0.5;

/// A detected negative frame: the crop rectangle in oriented normalized
/// coordinates, the film-base color in scene-linear values, and the gentle
/// tilt (degrees, display convention) that straightens the frame.
pub(crate) struct NegativeFrame {
    pub(crate) rect: [f32; 4],
    pub(crate) base: [f32; 3],
    pub(crate) angle: f32,
}

/// What the picture's own straight edges say about its level.
pub(crate) struct LevelEstimate {
    /// The crop angle, in degrees of the display convention (positive turns
    /// the picture clockwise), that stands the strongest edges upright.
    pub(crate) angle: f32,
    /// How many times stronger the winning angle's strongest line is than
    /// the strongest line of the run of angles. Texture alone scores about
    /// one everywhere; under about two there was no straight edge to speak
    /// of, and zero says the winner sat at the edge of the reach.
    pub(crate) confidence: f32,
}

/// Reads the tilt of a picture from its long straight edges.
///
/// A Hough vote: every strong gradient within `LEVEL_REACH` of vertical or
/// horizontal casts its magnitude for every candidate angle of its family,
/// at the distance from the centre the line through it would have at that
/// angle. Only at the true angle do a long edge's votes pile into one
/// distance bin, so an angle's score — the sum of its `LEVEL_LINES_PER_ANGLE`
/// strongest bins — peaks there sharply and a few long edges outvote any
/// amount of texture. The confidence is the single strongest line at the
/// winning angle against the median angle's strongest line, which is what
/// texture alone scores: three lines agreeing name the angle, one line
/// standing out says there was a line at all. The gradient's own direction
/// only picks the family: read as an angle it leans towards the pixel grid
/// by a degree, which is the whole error budget here.
///
/// Deviation follows the picture: an edge whose top leans right (clockwise)
/// is a positive deviation, and the angle that levels it is its negative —
/// the same convention as the crop node's angle.
pub(crate) fn estimate_level(image: &RgbImage) -> LevelEstimate {
    let width = image.width;
    let height = image.height;
    let none = LevelEstimate {
        angle: 0.0,
        confidence: 0.0,
    };
    if width < 32 || height < 32 {
        return none;
    }
    // Brightness on a logarithmic scale, so the shadows' edges count as much
    // as the highlights' — scene-linear data hands the vote to whatever is
    // brightest, and a square root still let a lit window outvote a whole
    // dark stage (tried: sqrt, cube root, fourth root, log; log separated
    // frames with a level from frames without by the widest margin).
    let grey: Vec<f32> = image
        .data
        .chunks_exact(3)
        .map(|pixel| {
            (1.0 + 200.0 * (0.2126 * pixel[0] + 0.7152 * pixel[1] + 0.0722 * pixel[2]).max(0.0))
                .ln()
        })
        .collect();
    let smooth = blur_3x3(&grey, width, height);
    let mut gradient_x = vec![0.0f32; width * height];
    let mut gradient_y = vec![0.0f32; width * height];
    let mut magnitude = vec![0.0f32; width * height];
    for y in 1..height - 1 {
        for x in 1..width - 1 {
            let at = |dx: isize, dy: isize| {
                smooth[((y as isize + dy) as usize) * width + (x as isize + dx) as usize]
            };
            let gx = (at(1, -1) + 2.0 * at(1, 0) + at(1, 1)) - (at(-1, -1) + 2.0 * at(-1, 0) + at(-1, 1));
            let gy = (at(-1, 1) + 2.0 * at(0, 1) + at(1, 1)) - (at(-1, -1) + 2.0 * at(0, -1) + at(1, -1));
            let index = y * width + x;
            gradient_x[index] = gx;
            gradient_y[index] = gy;
            magnitude[index] = (gx * gx + gy * gy).sqrt();
        }
    }
    let mut ranked: Vec<f32> = magnitude.iter().copied().filter(|m| *m > 0.0).collect();
    if ranked.len() < 256 {
        return none;
    }
    let cut = ((ranked.len() as f32 * (1.0 - LEVEL_EDGE_FRACTION)) as usize).min(ranked.len() - 1);
    let threshold = *ranked.select_nth_unstable_by(cut, f32::total_cmp).1;
    let angle_bins = (2.0 * LEVEL_REACH / LEVEL_STEP).round() as usize;
    let half_diagonal = ((width * width + height * height) as f32).sqrt() * 0.5;
    let rho_bins = (half_diagonal / LEVEL_RHO_BIN) as usize + 2;
    let mut votes = vec![0.0f32; angle_bins * rho_bins];
    let centre_x = width as f32 * 0.5;
    let centre_y = height as f32 * 0.5;
    // The normal of every candidate line, per family: a vertical edge's
    // normal lies near the x axis, a horizontal edge's near the y axis.
    let normals: Vec<[(f32, f32); 2]> = (0..angle_bins)
        .map(|bin| {
            let deviation = -LEVEL_REACH + (bin as f32 + 0.5) * LEVEL_STEP;
            [
                deviation.to_radians().sin_cos(),
                (90.0 + deviation).to_radians().sin_cos(),
            ]
        })
        .collect();
    for y in 1..height - 1 {
        for x in 1..width - 1 {
            let index = y * width + x;
            let weight = magnitude[index];
            // Ties with the threshold are in: a synthetic edge, or a hard one
            // in a graphic, repeats one magnitude along its whole length.
            if weight < threshold {
                continue;
            }
            // The gradient's direction, degrees, 0 pointing right, 90 down.
            let theta = gradient_y[index].atan2(gradient_x[index]).to_degrees();
            // A vertical edge has a horizontal gradient: fold theta about 0
            // and 180. A horizontal edge has a vertical one: fold about 90.
            let as_vertical = (theta + 90.0).rem_euclid(180.0) - 90.0;
            let as_horizontal = theta.rem_euclid(180.0) - 90.0;
            let family = if as_vertical.abs() < LEVEL_REACH {
                0
            } else if as_horizontal.abs() < LEVEL_REACH {
                1
            } else {
                continue;
            };
            let dx = x as f32 - centre_x;
            let dy = y as f32 - centre_y;
            for (bin, normal) in normals.iter().enumerate() {
                let (sin, cos) = normal[family];
                let rho = (dx * cos + dy * sin).abs();
                let rho_bin = ((rho / LEVEL_RHO_BIN) as usize).min(rho_bins - 1);
                votes[bin * rho_bins + rho_bin] += weight;
            }
        }
    }
    // Per angle: the strongest lines' votes together, and the strongest alone.
    let (scores, strongest_lines): (Vec<f32>, Vec<f32>) = (0..angle_bins)
        .map(|bin| {
            let row = &votes[bin * rho_bins..(bin + 1) * rho_bins];
            let mut strongest = [0.0f32; LEVEL_LINES_PER_ANGLE];
            for &vote in row {
                if vote > strongest[0] {
                    strongest[0] = vote;
                    strongest.sort_by(f32::total_cmp);
                }
            }
            (strongest.iter().sum::<f32>(), strongest[LEVEL_LINES_PER_ANGLE - 1])
        })
        .unzip();
    let best = scores
        .iter()
        .enumerate()
        .max_by(|a, b| a.1.total_cmp(b.1))
        .map(|(bin, _)| bin)
        .unwrap_or(0);
    let peak = scores[best];
    if peak <= 0.0 {
        return none;
    }
    // The middle of the plateau the peak sits on, weighted by score.
    let mut low = best;
    while low > 0 && scores[low - 1] >= LEVEL_PLATEAU_SHARE * peak {
        low -= 1;
    }
    let mut high = best;
    while high + 1 < angle_bins && scores[high + 1] >= LEVEL_PLATEAU_SHARE * peak {
        high += 1;
    }
    let (weight, weighted) = (low..=high).fold((0.0f32, 0.0f32), |(weight, weighted), bin| {
        let centre = -LEVEL_REACH + (bin as f32 + 0.5) * LEVEL_STEP;
        (weight + scores[bin], weighted + scores[bin] * centre)
    });
    let deviation = weighted / weight;
    let mut ordered = strongest_lines.clone();
    let median = *ordered.select_nth_unstable_by(angle_bins / 2, f32::total_cmp).1;
    let confidence = if deviation.abs() > LEVEL_REACH - LEVEL_EDGE_MARGIN {
        0.0
    } else if median > 0.0 {
        strongest_lines[best] / median
    } else {
        100.0
    };
    LevelEstimate {
        angle: -deviation,
        confidence,
    }
}

/// A [1 2 1]/4 blur along both axes: enough to take the demosaic's grain
/// out of the gradient directions without moving an edge.
fn blur_3x3(values: &[f32], width: usize, height: usize) -> Vec<f32> {
    let mut rows = values.to_vec();
    for y in 0..height {
        let row = &values[y * width..(y + 1) * width];
        for x in 1..width - 1 {
            rows[y * width + x] = 0.25 * (row[x - 1] + 2.0 * row[x] + row[x + 1]);
        }
    }
    let mut out = rows.clone();
    for y in 1..height - 1 {
        for x in 0..width {
            out[y * width + x] =
                0.25 * (rows[(y - 1) * width + x] + 2.0 * rows[y * width + x] + rows[(y + 1) * width + x]);
        }
    }
    out
}

/// The picture's level from its own straight edges, in the display frame.
pub(crate) fn analyze_level(decoded: &DecodedRaw) -> LevelEstimate {
    let image = match render::downscale_from(
        &decoded.data,
        decoded.width,
        decoded.height,
        LEVEL_ANALYSIS_EDGE,
        LEVEL_ANALYSIS_EDGE,
    ) {
        Some(scaled) => scaled,
        None => RgbImage {
            width: decoded.width,
            height: decoded.height,
            data: decoded.data.clone(),
        },
    };
    let estimate = estimate_level(&image);
    // Quarter turns keep a clockwise lean clockwise; mirrors reverse it.
    if matches!(decoded.orientation, 2 | 4 | 5 | 7) {
        LevelEstimate {
            angle: -estimate.angle,
            confidence: estimate.confidence,
        }
    } else {
        estimate
    }
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
    let mut patch: Vec<[f32; 3]> = Vec::new();
    for sample_y in center_y - pixel_radius..=center_y + pixel_radius {
        if sample_y < 0 || sample_y >= decoded.height as isize {
            continue;
        }
        for sample_x in center_x - pixel_radius..=center_x + pixel_radius {
            if sample_x < 0 || sample_x >= decoded.width as isize {
                continue;
            }
            let offset = (sample_y as usize * decoded.width + sample_x as usize) * 3;
            patch.push([
                decoded.data[offset],
                decoded.data[offset + 1],
                decoded.data[offset + 2],
            ]);
        }
    }
    if patch.is_empty() {
        return Err(Error::InvalidArgument("sample point is outside the image"));
    }
    Ok(brightest_half_median(&mut patch))
}

/// The colour of the thinnest film in a patch: the per-channel median of the
/// brighter half of its pixels, by luminance.
///
/// A mean read whatever the patch covered. The film border a base is picked
/// from is a strip a few dozen pixels wide between the black of the holder and
/// the picture, and a patch that strayed onto either pulled the average dark or
/// coloured, so the base had to be corrected by hand afterwards. The base is by
/// definition the brightest thing in such a patch, and a median over the bright
/// half reads it through the holder, the frame edge, and the odd noisy pixel.
fn brightest_half_median(patch: &mut [[f32; 3]]) -> [f32; 3] {
    let luminance = |pixel: &[f32; 3]| 0.2127 * pixel[0] + 0.7152 * pixel[1] + 0.0722 * pixel[2];
    patch.sort_by(|a, b| luminance(b).total_cmp(&luminance(a)));
    let bright = &patch[..patch.len().div_ceil(2)];
    let mut colour = [0.0_f32; 3];
    for (channel, value) in colour.iter_mut().enumerate() {
        let mut values: Vec<f32> = bright.iter().map(|pixel| pixel[channel]).collect();
        values.sort_by(f32::total_cmp);
        *value = values[values.len() / 2];
    }
    colour
}

pub(crate) fn analyze_negative_frame_file(
    input: &Path,
    cache_mode: u32,
) -> Result<NegativeFrame, Error> {
    render::validate_cache_mode(cache_mode)?;
    let decoded = render::decoded_for_render(input, cache_mode, false)?;
    Ok(analyze_negative_frame(&decoded))
}

/// Reads a RAW file's level from its straight edges through the decode cache.
pub(crate) fn analyze_level_file(input: &Path, cache_mode: u32) -> Result<LevelEstimate, Error> {
    render::validate_cache_mode(cache_mode)?;
    let decoded = render::decoded_for_render(input, cache_mode, false)?;
    Ok(analyze_level(&decoded))
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

    /// A frame of a dark room with a bright doorway, its edges turned
    /// clockwise by DEGREES about the centre — the way a camera rolled
    /// counter-clockwise records an upright door.
    fn leaning_doorway(degrees: f32) -> RgbImage {
        let width = 640;
        let height = 480;
        let (sin, cos) = degrees.to_radians().sin_cos();
        let mut data = vec![0.02f32; width * height * 3];
        // Coverage of a half-plane, ramping over one pixel: the edges of a
        // real picture are anti-aliased by the lens and the downscale, and a
        // hard staircase would hand the gradients the pixel grid's angles.
        let soft = |distance: f32| (distance + 0.5).clamp(0.0, 1.0);
        for y in 0..height {
            for x in 0..width {
                let dx = x as f32 - width as f32 * 0.5;
                let dy = y as f32 - height as f32 * 0.5;
                // Coordinates in the door's own upright frame.
                let across = dx * cos + dy * sin;
                let along = -dx * sin + dy * cos;
                // Two jambs and a lintel: three long straight edges.
                let span = soft(120.0 - along) * soft(along + 220.0);
                let door = soft(90.0 - across.abs()) * span;
                let jamb = soft(6.0 - (across.abs() - 90.0).abs()) * span;
                let lintel = soft(6.0 - (along + 220.0).abs()) * soft(96.0 - across.abs());
                let value = (0.02 + 0.38 * door).max(0.02 + 0.88 * jamb).max(0.02 + 0.88 * lintel);
                let offset = (y * width + x) * 3;
                data[offset..offset + 3].copy_from_slice(&[value, value, value]);
            }
        }
        RgbImage { width, height, data }
    }

    /// The level estimate names the angle that stands the door upright: a
    /// door leaning 3 degrees clockwise wants the picture turned 3 degrees
    /// the other way, and the crop node's sign is clockwise-positive.
    #[test]
    fn level_reads_the_lean_of_a_doorway() {
        for lean in [3.0f32, -4.6, 0.0] {
            let estimate = estimate_level(&leaning_doorway(lean));
            assert!(
                (estimate.angle + lean).abs() <= 0.3,
                "lean {lean}: levelling angle {} should be about {}",
                estimate.angle,
                -lean
            );
            assert!(
                estimate.confidence > 2.0,
                "lean {lean}: three long edges should be a confident level, got {}",
                estimate.confidence
            );
        }
    }

    /// Texture without a straight edge in it is no evidence of level: the
    /// confidence says so, whatever angle happens to come out on top.
    #[test]
    fn level_admits_when_there_is_no_edge_to_read() {
        let width = 480;
        let height = 320;
        let mut seed = 0x2545_f491u32;
        let data: Vec<f32> = (0..width * height * 3)
            .map(|_| {
                seed ^= seed << 13;
                seed ^= seed >> 17;
                seed ^= seed << 5;
                0.1 + 0.3 * (seed as f32 / u32::MAX as f32)
            })
            .collect();
        let estimate = estimate_level(&RgbImage { width, height, data });
        assert!(
            estimate.confidence < 1.8,
            "noise should not read as a level, got confidence {}",
            estimate.confidence
        );
    }

    /// A patch half on the border and half on the picture reads the border:
    /// the base is the brightest film in it, whatever else the patch covers.
    #[test]
    fn sampling_reads_the_film_base_through_the_frame_edge() {
        let decoded = synthetic_negative(1);
        // The tile begins at x = 50 of 200; a patch of radius 8 centred on
        // the edge is half tile, half border.
        let colour = sample_linear(&decoded, 50.0 / 199.0, 60.0 / 119.0, 8.0 / 120.0).unwrap();
        for (value, base) in colour.iter().zip([0.82, 0.51, 0.33]) {
            assert!((value - base).abs() < 1.0e-6, "{colour:?}");
        }
        // One noisy pixel in the middle of the border does not move it either.
        let mut noisy = decoded;
        let offset = (10 * noisy.width + 10) * 3;
        noisy.data[offset..offset + 3].copy_from_slice(&[0.2, 0.9, 0.1]);
        let colour = sample_linear(&noisy, 10.0 / 199.0, 10.0 / 119.0, 3.0 / 120.0).unwrap();
        for (value, base) in colour.iter().zip([0.82, 0.51, 0.33]) {
            assert!((value - base).abs() < 1.0e-6, "{colour:?}");
        }
    }

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
            full_pixels: 0,
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
            full_pixels: 0,
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
