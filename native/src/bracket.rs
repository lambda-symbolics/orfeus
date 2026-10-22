//! Merging an exposure bracket into one scene-linear frame.
//!
//! The camera shoots the frames a fraction of a second apart, so between them
//! the hand moves a little, and anything in the scene may move a lot. Merging
//! is therefore three steps: find how far each frame drifted from the metered
//! one, weigh every frame by how much light it gathered wherever it is not
//! clipped, and refuse a frame wherever it disagrees with the frame it is
//! compared to by more than its noise explains — a person who walked, a branch
//! in the wind, or the smear a long exposure made of either.
//!
//! Everything happens in camera RGB scaled to the sensor's white level, before
//! white balance and the colour matrix, because that is where every channel of
//! every frame clips at the same known value: 1.0.

use std::path::Path;

use rawler::imgop::develop::Intermediate;
use rawler::pixarray::Color2D;
use rayon::prelude::*;

use super::Error;
use super::render::{self, DecodedRaw, Demosaic, DevelopedFrame};

/// Above this a channel is near enough to clipping that its value is no longer
/// trusted; the weight fades to nothing between the two.
const TRUST_UP_TO: f32 = 0.85;
const CLIPPED_AT: f32 = 0.97;
/// Square blocks the agreement test is made over: big enough that the noise of
/// a short frame averages down, small enough to outline a moving person.
const BLOCK: usize = 8;
/// Sensor noise in white-level units: shot noise variance per unit signal, and
/// read noise. Conservative for the OM-1 at base ISO; a block mean divides both.
const SHOT_NOISE: f32 = 1.0e-4;
const READ_NOISE: f32 = 1.0e-3;
/// The ISO those two are stated at.
const BASE_ISO: f32 = 200.0;

pub(crate) struct Frame {
    width: usize,
    height: usize,
    pixels: Vec<[f32; 3]>,
    /// Light gathered relative to the reference frame: 4.0 is two stops more.
    exposure: f32,
    /// Sensor gain relative to base ISO. Noise in white-level units grows with
    /// it: a night bracket at ISO 800 is four times noisier than one at 200,
    /// and judging it by the base-ISO figures rejected every clean long frame
    /// as having moved.
    gain: f32,
}

impl Frame {
    /// The pixel at (X, Y) of the reference's grid. Every frame has been laid
    /// onto that grid by the time anything asks; NAN marks where a frame did
    /// not reach.
    fn at(&self, x: isize, y: isize) -> Option<[f32; 3]> {
        if x < 0 || y < 0 || x as usize >= self.width || y as usize >= self.height {
            return None;
        }
        let pixel = self.pixels[y as usize * self.width + x as usize];
        pixel[0].is_finite().then_some(pixel)
    }

    /// This frame resampled onto the reference's grid through PLACEMENT.
    fn warped(&self, placement: &Field) -> Vec<[f32; 3]> {
        let (width, height) = (self.width, self.height);
        (0..width * height)
            .into_par_iter()
            .map(|index| {
                let (x, y) = placement.map((index % width) as f32, (index / width) as f32);
                let (x0, y0) = (x.floor(), y.floor());
                if x0 < 0.0 || y0 < 0.0 || x0 + 1.0 >= width as f32 || y0 + 1.0 >= height as f32 {
                    return [f32::NAN; 3];
                }
                let (tx, ty) = (x - x0, y - y0);
                let (x0, y0) = (x0 as usize, y0 as usize);
                let at = |x: usize, y: usize| self.pixels[y * width + x];
                let (a, b, c, d) = (at(x0, y0), at(x0 + 1, y0), at(x0, y0 + 1), at(x0 + 1, y0 + 1));
                std::array::from_fn(|channel| {
                    let top = a[channel] * (1.0 - tx) + b[channel] * tx;
                    let bottom = c[channel] * (1.0 - tx) + d[channel] * tx;
                    top * (1.0 - ty) + bottom * ty
                })
            })
            .collect()
    }
}

/// Where each part of a frame lies relative to the reference: a rigid
/// PLACEMENT (turn, scale and shift) plus a per-tile residual on top of it,
/// eased between tile centres.
///
/// The residual, not the total displacement, is what is smoothed and
/// interpolated: a rigid rotation's own displacement already varies exactly
/// linearly across the frame by construction, so a median of neighbouring
/// *residuals* can suppress a bad match without also distorting the rotation
/// itself the way a median of neighbouring *total* displacements did, and
/// clamping a residual past the outer tile centres leaves PLACEMENT's own
/// continuous, exact prediction there instead of repeating the outermost
/// tile's total displacement past where it was actually measured.
#[derive(Debug)]
struct Field {
    placement: Placement,
    across: usize,
    down: usize,
    tile_width: f32,
    tile_height: f32,
    /// Each tile's own measured displacement minus PLACEMENT's prediction at
    /// that tile's centre.
    residuals: Vec<(f32, f32)>,
}

impl Field {
    fn map(&self, x: f32, y: f32) -> (f32, f32) {
        let fx = (x / self.tile_width - 0.5).clamp(0.0, (self.across - 1) as f32);
        let fy = (y / self.tile_height - 0.5).clamp(0.0, (self.down - 1) as f32);
        let (x0, y0) = (fx.floor() as usize, fy.floor() as usize);
        let (x1, y1) = ((x0 + 1).min(self.across - 1), (y0 + 1).min(self.down - 1));
        let (tx, ty) = (fx - x0 as f32, fy - y0 as f32);
        let at = |column: usize, row: usize| self.residuals[row * self.across + column];
        let blend = |a: (f32, f32), b: (f32, f32), t: f32| {
            (a.0 * (1.0 - t) + b.0 * t, a.1 * (1.0 - t) + b.1 * t)
        };
        let (rx, ry) = blend(blend(at(x0, y0), at(x1, y0), tx), blend(at(x0, y1), at(x1, y1), tx), ty);
        let (px, py) = self.placement.map(x, y);
        (px + rx, py + ry)
    }

    /// The mean of the per-tile residuals on top of PLACEMENT's own overall
    /// shift -- a diagnostic figure, not the rigid turn itself.
    fn mean_shift(&self) -> (f32, f32) {
        let count = self.residuals.len() as f32;
        let (x, y) = self.residuals.iter().fold((0.0, 0.0), |(x, y), (dx, dy)| (x + dx, y + dy));
        (self.placement.tx + x / count, self.placement.ty + y / count)
    }
}

/// Where a frame lies relative to the reference: the reference's pixel (x, y)
/// is the frame's (a·x − b·y + tx, b·x + a·y + ty). A turn and a slight change
/// of scale as well as a shift, because a hand holding a camera for a second
/// rolls it as well as drifting.
#[derive(Clone, Copy, Debug)]
struct Placement {
    a: f32,
    b: f32,
    tx: f32,
    ty: f32,
}

impl Placement {
    fn shift(dx: f32, dy: f32) -> Self {
        Self { a: 1.0, b: 0.0, tx: dx, ty: dy }
    }

    fn map(&self, x: f32, y: f32) -> (f32, f32) {
        (self.a * x - self.b * y + self.tx, self.b * x + self.a * y + self.ty)
    }

    /// The least-squares turn, scale and shift taking each point to its
    /// match, dropping matches that disagree with the rest.
    fn fit(matches: &[((f32, f32), (f32, f32))]) -> Option<Self> {
        let mut kept: Vec<((f32, f32), (f32, f32))> = matches.to_vec();
        let mut placement = None;
        for _ in 0..4 {
            if kept.len() < 4 {
                return placement;
            }
            let count = kept.len() as f32;
            let (mx, my) = kept.iter().fold((0.0, 0.0), |(x, y), ((px, py), _)| (x + px, y + py));
            let (nx, ny) = kept.iter().fold((0.0, 0.0), |(x, y), (_, (qx, qy))| (x + qx, y + qy));
            let (mx, my, nx, ny) = (mx / count, my / count, nx / count, ny / count);
            let (mut along, mut across, mut norm) = (0.0, 0.0, 0.0);
            for ((px, py), (qx, qy)) in &kept {
                let (px, py, qx, qy) = (px - mx, py - my, qx - nx, qy - ny);
                along += px * qx + py * qy;
                across += px * qy - py * qx;
                norm += px * px + py * py;
            }
            if norm <= 0.0 {
                return placement;
            }
            // A turn only: the lens does not zoom between frames of a
            // bracket, and a fitted scale extrapolates badly to whichever end
            // of the frame had less to match.
            let length = (along * along + across * across).sqrt();
            if length <= 0.0 {
                return placement;
            }
            let (a, b) = (along / length, across / length);
            let fitted = Self { a, b, tx: nx - (a * mx - b * my), ty: ny - (b * mx + a * my) };
            placement = Some(fitted);
            let residuals: Vec<f32> = kept
                .iter()
                .map(|((px, py), (qx, qy))| {
                    let (x, y) = fitted.map(*px, *py);
                    ((x - qx).powi(2) + (y - qy).powi(2)).sqrt()
                })
                .collect();
            let mut sorted = residuals.clone();
            sorted.sort_by(f32::total_cmp);
            let limit = (3.0 * sorted[sorted.len() / 2]).max(1.5);
            let before = kept.len();
            kept = kept
                .into_iter()
                .zip(residuals)
                .filter(|(_, residual)| *residual <= limit)
                .map(|(pair, _)| pair)
                .collect();
            if kept.len() == before {
                break;
            }
        }
        placement
    }
}

fn three_color(developed: Intermediate) -> Result<Color2D<f32, 3>, Error> {
    match developed {
        Intermediate::ThreeColor(pixels) => Ok(pixels),
        _ => Err(Error::Render(
            "only three-colour sensors can be merged as a bracket".into(),
        )),
    }
}

/// Develops INPUTS and merges them into one frame. The first input is the
/// reference: its framing, colour and metadata are the result's, and the merged
/// values are stated at its exposure, so highlights it clipped come back above
/// 1.0 rather than everything being scaled down.
pub(crate) fn merge_bracket(
    inputs: &[&Path],
    draft: bool,
    method: Demosaic,
    profiling: bool,
) -> Result<DecodedRaw, Error> {
    if inputs.is_empty() {
        return Err(Error::InvalidArgument("a bracket needs at least one frame"));
    }
    let developed = inputs
        .par_iter()
        .map(|input| render::develop_frame(input, draft, method, profiling))
        .collect::<Result<Vec<DevelopedFrame>, Error>>()?;
    let mut developed = developed.into_iter();
    let mut reference = developed.next().expect("checked non-empty");
    let reference_exposure = reference
        .exposure()
        .ok_or_else(|| Error::Render("the reference frame records no exposure".into()))?;
    let mut frames = Vec::with_capacity(inputs.len());
    let reference_pixels = three_color(std::mem::replace(
        &mut reference.developed,
        Intermediate::ThreeColor(Color2D::new_with(Vec::new(), 0, 0)),
    ))?;
    frames.push(Frame {
        width: reference_pixels.width,
        height: reference_pixels.height,
        pixels: reference_pixels.data,
        exposure: 1.0,
        gain: reference.iso() / BASE_ISO,
    });
    for other in developed {
        let exposure = other
            .exposure()
            .ok_or_else(|| Error::Render("a bracket frame records no exposure".into()))?;
        let gain = other.iso() / BASE_ISO;
        let pixels = three_color(other.developed)?;
        if (pixels.width, pixels.height) != (frames[0].width, frames[0].height) {
            return Err(Error::Render("bracket frames differ in size".into()));
        }
        frames.push(Frame {
            width: pixels.width,
            height: pixels.height,
            pixels: pixels.data,
            exposure: exposure / reference_exposure,
            gain,
        });
    }
    let started = std::time::Instant::now();
    // Each frame is laid onto its neighbour in exposure on the way to the
    // reference, once that neighbour is on the reference's grid itself. Two
    // stops apart, two frames see much the same parts of the scene; four or
    // six apart, the brighter one has clipped what the darker one shows, and
    // there is little left to match.
    let mut by_exposure: Vec<usize> = (0..frames.len()).collect();
    by_exposure.sort_by(|a, b| frames[*a].exposure.total_cmp(&frames[*b].exposure));
    let reference_rank = by_exposure
        .iter()
        .position(|index| *index == 0)
        .expect("the reference is a frame");
    let mut placements: Vec<Option<Field>> = (0..frames.len()).map(|_| None).collect();
    let mut steps: Vec<Vec<(usize, usize)>> = Vec::new();
    for distance in 1..frames.len() {
        let mut step = Vec::new();
        if let Some(rank) = reference_rank.checked_sub(distance) {
            step.push((by_exposure[rank], by_exposure[rank + 1]));
        }
        if reference_rank + distance < frames.len() {
            let rank = reference_rank + distance;
            step.push((by_exposure[rank], by_exposure[rank - 1]));
        }
        if step.is_empty() {
            break;
        }
        steps.push(step);
    }
    for step in steps {
        let placed: Vec<(usize, Field, f32)> = step
            .par_iter()
            .map(|(index, onto)| {
                let target = Guide::new(&frames[*onto]);
                let guide = Guide::new(&frames[*index]);
                let shift = target.align(&guide);
                let placement = target.place(&guide, shift);
                let offset = target.offset(&guide, shift).unwrap_or(0.0);
                (*index, placement, offset)
            })
            .collect();
        for (index, placement, offset) in placed {
            let frame = &mut frames[index];
            frame.pixels = frame.warped(&placement);
            // The recorded shutter time is a nominal one; what the frame
            // actually gathered is measured where both frames can be read.
            frame.exposure *= offset.exp2();
            placements[index] = Some(placement);
        }
    }
    let placements: Vec<(Field, f32)> = placements
        .into_iter()
        .skip(1)
        .map(|placement| (placement.expect("every frame was placed"), 0.0))
        .collect();
    if profiling {
        eprintln!(
            "orfeus-profile bracket-align milliseconds={:.3} placements={:?} exposures={:?}",
            started.elapsed().as_secs_f64() * 1000.0,
            placements.iter().map(|(placement, _)| placement.mean_shift()).collect::<Vec<_>>(),
            frames.iter().map(|frame| frame.exposure).collect::<Vec<_>>()
        );
    }
    #[cfg(test)]
    if let Ok(prefix) = std::env::var("ORFEUS_BRACKET_FRAMES") {
        // Each frame as merged sees it: shifted onto the reference and scaled
        // to its exposure, developed with the reference's colour.
        for (number, frame) in frames.iter().enumerate() {
            let (width, height) = (frames[0].width, frames[0].height);
            let pixels: Vec<[f32; 3]> = (0..width * height)
                .map(|index| {
                    frame
                        .at((index % width) as isize, (index / width) as isize)
                        .unwrap_or([0.0; 3])
                        .map(|value| value / frame.exposure)
                })
                .collect();
            let decoded = render::DevelopedFrame {
                developed: Intermediate::ThreeColor(Color2D::new_with(pixels, width, height)),
                white_balance: reference.white_balance,
                color_matrix: reference.color_matrix.clone(),
                metadata: reference.metadata.clone(),
                full_pixels: reference.full_pixels,
            }
            .into_decoded()?;
            tests::write_preview(&decoded, &format!("{prefix}-frame{number}.jpg"));
        }
    }
    let started = std::time::Instant::now();
    let merged = merge(&frames);
    if profiling {
        eprintln!(
            "orfeus-profile bracket-merge milliseconds={:.3}",
            started.elapsed().as_secs_f64() * 1000.0
        );
    }
    let (width, height) = (frames[0].width, frames[0].height);
    reference.developed = Intermediate::ThreeColor(Color2D::new_with(merged, width, height));
    reference.into_decoded()
}

/// How much to trust a pixel of a frame, from its brightest channel.
fn unclipped(pixel: [f32; 3]) -> f32 {
    let peak = pixel[0].max(pixel[1]).max(pixel[2]);
    ((CLIPPED_AT - peak) / (CLIPPED_AT - TRUST_UP_TO)).clamp(0.0, 1.0)
}

fn luminance(pixel: [f32; 3]) -> f32 {
    (pixel[0] + pixel[1] + pixel[2]) / 3.0
}

/// A frame's log brightness at a reduced size, stated at the reference's
/// exposure so frames of different exposures can be compared, with NAN where
/// the frame cannot say: clipped, or too dark for its noise.
struct Guide {
    levels: Vec<(usize, usize, Vec<f32>)>,
    /// Each level less its own local mean: edges and texture, which is what
    /// aligning should look at. Brightness itself would let a smooth sky, or
    /// the different falloff of a lens at two exposures, pull the answer.
    detail: Vec<Vec<f32>>,
}

/// VALUES halved until a level is small enough to search exhaustively, the
/// full size first. NAN wherever any of the four below it was.
fn pyramid(values: Vec<f32>, width: usize, height: usize) -> Vec<(usize, usize, Vec<f32>)> {
    let mut levels = vec![(width, height, values)];
    while levels.last().expect("one level").0 > 384 {
        let (width, height, source) = levels.last().expect("one level");
        let (width, half_width, half_height) = (*width, width / 2, height / 2);
        let next: Vec<f32> = (0..half_width * half_height)
            .into_par_iter()
            .map(|index| {
                let (x, y) = (index % half_width * 2, index / half_width * 2);
                (source[y * width + x]
                    + source[y * width + x + 1]
                    + source[(y + 1) * width + x]
                    + source[(y + 1) * width + x + 1])
                    / 4.0
            })
            .collect();
        levels.push((half_width, half_height, next));
    }
    levels
}

/// VALUES less their mean over a square of RADIUS each way, NAN-aware.
fn high_pass(values: &[f32], width: usize, height: usize, radius: usize) -> Vec<f32> {
    let rows: Vec<(f32, f32)> = (0..width * height)
        .into_par_iter()
        .map(|index| {
            let (x, y) = (index % width, index / width);
            let mut total = 0.0;
            let mut count = 0.0;
            for x in x.saturating_sub(radius)..=(x + radius).min(width - 1) {
                let value = values[y * width + x];
                if value.is_finite() {
                    total += value;
                    count += 1.0;
                }
            }
            (total, count)
        })
        .collect();
    (0..width * height)
        .into_par_iter()
        .map(|index| {
            let value = values[index];
            if !value.is_finite() {
                return f32::NAN;
            }
            let (x, y) = (index % width, index / width);
            let mut total = 0.0;
            let mut count = 0.0;
            for y in y.saturating_sub(radius)..=(y + radius).min(height - 1) {
                let (row_total, row_count) = rows[y * width + x];
                total += row_total;
                count += row_count;
            }
            value - total / count
        })
        .collect()
}

impl Guide {
    fn new(frame: &Frame) -> Self {
        // Brightness, for measuring exposure, keeps only what is bright enough
        // to be read without its noise. Detail, for aligning, keeps everything
        // that is not clipped: a dark half of the frame still has shapes to
        // align on, and leaving it out let the bright half decide a turn that
        // was wrong at the other end.
        let floor = 0.004;
        let readable: Vec<f32> = frame
            .pixels
            .par_iter()
            .map(|pixel| {
                let peak = pixel[0].max(pixel[1]).max(pixel[2]);
                let level = luminance(*pixel);
                if peak >= TRUST_UP_TO || level <= floor {
                    f32::NAN
                } else {
                    (level / frame.exposure).log2()
                }
            })
            .collect();
        let shaped: Vec<f32> = frame
            .pixels
            .par_iter()
            .map(|pixel| {
                let peak = pixel[0].max(pixel[1]).max(pixel[2]);
                // Linear, so that what carries the match is contrast: a clean
                // bright edge outweighs the noise of a dark roof, which in log
                // terms was the louder of the two.
                if peak >= TRUST_UP_TO {
                    f32::NAN
                } else {
                    luminance(*pixel) / frame.exposure
                }
            })
            .collect();
        let levels = pyramid(readable, frame.width, frame.height);
        let detail = pyramid(shaped, frame.width, frame.height)
            .into_iter()
            .map(|(width, height, values)| high_pass(&values, width, height, 3))
            .collect();
        Self { levels, detail }
    }

    /// Mean absolute log difference where both can say, sampling every STRIDE
    /// pixels, or NONE when too little overlaps to judge.
    fn cost(
        &self,
        other: &Guide,
        level: usize,
        shift: (isize, isize),
        stride: usize,
        region: Option<[isize; 4]>,
    ) -> Option<f32> {
        let (width, height, _) = &self.levels[level];
        let ours = &self.detail[level];
        let theirs = &other.detail[level];
        let (width, height) = (*width as isize, *height as isize);
        let [left, top, right, bottom] = region.unwrap_or([0, 0, width, height]);
        // One less the normalised correlation of the two: blind to how bright
        // either frame is, which is the one thing a bracket varies on purpose.
        let rows: Vec<[f64; 6]> = (top.max(0)..bottom.min(height))
            .into_par_iter()
            .step_by(stride)
            .map(|y| {
                let mut sums = [0.0_f64; 6];
                let their_y = y + shift.1;
                if their_y < 0 || their_y >= height {
                    return sums;
                }
                let mut x = left.max(0);
                while x < right.min(width) {
                    let their_x = x + shift.0;
                    if their_x >= 0 && their_x < width {
                        let a = ours[(y * width + x) as usize] as f64;
                        let b = theirs[(their_y * width + their_x) as usize] as f64;
                        if a.is_finite() && b.is_finite() {
                            sums[0] += a;
                            sums[1] += b;
                            sums[2] += a * a;
                            sums[3] += b * b;
                            sums[4] += a * b;
                            sums[5] += 1.0;
                        }
                    }
                    x += stride as isize;
                }
                sums
            })
            .collect();
        let [sa, sb, saa, sbb, sab, count] = rows.iter().fold([0.0_f64; 6], |mut total, row| {
            for (into, value) in total.iter_mut().zip(row) {
                *into += value;
            }
            total
        });
        let area = (right.min(width) - left.max(0)) * (bottom.min(height) - top.max(0));
        let samples = (area.max(0) as usize / (stride * stride)) as f64;
        if count * 10.0 < samples || count < 16.0 {
            return None;
        }
        let variance_a = saa - sa * sa / count;
        let variance_b = sbb - sb * sb / count;
        let covariance = sab - sa * sb / count;
        if variance_a <= 0.0 || variance_b <= 0.0 {
            return None;
        }
        Some((1.0 - covariance / (variance_a * variance_b).sqrt()) as f32)
    }

    /// How many stops brighter OTHER is than its recorded exposure says,
    /// relative to this guide: the median log difference where both can be
    /// read, at SHIFT.
    fn offset(&self, other: &Guide, shift: (isize, isize)) -> Option<f32> {
        let level = 2.min(self.levels.len() - 1);
        let scale = 1_isize << level;
        let (width, height, ours) = &self.levels[level];
        let theirs = &other.levels[level].2;
        let (width, height) = (*width as isize, *height as isize);
        let (dx, dy) = (shift.0 / scale, shift.1 / scale);
        let mut differences: Vec<f32> = Vec::new();
        for y in 0..height {
            let their_y = y + dy;
            if their_y < 0 || their_y >= height {
                continue;
            }
            for x in 0..width {
                let their_x = x + dx;
                if their_x < 0 || their_x >= width {
                    continue;
                }
                let difference =
                    theirs[(their_y * width + their_x) as usize] - ours[(y * width + x) as usize];
                if difference.is_finite() {
                    differences.push(difference);
                }
            }
        }
        if differences.len() < 1000 {
            return None;
        }
        let middle = differences.len() / 2;
        let (_, median, _) = differences.select_nth_unstable_by(middle, f32::total_cmp);
        Some(*median)
    }

    /// Where the part of the frame inside REGION (full-size pixels) best
    /// matches OTHER: searched REACH pixels around PREDICTED at pyramid level
    /// FROM, followed down level by level, and settled to a fraction of a
    /// pixel at full size. A coarse level searches far for little: one pixel
    /// there is eight at full size.
    fn match_tile(
        &self,
        other: &Guide,
        region: [isize; 4],
        predicted: (isize, isize),
        from: usize,
        reach: isize,
    ) -> Option<(f32, f32)> {
        let area = (region[2] - region[0]) * (region[3] - region[1]);
        let stride = |pixels: isize| ((pixels as f32 / 40_000.0).sqrt().ceil().max(1.0)) as usize;
        let from = from.min(self.levels.len() - 1);
        let mut best = (predicted.0 >> from, predicted.1 >> from);
        let mut reach = reach;
        for level in (1..=from).rev() {
            let scale = 1_isize << level;
            let scaled = region.map(|value| value / scale);
            let centre = best;
            let mut best_cost = f32::INFINITY;
            for dy in -reach..=reach {
                for dx in -reach..=reach {
                    let candidate = (centre.0 + dx, centre.1 + dy);
                    if let Some(cost) = self.cost(
                        other,
                        level,
                        candidate,
                        stride(area / (scale * scale)),
                        Some(scaled),
                    ) && cost < best_cost
                    {
                        best_cost = cost;
                        best = candidate;
                    }
                }
            }
            best = (best.0 * 2, best.1 * 2);
            reach = 1;
        }
        let centre = best;
        let fine = stride(area);
        let mut costs = [[f32::INFINITY; 5]; 5];
        for dy in -2..=2_isize {
            for dx in -2..=2_isize {
                costs[(dy + 2) as usize][(dx + 2) as usize] = self
                    .cost(other, 0, (centre.0 + dx, centre.1 + dy), fine, Some(region))
                    .unwrap_or(f32::INFINITY);
            }
        }
        let (mut bx, mut by) = (2, 2);
        for y in 0..5 {
            for x in 0..5 {
                if costs[y][x] < costs[by][bx] {
                    (bx, by) = (x, y);
                }
            }
        }
        if !costs[by][bx].is_finite() {
            return None;
        }
        // A parabola through the best and its neighbours says where between
        // pixels the match really is.
        let vertex = |before: f32, at: f32, after: f32| -> f32 {
            let curvature = before - 2.0 * at + after;
            if before.is_finite() && after.is_finite() && curvature > 0.0 {
                (0.5 * (before - after) / curvature).clamp(-0.5, 0.5)
            } else {
                0.0
            }
        };
        let sub_x = if bx > 0 && bx < 4 {
            vertex(costs[by][bx - 1], costs[by][bx], costs[by][bx + 1])
        } else {
            0.0
        };
        let sub_y = if by > 0 && by < 4 {
            vertex(costs[by - 1][bx], costs[by][bx], costs[by + 1][bx])
        } else {
            0.0
        };
        Some((
            (centre.0 + bx as isize - 2) as f32 + sub_x,
            (centre.1 + by as isize - 2) as f32 + sub_y,
        ))
    }

    /// How OTHER lies over this guide, part by part. A turn and shift fitted
    /// to a coarse grid first, then a finer grid matched around what that
    /// predicts: a camera held by hand also moves sideways, and then the roof
    /// two metres off shifts by more than the houses beyond it. No one turn of
    /// the whole frame lays both down.
    fn place(&self, other: &Guide, shift: (isize, isize)) -> Field {
        let (width, height, _) = self.levels[0];
        let (width, height) = (width as isize, height as isize);
        let grid = |across: isize, down: isize| -> Vec<[isize; 4]> {
            let (tile_width, tile_height) = (width / across, height / down);
            (0..down)
                .flat_map(|row| {
                    (0..across).map(move |column| {
                        [
                            column * tile_width,
                            row * tile_height,
                            (column + 1) * tile_width,
                            (row + 1) * tile_height,
                        ]
                    })
                })
                .collect()
        };
        let centre = |region: &[isize; 4]| {
            ((region[0] + region[2]) as f32 / 2.0, (region[1] + region[3]) as f32 / 2.0)
        };
        let coarse: Vec<((f32, f32), (f32, f32))> = grid(6, 4)
            .par_iter()
            .filter_map(|region| {
                let (dx, dy) = self.match_tile(other, *region, shift, 3, 4)?;
                let (cx, cy) = centre(region);
                Some(((cx, cy), (cx + dx, cy + dy)))
            })
            .collect();
        let overall = Placement::fit(&coarse)
            .unwrap_or_else(|| Placement::shift(shift.0 as f32, shift.1 as f32));
        let (across, down) = (16_usize, 12_usize);
        let fine = grid(across as isize, down as isize);
        // Each tile's own residual against OVERALL's prediction at its
        // centre, not its total displacement: OVERALL already accounts for
        // the frame's rigid turn and shift exactly, so what is left to
        // measure, smooth and interpolate per tile is only how far a real
        // match misses that prediction -- parallax, a moving subject, or
        // noise, all of which should be small and not vary smoothly with
        // position the way the rigid turn's own displacement does.
        let residuals: Vec<(f32, f32)> = fine
            .par_iter()
            .map(|region| {
                let (cx, cy) = centre(region);
                let (px, py) = overall.map(cx, cy);
                let predicted = (px - cx, py - cy);
                match self.match_tile(
                    other,
                    *region,
                    (predicted.0.round() as isize, predicted.1.round() as isize),
                    2,
                    2,
                ) {
                    // Far from the prediction is more likely a tile of sky or
                    // of a moving subject than of parallax.
                    Some(found)
                        if (found.0 - predicted.0).abs() <= 16.0
                            && (found.1 - predicted.1).abs() <= 16.0 =>
                    {
                        (found.0 - predicted.0, found.1 - predicted.1)
                    }
                    _ => (0.0, 0.0),
                }
            })
            .collect();
        // A median of each tile and its neighbours, so one bad match cannot
        // fold the picture.
        let median = |values: &mut Vec<f32>| {
            values.sort_by(f32::total_cmp);
            values[values.len() / 2]
        };
        let smoothed = (0..across * down)
            .map(|index| {
                let (column, row) = ((index % across) as isize, (index / across) as isize);
                let (mut xs, mut ys) = (Vec::new(), Vec::new());
                for dy in -1..=1 {
                    for dx in -1..=1 {
                        let (c, r) = (column + dx, row + dy);
                        if c >= 0 && r >= 0 && (c as usize) < across && (r as usize) < down {
                            let (x, y) = residuals[r as usize * across + c as usize];
                            xs.push(x);
                            ys.push(y);
                        }
                    }
                }
                (median(&mut xs), median(&mut ys))
            })
            .collect();
        Field {
            placement: overall,
            across,
            down,
            tile_width: (width / across as isize) as f32,
            tile_height: (height / down as isize) as f32,
            residuals: smoothed,
        }
    }

    /// The shift that best lays OTHER over this guide, in full-size pixels.
    fn align(&self, other: &Guide) -> (isize, isize) {
        let coarsest = self.levels.len() - 1;
        let mut best = (0_isize, 0_isize);
        // Eight coarse pixels each way covers a hand's drift at any zoom.
        let mut reach = 8;
        for level in (0..=coarsest).rev() {
            let (width, height, _) = self.levels[level];
            // Keep each evaluation near a quarter of a million samples.
            let stride = ((width * height) as f32 / 250_000.0).sqrt().ceil().max(1.0) as usize;
            let centre = best;
            let mut best_cost = f32::INFINITY;
            for dy in -reach..=reach {
                for dx in -reach..=reach {
                    let shift = (centre.0 + dx, centre.1 + dy);
                    if let Some(cost) = self.cost(other, level, shift, stride, None)
                        && cost < best_cost
                    {
                        best_cost = cost;
                        best = shift;
                    }
                }
            }
            if level > 0 {
                best = (best.0 * 2, best.1 * 2);
            }
            reach = 2;
        }
        best
    }
}

/// Mean and variance of a frame's luminance over one block, at the reference's
/// exposure, with the fraction of it that was trustworthy.
#[derive(Clone, Copy)]
struct BlockStats {
    mean: f32,
    variance: f32,
    trusted: f32,
    /// Raw signal level, before scaling to the reference: what the noise
    /// model is stated against.
    raw_mean: f32,
}

fn block_stats(frame: &Frame, bx: usize, by: usize) -> BlockStats {
    let mut sum = 0.0;
    let mut square = 0.0;
    let mut trusted = 0.0;
    let mut count = 0.0;
    for y in by * BLOCK..((by + 1) * BLOCK).min(frame.height) {
        for x in bx * BLOCK..((bx + 1) * BLOCK).min(frame.width) {
            count += 1.0;
            if let Some(pixel) = frame.at(x as isize, y as isize)
                && unclipped(pixel) >= 1.0
            {
                let level = luminance(pixel);
                sum += level;
                square += level * level;
                trusted += 1.0;
            }
        }
    }
    if trusted == 0.0 {
        return BlockStats { mean: 0.0, variance: 0.0, trusted: 0.0, raw_mean: 0.0 };
    }
    let raw_mean = sum / trusted;
    let raw_variance = (square / trusted - raw_mean * raw_mean).max(0.0);
    BlockStats {
        mean: raw_mean / frame.exposure,
        variance: raw_variance / (frame.exposure * frame.exposure),
        trusted: trusted / count,
        raw_mean,
    }
}

/// Per-pixel noise variance of a frame at raw level LEVEL, at the reference's
/// exposure.
fn noise_variance(frame: &Frame, level: f32) -> f32 {
    // Luminance averages three channels, which divides their noise by three.
    let read = READ_NOISE * frame.gain;
    (SHOT_NOISE * frame.gain * level.max(0.0) + read * read) / 3.0
        / (frame.exposure * frame.exposure)
}

/// Whether OTHER tells the same story as ANCHOR over a block: the same light,
/// and the same detail. A moving subject changes the first; a long exposure's
/// smear of it changes the second while keeping the first.
fn agrees(frames: &[Frame], anchor: usize, other: usize, a: BlockStats, b: BlockStats) -> bool {
    let pixels = (BLOCK * BLOCK) as f32;
    let noise_a = noise_variance(&frames[anchor], a.raw_mean);
    let noise_b = noise_variance(&frames[other], b.raw_mean);
    let mean_tolerance = 4.0 * ((noise_a + noise_b) / pixels).sqrt() + 0.08 * a.mean.max(b.mean);
    if (a.mean - b.mean).abs() > mean_tolerance {
        return false;
    }
    // Detail with each frame's own noise taken out.
    let detail_a = (a.variance - noise_a).max(0.0).sqrt();
    let detail_b = (b.variance - noise_b).max(0.0).sqrt();
    let detail_tolerance =
        0.3 * detail_a.max(detail_b) + 3.0 * (noise_a.sqrt() + noise_b.sqrt()) / pixels.sqrt();
    (detail_a - detail_b).abs() <= detail_tolerance
}

fn merge(frames: &[Frame]) -> Vec<[f32; 3]> {
    let (width, height) = (frames[0].width, frames[0].height);
    let (blocks_wide, blocks_high) = (width.div_ceil(BLOCK), height.div_ceil(BLOCK));
    // Frames from most light to least: the anchor of a block is the frame with
    // the most light that the reference itself would not have clipped there.
    let mut order: Vec<usize> = (0..frames.len()).collect();
    order.sort_by(|a, b| frames[*b].exposure.total_cmp(&frames[*a].exposure));
    // For every frame, how much it may contribute in each block.
    let (anchors, agreement): (Vec<usize>, Vec<Vec<f32>>) = {
        let per_block: Vec<(usize, Vec<f32>)> = (0..blocks_wide * blocks_high)
            .into_par_iter()
            .map(|block| {
                let (bx, by) = (block % blocks_wide, block / blocks_wide);
                let stats: Vec<BlockStats> =
                    frames.iter().map(|frame| block_stats(frame, bx, by)).collect();
                // The reference where it can speak; otherwise the frame with
                // the most light below it that can.
                let anchor = if stats[0].trusted > 0.95 {
                    0
                } else {
                    order
                        .iter()
                        .copied()
                        .filter(|index| frames[*index].exposure <= 1.0)
                        .find(|index| stats[*index].trusted > 0.95)
                        .unwrap_or(*order.last().expect("at least one frame"))
                };
                let agreement = (0..frames.len())
                    .map(|index| {
                        if index == anchor || stats[index].trusted == 0.0 {
                            1.0 // Clipped pixels are weighed out per pixel.
                        } else if agrees(frames, anchor, index, stats[anchor], stats[index]) {
                            1.0
                        } else {
                            0.0
                        }
                    })
                    .collect();
                (anchor, agreement)
            })
            .collect();
        let anchors = per_block.iter().map(|(anchor, _)| *anchor).collect();
        let agreement = (0..frames.len())
            .map(|index| {
                let raw: Vec<f32> = per_block.iter().map(|block| block.1[index]).collect();
                // A disagreement spreads one block each way, then the edge is
                // softened, so no seam shows where a frame stops counting.
                let eroded: Vec<f32> = (0..raw.len())
                    .map(|block| {
                        let (bx, by) = ((block % blocks_wide) as isize, (block / blocks_wide) as isize);
                        let mut low: f32 = 1.0;
                        for dy in -1..=1 {
                            for dx in -1..=1 {
                                let (x, y) = (bx + dx, by + dy);
                                if x >= 0 && y >= 0 && (x as usize) < blocks_wide && (y as usize) < blocks_high {
                                    low = low.min(raw[y as usize * blocks_wide + x as usize]);
                                }
                            }
                        }
                        low
                    })
                    .collect();
                (0..eroded.len())
                    .map(|block| {
                        let (bx, by) = ((block % blocks_wide) as isize, (block / blocks_wide) as isize);
                        let mut total = 0.0;
                        let mut count = 0.0;
                        for dy in -1..=1 {
                            for dx in -1..=1 {
                                let (x, y) = (bx + dx, by + dy);
                                if x >= 0 && y >= 0 && (x as usize) < blocks_wide && (y as usize) < blocks_high {
                                    total += eroded[y as usize * blocks_wide + x as usize];
                                    count += 1.0;
                                }
                            }
                        }
                        total / count
                    })
                    .collect()
            })
            .collect();
        (anchors, agreement)
    };
    // The block test misses what is thin: the edge of an arm from a frame where
    // the arm was elsewhere moves an 8x8 mean by a few percent. So every pixel
    // is also held against its block's anchor, lightly blurred to calm the
    // noise, and a pixel that disagrees takes its neighbourhood out with it.
    let local: Vec<(Vec<f32>, Vec<f32>)> =
        frames.iter().map(|frame| local_luminance(frame, width, height)).collect();
    let pixel_agreement: Vec<Vec<f32>> = (0..frames.len())
        .map(|index| {
            let disagrees: Vec<bool> = (0..width * height)
                .into_par_iter()
                .map(|pixel| {
                    let (x, y) = (pixel % width, pixel / width);
                    let anchor = anchors[(y / BLOCK) * blocks_wide + x / BLOCK];
                    if anchor == index {
                        return false;
                    }
                    let (a, a_raw) = (local[anchor].0[pixel], local[anchor].1[pixel]);
                    let (b, b_raw) = (local[index].0[pixel], local[index].1[pixel]);
                    if !b.is_finite() {
                        // Part of its neighbourhood clipped or off the frame:
                        // nothing to check it with, so it is not used here.
                        return true;
                    }
                    if !a.is_finite() {
                        // The anchor clipped here. A darker frame is what
                        // fills a clipped highlight; a brighter one cannot be
                        // checked, and is the one that smears.
                        return frames[index].exposure > frames[anchor].exposure;
                    }
                    let noise = (noise_variance(&frames[anchor], a_raw)
                        + noise_variance(&frames[index], b_raw))
                        / 9.0;
                    (a - b).abs() > 4.0 * noise.sqrt() + 0.05 * a.max(b)
                })
                .collect();
            soften(&grow(&disagrees, width, height, 3), width, height, 4)
        })
        .collect();
    // Clipping is trusted to no closer than two pixels. A developed pixel
    // next to a clipped one may have been made partly from it — binning
    // averages the photosites of a quad, demosaicing borrows from neighbours —
    // and then reads as unclipped while one channel is short: the magenta of a
    // leaf beside a street lamp.
    let trust: Vec<Vec<f32>> = frames
        .iter()
        .map(|frame| {
            let own: Vec<f32> = (0..width * height)
                .into_par_iter()
                .map(|index| {
                    frame
                        .at((index % width) as isize, (index / width) as isize)
                        .map_or(0.0, unclipped)
                })
                .collect();
            lowest_around(&own, width, height, 2)
        })
        .collect();
    let shortest = *order.last().expect("at least one frame");
    let sample = |map: &[f32], x: usize, y: usize| -> f32 {
        // Bilinear between block centres.
        let fx = ((x as f32 + 0.5) / BLOCK as f32 - 0.5).clamp(0.0, (blocks_wide - 1) as f32);
        let fy = ((y as f32 + 0.5) / BLOCK as f32 - 0.5).clamp(0.0, (blocks_high - 1) as f32);
        let (x0, y0) = (fx.floor() as usize, fy.floor() as usize);
        let (x1, y1) = ((x0 + 1).min(blocks_wide - 1), (y0 + 1).min(blocks_high - 1));
        let (tx, ty) = (fx - x0 as f32, fy - y0 as f32);
        let top = map[y0 * blocks_wide + x0] * (1.0 - tx) + map[y0 * blocks_wide + x1] * tx;
        let bottom = map[y1 * blocks_wide + x0] * (1.0 - tx) + map[y1 * blocks_wide + x1] * tx;
        top * (1.0 - ty) + bottom * ty
    };
    (0..width * height)
        .into_par_iter()
        .map(|index| {
            let (x, y) = (index % width, index / width);
            let mut total = [0.0_f32; 3];
            let mut weight = 0.0;
            for (number, frame) in frames.iter().enumerate() {
                let Some(pixel) = frame.at(x as isize, y as isize) else {
                    continue;
                };
                // Light gathered is what lowers noise, so it is the weight.
                // Inverse variance: in light, shot noise rules and a frame
                // counts for the light it gathered; in shadow, read noise
                // rules and it counts for the square of that.
                let level = luminance(pixel).max(0.0);
                let w = trust[number][index]
                    * sample(&agreement[number], x, y).min(pixel_agreement[number][index])
                    / noise_variance(frame, level);
                if w > 0.0 {
                    for channel in 0..3 {
                        total[channel] += w * pixel[channel] / frame.exposure;
                    }
                    weight += w;
                }
            }
            if weight > 1.0e-6 {
                total.map(|value| value / weight)
            } else {
                // Clipped in every frame: the shortest frame's clipped value
                // is still the brightest honest answer, but only together
                // with its own exposure. A warp rejects a border every
                // bilinear neighbour has to lie inside the frame for, and
                // the reference's own raw pixel stands in there instead --
                // that pixel belongs with the reference's own exposure, not
                // the shortest frame's, or the stand-in is normalized by an
                // exposure it was never shot at.
                let frame = &frames[shortest];
                match frame.at(x as isize, y as isize) {
                    Some(pixel) => pixel.map(|value| value / frame.exposure),
                    None => frames[0]
                        .pixels[index]
                        .map(|value| value / frames[0].exposure),
                }
            }
        })
        .collect()
}

/// A frame's luminance over each 3x3 neighbourhood in the reference's
/// coordinates, at the reference's exposure and raw, NAN where any of it is
/// outside the frame or untrusted.
fn local_luminance(frame: &Frame, width: usize, height: usize) -> (Vec<f32>, Vec<f32>) {
    (0..width * height)
        .into_par_iter()
        .map(|pixel| {
            let (x, y) = ((pixel % width) as isize, (pixel / width) as isize);
            if frame.at(x, y).is_none() {
                return (f32::NAN, f32::NAN);
            }
            let mut total = 0.0;
            let mut count = 0.0;
            for dy in -1..=1 {
                for dx in -1..=1 {
                    let (nx, ny) = (x + dx, y + dy);
                    // Past the edge of the picture there is nothing to
                    // average, which is not the same as something clipped.
                    if nx < 0 || ny < 0 || nx as usize >= width || ny as usize >= height {
                        continue;
                    }
                    match frame.at(nx, ny) {
                        Some(value) if unclipped(value) >= 1.0 => {
                            total += luminance(value);
                            count += 1.0;
                        }
                        _ => return (f32::NAN, f32::NAN),
                    }
                }
            }
            let raw = total / count;
            (raw / frame.exposure, raw)
        })
        .unzip()
}

/// The lowest of VALUES within RADIUS pixels each way (a square).
fn lowest_around(values: &[f32], width: usize, height: usize, radius: usize) -> Vec<f32> {
    let across: Vec<f32> = (0..width * height)
        .into_par_iter()
        .map(|pixel| {
            let (x, y) = (pixel % width, pixel / width);
            (x.saturating_sub(radius)..=(x + radius).min(width - 1))
                .map(|x| values[y * width + x])
                .fold(f32::INFINITY, f32::min)
        })
        .collect();
    (0..width * height)
        .into_par_iter()
        .map(|pixel| {
            let (x, y) = (pixel % width, pixel / width);
            (y.saturating_sub(radius)..=(y + radius).min(height - 1))
                .map(|y| across[y * width + x])
                .fold(f32::INFINITY, f32::min)
        })
        .collect()
}

/// Spreads every set pixel RADIUS pixels each way (a square).
fn grow(mask: &[bool], width: usize, height: usize, radius: usize) -> Vec<bool> {
    let across: Vec<bool> = (0..width * height)
        .into_par_iter()
        .map(|pixel| {
            let (x, y) = (pixel % width, pixel / width);
            (x.saturating_sub(radius)..=(x + radius).min(width - 1)).any(|x| mask[y * width + x])
        })
        .collect();
    (0..width * height)
        .into_par_iter()
        .map(|pixel| {
            let (x, y) = (pixel % width, pixel / width);
            (y.saturating_sub(radius)..=(y + radius).min(height - 1)).any(|y| across[y * width + x])
        })
        .collect()
}

/// One where nothing disagreed, falling smoothly to zero over RADIUS pixels
/// into what did: a box blur of the agreement, each way.
fn soften(disagrees: &[bool], width: usize, height: usize, radius: usize) -> Vec<f32> {
    let agree: Vec<f32> = disagrees.iter().map(|value| if *value { 0.0 } else { 1.0 }).collect();
    let rows: Vec<f32> = (0..width * height)
        .into_par_iter()
        .map(|pixel| {
            let (x, y) = (pixel % width, pixel / width);
            let (left, right) = (x.saturating_sub(radius), (x + radius).min(width - 1));
            let row = &agree[y * width + left..=y * width + right];
            row.iter().sum::<f32>() / row.len() as f32
        })
        .collect();
    let softened: Vec<f32> = (0..width * height)
        .into_par_iter()
        .map(|pixel| {
            let (x, y) = (pixel % width, pixel / width);
            let (top, bottom) = (y.saturating_sub(radius), (y + radius).min(height - 1));
            (top..=bottom).map(|y| rows[y * width + x]).sum::<f32>() / (bottom - top + 1) as f32
        })
        .collect();
    // Where a pixel itself disagreed it gets nothing, however its
    // neighbourhood averaged.
    softened
        .into_iter()
        .zip(disagrees)
        .map(|(value, disagrees)| if *disagrees { 0.0 } else { value })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A frame of SCENE (radiance at the reference's exposure) taken at
    /// EXPOSURE with read noise, clipped at the white level.
    fn shot(scene: &[f32], width: usize, exposure: f32, seed: u64) -> Frame {
        let mut state = seed;
        let mut noise = || {
            state = state.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            ((state >> 40) as f32 / (1u64 << 24) as f32 - 0.5) * 0.004
        };
        let pixels = scene
            .iter()
            .map(|value| {
                // Each channel its own noise, as a sensor's photosites have.
                std::array::from_fn(|_| (value * exposure + noise()).clamp(0.0, 1.0))
            })
            .collect();
        Frame { width, height: scene.len() / width, pixels, exposure, gain: 1.0 }
    }

    #[test]
    fn field_map_reproduces_a_rigid_rotation_exactly_near_the_edge() {
        // The audit's own reproduction: a 0.5 degree rotation on a
        // 5184x3888 frame, the production 16x12 grid, exact (noise-free)
        // tile-centre matches throughout -- so every tile's true residual
        // against the rigid placement is exactly zero -- checked at (32,
        // 1944), close to the left edge at mid-height. Before the fix, even
        // this noise-free case lost 3.95 pixels there: the median and the
        // clamped interpolation operated on each tile's *total*
        // displacement, which a rotation varies linearly with position, so
        // smoothing and clamping that quantity distorted the rotation
        // itself. The residual that is actually smoothed and clamped now is
        // uniformly zero in this noise-free case, so the fixed path must
        // reproduce the rigid placement exactly, and the old path -- built
        // the same way inline here, for a direct before/after comparison --
        // must still show close to that same 3.95 pixel error.
        let (width, height) = (5184.0_f32, 3888.0_f32);
        let (center_x, center_y) = (width / 2.0, height / 2.0);
        let angle = 0.5_f32.to_radians();
        let (a, b) = (angle.cos(), angle.sin());
        let placement = Placement {
            a,
            b,
            tx: center_x - a * center_x + b * center_y,
            ty: center_y - b * center_x - a * center_y,
        };
        let (across, down) = (16_usize, 12_usize);
        let (tile_width, tile_height) = (width / across as f32, height / down as f32);
        let centre = |column: usize, row: usize| {
            (
                (column as f32 + 0.5) * tile_width,
                (row as f32 + 0.5) * tile_height,
            )
        };
        // Exact tile-centre matches: each tile's own true total displacement
        // under PLACEMENT, with no measurement noise at all.
        let total_displacement = |column: usize, row: usize| {
            let (cx, cy) = centre(column, row);
            let (px, py) = placement.map(cx, cy);
            (px - cx, py - cy)
        };
        let median = |values: &mut Vec<f32>| {
            values.sort_by(f32::total_cmp);
            values[values.len() / 2]
        };
        let smoothed_neighbourhood = |values: &dyn Fn(usize, usize) -> (f32, f32), column: usize, row: usize| {
            let (mut xs, mut ys) = (Vec::new(), Vec::new());
            for dy in -1..=1_isize {
                for dx in -1..=1_isize {
                    let (c, r) = (column as isize + dx, row as isize + dy);
                    if c >= 0 && r >= 0 && (c as usize) < across && (r as usize) < down {
                        let (x, y) = values(c as usize, r as usize);
                        xs.push(x);
                        ys.push(y);
                    }
                }
            }
            (median(&mut xs), median(&mut ys))
        };

        // The old path: median-smoothed *total* displacements, clamped at
        // the outer tile centres with no rigid placement underneath at all.
        let old_shifts: Vec<(f32, f32)> = (0..down)
            .flat_map(|row| (0..across).map(move |column| (column, row)))
            .map(|(column, row)| smoothed_neighbourhood(&total_displacement, column, row))
            .collect();
        let old_field = Field {
            placement: Placement::shift(0.0, 0.0),
            across,
            down,
            tile_width,
            tile_height,
            residuals: old_shifts,
        };
        let (true_x, true_y) = placement.map(32.0, 1944.0);
        let (old_x, old_y) = old_field.map(32.0, 1944.0);
        let old_error = ((old_x - true_x).powi(2) + (old_y - true_y).powi(2)).sqrt();
        assert!(
            (old_error - 3.95).abs() < 0.1,
            "the old path's own error moved from the audit's reported 3.95px to {old_error}"
        );

        // The fixed path: every tile's residual against PLACEMENT's own
        // prediction is exactly zero in this noise-free case, so the median
        // of all-zero neighbourhoods is zero everywhere, and the field must
        // reproduce the rigid rotation exactly, including at the edge.
        let residuals = vec![(0.0, 0.0); across * down];
        let fixed_field = Field {
            placement,
            across,
            down,
            tile_width,
            tile_height,
            residuals,
        };
        let (fixed_x, fixed_y) = fixed_field.map(32.0, 1944.0);
        let fixed_error = ((fixed_x - true_x).powi(2) + (fixed_y - true_y).powi(2)).sqrt();
        assert!(
            fixed_error < 1.0e-3,
            "the fixed path must reproduce the rigid rotation exactly, got error {fixed_error}"
        );
    }

    #[test]
    fn a_bracket_recovers_highlights_cleans_shadows_and_leaves_no_ghost() {
        let (width, height) = (96, 64);
        // Deep shadow on the left rising to four times the white level on the
        // right, with fine texture so moving it shows.
        let scene: Vec<f32> = (0..width * height)
            .map(|index| {
                let (x, y) = (index % width, index / width);
                let ramp = 0.01 * (400.0_f32).powf(x as f32 / (width - 1) as f32);
                ramp * (1.0 + 0.2 * (((x / 3 + y / 3) % 2) as f32 - 0.5))
            })
            .collect();
        // In the long frame a dark object passed through the middle.
        let mut moved = scene.clone();
        for y in 24..40 {
            for x in 20..36 {
                moved[y * width + x] = 0.002;
            }
        }
        let frames = vec![
            shot(&scene, width, 1.0, 1),
            shot(&scene, width, 0.25, 2),
            shot(&moved, width, 4.0, 3),
        ];
        let merged = merge(&frames);
        let error = |x: usize, y: usize| {
            let truth = scene[y * width + x];
            (merged[y * width + x][0] - truth).abs() / truth.max(1.0e-3)
        };
        // The right edge is clipped in the reference; the short frame has it.
        assert!(error(width - 2, 10) < 0.05, "highlight off by {}", error(width - 2, 10));
        assert!(frames[0].pixels[10 * width + width - 2][0] >= 0.99);
        // Where the object passed, the merge shows the scene, not the object.
        for (x, y) in [(24, 28), (30, 32), (34, 38)] {
            assert!(error(x, y) < 0.35, "ghost at {x},{y}: {}", error(x, y));
        }
        // Away from it, the shadows are cleaner than the reference's own.
        let rms = |pick: &dyn Fn(usize) -> f32| {
            let mut total = 0.0;
            let mut count = 0.0;
            for y in 4..20 {
                for x in 2..10 {
                    let index = y * width + x;
                    total += (pick(index) - scene[index]).powi(2);
                    count += 1.0;
                }
            }
            (total / count).sqrt()
        };
        let reference = rms(&|index| frames[0].pixels[index][0]);
        let merged_rms = rms(&|index| merged[index][0]);
        assert!(
            merged_rms < reference * 0.5,
            "shadow noise {merged_rms} against the reference's {reference}"
        );
    }

    #[test]
    fn a_fallback_pixel_keeps_its_own_exposure() {
        // Every pixel of every frame clipped, so every weight is exactly
        // zero and the fallback runs everywhere -- deterministic, with no
        // dependence on the agreement machinery's own thresholds. The short
        // frame (0.25x) has no pixel at all at the test position, as a warp
        // leaves at a border every bilinear neighbour has to be inside the
        // frame for; the reference's own raw pixel stands in there, and it
        // must come back divided by the reference's own exposure, not the
        // short frame's.
        let (width, height) = (16, 16);
        let reference = Frame {
            width,
            height,
            pixels: vec![[0.99, 0.99, 0.99]; width * height],
            exposure: 1.0,
            gain: 1.0,
        };
        let test_index = (height / 2) * width + width / 2;
        let mut short_pixels = vec![[0.99, 0.99, 0.99]; width * height];
        short_pixels[test_index] = [f32::NAN, f32::NAN, f32::NAN];
        let short = Frame {
            width,
            height,
            pixels: short_pixels,
            exposure: 0.25,
            gain: 1.0,
        };
        let merged = merge(&[reference, short]);
        assert!(
            (merged[test_index][0] - 0.99).abs() < 1.0e-6,
            "fallback pixel {} did not match the reference's own exposure (expected 0.99)",
            merged[test_index][0]
        );
    }

    /// Writes a merged bracket, and each frame alone, as tone-mapped JPEGs for
    /// looking at: ORFEUS_BRACKET="out-prefix a.ORF b.ORF ..."
    #[test]
    #[ignore]
    fn write_a_merged_bracket() {
        let spec = std::env::var("ORFEUS_BRACKET").expect("ORFEUS_BRACKET is set");
        let mut words = spec.split_whitespace();
        let prefix = words.next().expect("an output prefix").to_owned();
        let inputs: Vec<std::path::PathBuf> = words.map(Into::into).collect();
        let paths: Vec<&Path> = inputs.iter().map(|path| path.as_path()).collect();
        let draft = std::env::var_os("ORFEUS_BRACKET_FULL").is_none();
        let merged = merge_bracket(&paths, draft, Demosaic::default(), true).unwrap();
        write_preview(&merged, &format!("{prefix}-merged.jpg"));
        let reference = render::develop_frame(paths[0], draft, Demosaic::default(), false)
            .unwrap()
            .into_decoded()
            .unwrap();
        write_preview(&reference, &format!("{prefix}-reference.jpg"));
    }

    pub(super) fn write_preview(decoded: &DecodedRaw, path: &str) {
        // A plain global curve, the same for both, so the comparison is of
        // what the merge recovered rather than of tone mapping.
        // ORFEUS_BRACKET_EV moves both, to look into the shadows or highlights.
        let gain = std::env::var("ORFEUS_BRACKET_EV")
            .ok()
            .and_then(|text| text.parse::<f32>().ok())
            .map_or(1.0, f32::exp2);
        let pixels: Vec<u8> = decoded
            .data
            .iter()
            .map(|value| {
                let value = value.max(0.0) * 2.0 * gain;
                let mapped = value / (1.0 + value);
                (mapped.powf(1.0 / 2.2) * 255.0).round().clamp(0.0, 255.0) as u8
            })
            .collect();
        image::save_buffer(
            path,
            &pixels,
            decoded.width as u32,
            decoded.height as u32,
            image::ColorType::Rgb8,
        )
        .unwrap();
    }
}
