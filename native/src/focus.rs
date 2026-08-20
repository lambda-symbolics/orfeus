//! Whether a photograph is in focus anywhere, and how sharply.
//!
//! The usual answer to this is the variance of the Laplacian over the whole
//! frame, which does not work. It is an absolute number with no units, so its
//! threshold has to be re-guessed for every camera, resolution, and ISO; a
//! picture of a brick wall scores higher than a portrait because it has more
//! edges, not because it is better focused; and — worst for the pictures worth
//! keeping — it averages the whole frame, so a macro shot wide open, whose
//! subject is sharp and whose background is deliberately not, scores like a
//! mistake.
//!
//! Two changes fix all three.
//!
//! **Measure blur, not contrast.** Sample the image with two centred
//! differences of different reach: `d1` spans two pixels, `d2` spans four. At
//! an edge blurred by a Gaussian of width sigma, the peak of `d1` is
//! `A*erf(1/(sigma*sqrt 2))` and the peak of `d2` is `A*erf(2/(sigma*sqrt 2))`.
//! Their ratio drops the edge's amplitude `A` and leaves a function of sigma
//! alone, rising monotonically from 1 for a perfect step to 2 for a smear.
//! Inverting it reports a blur radius **in pixels** — a number with a meaning,
//! which is what makes a threshold transferable. Contrast, exposure, and the
//! camera's tone curve all cancel in the ratio.
//!
//! **Ask where the frame is sharpest, not how sharp it is on average.** The
//! measurement is per patch, and the frame is judged by its sharpest patches.
//! A photograph is a keeper if something in it is sharp; what the photographer
//! did with the rest of the depth of field is composition, not error.
//!
//! Two smaller things this has to get right. Sensor noise is high frequency and
//! reads as sharp to anything gradient-shaped, so only edges standing clear of
//! a noise floor measured from the frame itself are counted — and, less
//! obviously, an edge must be *found* by something other than the differences
//! that then measure it. Picking the pixel where `d1` is largest picks the
//! pixel where the noise in `d1` is largest, which inflates the denominator,
//! shrinks the ratio, and hands back a smaller blur than the frame has: a
//! four-pixel smear with visible grain came back as one pixel. Edges are
//! located instead by a wider difference averaged down several rows, whose
//! noise is nearly independent of the noise in the two reaches read at the
//! pixel it points to. And a frame with
//! nothing in it to judge — a sky, a wall, a lens cap — must come back as
//! *unknown* rather than as blurry, so the count of patches that carried enough
//! edge structure is reported alongside the answer.

/// Output pixels on a side of one measured patch.
///
/// Small enough that a subject occupying a few percent of the frame owns
/// several patches outright, large enough to hold the dozen-odd edges a patch
/// needs before its ratio means anything.
const PATCH: usize = 64;

/// Edge peaks one axis of a patch needs before it is allowed an opinion.
const MIN_EDGES: usize = 12;

/// Smallest edge amplitude worth locating, as a fraction of full range.
///
/// A floor under the noise estimate, for frames clean enough that the estimate
/// collapses towards zero: a JPEG's quantization ripple is not an edge.
///
/// Note where this is *not* applied. Thresholding the two-pixel difference
/// itself — the obvious place, and where this started — throws away every
/// reading that fell below the line and keeps every reading that noise pushed
/// above it, which inflates the denominator of the ratio and reports a blurred
/// frame as a sharp one. At a four-pixel smear with visible grain the threshold
/// sat *above* the signal it was filtering and the answer came back three times
/// too sharp. Only the locator is thresholded; what it points at is read
/// unconditionally.
const MIN_AMPLITUDE: f32 = 0.02;

/// Rank of the patch that speaks for the frame, counting from the sharpest.
///
/// Not the sharpest patch: one patch can be a speck of dust on the sensor, a
/// clipped specular highlight, or a run of noise that got lucky. Four patches
/// agreeing is 128 pixels of something, which is a subject. Photographs whose
/// subject is genuinely smaller than that do exist, and this will call them
/// soft; the alternative is calling every noisy failure sharp.
const SPEAKING_RANK: usize = 3;

/// Weight the locator gives each reach, and rows it is averaged over.
///
/// Reaching three pixels either side responds to a wide edge that the two-pixel
/// difference has almost lost, and averaging five rows of it makes the noise in
/// the locator largely the noise of *other* rows — which is the point, since
/// that is what keeps locating an edge from biasing the measurement of it.
///
/// The nearest reach cannot be dropped, tempting as it is: without it the
/// response to a step is flat-topped four pixels wide and the peak lands beside
/// the edge instead of on it, where both differences read zero. It can be
/// weighted down, though, and is. What correlation remains then leans on the
/// pixels the four-pixel difference reads rather than the two-pixel one, in
/// about the proportion by which the former exceeds the latter — so the little
/// bias that survives lands on both sums evenly and cancels in their ratio.
const LOCATOR_WEIGHTS: [f32; 3] = [1.0, 2.0, 2.0];
const LOCATOR_SPAN: usize = 2;

/// Multiple of the locator's own noise deviation a candidate edge must clear.
///
/// Looser than [`NOISE_MARGIN`], because this only decides where to look: what
/// is found there still has to pass the amplitude test on its own.
const LOCATOR_MARGIN: f32 = 3.0;

/// Blur radius reported when even the four-pixel difference has nothing left.
const MAX_BLUR: f32 = 8.0;

/// The long edge every blur radius is reported against.
///
/// Blur scales with resolution, so a radius in pixels only compares across
/// cameras once the frame it was measured in has a stated size. This one is
/// the size a preview is looked at, which makes the number answer the question
/// a photographer is actually asking: is it sharp at the size I view it?
pub(crate) const REFERENCE_EDGE: f32 = 1600.0;

/// What a frame's focus looks like.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct FocusReport {
    /// Blur radius of the best-focused part of the frame, in pixels at a
    /// [`REFERENCE_EDGE`] long edge.
    pub(crate) blur_radius: f32,
    /// Blur radius of the middling patch, on the same scale.
    ///
    /// Read together with `blur_radius` this says which kind of photograph it
    /// is rather than whether it is a good one. Both small: sharp throughout.
    /// The first small and this one large: a shallow plane of focus, a portrait
    /// wide open or a macro. Both large: nothing in the frame is sharp.
    ///
    /// The first version of this reported instead what fraction of the frame
    /// was nearly as sharp as its sharpest part, which sounded more useful and
    /// was not: on ordinary photographs it came out at two to nine percent,
    /// because a frame's sharpest patch is almost always sharper than its
    /// typical one whether or not anything was defocused.
    pub(crate) typical_blur: f32,
    /// Fraction of patches that carried enough edge structure to judge.
    ///
    /// Near zero means the frame answered nothing: read `blur_radius` as
    /// unknown rather than as bad.
    pub(crate) judgeable: f32,
}

/// The error function, Abramowitz and Stegun 7.1.26.
///
/// Accurate to about 1.5e-7, which is far past what an edge ratio resolves.
fn erf(x: f32) -> f32 {
    let sign = if x < 0.0 { -1.0 } else { 1.0 };
    let x = x.abs();
    let t = 1.0 / (1.0 + 0.327_591_1 * x);
    let poly = t
        * (0.254_829_59
            + t * (-0.284_496_74 + t * (1.421_413_7 + t * (-1.453_152 + t * 1.061_405_4))));
    sign * (1.0 - poly * (-x * x).exp())
}

/// The `d2`-to-`d1` peak ratio an edge blurred by SIGMA produces.
fn ratio_at(sigma: f32) -> f32 {
    let scale = std::f32::consts::SQRT_2 * sigma;
    erf(2.0 / scale) / erf(1.0 / scale)
}

/// The blur radius that produces RATIO, by bisection.
///
/// `ratio_at` rises monotonically from 1 to 2 over the whole positive line, so
/// bisection cannot pick the wrong root and needs no starting guess.
///
/// It flattens against 1 below about a third of a pixel, where a difference
/// spanning two pixels can no longer tell one blur from another, and anything
/// under that comes back as zero. That is the honest answer: blur finer than
/// the sampling grid is not there to be measured, and calling it zero says the
/// frame is as sharp as the frame can be.
fn blur_from_ratio(ratio: f32) -> f32 {
    if !(ratio > 1.0) {
        return 0.0;
    }
    if ratio >= ratio_at(MAX_BLUR) {
        return MAX_BLUR;
    }
    let (mut low, mut high) = (0.0_f32, MAX_BLUR);
    for _ in 0..24 {
        let middle = 0.5 * (low + high);
        if ratio_at(middle) < ratio {
            low = middle;
        } else {
            high = middle;
        }
    }
    0.5 * (low + high)
}

/// A plane of brightness to measure: gamma-encoded, roughly zero to one.
pub(crate) struct FocusPlane<'a> {
    pub(crate) data: &'a [f32],
    pub(crate) width: usize,
    pub(crate) height: usize,
}

impl FocusPlane<'_> {
    #[inline]
    fn at(&self, row: usize, column: usize) -> f32 {
        self.data[row * self.width + column]
    }
}

/// A robust estimate of the plane's noise deviation.
///
/// The median of the two-pixel differences, taken over a stride so a large
/// frame costs the same as a small one. Most of any photograph is flat compared
/// to its edges, so the median difference is the noise and not the picture;
/// scaling a median absolute deviation by 1.4826 turns it into a deviation, and
/// dividing by root two undoes the differencing of two independent samples.
fn noise_deviation(plane: &FocusPlane<'_>) -> f32 {
    const STRIDE: usize = 3;
    let mut samples = Vec::new();
    let mut row = 1;
    while row + 1 < plane.height {
        let mut column = 1;
        while column + 1 < plane.width {
            samples.push((plane.at(row, column + 1) - plane.at(row, column - 1)).abs());
            column += STRIDE;
        }
        row += STRIDE;
    }
    if samples.is_empty() {
        return 0.0;
    }
    let middle = samples.len() / 2;
    let (_, median, _) = samples.select_nth_unstable_by(middle, |a, b| a.total_cmp(b));
    *median * 1.4826 / std::f32::consts::SQRT_2
}

/// Accumulated edge peaks along one axis of one patch.
#[derive(Default)]
struct Peaks {
    d1: f32,
    d2: f32,
    count: usize,
}

impl Peaks {
    /// The blur radius these peaks imply, or `None` if there were too few.
    ///
    /// Summing the two reaches and dividing once, rather than averaging each
    /// peak's own ratio, weights every edge by how much of an edge it was. A
    /// faint one near the noise floor should not outvote the frame's real
    /// subject just because it is also a local maximum.
    ///
    /// A ratio meaningfully below one abstains rather than reporting no blur.
    /// A blurred step cannot produce one — the wider difference spans the
    /// narrower one — so a patch that does is a patch the model does not
    /// describe: a one-pixel line, a clipped specular highlight, fine periodic
    /// texture. Reporting zero for those would be reporting *perfect* sharpness
    /// from the one kind of patch that cannot support the claim, and since the
    /// frame is judged by its sharpest patches, one such patch would speak for
    /// the whole photograph.
    fn blur(&self) -> Option<f32> {
        if self.count < MIN_EDGES || self.d1 <= 0.0 {
            return None;
        }
        let ratio = self.d2 / self.d1;
        (ratio > 0.95).then(|| blur_from_ratio(ratio))
    }

    #[inline]
    fn add(&mut self, d1: f32, d2: f32) {
        self.d1 += d1;
        self.d2 += d2;
        self.count += 1;
    }
}

/// Whether the middle of three magnitudes is a peak.
///
/// Loose on the left and strict on the right so a plateau — two adjacent
/// pixels straddling an edge that fell between them — is counted once.
#[inline]
fn is_peak(before: f32, middle: f32, after: f32) -> bool {
    middle >= before && middle > after
}

/// Adds one edge's two reaches to PEAKS, oriented by which way the edge leans.
///
/// Turned the right way up by the locator's sign rather than by taking absolute
/// values. It matters: an absolute value rectifies noise, so a reading whose
/// signal is small contributes the *size* of its noise instead of zero, which
/// again inflates the denominator and reports a blurred frame as sharper than
/// it is. Signed against a direction decided by other pixels, noise cancels in
/// the sum the way noise should.
///
/// Nothing here bounds the ratio either. A blurred step guarantees `d2` reaches
/// between one and two times as far as `d1`, so it is tempting to clip what
/// does not comply — but at a four-pixel smear the true ratio is 1.94, close
/// enough to that ceiling that noise pushes half the readings past it, and
/// clipping them back is another one-sided truncation. Left alone the
/// overshoots cancel the undershoots, and the ratio of the two sums is bounded
/// once, at the end, where a bound costs nothing.
#[inline]
fn consider(peaks: &mut Peaks, d1: f32, d2: f32, lean: f32) {
    peaks.add(d1 * lean, d2 * lean);
}

/// Measures one patch along both axes and returns the worse one's blur.
///
/// The worse, not the better: blur from a moved camera is directional, sharp
/// across the movement and smeared along it, and a frame that is sharp in one
/// direction only is a frame that shook. An axis that found too few edges to
/// judge — a patch of nothing but horizontal detail has nothing to say about
/// horizontal blur — abstains instead of voting badly.
fn patch_blur(
    plane: &FocusPlane<'_>,
    edges: &Locator,
    top: usize,
    left: usize,
    height: usize,
    width: usize,
    locator_floor: f32,
) -> Option<f32> {
    let mut across = Peaks::default();
    let mut down = Peaks::default();
    let (across, down) = (&mut across, &mut down);
    // The margin the locator needs, which is wider than the differences it
    // points at: past it there is nothing to locate an edge with.
    const MARGIN: usize = LOCATOR_WEIGHTS.len() + LOCATOR_SPAN;
    for row in top..top + height {
        if row < MARGIN || row + MARGIN >= plane.height {
            continue;
        }
        for column in left..left + width {
            if column < MARGIN || column + MARGIN >= plane.width {
                continue;
            }
            let edge = edges.across(row, column);
            if edge.abs() > locator_floor
                && is_peak(
                    edges.across(row, column - 1).abs(),
                    edge.abs(),
                    edges.across(row, column + 1).abs(),
                )
            {
                consider(
                    across,
                    plane.at(row, column + 1) - plane.at(row, column - 1),
                    plane.at(row, column + 2) - plane.at(row, column - 2),
                    edge.signum(),
                );
            }
            let edge = edges.down(row, column);
            if edge.abs() > locator_floor
                && is_peak(
                    edges.down(row - 1, column).abs(),
                    edge.abs(),
                    edges.down(row + 1, column).abs(),
                )
            {
                consider(
                    down,
                    plane.at(row + 1, column) - plane.at(row - 1, column),
                    plane.at(row + 2, column) - plane.at(row - 2, column),
                    edge.signum(),
                );
            }
        }
    }
    match (across.blur(), down.blur()) {
        (Some(a), Some(b)) => Some(a.max(b)),
        (Some(only), None) | (None, Some(only)) => Some(only),
        (None, None) => None,
    }
}

/// Where the edges are, one field per axis, signed.
struct Locator {
    across: Vec<f32>,
    down: Vec<f32>,
    width: usize,
}

impl Locator {
    #[inline]
    fn across(&self, row: usize, column: usize) -> f32 {
        self.across[row * self.width + column]
    }

    #[inline]
    fn down(&self, row: usize, column: usize) -> f32 {
        self.down[row * self.width + column]
    }
}

/// How much larger the locator's noise is than one pixel's.
///
/// Independent samples: two sides, each reach on each, over
/// `2 * LOCATOR_SPAN + 1` rows.
fn locator_noise_gain() -> f32 {
    let energy: f32 = LOCATOR_WEIGHTS.iter().map(|weight| weight * weight).sum();
    (2.0 * energy * (2 * LOCATOR_SPAN + 1) as f32).sqrt()
}

/// What the locator answers to a sharp step of unit amplitude.
fn locator_step_response() -> f32 {
    LOCATOR_WEIGHTS.iter().sum::<f32>() * (2 * LOCATOR_SPAN + 1) as f32
}

/// Builds both locator fields: a reach-three difference along one axis,
/// summed over neighbouring lines of the other.
fn locate_edges(plane: &FocusPlane<'_>) -> Locator {
    let (width, height) = (plane.width, plane.height);
    let mut wide = vec![0.0_f32; width * height];
    let mut across = vec![0.0_f32; width * height];
    let mut down = vec![0.0_f32; width * height];
    let reach = LOCATOR_WEIGHTS.len();
    for row in 0..height {
        for column in reach..width.saturating_sub(reach) {
            wide[row * width + column] = LOCATOR_WEIGHTS
                .iter()
                .enumerate()
                .map(|(index, weight)| {
                    let step = index + 1;
                    weight * (plane.at(row, column + step) - plane.at(row, column - step))
                })
                .sum();
        }
    }
    for row in LOCATOR_SPAN..height.saturating_sub(LOCATOR_SPAN) {
        for column in 0..width {
            across[row * width + column] = (0..=2 * LOCATOR_SPAN)
                .map(|offset| wide[(row + offset - LOCATOR_SPAN) * width + column])
                .sum();
        }
    }
    for row in reach..height.saturating_sub(reach) {
        for column in 0..width {
            wide[row * width + column] = LOCATOR_WEIGHTS
                .iter()
                .enumerate()
                .map(|(index, weight)| {
                    let step = index + 1;
                    weight * (plane.at(row + step, column) - plane.at(row - step, column))
                })
                .sum();
        }
    }
    for row in 0..height {
        for column in LOCATOR_SPAN..width.saturating_sub(LOCATOR_SPAN) {
            down[row * width + column] = (0..=2 * LOCATOR_SPAN)
                .map(|offset| wide[row * width + column + offset - LOCATOR_SPAN])
                .sum();
        }
    }
    Locator {
        across,
        down,
        width,
    }
}

/// Measures how well PLANE is focused.
///
/// The blur radius is reported against [`REFERENCE_EDGE`], scaled from whatever
/// size the plane happens to be. No attempt is made to subtract the blur the
/// caller's own downsample contributed: at these sizes a box of one output
/// pixel is a radius of 0.29, the same for every frame however it was reduced,
/// and a constant offset is a thing a threshold already absorbs.
pub(crate) fn measure(plane: &FocusPlane<'_>) -> FocusReport {
    let unknown = FocusReport {
        blur_radius: MAX_BLUR,
        typical_blur: MAX_BLUR,
        judgeable: 0.0,
    };
    if plane.width < 8 || plane.height < 8 || plane.data.len() < plane.width * plane.height {
        return unknown;
    }
    let deviation = noise_deviation(plane);
    let locator_floor = (LOCATOR_MARGIN * locator_noise_gain() * deviation)
        .max(MIN_AMPLITUDE * locator_step_response());
    let edges = locate_edges(plane);
    let mut blurs = Vec::new();
    let mut patches = 0_usize;
    let mut top = 0;
    while top < plane.height {
        let height = PATCH.min(plane.height - top);
        let mut left = 0;
        while left < plane.width {
            let width = PATCH.min(plane.width - left);
            patches += 1;
            if let Some(blur) = patch_blur(
                plane,
                &edges,
                top,
                left,
                height,
                width,
                locator_floor,
            ) {
                blurs.push(blur);
            }
            left += PATCH;
        }
        top += PATCH;
    }
    if blurs.is_empty() {
        return unknown;
    }
    let rank = SPEAKING_RANK.min(blurs.len() - 1);
    let (_, best, _) = blurs.select_nth_unstable_by(rank, |a, b| a.total_cmp(b));
    let best = *best;
    let middle = blurs.len() / 2;
    let (_, typical, _) = blurs.select_nth_unstable_by(middle, |a, b| a.total_cmp(b));
    let typical = *typical;
    let scale = REFERENCE_EDGE / plane.width.max(plane.height) as f32;
    FocusReport {
        blur_radius: best * scale,
        typical_blur: typical * scale,
        judgeable: blurs.len() as f32 / patches as f32,
    }
}

/// Measures how well a developed frame of interleaved scene-linear RGB is
/// focused.
///
/// Judged on brightness through the display transfer, not on the linear signal:
/// an edge's contrast is what the eye reads, and linear values give a highlight
/// a hundred times the weight the print will show it at.
pub(crate) fn measure_frame(width: usize, height: usize, rgb: &[f32]) -> FocusReport {
    let luma: Vec<f32> = rgb
        .chunks_exact(3)
        .map(|pixel| {
            super::render::srgb_encode(0.2126 * pixel[0] + 0.7152 * pixel[1] + 0.0722 * pixel[2])
        })
        .collect();
    measure(&FocusPlane {
        data: &luma,
        width,
        height,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A cheap deterministic sequence, so every run measures the same picture.
    fn next(state: &mut u64) -> f32 {
        *state = state.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
        ((*state >> 33) & 0xffff) as f32 / 65535.0
    }

    /// Cell side of the test pattern. Wide enough that no four-pixel difference
    /// ever straddles two edges, which is what lets the assertions below name a
    /// blur radius instead of a range.
    const CELL: usize = 16;

    /// A grid of flat tiles: step edges along both axes, well separated, at
    /// amplitudes far above any threshold in the module.
    fn tiles(width: usize, height: usize) -> Vec<f32> {
        let mut state = 0x5eed_1234_9876_4321;
        let (columns, rows) = (width / CELL + 1, height / CELL + 1);
        let levels: Vec<f32> = (0..columns * rows)
            .map(|_| 0.15 + 0.7 * (next(&mut state) * 4.0).floor() / 3.0)
            .collect();
        (0..width * height)
            .map(|index| {
                let (row, column) = (index / width, index % width);
                levels[(row / CELL) * columns + column / CELL]
            })
            .collect()
    }

    /// Separable Gaussian blur, reflecting at the borders.
    fn blur(plane: &[f32], width: usize, height: usize, sigma: (f32, f32)) -> Vec<f32> {
        fn kernel(sigma: f32) -> Vec<f32> {
            if sigma <= 0.0 {
                return vec![1.0];
            }
            let radius = (4.0 * sigma).ceil() as isize;
            let weights: Vec<f32> = (-radius..=radius)
                .map(|offset| (-(offset * offset) as f32 / (2.0 * sigma * sigma)).exp())
                .collect();
            let total: f32 = weights.iter().sum();
            weights.into_iter().map(|weight| weight / total).collect()
        }
        fn pass(source: &[f32], width: usize, height: usize, weights: &[f32]) -> Vec<f32> {
            let radius = (weights.len() / 2) as isize;
            let mut output = vec![0.0; width * height];
            for row in 0..height {
                for column in 0..width {
                    let mut sum = 0.0;
                    for (index, weight) in weights.iter().enumerate() {
                        let offset = index as isize - radius;
                        let taken = (column as isize + offset).clamp(0, width as isize - 1);
                        sum += weight * source[row * width + taken as usize];
                    }
                    output[row * width + column] = sum;
                }
            }
            output
        }
        fn transpose(source: &[f32], width: usize, height: usize) -> Vec<f32> {
            let mut output = vec![0.0; width * height];
            for row in 0..height {
                for column in 0..width {
                    output[column * height + row] = source[row * width + column];
                }
            }
            output
        }
        let across = pass(plane, width, height, &kernel(sigma.0));
        let down = pass(&transpose(&across, width, height), height, width, &kernel(sigma.1));
        transpose(&down, height, width)
    }

    /// The blur radius `measure` finds, back in the plane's own pixels.
    fn measured(plane: &[f32], width: usize, height: usize) -> FocusReport {
        let mut report = measure(&FocusPlane {
            data: plane,
            width,
            height,
        });
        let scale = width.max(height) as f32 / REFERENCE_EDGE;
        report.blur_radius *= scale;
        report.typical_blur *= scale;
        report
    }

    #[test]
    fn the_edge_ratio_inverts_to_the_radius_that_made_it() {
        for step in 8..40 {
            let sigma = step as f32 * 0.05 + 0.4;
            let recovered = blur_from_ratio(ratio_at(sigma));
            assert!(
                (recovered - sigma).abs() < 0.01,
                "sigma {sigma} came back as {recovered}"
            );
        }
        for sigma in [0.0_f32, 0.05, 0.1] {
            assert_eq!(
                blur_from_ratio(ratio_at(sigma)),
                0.0,
                "blur finer than the grid should read as none, not as {sigma}"
            );
        }
        assert_eq!(blur_from_ratio(1.0), 0.0);
        assert_eq!(blur_from_ratio(0.5), 0.0);
        assert_eq!(blur_from_ratio(2.0), MAX_BLUR);
    }

    #[test]
    fn a_sharp_frame_measures_near_zero_blur() {
        let (width, height) = (384, 256);
        let report = measured(&tiles(width, height), width, height);
        assert!(
            report.blur_radius < 0.9,
            "sharp frame reported {report:?}; a step landing between two pixels \
             is worth a few tenths, but no more"
        );
        assert!(report.judgeable > 0.9, "sharp frame reported {report:?}");
        assert!(
            report.typical_blur < 0.9,
            "a frame that is sharp all over should have no soft middle: {report:?}"
        );
    }

    #[test]
    fn a_blurred_frame_reports_the_radius_it_was_blurred_by() {
        let (width, height) = (384, 256);
        let sharp = tiles(width, height);
        for sigma in [1.5_f32, 2.0, 3.0, 4.0] {
            let report = measured(&blur(&sharp, width, height, (sigma, sigma)), width, height);
            assert!(
                (report.blur_radius - sigma).abs() < 0.6,
                "blur {sigma} reported {report:?}"
            );
        }
    }

    #[test]
    fn more_blur_always_reads_as_more_blur() {
        let (width, height) = (384, 256);
        let sharp = tiles(width, height);
        let mut previous = -1.0_f32;
        for step in 0..9 {
            let sigma = step as f32 * 0.5;
            let report = measured(&blur(&sharp, width, height, (sigma, sigma)), width, height);
            assert!(
                report.blur_radius > previous,
                "blur {sigma} reported {:.3}, no more than {previous:.3} did",
                report.blur_radius
            );
            previous = report.blur_radius;
        }
    }

    /// The failure this module exists to avoid: a macro or a portrait wide open,
    /// where most of the frame is blurred on purpose.
    #[test]
    fn a_sharp_subject_against_a_blurred_background_is_in_focus() {
        let (width, height) = (512, 512);
        let sharp = tiles(width, height);
        let background = blur(&sharp, width, height, (5.0, 5.0));
        let mut composed = background.clone();
        // A subject over an eighth of the frame's area, off centre, the way a
        // photographer would place it.
        for row in 96..288 {
            for column in 288..480 {
                composed[row * width + column] = sharp[row * width + column];
            }
        }
        let report = measured(&composed, width, height);
        assert!(
            report.blur_radius < 1.0,
            "a frame with a sharp subject reported {report:?}"
        );
        assert!(
            report.typical_blur > 3.0 * report.blur_radius,
            "a shallow plane of focus should leave a soft middle: {report:?}"
        );
        let whole = measured(&background, width, height);
        assert!(
            whole.blur_radius > 3.0,
            "the same frame without the subject reported {whole:?}"
        );
    }

    #[test]
    fn a_frame_with_nothing_in_it_answers_nothing() {
        let (width, height) = (256, 256);
        let flat = vec![0.42_f32; width * height];
        let report = measured(&flat, width, height);
        assert_eq!(report.judgeable, 0.0, "a flat frame reported {report:?}");
        let mut state = 7;
        let ramp: Vec<f32> = (0..width * height)
            .map(|index| 0.3 + 0.0005 * (index % width) as f32 + 0.0001 * next(&mut state))
            .collect();
        let report = measured(&ramp, width, height);
        assert!(
            report.judgeable < 0.1,
            "a gradient with no edges reported {report:?}"
        );
    }

    /// Noise is high frequency, so anything counting contrast calls a noisy
    /// frame sharp. This one must not.
    #[test]
    fn a_noisy_blurred_frame_is_not_mistaken_for_a_sharp_one() {
        let (width, height) = (384, 256);
        let mut state = 0xabcd_ef01;
        let smeared = blur(&tiles(width, height), width, height, (4.0, 4.0));
        let noisy: Vec<f32> = smeared
            .iter()
            .map(|value| value + 0.06 * (next(&mut state) - 0.5))
            .collect();
        let report = measured(&noisy, width, height);
        assert!(
            report.blur_radius > 2.0,
            "noise talked a blurred frame into focus: {report:?}"
        );
    }

    /// Blur measured in pixels is only comparable once the frame it was
    /// measured in has a stated size, which is what the reference edge is for.
    #[test]
    fn the_reported_radius_does_not_depend_on_the_frame_size() {
        let small = {
            let (width, height) = (400, 400);
            measure(&FocusPlane {
                data: &blur(&tiles(width, height), width, height, (2.0, 2.0)),
                width,
                height,
            })
        };
        let large = {
            let (width, height) = (800, 800);
            measure(&FocusPlane {
                data: &blur(&tiles(width, height), width, height, (4.0, 4.0)),
                width,
                height,
            })
        };
        let difference = (small.blur_radius - large.blur_radius).abs();
        assert!(
            difference < 0.15 * small.blur_radius,
            "the same blur at two sizes reported {:.3} and {:.3}",
            small.blur_radius,
            large.blur_radius
        );
    }

    /// A camera that moved is sharp across the movement and smeared along it.
    #[test]
    fn a_frame_smeared_along_one_axis_reads_as_blurred() {
        let (width, height) = (384, 256);
        let smeared = blur(&tiles(width, height), width, height, (3.0, 0.0));
        let report = measured(&smeared, width, height);
        assert!(
            report.blur_radius > 2.2,
            "a shaken frame reported {report:?}; the sharp axis must not \
             excuse the smeared one"
        );
    }

    /// The numbers the module was tuned against, kept as an assertion. Sensor
    /// noise moved the answer by a factor of three before the locator stopped
    /// biasing the measurement; over the range a threshold is set in, it now
    /// moves it by hundredths.
    #[test]
    fn noise_leaves_the_answer_alone_where_the_threshold_lives() {
        let (width, height) = (384, 256);
        let sharp = tiles(width, height);
        for (sigma, tolerance) in [(0.0_f32, 0.5_f32), (1.0, 0.2), (2.0, 0.25)] {
            let smeared = blur(&sharp, width, height, (sigma, sigma));
            let clean = measured(&smeared, width, height).blur_radius;
            // Up to five levels out of 255, which is a high-ISO frame before
            // any downsample has averaged it down.
            for amplitude in [0.02_f32, 0.04, 0.06] {
                let mut state = 0xabcd_ef01;
                let noisy: Vec<f32> = smeared
                    .iter()
                    .map(|value| value + amplitude * (next(&mut state) - 0.5))
                    .collect();
                let report = measured(&noisy, width, height);
                assert!(
                    (report.blur_radius - clean).abs() < tolerance,
                    "blur {sigma} read {:.3} clean and {:.3} with noise {amplitude}",
                    clean,
                    report.blur_radius
                );
            }
        }
    }
}
#[cfg(test)]
mod photographs {
    use super::*;

    /// Prints what real photographs measure, so a threshold can be set against
    /// something. Ignored: it needs files that are not in the repository.
    #[test]
    #[ignore = "reads photographs from ORFEUS_FOCUS_FILES"]
    fn measure_named_files() {
        let files = std::env::var("ORFEUS_FOCUS_FILES").unwrap_or_default();
        for path in files.split(':').filter(|path| !path.is_empty()) {
            let started = std::time::Instant::now();
            match crate::render::decode_linear_srgb(std::path::Path::new(path), true, false) {
                Ok(decoded) => {
                    let report = measure_frame(decoded.width, decoded.height, &decoded.data);
                    println!(
                        "{path}: {}x{} blur {:.3} typical {:.2} judgeable {:.2} in {} ms",
                        decoded.width,
                        decoded.height,
                        report.blur_radius,
                        report.typical_blur,
                        report.judgeable,
                        started.elapsed().as_millis()
                    );
                }
                Err(error) => println!("{path}: {error}"),
            }
        }
    }
}
