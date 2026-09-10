//! Default scene-linear to display-linear tone mapping.

#[cfg(test)]
use rayon::prelude::*;

const CURVE_INPUT_GAIN: f32 = 1.6;
const CURVE_NUMERATOR: f32 = 8.75;
const CURVE_DENOMINATOR: f32 = 1.46;
const SRGB_LUMINANCE: [f32; 3] = [0.212_672_9, 0.715_152_2, 0.072_175];

/// From this display luminance up, a colour's chroma eases away as the tone
/// nears white, the way a camera's or a film's shoulder lets highlights go
/// neutral: an overcast sky balanced warm renders white, not cream, and a
/// cloud that clipped renders grey rather than the colour of whichever channel
/// clipped last. Below the onset colour is carried through untouched.
const HIGHLIGHT_DESATURATION_ONSET: f32 = 0.6;

/// How much of the chroma is gone by white: four fifths, leaving a trace of the
/// colour so a bright yellow flower still reads as one.
const HIGHLIGHT_DESATURATION: f32 = 0.8;

/// Applies Orfeus's moderately lifted default display tone to linear-light sRGB.
///
/// The luminance curve first establishes the intended display luminance. Colors
/// outside display-linear sRGB are then compressed along their direction from a
/// neutral pixel at that luminance. This keeps the tone target and hue direction
/// while reducing saturation only as much as the output gamut requires, and,
/// above `HIGHLIGHT_DESATURATION_ONSET`, as much as a shoulder nearing white
/// lets go. The scalar curve lifts deep shadows and midtones to a practical
/// display baseline. Explicit user exposure compensation remains a separate
/// earlier step.
///
/// Kept as the unfused reference the tone tests and the GPU parity check
/// measure against; renders fuse `tone_pixel` into a pass of their own.
#[cfg(test)]
pub(crate) fn apply_default_display_tone(linear_rgb: &mut [f32]) {
    assert_eq!(linear_rgb.len() % 3, 0, "RGB data must contain triplets");
    // Chunked well above one pixel: at preview sizes the per-item cost of a
    // three-float parallel iterator is the same order as the arithmetic.
    linear_rgb.par_chunks_mut(3 * 8192).for_each(|chunk| {
        for pixel in chunk.as_chunks_mut::<3>().0 {
            tone_pixel(pixel);
        }
    });
}

/// One pixel of `apply_default_display_tone`, for callers that fuse it into a
/// pass of their own rather than traversing the image again.
#[inline]
pub(crate) fn tone_pixel(pixel: &mut [f32; 3]) {
    let luminance = pixel
        .iter()
        .zip(SRGB_LUMINANCE)
        .map(|(channel, coefficient)| channel * coefficient)
        .sum::<f32>();
    if !luminance.is_finite() || luminance <= 0.0 {
        *pixel = [0.0; 3];
        return;
    }

    let target = default_display_tone(luminance);
    let luminance_gain = target / luminance;
    let toned = [
        pixel[0] * luminance_gain,
        pixel[1] * luminance_gain,
        pixel[2] * luminance_gain,
    ];
    let mut saturation = 1.0_f32;
    for channel in toned {
        let chroma = channel - target;
        if chroma > 0.0 {
            saturation = saturation.min((1.0 - target) / chroma);
        } else if chroma < 0.0 {
            saturation = saturation.min(target / -chroma);
        }
    }
    saturation = saturation.clamp(0.0, 1.0);
    saturation *= highlight_chroma(target);
    for (channel, toned_channel) in pixel.iter_mut().zip(toned) {
        *channel = target + saturation * (toned_channel - target);
    }
}

/// The share of its chroma a colour keeps at display luminance TARGET: all of
/// it up to the onset, easing smoothly to a fifth at white. The tone transfer
/// shader carries the same curve.
#[inline]
pub(crate) fn highlight_chroma(target: f32) -> f32 {
    let t = ((target - HIGHLIGHT_DESATURATION_ONSET) / (1.0 - HIGHLIGHT_DESATURATION_ONSET))
        .clamp(0.0, 1.0);
    1.0 - HIGHLIGHT_DESATURATION * t * t * (3.0 - 2.0 * t)
}

pub(crate) fn default_display_tone(value: f32) -> f32 {
    let value = value.max(0.0) * CURVE_INPUT_GAIN;
    let numerator = value * (1.0 + CURVE_NUMERATOR * value);
    let denominator = 1.0 + CURVE_DENOMINATOR * value + CURVE_NUMERATOR * value * value;
    numerator / denominator
}

#[cfg(test)]
mod tests {
    use super::*;

    fn close(actual: f32, expected: f32) {
        assert!(
            (actual - expected).abs() < 1.0e-6,
            "expected {expected}, got {actual}"
        );
    }

    fn luminance(pixel: [f32; 3]) -> f32 {
        pixel
            .iter()
            .zip(SRGB_LUMINANCE)
            .map(|(channel, coefficient)| channel * coefficient)
            .sum()
    }

    #[test]
    fn curve_has_deterministic_reference_values() {
        for (input, expected) in [
            (-0.1, 0.0),
            (0.0, 0.0),
            (0.18, 0.472_342_34),
            (0.5, 0.823_892_9),
            (1.0, 0.932_545_84),
            (2.0, 0.974_053_2),
            (4.0, 0.989_304_24),
        ] {
            close(default_display_tone(input), expected);
        }
    }

    #[test]
    fn curve_is_monotonic_across_scene_values() {
        let mut previous = default_display_tone(0.0);
        for step in 1..=16_000 {
            let value = step as f32 / 1000.0;
            let mapped = default_display_tone(value);
            assert!(mapped + 1.0e-7 >= previous, "curve decreased at {value}");
            previous = mapped;
        }
    }

    #[test]
    fn curve_has_calibrated_display_lift_at_black() {
        let input = 0.000_1;
        close(default_display_tone(input) / input, 1.601_865_4);
    }

    #[test]
    fn exposure_remains_separate_and_monotonic_before_tone_mapping() {
        let middle_gray = default_display_tone(0.18);
        let plus_one_ev = default_display_tone(0.18 * 2.0_f32.powf(1.0));
        let minus_one_ev = default_display_tone(0.18 * 2.0_f32.powf(-1.0));

        assert!(minus_one_ev < middle_gray);
        assert!(middle_gray < plus_one_ev);
        close(plus_one_ev, 0.733_355_8);
    }

    #[test]
    fn neutral_pixels_follow_the_scalar_curve() {
        let mut values = [0.18, 0.18, 0.18, 4.0, 4.0, 4.0];
        apply_default_display_tone(&mut values);
        for value in &values[..3] {
            close(*value, 0.472_342_34);
        }
        for value in &values[3..] {
            close(*value, 0.989_304_24);
        }
    }

    #[test]
    fn saturated_highlights_reach_tone_target_without_changing_hue_direction() {
        let input = [4.0, 2.0, 1.0];
        let input_luminance = luminance(input);
        let target = default_display_tone(input_luminance);
        let mut pixel = input;
        apply_default_display_tone(&mut pixel);

        close(luminance(pixel), target);
        assert!(pixel.iter().all(|channel| (0.0..=1.0).contains(channel)));
        let scales = std::array::from_fn::<_, 3, _>(|index| {
            (pixel[index] - target) / (input[index] - input_luminance)
        });
        close(scales[0], scales[1]);
        close(scales[1], scales[2]);
    }

    #[test]
    fn negative_components_are_compressed_to_gamut_without_darkening() {
        let input = [-0.5, 0.5, 0.25];
        let target = default_display_tone(luminance(input));
        let mut pixel = input;
        apply_default_display_tone(&mut pixel);

        // Compressed to the gamut's edge; the highlight easing, which has only
        // just begun at this luminance, then draws it a hair inside.
        assert!((0.0..0.01).contains(&pixel[0]), "red landed at {}", pixel[0]);
        close(luminance(pixel), target);
        assert!(pixel[1] > pixel[2]);
        assert!(pixel[2] > 0.0);
    }

    #[test]
    fn highlights_ease_toward_neutral_and_midtones_keep_their_colour() {
        // A warm highlight: what an overcast sky is after a warm white balance.
        let bright = [1.4, 1.2, 1.0];
        let target = default_display_tone(luminance(bright));
        let toned = bright.map(|channel| channel * target / luminance(bright));
        let mut pixel = bright;
        apply_default_display_tone(&mut pixel);
        close(luminance(pixel), target);
        let kept = (pixel[0] - pixel[2]) / (toned[0] - toned[2]);
        assert!(kept > 0.0 && kept < 0.15, "the highlight kept {kept} of its chroma");
        assert!(pixel[0] > pixel[1] && pixel[1] > pixel[2], "the hue direction turned");
        // A midtone of the same colour, below the onset: carried through whole.
        let mid = [0.2, 0.15, 0.1];
        let mid_target = default_display_tone(luminance(mid));
        assert!(mid_target < HIGHLIGHT_DESATURATION_ONSET);
        let mut mid_pixel = mid;
        apply_default_display_tone(&mut mid_pixel);
        for (channel, input) in mid_pixel.iter().zip(mid) {
            close(*channel, input * mid_target / luminance(mid));
        }
        // The easing itself: whole at the onset, a fifth at white, monotone.
        close(highlight_chroma(HIGHLIGHT_DESATURATION_ONSET), 1.0);
        close(highlight_chroma(1.0), 1.0 - HIGHLIGHT_DESATURATION);
        let mut previous = 1.0;
        for step in 0..=100 {
            let share = highlight_chroma(step as f32 / 100.0);
            assert!(share <= previous + 1.0e-6);
            previous = share;
        }
    }
}
