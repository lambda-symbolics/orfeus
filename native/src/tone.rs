//! Default scene-linear to display-linear tone mapping.

const CURVE_NUMERATOR: f32 = 8.75;
const CURVE_DENOMINATOR: f32 = 1.46;
const SRGB_LUMINANCE: [f32; 3] = [0.212_672_9, 0.715_152_2, 0.072_175];

/// Applies Orfeus's moderately lifted default display tone to linear-light sRGB.
///
/// The luminance curve first establishes the intended display luminance. Colors
/// outside display-linear sRGB are then compressed along their direction from a
/// neutral pixel at that luminance. This keeps the tone target and hue direction
/// while reducing saturation only as much as the output gamut requires. The
/// scalar curve has unit slope at black, so this is not a hidden exposure
/// multiplier. Explicit exposure compensation remains a separate earlier step.
pub(crate) fn apply_default_display_tone(linear_rgb: &mut [f32]) {
    let (pixels, remainder) = linear_rgb.as_chunks_mut::<3>();
    assert!(remainder.is_empty(), "RGB data must contain triplets");
    for pixel in pixels {
        let luminance = pixel
            .iter()
            .zip(SRGB_LUMINANCE)
            .map(|(channel, coefficient)| channel * coefficient)
            .sum::<f32>();
        if !luminance.is_finite() || luminance <= 0.0 {
            *pixel = [0.0; 3];
            continue;
        }

        let target = default_display_tone(luminance);
        let luminance_gain = target / luminance;
        let toned = pixel.map(|channel| channel * luminance_gain);
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
        for (channel, toned_channel) in pixel.iter_mut().zip(toned) {
            *channel = target + saturation * (toned_channel - target);
        }
    }
}

fn default_display_tone(value: f32) -> f32 {
    let value = value.max(0.0);
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
            (0.18, 0.299_747_8),
            (0.5, 0.686_024_25),
            (1.0, 0.869_759_14),
            (2.0, 0.950_668_04),
            (4.0, 0.980_659_25),
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
    fn curve_has_no_constant_exposure_gain_at_black() {
        let input = 0.000_1;
        close(default_display_tone(input) / input, 1.000_728_8);
    }

    #[test]
    fn exposure_remains_separate_and_monotonic_before_tone_mapping() {
        let middle_gray = default_display_tone(0.18);
        let plus_one_ev = default_display_tone(0.18 * 2.0_f32.powf(1.0));
        let minus_one_ev = default_display_tone(0.18 * 2.0_f32.powf(-1.0));

        assert!(minus_one_ev < middle_gray);
        assert!(middle_gray < plus_one_ev);
        close(plus_one_ev, 0.561_738_6);
    }

    #[test]
    fn neutral_pixels_follow_the_scalar_curve() {
        let mut values = [0.18, 0.18, 0.18, 4.0, 4.0, 4.0];
        apply_default_display_tone(&mut values);
        for value in &values[..3] {
            close(*value, 0.299_747_8);
        }
        for value in &values[3..] {
            close(*value, 0.980_659_25);
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

        close(pixel[0], 0.0);
        close(luminance(pixel), target);
        assert!(pixel[1] > pixel[2]);
        assert!(pixel[2] > 0.0);
    }
}
