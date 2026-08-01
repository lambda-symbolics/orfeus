//! Default scene-linear to display-linear tone mapping.

const MIDTONE_LIFT: f32 = 5.0;
const MIDTONE_FALLOFF: f32 = 4.0;

/// Applies Orfeus's restrained default display tone curve to linear-light RGB.
///
/// The curve has unit slope at black, so it does not conceal a constant exposure
/// boost. It lifts low midtones while an exponential shoulder rolls scene values
/// above display white smoothly toward one. Exposure compensation remains a
/// separate operation and must be applied before this function.
pub(crate) fn apply_default_display_tone(linear_rgb: &mut [f32]) {
    linear_rgb
        .iter_mut()
        .for_each(|value| *value = default_display_tone(*value));
}

fn default_display_tone(value: f32) -> f32 {
    let value = value.max(0.0);
    let shoulder = -(-value).exp_m1();
    let midtone_lift = MIDTONE_LIFT * value * value * (-MIDTONE_FALLOFF * value).exp();
    shoulder + midtone_lift
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

    #[test]
    fn curve_has_deterministic_reference_values() {
        for (input, expected) in [
            (-0.1, 0.0),
            (0.0, 0.0),
            (0.18, 0.243_583_65),
            (0.5, 0.562_638_46),
            (1.0, 0.723_698_74),
            (2.0, 0.871_373_95),
            (4.0, 0.981_693_4),
        ] {
            close(default_display_tone(input), expected);
        }
    }

    #[test]
    fn curve_has_no_constant_exposure_gain_at_black() {
        let input = 0.000_1;
        close(default_display_tone(input) / input, 1.000_449_8);
    }

    #[test]
    fn exposure_remains_separate_and_monotonic_before_tone_mapping() {
        let middle_gray = default_display_tone(0.18);
        let plus_one_ev = default_display_tone(0.18 * 2.0_f32.powf(1.0));
        let minus_one_ev = default_display_tone(0.18 * 2.0_f32.powf(-1.0));

        assert!(minus_one_ev < middle_gray);
        assert!(middle_gray < plus_one_ev);
        close(plus_one_ev, 0.455_852_87);
    }

    #[test]
    fn slice_api_maps_each_linear_channel() {
        let mut values = [-1.0, 0.18, 1.0, 4.0];
        apply_default_display_tone(&mut values);
        close(values[0], 0.0);
        close(values[1], 0.243_583_65);
        close(values[2], 0.723_698_74);
        close(values[3], 0.981_693_4);
    }
}
