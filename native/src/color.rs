//! Camera-color to deliberate linear-sRGB conversion.

use std::collections::HashMap;
use std::fmt;

use rawler::imgop::develop::Intermediate;
use rawler::imgop::matrix::{multiply, normalize, pseudo_inverse, transform_1d};
use rawler::imgop::xyz::{FlatColorMatrix, Illuminant, SRGB_TO_XYZ_D65};

#[derive(Debug, PartialEq)]
pub(crate) enum ColorError {
    MissingMatrix,
    MissingD65Matrix {
        available: Vec<Illuminant>,
    },
    InvalidMatrixLength {
        illuminant: Illuminant,
        expected: usize,
        actual: usize,
    },
    NonFiniteMatrix {
        illuminant: Illuminant,
    },
    DegenerateMatrix {
        illuminant: Illuminant,
    },
    InvalidWhiteBalance {
        channels: usize,
    },
    UnsupportedChannels,
}

impl fmt::Display for ColorError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingMatrix => formatter.write_str("RAW has no camera color matrix"),
            Self::MissingD65Matrix { available } => write!(
                formatter,
                "RAW has no D65 camera color matrix (available illuminants: {available:?})"
            ),
            Self::InvalidMatrixLength {
                illuminant,
                expected,
                actual,
            } => write!(
                formatter,
                "camera color matrix for {illuminant:?} has {actual} values; expected {expected}"
            ),
            Self::NonFiniteMatrix { illuminant } => write!(
                formatter,
                "camera color matrix for {illuminant:?} contains a non-finite value"
            ),
            Self::DegenerateMatrix { illuminant } => write!(
                formatter,
                "camera color matrix for {illuminant:?} is degenerate"
            ),
            Self::InvalidWhiteBalance { channels } => write!(
                formatter,
                "camera white balance is invalid for {channels}-channel color"
            ),
            Self::UnsupportedChannels => {
                formatter.write_str("RAW development did not produce three- or four-channel color")
            }
        }
    }
}

#[derive(Debug)]
pub(crate) struct LinearSrgbImage {
    pub(crate) width: usize,
    pub(crate) height: usize,
    pub(crate) data: Vec<f32>,
}

fn preferred_matrix(
    matrices: &HashMap<Illuminant, FlatColorMatrix>,
) -> Result<(Illuminant, &FlatColorMatrix), ColorError> {
    if matrices.is_empty() {
        return Err(ColorError::MissingMatrix);
    }
    matrices
        .get(&Illuminant::D65)
        .map(|matrix| (Illuminant::D65, matrix))
        .ok_or_else(|| {
            let mut available: Vec<_> = matrices.keys().copied().collect();
            available.sort();
            ColorError::MissingD65Matrix { available }
        })
}

fn camera_to_srgb<const N: usize>(
    illuminant: Illuminant,
    flat: &[f32],
) -> Result<[[f32; N]; 3], ColorError> {
    let expected = N * 3;
    if flat.len() != expected {
        return Err(ColorError::InvalidMatrixLength {
            illuminant,
            expected,
            actual: flat.len(),
        });
    }
    if !flat.iter().all(|value| value.is_finite()) {
        return Err(ColorError::NonFiniteMatrix { illuminant });
    }
    let xyz_to_camera = transform_1d::<N, 3>(flat).expect("matrix length was checked");
    let rgb_to_camera = multiply(&xyz_to_camera, &SRGB_TO_XYZ_D65);
    if rgb_to_camera
        .iter()
        .any(|row| row.iter().sum::<f32>().abs() < 1.0e-7)
    {
        return Err(ColorError::DegenerateMatrix { illuminant });
    }
    let rgb_to_camera = normalize(rgb_to_camera);
    let camera_to_rgb = pseudo_inverse(rgb_to_camera);
    let round_trip = multiply(&camera_to_rgb, &rgb_to_camera);
    let valid = camera_to_rgb
        .iter()
        .flatten()
        .all(|value| value.is_finite())
        && round_trip.iter().enumerate().all(|(row, values)| {
            values.iter().enumerate().all(|(column, value)| {
                let expected = if row == column { 1.0 } else { 0.0 };
                (*value - expected).abs() < 1.0e-3
            })
        });
    if !valid {
        return Err(ColorError::DegenerateMatrix { illuminant });
    }
    Ok(camera_to_rgb)
}

fn usable_white_balance<const N: usize>(white_balance: &[f32; 4]) -> Result<[f32; N], ColorError> {
    let relevant = &white_balance[..N];
    if relevant.iter().any(|coefficient| !coefficient.is_finite()) {
        return Ok([1.0; N]);
    }
    if relevant.iter().any(|coefficient| *coefficient <= 0.0) {
        return Err(ColorError::InvalidWhiteBalance { channels: N });
    }
    Ok(std::array::from_fn(|index| white_balance[index]))
}

fn transform_pixels<const N: usize>(
    pixels: Vec<[f32; N]>,
    white_balance: &[f32; N],
    matrix: &[[f32; N]; 3],
) -> Vec<f32> {
    use rayon::prelude::*;
    // Fold the white-balance gains into the matrix so the per-pixel work is a
    // single small matrix product.
    let balanced: [[f32; N]; 3] = std::array::from_fn(|row| {
        std::array::from_fn(|column| matrix[row][column] * white_balance[column])
    });
    let mut output = vec![0.0_f32; pixels.len() * 3];
    output
        .par_chunks_exact_mut(3)
        .zip(pixels.par_iter())
        .for_each(|(destination, pixel)| {
            for (value, row) in destination.iter_mut().zip(&balanced) {
                *value = row
                    .iter()
                    .zip(pixel)
                    .map(|(coefficient, channel)| coefficient * channel)
                    .sum();
            }
        });
    output
}

/// Converts demosaiced camera channels to white-balanced, unclipped linear sRGB.
///
/// A valid D65 camera matrix is required because Orfeus's MVP output transform
/// is D65 sRGB and does not yet implement chromatic adaptation between camera
/// calibration illuminants. Missing or non-finite camera white balance falls
/// back to unity, matching rawler's safe unavailable-metadata behavior. The
/// transform deliberately performs no gamut or highlight clipping.
pub(crate) fn intermediate_to_linear_srgb(
    intermediate: Intermediate,
    matrices: &HashMap<Illuminant, FlatColorMatrix>,
    white_balance: [f32; 4],
) -> Result<LinearSrgbImage, ColorError> {
    let (illuminant, flat) = preferred_matrix(matrices)?;
    match intermediate {
        Intermediate::ThreeColor(pixels) => {
            let matrix = camera_to_srgb::<3>(illuminant, flat)?;
            let white_balance = usable_white_balance::<3>(&white_balance)?;
            Ok(LinearSrgbImage {
                width: pixels.width,
                height: pixels.height,
                data: transform_pixels(pixels.into_inner(), &white_balance, &matrix),
            })
        }
        Intermediate::FourColor(pixels) => {
            let matrix = camera_to_srgb::<4>(illuminant, flat)?;
            let white_balance = usable_white_balance::<4>(&white_balance)?;
            Ok(LinearSrgbImage {
                width: pixels.width,
                height: pixels.height,
                data: transform_pixels(pixels.into_inner(), &white_balance, &matrix),
            })
        }
        Intermediate::Monochrome(_) => Err(ColorError::UnsupportedChannels),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rawler::imgop::xyz::XYZ_TO_SRGB_D65;
    use rawler::pixarray::Color2D;

    fn matrices(illuminant: Illuminant, matrix: Vec<f32>) -> HashMap<Illuminant, Vec<f32>> {
        HashMap::from([(illuminant, matrix)])
    }

    fn close(actual: f32, expected: f32) {
        assert!(
            (actual - expected).abs() < 1.0e-5,
            "expected {expected}, got {actual}"
        );
    }

    #[test]
    fn identity_srgb_camera_transform_preserves_unclipped_values() {
        let pixels = Color2D::<f32, 3>::new_with(vec![[2.0, 0.5, -0.25]], 1, 1);
        let matrix = XYZ_TO_SRGB_D65
            .iter()
            .flat_map(|row| row.iter().copied())
            .collect();
        let result = intermediate_to_linear_srgb(
            Intermediate::ThreeColor(pixels),
            &matrices(Illuminant::D65, matrix),
            [1.0; 4],
        )
        .unwrap();

        close(result.data[0], 2.0);
        close(result.data[1], 0.5);
        close(result.data[2], -0.25);
    }

    #[test]
    fn three_channel_white_balance_ignores_unused_fourth_coefficient() {
        let pixels = Color2D::<f32, 3>::new_with(vec![[0.25, 0.5, 0.75]], 1, 1);
        let matrix = XYZ_TO_SRGB_D65
            .iter()
            .flat_map(|row| row.iter().copied())
            .collect();
        let result = intermediate_to_linear_srgb(
            Intermediate::ThreeColor(pixels),
            &matrices(Illuminant::D65, matrix),
            [2.0, 1.0, 0.5, f32::NAN],
        )
        .unwrap();
        for (actual, expected) in result.data.into_iter().zip([0.5, 0.5, 0.375]) {
            close(actual, expected);
        }
    }

    #[test]
    fn unavailable_or_non_finite_camera_white_balance_falls_back_to_unity() {
        let matrix: Vec<f32> = XYZ_TO_SRGB_D65
            .iter()
            .flat_map(|row| row.iter().copied())
            .collect();
        for white_balance in [[f32::NAN, 2.0, 3.0, 1.0], [2.0, f32::INFINITY, 3.0, 1.0]] {
            let pixels = Color2D::<f32, 3>::new_with(vec![[0.25, 0.5, 0.75]], 1, 1);
            let result = intermediate_to_linear_srgb(
                Intermediate::ThreeColor(pixels),
                &matrices(Illuminant::D65, matrix.clone()),
                white_balance,
            )
            .unwrap();
            for (actual, expected) in result.data.into_iter().zip([0.25, 0.5, 0.75]) {
                close(actual, expected);
            }
        }
    }

    #[test]
    fn d65_is_used_regardless_of_insertion_order() {
        let pixels = Color2D::<f32, 3>::new_with(vec![[0.2, 0.4, 0.6]], 1, 1);
        let identity_camera: Vec<f32> = XYZ_TO_SRGB_D65
            .iter()
            .flat_map(|row| row.iter().copied())
            .collect();
        let mut available = matrices(Illuminant::A, vec![1.0; 9]);
        available.insert(Illuminant::D65, identity_camera);
        let result =
            intermediate_to_linear_srgb(Intermediate::ThreeColor(pixels), &available, [1.0; 4])
                .unwrap();
        for (actual, expected) in result.data.into_iter().zip([0.2, 0.4, 0.6]) {
            close(actual, expected);
        }
    }

    #[test]
    fn four_channel_transform_uses_all_camera_channels_without_clipping() {
        let pixels = Color2D::<f32, 4>::new_with(vec![[1.0, 2.0, 3.0, 4.0]], 1, 1);
        let matrix = vec![
            0.4124564, 0.3575761, 0.1804375, 0.2126729, 0.7151522, 0.0721750, 0.0193339, 0.119_192,
            0.9503041, 0.2126729, 0.7151522, 0.0721750,
        ];
        let result = intermediate_to_linear_srgb(
            Intermediate::FourColor(pixels),
            &matrices(Illuminant::D65, matrix),
            [1.0; 4],
        )
        .unwrap();
        assert_eq!(result.data.len(), 3);
        assert!(result.data.iter().all(|value| value.is_finite()));
        assert!(result.data.iter().any(|value| *value > 1.0));
    }

    #[test]
    fn missing_malformed_and_degenerate_matrices_are_typed_errors() {
        let pixels = || Color2D::<f32, 3>::new_with(vec![[1.0; 3]], 1, 1);
        assert_eq!(
            intermediate_to_linear_srgb(
                Intermediate::ThreeColor(pixels()),
                &HashMap::new(),
                [1.0; 4]
            )
            .unwrap_err(),
            ColorError::MissingMatrix
        );
        let missing_d65 = intermediate_to_linear_srgb(
            Intermediate::ThreeColor(pixels()),
            &matrices(Illuminant::A, vec![1.0; 9]),
            [1.0; 4],
        )
        .unwrap_err();
        assert_eq!(
            missing_d65,
            ColorError::MissingD65Matrix {
                available: vec![Illuminant::A]
            }
        );
        assert_eq!(
            missing_d65.to_string(),
            "RAW has no D65 camera color matrix (available illuminants: [A])"
        );
        assert!(matches!(
            intermediate_to_linear_srgb(
                Intermediate::ThreeColor(pixels()),
                &matrices(Illuminant::D65, vec![1.0; 8]),
                [1.0; 4]
            ),
            Err(ColorError::InvalidMatrixLength { .. })
        ));
        assert_eq!(
            intermediate_to_linear_srgb(
                Intermediate::ThreeColor(pixels()),
                &matrices(Illuminant::D65, vec![f32::NAN; 9]),
                [1.0; 4]
            )
            .unwrap_err(),
            ColorError::NonFiniteMatrix {
                illuminant: Illuminant::D65
            }
        );
        assert_eq!(
            intermediate_to_linear_srgb(
                Intermediate::ThreeColor(pixels()),
                &matrices(
                    Illuminant::D65,
                    XYZ_TO_SRGB_D65
                        .iter()
                        .flat_map(|row| row.iter().copied())
                        .collect(),
                ),
                [1.0, 0.0, 1.0, 1.0]
            )
            .unwrap_err(),
            ColorError::InvalidWhiteBalance { channels: 3 }
        );
        assert_eq!(
            intermediate_to_linear_srgb(
                Intermediate::ThreeColor(pixels()),
                &matrices(Illuminant::D65, vec![0.0; 9]),
                [1.0; 4]
            )
            .unwrap_err(),
            ColorError::DegenerateMatrix {
                illuminant: Illuminant::D65
            }
        );
    }
}
