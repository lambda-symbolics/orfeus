//! Ratio Corrected Demosaicing, tiled to stay in cache.
//!
//! RCD is Luis Sanz Rodríguez's algorithm, release 2.3, as RawTherapee and
//! darktable ship it (Ingo Weyrich's tiled arrangement, GPL-3). It decides a
//! direction the way PPG does but from a squared high-pass filter summed over
//! three neighbours rather than one gradient, so a texture near the sensor's
//! limit is read the same way at every pixel of it instead of flipping from
//! one photosite to the next — which is the maze pattern PPG draws on fabric,
//! brick and distant foliage. Green is estimated along each cardinal direction
//! as the neighbour scaled by the ratio of low-pass filtered values (hence the
//! name), the two directions are mixed by the discrimination, and red and blue
//! follow as colour differences against that green, first at the photosites of
//! the other colour along the diagonals, then at the green photosites along
//! the cardinals.
//!
//! Every pixel's value depends on the sensor ten pixels around it, so a tile
//! carries a ten-pixel halo and the frame is mirrored past its edges, which
//! keeps the colour filter's phase and gives the border pixels a proper develop
//! instead of the bilinear fill the reference uses there. The whole develop is
//! defined on the frame, not on the tiles: two tilings give the same pixels,
//! and a test holds them to it.
//!
//! One quirk is kept on purpose. The reference evaluates the diagonal high-pass
//! filter at odd columns only, and reads it at the nearest evaluated column
//! for a photosite that sits on an even one. It is how the algorithm behaves
//! everywhere it is judged, so it is reproduced here rather than corrected,
//! and the reference comparison in the tests depends on it.

use rawler::pixarray::Color2D;
use rayon::prelude::*;

use super::demosaic::{BLUE, BayerFrame, GREEN, RED, Window};

/// Output pixels across one tile, and rows in one parallel band.
///
/// Eleven planes of `(TILE + 2 * HALO) * (BAND + 2 * HALO)` floats, about
/// 340 KB, which stays in a core's own cache through all eight sweeps.
const TILE: usize = 128;
const BAND: usize = 32;

/// Pixels of context a developed pixel depends on: red and blue at a green
/// photosite read the red and blue interpolated three away, which read green
/// two further, which read the direction map one further, itself built from a
/// high-pass filter reaching four.
const HALO: usize = 10;

/// Tolerances that keep the ratios finite, as the reference sets them.
const EPS: f32 = 1.0e-5;
const EPS_SQUARED: f32 = 1.0e-10;

/// Develops the part of FRAME that CROP names, to interleaved RGB.
pub(crate) fn demosaic_rcd<T: Copy + Into<f32> + Sync>(
    frame: &BayerFrame<'_, T>,
    crop: Window,
) -> Color2D<f32, 3> {
    demosaic_rcd_tiled(frame, crop, TILE, BAND)
}

/// The same develop with the tile geometry named, so a test can prove that
/// changing it changes nothing about the result.
fn demosaic_rcd_tiled<T: Copy + Into<f32> + Sync>(
    frame: &BayerFrame<'_, T>,
    crop: Window,
    tile_width: usize,
    band_height: usize,
) -> Color2D<f32, 3> {
    let (width, height) = (crop.width, crop.height);
    let mut output = vec![[0.0_f32; 3]; width * height];
    output
        .par_chunks_mut(width * band_height)
        .enumerate()
        .for_each_init(
            || Planes::new(tile_width, band_height),
            |planes, (band, rows)| {
                let first_row = crop.top + band * band_height;
                let last_row = (first_row + rows.len() / width).min(crop.top + height);
                let mut first_column = crop.left;
                while first_column < crop.left + width {
                    let last_column = (first_column + tile_width).min(crop.left + width);
                    develop_tile(frame, planes, first_row, last_row, first_column, last_column);
                    for row in first_row..last_row {
                        let out = &mut rows[(row - first_row) * width..]
                            [first_column - crop.left..last_column - crop.left];
                        for (column, pixel) in out.iter_mut().enumerate() {
                            *pixel = planes.pixel(row, first_column + column);
                        }
                    }
                    first_column = last_column;
                }
            },
        );
    Color2D::new_with(output, width, height)
}

/// A tile's working planes, addressed in tile coordinates with the halo
/// included: row 0 is `HALO` rows above the first output row.
struct Planes {
    /// Developed coordinate of the buffer's first pixel; negative at the
    /// frame's edge, where the halo reaches past it.
    top: isize,
    left: isize,
    stride: usize,
    rows: usize,
    cfa: Vec<f32>,
    high_pass_v: Vec<f32>,
    high_pass_h: Vec<f32>,
    /// Vertical against horizontal discrimination, 0 all horizontal to 1 all
    /// vertical.
    vh: Vec<f32>,
    low_pass: Vec<f32>,
    high_pass_p: Vec<f32>,
    high_pass_q: Vec<f32>,
    /// The two diagonals' discrimination, P (north-west to south-east)
    /// against Q.
    pq: Vec<f32>,
    rgb: [Vec<f32>; 3],
}

impl Planes {
    fn new(tile_width: usize, band_height: usize) -> Self {
        let capacity = (tile_width + 2 * HALO) * (band_height + 2 * HALO);
        let plane = || vec![0.0_f32; capacity];
        Self {
            top: 0,
            left: 0,
            stride: 0,
            rows: 0,
            cfa: plane(),
            high_pass_v: plane(),
            high_pass_h: plane(),
            vh: plane(),
            low_pass: plane(),
            high_pass_p: plane(),
            high_pass_q: plane(),
            pq: plane(),
            rgb: [plane(), plane(), plane()],
        }
    }

    fn place(&mut self, first_row: usize, last_row: usize, first_column: usize, last_column: usize) {
        self.top = first_row as isize - HALO as isize;
        self.left = first_column as isize - HALO as isize;
        self.stride = last_column - first_column + 2 * HALO;
        self.rows = last_row - first_row + 2 * HALO;
    }

    /// The developed pixel at a developed coordinate inside the output tile.
    #[inline]
    fn pixel(&self, row: usize, column: usize) -> [f32; 3] {
        let index = (row as isize - self.top) as usize * self.stride
            + (column as isize - self.left) as usize;
        // Colour differences can land below zero; the reference clamps too.
        [
            self.rgb[RED][index].max(0.0),
            self.rgb[GREEN][index].max(0.0),
            self.rgb[BLUE][index].max(0.0),
        ]
    }
}

#[inline]
fn squared(value: f32) -> f32 {
    value * value
}

/// `weight * first + (1 - weight) * second`, written as the reference writes it.
#[inline]
fn mix(weight: f32, first: f32, second: f32) -> f32 {
    weight * (first - second) + second
}

/// The direction weight to use: the neighbourhood's when it is more decided
/// than the pixel's own, the pixel's own otherwise.
#[inline]
fn refined(central: f32, neighbourhood: f32) -> f32 {
    if (0.5 - central).abs() < (0.5 - neighbourhood).abs() {
        neighbourhood
    } else {
        central
    }
}

fn develop_tile<T: Copy + Into<f32>>(
    frame: &BayerFrame<'_, T>,
    planes: &mut Planes,
    first_row: usize,
    last_row: usize,
    first_column: usize,
    last_column: usize,
) {
    planes.place(first_row, last_row, first_column, last_column);
    let Planes {
        top,
        left,
        stride,
        rows,
        cfa,
        high_pass_v,
        high_pass_h,
        vh,
        low_pass,
        high_pass_p,
        high_pass_q,
        pq,
        rgb,
    } = planes;
    let (top, left, stride, rows) = (*top, *left, *stride, *rows);
    let (w1, w2, w3, w4) = (stride, 2 * stride, 3 * stride, 4 * stride);
    let green_at = |row: usize, column: usize| {
        frame.color_at_signed(top + row as isize, left + column as isize) == GREEN
    };
    // The first column at or after FROM whose photosite is not green, in ROW.
    let first_non_green = |row: usize, from: usize| if green_at(row, from) { from + 1 } else { from };
    // Whether a tile column lies on an odd column of the frame.
    let odd_column = |column: usize| (left + column as isize) & 1 == 1;

    // The sensor, mirrored past the frame's edges, each photosite in its own
    // plane and nothing in the other two.
    let [red, green, blue] = rgb;
    for row in 0..rows {
        for column in 0..stride {
            let (frame_row, frame_column) = (top + row as isize, left + column as isize);
            let value = frame.sample_reflected(frame_row, frame_column);
            let index = row * stride + column;
            cfa[index] = value;
            let colour = frame.color_at_signed(frame_row, frame_column);
            red[index] = if colour == RED { value } else { 0.0 };
            green[index] = if colour == GREEN { value } else { 0.0 };
            blue[index] = if colour == BLUE { value } else { 0.0 };
        }
    }

    // Step 1: squared vertical and horizontal high-pass filters on the colour
    // differences, and from their three-neighbour sums the vertical against
    // horizontal discrimination.
    for row in 3..rows - 3 {
        for column in 3..stride - 3 {
            let i = row * stride + column;
            high_pass_v[i] = squared(
                (cfa[i - w3] - cfa[i - w1] - cfa[i + w1] + cfa[i + w3])
                    - 3.0 * (cfa[i - w2] + cfa[i + w2])
                    + 6.0 * cfa[i],
            );
            high_pass_h[i] = squared(
                (cfa[i - 3] - cfa[i - 1] - cfa[i + 1] + cfa[i + 3])
                    - 3.0 * (cfa[i - 2] + cfa[i + 2])
                    + 6.0 * cfa[i],
            );
        }
    }
    for row in 4..rows - 4 {
        for column in 4..stride - 4 {
            let i = row * stride + column;
            let vertical =
                EPS_SQUARED.max(high_pass_v[i - w1] + high_pass_v[i] + high_pass_v[i + w1]);
            let horizontal =
                EPS_SQUARED.max(high_pass_h[i - 1] + high_pass_h[i] + high_pass_h[i + 1]);
            vh[i] = vertical / (vertical + horizontal);
        }
    }

    // Step 2: a low-pass filter over the sensor at every red and blue
    // photosite, mixing its own colour with the greens beside it and the
    // other colour at its corners.
    for row in 3..rows - 3 {
        let mut column = first_non_green(row, 3);
        while column < stride - 3 {
            let i = row * stride + column;
            low_pass[i] = cfa[i]
                + 0.5 * (cfa[i - w1] + cfa[i + w1] + cfa[i - 1] + cfa[i + 1])
                + 0.25 * (cfa[i - w1 - 1] + cfa[i - w1 + 1] + cfa[i + w1 - 1] + cfa[i + w1 + 1]);
            column += 2;
        }
    }

    // Step 3: green at the red and blue photosites.
    for row in 5..rows - 5 {
        let mut column = first_non_green(row, 5);
        while column < stride - 5 {
            let i = row * stride + column;
            let centre = cfa[i];
            // Cardinal gradients.
            let north = EPS
                + ((cfa[i - w1] - cfa[i + w1]).abs() + (centre - cfa[i - w2]).abs())
                + ((cfa[i - w1] - cfa[i - w3]).abs() + (cfa[i - w2] - cfa[i - w4]).abs());
            let south = EPS
                + ((cfa[i - w1] - cfa[i + w1]).abs() + (centre - cfa[i + w2]).abs())
                + ((cfa[i + w1] - cfa[i + w3]).abs() + (cfa[i + w2] - cfa[i + w4]).abs());
            let west = EPS
                + ((cfa[i - 1] - cfa[i + 1]).abs() + (centre - cfa[i - 2]).abs())
                + ((cfa[i - 1] - cfa[i - 3]).abs() + (cfa[i - 2] - cfa[i - 4]).abs());
            let east = EPS
                + ((cfa[i - 1] - cfa[i + 1]).abs() + (centre - cfa[i + 2]).abs())
                + ((cfa[i + 1] - cfa[i + 3]).abs() + (cfa[i + 2] - cfa[i + 4]).abs());
            // Cardinal estimates: the neighbour, scaled by the ratio of the
            // low-pass values here and two photosites beyond it.
            let low = low_pass[i];
            let north_estimate = cfa[i - w1] * (low + low) / (EPS + low + low_pass[i - w2]);
            let south_estimate = cfa[i + w1] * (low + low) / (EPS + low + low_pass[i + w2]);
            let west_estimate = cfa[i - 1] * (low + low) / (EPS + low + low_pass[i - 2]);
            let east_estimate = cfa[i + 1] * (low + low) / (EPS + low + low_pass[i + 2]);
            let vertical =
                (south * north_estimate + north * south_estimate) / (north + south);
            let horizontal =
                (west * east_estimate + east * west_estimate) / (east + west);
            let neighbourhood =
                0.25 * ((vh[i - w1 - 1] + vh[i - w1 + 1]) + (vh[i + w1 - 1] + vh[i + w1 + 1]));
            green[i] = mix(refined(vh[i], neighbourhood), horizontal, vertical);
            column += 2;
        }
    }

    // Step 4.0: the squared high-pass filters along the two diagonals, at the
    // odd columns of the frame — see the module note.
    for row in 4..rows - 4 {
        let mut column = if odd_column(4) { 4 } else { 5 };
        while column < stride - 4 {
            let i = row * stride + column;
            high_pass_p[i] = squared(
                (cfa[i - w3 - 3] - cfa[i - w1 - 1] - cfa[i + w1 + 1] + cfa[i + w3 + 3])
                    - 3.0 * (cfa[i - w2 - 2] + cfa[i + w2 + 2])
                    + 6.0 * cfa[i],
            );
            high_pass_q[i] = squared(
                (cfa[i - w3 + 3] - cfa[i - w1 + 1] - cfa[i + w1 - 1] + cfa[i + w3 - 3])
                    - 3.0 * (cfa[i - w2 + 2] + cfa[i + w2 - 2])
                    + 6.0 * cfa[i],
            );
            column += 2;
        }
    }

    // Step 4.1: the diagonal discrimination at the red and blue photosites.
    for row in 6..rows - 6 {
        let mut column = first_non_green(row, 6);
        while column < stride - 6 {
            let i = row * stride + column;
            let (p, q) = if odd_column(column) {
                (
                    high_pass_p[i - w1] + high_pass_p[i] + high_pass_p[i + w1 + 2],
                    high_pass_q[i - w1 + 2] + high_pass_q[i] + high_pass_q[i + w1],
                )
            } else {
                (
                    high_pass_p[i - w1 - 1] + high_pass_p[i + 1] + high_pass_p[i + w1 + 1],
                    high_pass_q[i - w1 + 1] + high_pass_q[i + 1] + high_pass_q[i + w1 - 1],
                )
            };
            let (p, q) = (EPS_SQUARED.max(p), EPS_SQUARED.max(q));
            pq[i] = p / (p + q);
            column += 2;
        }
    }

    // Step 4.2: red at the blue photosites and blue at the red ones, as a
    // colour difference against green carried along the better diagonal.
    for row in 7..rows - 7 {
        let mut column = first_non_green(row, 7);
        while column < stride - 7 {
            let i = row * stride + column;
            let other = if frame.color_at_signed(top + row as isize, left + column as isize) == RED
            {
                &mut *blue
            } else {
                &mut *red
            };
            let neighbourhood =
                0.25 * (pq[i - w1 - 1] + pq[i - w1 + 1] + pq[i + w1 - 1] + pq[i + w1 + 1]);
            let weight = refined(pq[i], neighbourhood);
            let north_west = EPS
                + (other[i - w1 - 1] - other[i + w1 + 1]).abs()
                + (other[i - w1 - 1] - other[i - w3 - 3]).abs()
                + (green[i] - green[i - w2 - 2]).abs();
            let north_east = EPS
                + (other[i - w1 + 1] - other[i + w1 - 1]).abs()
                + (other[i - w1 + 1] - other[i - w3 + 3]).abs()
                + (green[i] - green[i - w2 + 2]).abs();
            let south_west = EPS
                + (other[i - w1 + 1] - other[i + w1 - 1]).abs()
                + (other[i + w1 - 1] - other[i + w3 - 3]).abs()
                + (green[i] - green[i + w2 - 2]).abs();
            let south_east = EPS
                + (other[i - w1 - 1] - other[i + w1 + 1]).abs()
                + (other[i + w1 + 1] - other[i + w3 + 3]).abs()
                + (green[i] - green[i + w2 + 2]).abs();
            let north_west_estimate = other[i - w1 - 1] - green[i - w1 - 1];
            let north_east_estimate = other[i - w1 + 1] - green[i - w1 + 1];
            let south_west_estimate = other[i + w1 - 1] - green[i + w1 - 1];
            let south_east_estimate = other[i + w1 + 1] - green[i + w1 + 1];
            let p = (north_west * south_east_estimate + south_east * north_west_estimate)
                / (north_west + south_east);
            let q = (north_east * south_west_estimate + south_west * north_east_estimate)
                / (north_east + south_west);
            other[i] = green[i] + mix(weight, q, p);
            column += 2;
        }
    }

    // Step 4.3: red and blue at the green photosites, as colour differences
    // carried along the better cardinal direction.
    for row in HALO..rows - HALO {
        let mut column = if green_at(row, HALO) { HALO } else { HALO + 1 };
        while column < stride - HALO {
            let i = row * stride + column;
            let neighbourhood =
                0.25 * ((vh[i - w1 - 1] + vh[i - w1 + 1]) + (vh[i + w1 - 1] + vh[i + w1 + 1]));
            let weight = refined(vh[i], neighbourhood);
            let centre = green[i];
            let north_green = EPS + (centre - green[i - w2]).abs();
            let south_green = EPS + (centre - green[i + w2]).abs();
            let west_green = EPS + (centre - green[i - 2]).abs();
            let east_green = EPS + (centre - green[i + 2]).abs();
            for plane in [&mut *red, &mut *blue] {
                let vertical_span = (plane[i - w1] - plane[i + w1]).abs();
                let horizontal_span = (plane[i - 1] - plane[i + 1]).abs();
                let north = north_green + vertical_span + (plane[i - w1] - plane[i - w3]).abs();
                let south = south_green + vertical_span + (plane[i + w1] - plane[i + w3]).abs();
                let west = west_green + horizontal_span + (plane[i - 1] - plane[i - 3]).abs();
                let east = east_green + horizontal_span + (plane[i + 1] - plane[i + 3]).abs();
                let north_estimate = plane[i - w1] - green[i - w1];
                let south_estimate = plane[i + w1] - green[i + w1];
                let west_estimate = plane[i - 1] - green[i - 1];
                let east_estimate = plane[i + 1] - green[i + 1];
                let vertical =
                    (north * south_estimate + south * north_estimate) / (north + south);
                let horizontal =
                    (east * west_estimate + west * east_estimate) / (east + west);
                plane[i] = centre + mix(weight, horizontal, vertical);
            }
            column += 2;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A frame with a ramp, a diagonal edge, oblique stripes at the sensor's
    /// limit and hashed noise. Every term is exact in single precision, so the
    /// C++ reference harness computes the same input bit for bit.
    pub(crate) fn rcd_frame(width: usize, height: usize) -> Vec<f32> {
        (0..width * height)
            .map(|index| {
                let (x, y) = (index % width, index / width);
                let ramp = 300.0 + x as f32 * 1.5 + y as f32 * 2.5;
                let edge = if x + y > width { 600.0 } else { 0.0 };
                let stripes = (((x * 7 + y * 3) % 23) * 6) as f32;
                let noise = (index as u32).wrapping_mul(2_654_435_761) >> 20;
                (ramp + edge + stripes + (noise % 64) as f32 - 32.0) / 2048.0
            })
            .collect()
    }

    fn frame<'a>(
        data: &'a [f32],
        width: usize,
        height: usize,
        colors: [usize; 4],
    ) -> BayerFrame<'a, f32> {
        BayerFrame {
            data,
            stride: width,
            left: 0,
            top: 0,
            width,
            height,
            colors,
            levels: [(0.0, 1.0); 4],
        }
    }

    fn whole(width: usize, height: usize) -> Window {
        Window {
            left: 0,
            top: 0,
            width,
            height,
        }
    }

    const PATTERNS: [(&str, [usize; 4]); 4] = [
        ("RGGB", [RED, GREEN, GREEN, BLUE]),
        ("BGGR", [BLUE, GREEN, GREEN, RED]),
        ("GRBG", [GREEN, RED, BLUE, GREEN]),
        ("GBRG", [GREEN, BLUE, RED, GREEN]),
    ];

    /// The reference implementation, transliterated from librtprocess's
    /// `rcd_demosaic` for a frame that fits in one of its tiles: the same
    /// loops, bounds and half-resolution index arithmetic, so that what it
    /// computes is the reference's own numbers and not a reading of them. It
    /// leaves the nine-pixel border the reference fills bilinearly at zero.
    fn reference_rcd(values: &[f32], width: usize, height: usize, colors: [usize; 4]) -> Vec<[f32; 3]> {
        let fc = |row: usize, column: usize| colors[(row & 1) * 2 + (column & 1)];
        let (w1, w2, w3, w4) = (width, 2 * width, 3 * width, 4 * width);
        let n = width * height;
        let cfa: Vec<f32> = values.iter().map(|value| value.clamp(0.0, 1.0)).collect();
        let mut rgb = vec![vec![0.0_f32; n]; 3];
        for row in 0..height {
            let (c0, c1) = (fc(row, 0), fc(row, 1));
            for column in 0..width {
                let i = row * width + column;
                rgb[c0][i] = cfa[i];
                rgb[c1][i] = cfa[i];
            }
        }
        let mut vh_dir = vec![0.0_f32; n];
        // Step 1: the vertical and horizontal high pass, summed over rows
        // and columns exactly as the reference's rolling buffers do.
        let hpf_v = |i: usize| {
            squared((cfa[i - w3] - cfa[i - w1] - cfa[i + w1] + cfa[i + w3]) - 3.0 * (cfa[i - w2] + cfa[i + w2]) + 6.0 * cfa[i])
        };
        let hpf_h = |i: usize| {
            squared((cfa[i - 3] - cfa[i - 1] - cfa[i + 1] + cfa[i + 3]) - 3.0 * (cfa[i - 2] + cfa[i + 2]) + 6.0 * cfa[i])
        };
        for row in 4..height - 4 {
            for column in 4..width - 4 {
                let i = row * width + column;
                let v_stat = EPS_SQUARED.max(hpf_v(i - w1) + hpf_v(i) + hpf_v(i + w1));
                let h_stat = EPS_SQUARED.max(hpf_h(i - 1) + hpf_h(i) + hpf_h(i + 1));
                vh_dir[i] = v_stat / (v_stat + h_stat);
            }
        }
        // Step 2: low pass at half resolution, indexed by indx / 2.
        let mut lpf = vec![0.0_f32; n / 2 + 1];
        for row in 2..height - 2 {
            let mut column = 2 + (fc(row, 0) & 1);
            while column < width - 2 {
                let i = row * width + column;
                lpf[i / 2] = cfa[i]
                    + 0.5 * (cfa[i - w1] + cfa[i + w1] + cfa[i - 1] + cfa[i + 1])
                    + 0.25 * (cfa[i - w1 - 1] + cfa[i - w1 + 1] + cfa[i + w1 - 1] + cfa[i + w1 + 1]);
                column += 2;
            }
        }
        // Step 3: green at red and blue.
        for row in 4..height - 4 {
            let mut column = 4 + (fc(row, 0) & 1);
            while column < width - 4 {
                let i = row * width + column;
                let lp = i / 2;
                let cfai = cfa[i];
                let n_grad = EPS + ((cfa[i - w1] - cfa[i + w1]).abs() + (cfai - cfa[i - w2]).abs()) + ((cfa[i - w1] - cfa[i - w3]).abs() + (cfa[i - w2] - cfa[i - w4]).abs());
                let s_grad = EPS + ((cfa[i - w1] - cfa[i + w1]).abs() + (cfai - cfa[i + w2]).abs()) + ((cfa[i + w1] - cfa[i + w3]).abs() + (cfa[i + w2] - cfa[i + w4]).abs());
                let w_grad = EPS + ((cfa[i - 1] - cfa[i + 1]).abs() + (cfai - cfa[i - 2]).abs()) + ((cfa[i - 1] - cfa[i - 3]).abs() + (cfa[i - 2] - cfa[i - 4]).abs());
                let e_grad = EPS + ((cfa[i - 1] - cfa[i + 1]).abs() + (cfai - cfa[i + 2]).abs()) + ((cfa[i + 1] - cfa[i + 3]).abs() + (cfa[i + 2] - cfa[i + 4]).abs());
                let lpfi = lpf[lp];
                let n_est = cfa[i - w1] * (lpfi + lpfi) / (EPS + lpfi + lpf[lp - w1]);
                let s_est = cfa[i + w1] * (lpfi + lpfi) / (EPS + lpfi + lpf[lp + w1]);
                let w_est = cfa[i - 1] * (lpfi + lpfi) / (EPS + lpfi + lpf[lp - 1]);
                let e_est = cfa[i + 1] * (lpfi + lpfi) / (EPS + lpfi + lpf[lp + 1]);
                let v_est = (s_grad * n_est + n_grad * s_est) / (n_grad + s_grad);
                let h_est = (w_grad * e_est + e_grad * w_est) / (e_grad + w_grad);
                let central = vh_dir[i];
                let neighbourhood = 0.25 * ((vh_dir[i - w1 - 1] + vh_dir[i - w1 + 1]) + (vh_dir[i + w1 - 1] + vh_dir[i + w1 + 1]));
                rgb[1][i] = mix(refined(central, neighbourhood), h_est, v_est);
                column += 2;
            }
        }
        // Step 4.0: the diagonal high pass at odd columns, indexed by indx / 2.
        let mut p_hpf = vec![0.0_f32; n / 2 + 1];
        let mut q_hpf = vec![0.0_f32; n / 2 + 1];
        for row in 3..height - 3 {
            let mut column = 3;
            while column < width - 3 {
                let i = row * width + column;
                p_hpf[i / 2] = squared((cfa[i - w3 - 3] - cfa[i - w1 - 1] - cfa[i + w1 + 1] + cfa[i + w3 + 3]) - 3.0 * (cfa[i - w2 - 2] + cfa[i + w2 + 2]) + 6.0 * cfa[i]);
                q_hpf[i / 2] = squared((cfa[i - w3 + 3] - cfa[i - w1 + 1] - cfa[i + w1 - 1] + cfa[i + w3 - 3]) - 3.0 * (cfa[i - w2 + 2] + cfa[i + w2 - 2]) + 6.0 * cfa[i]);
                column += 2;
            }
        }
        // Step 4.1: the diagonal discrimination, indexed by indx / 2.
        let mut pq_dir = vec![0.0_f32; n / 2 + 1];
        for row in 4..height - 4 {
            let mut column = 4 + (fc(row, 0) & 1);
            while column < width - 4 {
                let i = row * width + column;
                let (i2, i3, i4) = (i / 2, (i - w1 - 1) / 2, (i + w1 - 1) / 2);
                let p_stat = EPS_SQUARED.max(p_hpf[i3] + p_hpf[i2] + p_hpf[i4 + 1]);
                let q_stat = EPS_SQUARED.max(q_hpf[i3 + 1] + q_hpf[i2] + q_hpf[i4]);
                pq_dir[i2] = p_stat / (p_stat + q_stat);
                column += 2;
            }
        }
        // Step 4.2: red at blue and blue at red.
        for row in 4..height - 4 {
            let mut column = 4 + (fc(row, 0) & 1);
            while column < width - 4 {
                let i = row * width + column;
                let c = 2 - fc(row, column);
                let (pq1, pq2, pq3) = (i / 2, (i - w1 - 1) / 2, (i + w1 - 1) / 2);
                let central = pq_dir[pq1];
                let neighbourhood = 0.25 * (pq_dir[pq2] + pq_dir[pq2 + 1] + pq_dir[pq3] + pq_dir[pq3 + 1]);
                let disc = refined(central, neighbourhood);
                let (rc, g) = (&rgb[c], &rgb[1]);
                let nw_grad = EPS + (rc[i - w1 - 1] - rc[i + w1 + 1]).abs() + (rc[i - w1 - 1] - rc[i - w3 - 3]).abs() + (g[i] - g[i - w2 - 2]).abs();
                let ne_grad = EPS + (rc[i - w1 + 1] - rc[i + w1 - 1]).abs() + (rc[i - w1 + 1] - rc[i - w3 + 3]).abs() + (g[i] - g[i - w2 + 2]).abs();
                let sw_grad = EPS + (rc[i - w1 + 1] - rc[i + w1 - 1]).abs() + (rc[i + w1 - 1] - rc[i + w3 - 3]).abs() + (g[i] - g[i + w2 - 2]).abs();
                let se_grad = EPS + (rc[i - w1 - 1] - rc[i + w1 + 1]).abs() + (rc[i + w1 + 1] - rc[i + w3 + 3]).abs() + (g[i] - g[i + w2 + 2]).abs();
                let nw_est = rc[i - w1 - 1] - g[i - w1 - 1];
                let ne_est = rc[i - w1 + 1] - g[i - w1 + 1];
                let sw_est = rc[i + w1 - 1] - g[i + w1 - 1];
                let se_est = rc[i + w1 + 1] - g[i + w1 + 1];
                let p_est = (nw_grad * se_est + se_grad * nw_est) / (nw_grad + se_grad);
                let q_est = (ne_grad * sw_est + sw_grad * ne_est) / (ne_grad + sw_grad);
                let value = g[i] + mix(disc, q_est, p_est);
                rgb[c][i] = value;
                column += 2;
            }
        }
        // Step 4.3: red and blue at green.
        for row in 4..height - 4 {
            let mut column = 4 + (fc(row, 1) & 1);
            while column < width - 4 {
                let i = row * width + column;
                let central = vh_dir[i];
                let neighbourhood = 0.25 * ((vh_dir[i - w1 - 1] + vh_dir[i - w1 + 1]) + (vh_dir[i + w1 - 1] + vh_dir[i + w1 + 1]));
                let disc = refined(central, neighbourhood);
                let g = rgb[1].clone();
                let rgb1 = g[i];
                let n1 = EPS + (rgb1 - g[i - w2]).abs();
                let s1 = EPS + (rgb1 - g[i + w2]).abs();
                let west1 = EPS + (rgb1 - g[i - 2]).abs();
                let e1 = EPS + (rgb1 - g[i + 2]).abs();
                for c in [0, 2] {
                    let rc = &mut rgb[c];
                    let sn = (rc[i - w1] - rc[i + w1]).abs();
                    let ew = (rc[i - 1] - rc[i + 1]).abs();
                    let n_grad = n1 + sn + (rc[i - w1] - rc[i - w3]).abs();
                    let s_grad = s1 + sn + (rc[i + w1] - rc[i + w3]).abs();
                    let w_grad = west1 + ew + (rc[i - 1] - rc[i - 3]).abs();
                    let e_grad = e1 + ew + (rc[i + 1] - rc[i + 3]).abs();
                    let n_est = rc[i - w1] - g[i - w1];
                    let s_est = rc[i + w1] - g[i + w1];
                    let w_est = rc[i - 1] - g[i - 1];
                    let e_est = rc[i + 1] - g[i + 1];
                    let v_est = (n_grad * s_est + s_grad * n_est) / (n_grad + s_grad);
                    let h_est = (e_grad * w_est + w_grad * e_est) / (e_grad + w_grad);
                    rc[i] = rgb1 + mix(disc, h_est, v_est);
                }
                column += 2;
            }
        }
        let border = 9;
        (0..n)
            .map(|i| {
                let (row, column) = (i / width, i % width);
                if row < border || column < border || row >= height - border || column >= width - border {
                    [0.0; 3]
                } else {
                    [rgb[0][i].max(0.0), rgb[1][i].max(0.0), rgb[2][i].max(0.0)]
                }
            })
            .collect()
    }

    fn assert_close(
        what: &str,
        mine: &[[f32; 3]],
        theirs: &[[f32; 3]],
        width: usize,
        height: usize,
        margin: usize,
        tolerance: f32,
    ) {
        let mut worst = (0.0_f32, 0, 0, 0);
        for row in margin..height - margin {
            for column in margin..width - margin {
                let index = row * width + column;
                for channel in 0..3 {
                    let difference = (mine[index][channel] - theirs[index][channel]).abs();
                    if difference > worst.0 {
                        worst = (difference, row, column, channel);
                    }
                }
            }
        }
        assert!(
            worst.0 < tolerance,
            "{what}: worst difference {} at row {} column {} channel {}",
            worst.0,
            worst.1,
            worst.2,
            worst.3
        );
    }

    #[test]
    fn a_flat_frame_develops_to_one_flat_colour() {
        let (width, height) = (64, 48);
        let data = vec![0.5_f32; width * height];
        let developed = demosaic_rcd(
            &frame(&data, width, height, [RED, GREEN, GREEN, BLUE]),
            whole(width, height),
        );
        // Not exact: the tolerance in the ratio's denominator biases every
        // estimate by a couple of millionths, in the reference as here.
        for pixel in developed.data.iter() {
            for channel in pixel {
                assert!((channel - 0.5).abs() < 1.0e-5, "flat frame produced {channel}");
            }
        }
    }

    #[test]
    fn the_tile_geometry_does_not_change_the_result() {
        // Tiling is an implementation detail: any tile shape, including one
        // holding the frame whole, has to give the same pixels everywhere,
        // border included, or a seam is being drawn somewhere.
        let (width, height) = (TILE * 2 + 37, BAND + 21);
        let data = rcd_frame(width, height);
        let source = frame(&data, width, height, [RED, GREEN, GREEN, BLUE]);
        let reference = demosaic_rcd_tiled(&source, whole(width, height), width, height);
        for (tile_width, band_height) in [(TILE, BAND), (16, 8), (37, 64), (width, 3), (11, 1)] {
            let tiled = demosaic_rcd_tiled(&source, whole(width, height), tile_width, band_height);
            assert_close(
                &format!("tiles {tile_width}x{band_height}"),
                &tiled.data,
                &reference.data,
                width,
                height,
                0,
                1.0e-6,
            );
        }
    }

    #[test]
    fn a_cropped_develop_matches_the_same_part_of_a_whole_one() {
        let (width, height) = (TILE + 60, BAND + 40);
        let data = rcd_frame(width, height);
        let source = frame(&data, width, height, [GREEN, RED, BLUE, GREEN]);
        let developed = demosaic_rcd(&source, whole(width, height));
        let crop = Window {
            left: 8,
            top: 6,
            width: width - 30,
            height: height - 20,
        };
        let cropped = demosaic_rcd(&source, crop);
        for row in 0..crop.height {
            for column in 0..crop.width {
                let taken = cropped.data[row * crop.width + column];
                let expected = developed.data[(row + crop.top) * width + column + crop.left];
                for (taken, expected) in taken.iter().zip(expected) {
                    assert!(
                        (taken - expected).abs() < 1.0e-6,
                        "row {row} column {column}: {taken} against {expected}"
                    );
                }
            }
        }
    }

    #[test]
    fn matches_the_reference_implementation() {
        // Inside the reference's own border every pixel has its full context
        // on both sides, so the two must agree to rounding. The reference
        // reads a zeroed direction map on its outermost rows and that reaches
        // one row further in through the red and blue sweeps, hence ten.
        let (width, height) = (180, 120);
        let data = rcd_frame(width, height);
        for (name, colors) in PATTERNS {
            let theirs = reference_rcd(&data, width, height, colors);
            let mine = demosaic_rcd(&frame(&data, width, height, colors), whole(width, height));
            assert_close(name, &mine.data, &theirs, width, height, HALO, 2.0e-6);
        }
    }

    #[test]
    fn every_colour_filter_phase_develops_its_own_channels() {
        let (width, height) = (96, 72);
        let data = rcd_frame(width, height);
        for (name, colors) in PATTERNS {
            let developed = demosaic_rcd(&frame(&data, width, height, colors), whole(width, height));
            for channel in 0..3 {
                let spread = developed.data.iter().map(|pixel| pixel[channel]).fold(f32::MIN, f32::max)
                    - developed.data.iter().map(|pixel| pixel[channel]).fold(f32::MAX, f32::min);
                assert!(spread > 0.05, "{name}: channel {channel} is flat");
            }
        }
    }

    /// Checks the transliteration above, and the tiled develop, against a
    /// dump written by librtprocess's own `rcd_demosaic` over the same frame:
    /// run `harness` from the session notes for each pattern, then
    /// `ORFEUS_RCD_REFERENCE_DIR=<dir> cargo test --release -- --ignored`.
    #[test]
    #[ignore]
    fn matches_the_c_reference_dump() {
        let directory = std::env::var("ORFEUS_RCD_REFERENCE_DIR").expect("ORFEUS_RCD_REFERENCE_DIR");
        for (name, colors) in PATTERNS {
            let bytes = std::fs::read(format!("{directory}/rcd-ref-{name}.bin")).expect("reference dump");
            let word = |at: usize| u32::from_le_bytes(bytes[at..at + 4].try_into().unwrap());
            let (width, height) = (word(0) as usize, word(4) as usize);
            let dump: Vec<[f32; 3]> = bytes[8..]
                .chunks_exact(12)
                .map(|pixel| {
                    let value = |at: usize| f32::from_le_bytes(pixel[at..at + 4].try_into().unwrap());
                    [value(0), value(4), value(8)]
                })
                .collect();
            assert_eq!(dump.len(), width * height);
            let data = rcd_frame(width, height);
            let theirs = reference_rcd(&data, width, height, colors);
            assert_close(&format!("{name} transliteration"), &theirs, &dump, width, height, 9, 1.0e-6);
            let mine = demosaic_rcd(&frame(&data, width, height, colors), whole(width, height));
            assert_close(&format!("{name} tiled"), &mine.data, &dump, width, height, HALO, 2.0e-6);
        }
    }
}
