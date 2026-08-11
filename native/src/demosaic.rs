//! Pattern Pixel Grouping demosaic, tiled to stay in cache.
//!
//! PPG is Chuan-kai Lin's algorithm: interpolate green along whichever of the
//! four directions has the smallest gradient, then fill red and blue by hue
//! transit against that green. Rawler implements it as four sweeps over a
//! whole-image RGB buffer, which at 80 MP means reading and writing about a
//! gigabyte five times over — nearly two seconds on this machine, almost all of
//! it waiting for memory.
//!
//! This runs the same arithmetic over 128-pixel tiles instead. A tile and its
//! three-pixel halo fit in L2, so the four sweeps happen in cache and main
//! memory sees one read of the sensor plane and one write of the result. The
//! sensor's integers are scaled on the way in rather than widened into a
//! separate plane first, and only the requested crop is developed.

use rawler::pixarray::Color2D;
use rayon::prelude::*;

/// Colour plane indices, matching rawler's `CFA_COLOR_*`.
const RED: usize = 0;
const GREEN: usize = 1;
const BLUE: usize = 2;

/// Output pixels across one tile, and rows in one parallel band.
///
/// A band's tile buffer is `(TILE + 2 * HALO) * (BAND + 2 * HALO) * 3` floats —
/// about 110 KB, which stays in a core's own cache while all four sweeps run
/// over it.
const TILE: usize = 128;
const BAND: usize = 64;

/// Pixels of context the sweeps read around a tile: green looks two away,
/// and the red/blue sweeps read a green that was itself interpolated.
const HALO: usize = 3;

/// A rectangle of pixels.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Window {
    pub(crate) left: usize,
    pub(crate) top: usize,
    pub(crate) width: usize,
    pub(crate) height: usize,
}

/// A Bayer frame ready to develop: the sensor plane, the area that may be read
/// from it, and what each corner of the colour filter's 2x2 cell means.
pub(crate) struct BayerFrame<'a, T> {
    pub(crate) data: &'a [T],
    pub(crate) stride: usize,
    pub(crate) left: usize,
    pub(crate) top: usize,
    pub(crate) width: usize,
    pub(crate) height: usize,
    /// Colour plane at each corner of the cell, in developed coordinates.
    pub(crate) colors: [usize; 4],
    /// Black level and the span from it to white, per corner.
    pub(crate) levels: [(f32, f32); 4],
}

impl<T: Copy + Into<f32>> BayerFrame<'_, T> {
    /// The scaled sensor value at a developed coordinate.
    #[inline]
    fn sample(&self, row: usize, column: usize) -> f32 {
        let corner = (row & 1) * 2 + (column & 1);
        let (black, span) = self.levels[corner];
        let value: f32 = self.data[(self.top + row) * self.stride + self.left + column].into();
        // Divided rather than scaled by a reciprocal: the reciprocal is a
        // rounding step of its own, and the hue transit below divides by
        // differences small enough to turn one of those into a visible pixel.
        (value - black).max(0.0) / span
    }

    #[inline]
    fn color_at(&self, row: usize, column: usize) -> usize {
        self.colors[(row & 1) * 2 + (column & 1)]
    }
}

/// A tile's own RGB buffer, addressed in developed coordinates.
struct Tile {
    /// Developed coordinate of the buffer's first pixel.
    left: isize,
    top: isize,
    width: usize,
    height: usize,
    data: Vec<[f32; 3]>,
}

impl Tile {
    fn new(tile_width: usize, band_height: usize) -> Self {
        Self {
            left: 0,
            top: 0,
            width: 0,
            height: 0,
            data: vec![[0.0; 3]; (tile_width + 2 * HALO) * (band_height + 2 * HALO)],
        }
    }

    #[inline]
    fn index(&self, row: usize, column: usize) -> usize {
        (row as isize - self.top) as usize * self.width + (column as isize - self.left) as usize
    }

    #[inline]
    fn at(&self, row: usize, column: usize) -> [f32; 3] {
        self.data[self.index(row, column)]
    }

    #[inline]
    fn set(&mut self, row: usize, column: usize, channel: usize, value: f32) {
        let index = self.index(row, column);
        self.data[index][channel] = value;
    }

    /// Whether a developed coordinate lies inside this buffer.
    #[inline]
    fn holds(&self, row: usize, column: usize) -> bool {
        let (row, column) = (row as isize, column as isize);
        row >= self.top
            && column >= self.left
            && row < self.top + self.height as isize
            && column < self.left + self.width as isize
    }
}

/// Develops the part of FRAME that CROP names, to interleaved RGB.
///
/// The crop only chooses what is emitted: every pixel is interpolated with the
/// same context it would have had in a whole-frame develop, so a cropped
/// develop and a cropped whole develop agree exactly.
pub(crate) fn demosaic_ppg<T: Copy + Into<f32> + Sync>(
    frame: &BayerFrame<'_, T>,
    crop: Window,
) -> Color2D<f32, 3> {
    demosaic_ppg_tiled(frame, crop, TILE, BAND)
}

/// The same develop with the tile geometry named, so a test can prove that
/// changing it changes nothing about the result.
fn demosaic_ppg_tiled<T: Copy + Into<f32> + Sync>(
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
            || Tile::new(tile_width, band_height),
            |tile, (band, rows)| {
                let first_row = crop.top + band * band_height;
                let last_row = (first_row + rows.len() / width).min(crop.top + height);
                let mut first_column = crop.left;
                while first_column < crop.left + width {
                    let last_column = (first_column + tile_width).min(crop.left + width);
                    develop_tile(frame, tile, first_row, last_row, first_column, last_column);
                    for row in first_row..last_row {
                        let out = &mut rows[(row - first_row) * width..]
                            [first_column - crop.left..last_column - crop.left];
                        for (column, pixel) in out.iter_mut().enumerate() {
                            *pixel = tile.at(row, first_column + column);
                        }
                    }
                    first_column = last_column;
                }
            },
        );
    Color2D::new_with(output, width, height)
}

fn develop_tile<T: Copy + Into<f32>>(
    frame: &BayerFrame<'_, T>,
    tile: &mut Tile,
    first_row: usize,
    last_row: usize,
    first_column: usize,
    last_column: usize,
) {
    let (width, height) = (frame.width, frame.height);
    tile.left = first_column as isize - HALO as isize;
    tile.top = first_row as isize - HALO as isize;
    tile.width = last_column - first_column + 2 * HALO;
    tile.height = last_row - first_row + 2 * HALO;

    // Every plane a photosite does not carry starts at zero, exactly as
    // rawler's whole-image expansion leaves it.
    let rows = tile.top.max(0) as usize..(tile.top + tile.height as isize).min(height as isize) as usize;
    let columns =
        tile.left.max(0) as usize..(tile.left + tile.width as isize).min(width as isize) as usize;
    for row in rows.clone() {
        for column in columns.clone() {
            let index = tile.index(row, column);
            tile.data[index] = [0.0; 3];
            tile.data[index][frame.color_at(row, column)] = frame.sample(row, column);
        }
    }

    // The outermost three pixels of the frame have no room for the gradient
    // sweeps, so they interpolate bilinearly from whatever neighbours exist.
    for row in rows.clone() {
        for column in columns.clone() {
            if interior(row, column, width, height) {
                continue;
            }
            interpolate_border(frame, tile, row, column);
        }
    }

    // Green first: the red and blue sweeps read it one pixel out from the tile.
    for row in rows.clone() {
        for column in columns.clone() {
            if !interior(row, column, width, height)
                || !within(row, column, first_row, last_row, first_column, last_column, 1)
            {
                continue;
            }
            interpolate_green(frame, tile, row, column);
        }
    }

    for row in first_row..last_row {
        for column in first_column..last_column {
            if !interior(row, column, width, height) {
                continue;
            }
            if frame.color_at(row, column) == GREEN {
                interpolate_rb_at_green(frame, tile, row, column);
            } else {
                interpolate_rb_at_non_green(frame, tile, row, column);
            }
        }
    }
}

/// Whether the gradient sweeps have room to run at this coordinate.
#[inline]
fn interior(row: usize, column: usize, width: usize, height: usize) -> bool {
    row >= HALO && column >= HALO && row + HALO < height && column + HALO < width
}

/// Whether a coordinate lies within MARGIN pixels of a tile.
#[allow(clippy::too_many_arguments)]
#[inline]
fn within(
    row: usize,
    column: usize,
    first_row: usize,
    last_row: usize,
    first_column: usize,
    last_column: usize,
    margin: usize,
) -> bool {
    row + margin >= first_row
        && column + margin >= first_column
        && row < last_row + margin
        && column < last_column + margin
}

fn interpolate_border<T: Copy + Into<f32>>(
    frame: &BayerFrame<'_, T>,
    tile: &mut Tile,
    row: usize,
    column: usize,
) {
    let mut sums = [(0.0_f32, 0_usize); 3];
    for y in row.saturating_sub(1)..=row + 1 {
        for x in column.saturating_sub(1)..=column + 1 {
            if y >= frame.height || x >= frame.width || !tile.holds(y, x) {
                continue;
            }
            let channel = frame.color_at(y, x);
            sums[channel].0 += tile.at(y, x)[channel];
            sums[channel].1 += 1;
        }
    }
    let known = frame.color_at(row, column);
    for (channel, (sum, count)) in sums.iter().enumerate() {
        if channel != known && *count > 0 {
            tile.set(row, column, channel, sum / *count as f32);
        }
    }
}

fn interpolate_green<T: Copy + Into<f32>>(
    frame: &BayerFrame<'_, T>,
    tile: &mut Tile,
    row: usize,
    column: usize,
) {
    let channel = frame.color_at(row, column);
    if channel == GREEN {
        return;
    }
    let center = tile.at(row, column)[channel];
    let north = tile.at(row - 1, column)[GREEN];
    let north_far = tile.at(row - 2, column)[channel];
    let east = tile.at(row, column + 1)[GREEN];
    let east_far = tile.at(row, column + 2)[channel];
    let south = tile.at(row + 1, column)[GREEN];
    let south_far = tile.at(row + 2, column)[channel];
    let west = tile.at(row, column - 1)[GREEN];
    let west_far = tile.at(row, column - 2)[channel];

    let vertical = north - south;
    let horizontal = west - east;
    let to_north = (center - north_far).abs() * 2.0 + vertical;
    let to_east = (center - east_far).abs() * 2.0 + horizontal;
    let to_west = (center - west_far).abs() * 2.0 + horizontal;
    let to_south = (center - south_far).abs() * 2.0 + vertical;

    let least = to_north.min(to_east).min(to_west).min(to_south);
    // Ties resolve north, east, west, south, matching rawler's chain of
    // comparisons; the order is arbitrary but it has to be the same order.
    let green = if least == to_north {
        (north * 3.0 + south + center - north_far) / 4.0
    } else if least == to_east {
        (east * 3.0 + west + center - east_far) / 4.0
    } else if least == to_west {
        (west * 3.0 + east + center - west_far) / 4.0
    } else {
        (south * 3.0 + north + center - south_far) / 4.0
    };
    tile.set(row, column, GREEN, green);
}

fn interpolate_rb_at_green<T: Copy + Into<f32>>(
    frame: &BayerFrame<'_, T>,
    tile: &mut Tile,
    row: usize,
    column: usize,
) {
    let horizontal_channel = frame.color_at(row, column + 1);
    let vertical_channel = frame.color_at(row + 1, column);
    let center = tile.at(row, column)[GREEN];
    let west = tile.at(row, column - 1)[GREEN];
    let east = tile.at(row, column + 1)[GREEN];
    let north = tile.at(row - 1, column)[GREEN];
    let south = tile.at(row + 1, column)[GREEN];
    let horizontal_west = tile.at(row, column - 1)[horizontal_channel];
    let horizontal_east = tile.at(row, column + 1)[horizontal_channel];
    let vertical_north = tile.at(row - 1, column)[vertical_channel];
    let vertical_south = tile.at(row + 1, column)[vertical_channel];

    tile.set(
        row,
        column,
        horizontal_channel,
        hue_transit(west, center, east, horizontal_west, horizontal_east),
    );
    tile.set(
        row,
        column,
        vertical_channel,
        hue_transit(north, center, south, vertical_north, vertical_south),
    );
}

fn interpolate_rb_at_non_green<T: Copy + Into<f32>>(
    frame: &BayerFrame<'_, T>,
    tile: &mut Tile,
    row: usize,
    column: usize,
) {
    let own = frame.color_at(row, column);
    let other = if own == RED { BLUE } else { RED };

    let other_north_east = tile.at(row - 1, column + 1)[other];
    let other_south_west = tile.at(row + 1, column - 1)[other];
    let own_north_east = tile.at(row - 2, column + 2)[own];
    let own_center = tile.at(row, column)[own];
    let own_south_west = tile.at(row + 2, column - 2)[own];
    let green_north_east = tile.at(row - 1, column + 1)[GREEN];
    let green_center = tile.at(row, column)[GREEN];
    let green_south_west = tile.at(row + 1, column - 1)[GREEN];
    let other_north_west = tile.at(row - 1, column - 1)[other];
    let other_south_east = tile.at(row + 1, column + 1)[other];
    let own_north_west = tile.at(row - 2, column - 2)[own];
    let own_south_east = tile.at(row + 2, column + 2)[own];
    let green_north_west = tile.at(row - 1, column - 1)[GREEN];
    let green_south_east = tile.at(row + 1, column + 1)[GREEN];

    let north_east = (other_north_east - other_south_west).abs()
        + (own_north_east - own_center).abs()
        + (own_center - own_south_west).abs()
        + (green_north_east - green_center).abs()
        + (green_center - green_south_west).abs();
    // The third term adds where the others subtract. That is what rawler
    // computes, and PPG's own reference does the same, so it stays.
    let north_west = (other_north_west - other_south_east).abs()
        + (own_north_west - own_center).abs()
        + (own_center - own_south_east).abs()
        + (green_north_west + green_center).abs()
        + (green_center - green_south_east).abs();

    let value = if north_east < north_west {
        hue_transit(
            green_north_east,
            green_center,
            green_south_west,
            other_north_east,
            other_south_west,
        )
    } else {
        hue_transit(
            green_north_west,
            green_center,
            green_south_east,
            other_north_west,
            other_south_east,
        )
    };
    tile.set(row, column, other, value);
}

/// Interpolates a colour along a green ramp, or averages when green is not
/// monotone across the three samples.
#[inline]
fn hue_transit(low: f32, middle: f32, high: f32, first: f32, last: f32) -> f32 {
    if (low < middle && middle < high) || (low > middle && middle > high) {
        first + (last - first) * (middle - low) / (high - low)
    } else {
        (first + last) / 2.0 + (middle * 2.0 - low - high) / 4.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A smooth frame with a diagonal edge across it, so both the gradient
    /// branches and the flat case are exercised. Nothing here wraps: a
    /// discontinuity would look exactly like a tiling seam.
    fn frame_data(width: usize, height: usize) -> Vec<u16> {
        (0..width * height)
            .map(|index| {
                let (x, y) = ((index % width) as f32, (index / width) as f32);
                let ramp = 300.0 + x * 1.5 + y * 2.5;
                let edge = if x + y > width as f32 { 600.0 } else { 0.0 };
                let ripple = 40.0 * (x * 0.05).sin() * (y * 0.03).cos();
                (ramp + edge + ripple) as u16
            })
            .collect()
    }

    fn frame<'a>(
        data: &'a [u16],
        width: usize,
        height: usize,
        colors: [usize; 4],
    ) -> BayerFrame<'a, u16> {
        BayerFrame {
            data,
            stride: width,
            left: 0,
            top: 0,
            width,
            height,
            colors,
            levels: [(0.0, 1024.0); 4],
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

    fn develop(width: usize, height: usize, colors: [usize; 4]) -> Color2D<f32, 3> {
        let data = frame_data(width, height);
        demosaic_ppg(&frame(&data, width, height, colors), whole(width, height))
    }

    #[test]
    fn a_flat_frame_develops_to_one_flat_colour() {
        let (width, height) = (64, 48);
        let data = vec![512_u16; width * height];
        let developed = demosaic_ppg(
            &frame(&data, width, height, [RED, GREEN, GREEN, BLUE]),
            whole(width, height),
        );
        for pixel in developed.data.iter() {
            for channel in pixel {
                assert!(
                    (channel - 0.5).abs() < 1.0e-6,
                    "flat frame produced {channel}"
                );
            }
        }
    }

    #[test]
    fn the_tile_geometry_does_not_change_the_result() {
        // The whole point of tiling is that it is invisible. Developing the
        // same frame with tiles of different shapes — including one large
        // enough to hold it whole — has to give the same pixels, or a seam is
        // being drawn somewhere.
        let (width, height) = (TILE * 2 + 37, BAND + 21);
        let data = frame_data(width, height);
        let source = frame(&data, width, height, [RED, GREEN, GREEN, BLUE]);
        let reference = demosaic_ppg_tiled(&source, whole(width, height), width, height);
        for (tile_width, band_height) in [(TILE, BAND), (16, 8), (37, 64), (width, 3)] {
            let tiled =
                demosaic_ppg_tiled(&source, whole(width, height), tile_width, band_height);
            for (index, (tiled, reference)) in
                tiled.data.iter().zip(reference.data.iter()).enumerate()
            {
                for (channel, (tiled, reference)) in tiled.iter().zip(reference).enumerate() {
                    assert!(
                        (tiled - reference).abs() < 1.0e-6,
                        "tiles {tile_width}x{band_height} differ at pixel {index} channel {channel}: {tiled} against {reference}"
                    );
                }
            }
        }
    }

    #[test]
    fn a_cropped_develop_matches_the_same_part_of_a_whole_one() {
        // The crop decides what is emitted, not how it is interpolated, so
        // every emitted pixel must equal the whole-frame develop exactly —
        // including the ones a border pixel of the crop would otherwise reach.
        let (width, height) = (TILE + 60, BAND + 40);
        let data = frame_data(width, height);
        let source = frame(&data, width, height, [RED, GREEN, GREEN, BLUE]);
        let developed = demosaic_ppg(&source, whole(width, height));
        let crop = Window {
            left: 8,
            top: 6,
            width: width - 30,
            height: height - 20,
        };
        let cropped = demosaic_ppg(&source, crop);
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
    fn every_colour_filter_phase_develops_its_own_channels() {
        // The corner a photosite sits on decides which plane keeps its own
        // value, so a red-first frame and a green-first frame of the same data
        // must differ, and neither may leave a plane empty.
        let (width, height) = (96, 72);
        for colors in [
            [RED, GREEN, GREEN, BLUE],
            [GREEN, RED, BLUE, GREEN],
            [GREEN, BLUE, RED, GREEN],
            [BLUE, GREEN, GREEN, RED],
        ] {
            let developed = develop(width, height, colors);
            for channel in 0..3 {
                let spread = developed
                    .data
                    .iter()
                    .map(|pixel| pixel[channel])
                    .fold(f32::MIN, f32::max)
                    - developed
                        .data
                        .iter()
                        .map(|pixel| pixel[channel])
                        .fold(f32::MAX, f32::min);
                assert!(spread > 0.05, "channel {channel} is flat for {colors:?}");
            }
        }
    }

    #[test]
    fn matches_rawlers_own_ppg_sweeps() {
        use rawler::cfa::{CFA, PlaneColor};
        use rawler::imgop::sensor::bayer::Demosaic as _;
        use rawler::imgop::sensor::bayer::ppg::PPGDemosaic;
        use rawler::imgop::{Dim2, Point, Rect};
        use rawler::pixarray::PixF32;

        let (width, height) = (200, 140);
        // Photon noise, deterministically: a smooth frame never reaches the
        // branches where two gradients tie or a hue transit divides by almost
        // nothing, which is exactly where two implementations can part ways.
        let values: Vec<f32> = frame_data(width, height)
            .iter()
            .enumerate()
            .map(|(index, value)| {
                let noise = (index as u32).wrapping_mul(2_654_435_761) >> 20;
                (*value as f32 + (noise % 64) as f32 - 32.0) / 1024.0
            })
            .collect();
        let pixels = PixF32::new_with(values.clone(), width, height);
        let colors = PlaneColor::new("RGB");
        for pattern in ["RGGB", "BGGR", "GRBG", "GBRG"] {
        let cfa = CFA::new(pattern);
        let theirs = PPGDemosaic::new().demosaic(
            &pixels,
            &cfa,
            &colors,
            Rect::new(Point::new(0, 0), Dim2::new(width, height)),
        );
        let mine_colors = [
            cfa.color_at(0, 0),
            cfa.color_at(0, 1),
            cfa.color_at(1, 0),
            cfa.color_at(1, 1),
        ];
        // The same develop, emitting only an inner window, which is how a
        // camera's default crop reaches it.
        let window = Window {
            left: 10,
            top: 10,
            width: width - 32,
            height: height - 20,
        };
        let mine = demosaic_ppg(
            &BayerFrame {
                data: &values,
                stride: width,
                left: 0,
                top: 0,
                width,
                height,
                colors: mine_colors,
                levels: [(0.0, 1.0); 4],
            },
            window,
        );
        let mut worst = (0.0_f32, 0, 0);
        for row in 0..window.height {
            for column in 0..window.width {
                let a = theirs.data[(row + window.top) * width + column + window.left];
                let b = mine.data[row * window.width + column];
                for channel in 0..3 {
                    let difference = (a[channel] - b[channel]).abs();
                    if difference > worst.0 {
                        worst = (difference, row, column);
                    }
                }
            }
        }
        assert!(
            worst.0 < 1.0e-6,
            "{pattern}: worst difference {} at row {} column {}",
            worst.0,
            worst.1,
            worst.2
        );
        }
    }
}
