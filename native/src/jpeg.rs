//! Baseline JPEG encoding on every core at once.
//!
//! A JPEG's entropy-coded data is one long stream with a running DC predictor,
//! which is why encoders are serial: at twenty megapixels the encode was 320
//! milliseconds, the second largest stage of an export, on one core while the
//! other forty-seven waited. The format itself has the way out. A *restart
//! interval* resets the predictor every so many MCUs and marks the spot, so a
//! decoder can pick up from any marker — and an encoder can therefore produce
//! each interval on its own, from nothing but its own rows.
//!
//! So the frame is cut into stripes of whole MCU rows, each stripe is encoded as
//! a complete JPEG on its own thread with the same quantisation and Huffman
//! tables as every other, and their scans are spliced: one set of headers, the
//! frame height patched in, a restart-interval marker declaring the stripe
//! length, then each stripe's entropy-coded bytes with a restart marker between.
//! The result decodes pixel for pixel to what a single encode would have made,
//! because nothing about a coefficient depends on the predictor — only the
//! bits spent writing it. `STRIPED_ENCODE_DECODES_TO_THE_PLAIN_ONE` holds that
//! to zero difference.

use jpeg_encoder::{ColorType, Encoder, SamplingFactor};
use rayon::prelude::*;

/// An MCU is sixteen rows tall with 4:2:0 chroma, so stripes are cut there.
const MCU_ROWS: usize = 16;

/// Marker bytes the splice looks for.
const SOF0: u8 = 0xC0;
const DRI: u8 = 0xDD;
const SOS: u8 = 0xDA;
const EOI: u8 = 0xD9;
const RST0: u8 = 0xD0;

/// Encodes interleaved 8-bit RGB as a baseline 4:2:0 JPEG, striped across the
/// thread pool. APP_SEGMENTS are written once, in the header.
pub fn encode_rgb(
    rgb: &[u8],
    width: usize,
    height: usize,
    quality: u8,
    app_segments: &[(u8, Vec<u8>)],
) -> Result<Vec<u8>, String> {
    if width == 0 || height == 0 || rgb.len() != width * height * 3 {
        return Err("JPEG encode: buffer does not match its dimensions".into());
    }
    let width16 = u16::try_from(width).map_err(|_| "JPEG output is wider than 65535 pixels")?;
    let height16 = u16::try_from(height).map_err(|_| "JPEG output is taller than 65535 pixels")?;
    let mcus_per_row = width.div_ceil(MCU_ROWS);
    let mcu_rows = height.div_ceil(MCU_ROWS);
    let stripe_mcu_rows = stripe_rows(mcus_per_row, mcu_rows, rayon::current_num_threads());
    let stripes = mcu_rows.div_ceil(stripe_mcu_rows);
    let encode_stripe = |first_row: usize, rows: usize, segments: &[(u8, Vec<u8>)]| {
        let mut out = Vec::with_capacity(rows * width / 2);
        let mut encoder = Encoder::new(&mut out, quality);
        encoder.set_sampling_factor(SamplingFactor::F_2_2);
        for (kind, payload) in segments {
            encoder
                .add_app_segment(*kind, payload.clone())
                .map_err(|e| format!("JPEG application segment: {e}"))?;
        }
        encoder
            .encode(
                &rgb[first_row * width * 3..(first_row + rows) * width * 3],
                width16,
                rows as u16,
                ColorType::Rgb,
            )
            .map_err(|e| format!("JPEG encoding failed: {e}"))?;
        Ok::<Vec<u8>, String>(out)
    };
    if stripes <= 1 {
        return encode_stripe(0, height, app_segments);
    }
    let stripe_height = stripe_mcu_rows * MCU_ROWS;
    let encoded: Vec<Vec<u8>> = (0..stripes)
        .into_par_iter()
        .map(|stripe| {
            let first_row = stripe * stripe_height;
            let rows = stripe_height.min(height - first_row);
            encode_stripe(first_row, rows, if stripe == 0 { app_segments } else { &[] })
        })
        .collect::<Result<_, _>>()?;
    let interval = u16::try_from(mcus_per_row * stripe_mcu_rows)
        .map_err(|_| "JPEG restart interval overflowed")?;
    splice(&encoded, height16, interval)
}

/// How many MCU rows each stripe gets: enough stripes to keep every thread
/// busy twice over, a restart interval that fits its two bytes, and never fewer
/// than one row.
fn stripe_rows(mcus_per_row: usize, mcu_rows: usize, threads: usize) -> usize {
    let wanted = mcu_rows.div_ceil((2 * threads).max(1)).max(1);
    let largest = (u16::MAX as usize / mcus_per_row.max(1)).max(1);
    wanted.min(largest)
}

/// Where a marker's segment starts and how long it is, or none when the marker
/// is absent before the scan begins.
fn find_marker(bytes: &[u8], marker: u8) -> Option<(usize, usize)> {
    let mut at = 2; // past SOI
    while at + 4 <= bytes.len() {
        if bytes[at] != 0xFF {
            return None;
        }
        let kind = bytes[at + 1];
        let length = usize::from(u16::from_be_bytes([bytes[at + 2], bytes[at + 3]]));
        if kind == marker {
            return Some((at, 2 + length));
        }
        if kind == SOS {
            return None;
        }
        at += 2 + length;
    }
    None
}

/// The entropy-coded bytes of a JPEG with no restart markers of its own: from
/// the end of the scan header to the end-of-image marker.
fn scan_data(bytes: &[u8]) -> Result<&[u8], String> {
    let (sos, sos_length) = find_marker(bytes, SOS).ok_or("JPEG stripe has no scan")?;
    let start = sos + sos_length;
    let mut at = start;
    while at + 1 < bytes.len() {
        if bytes[at] == 0xFF && bytes[at + 1] == EOI {
            return Ok(&bytes[start..at]);
        }
        at += 1;
    }
    Err("JPEG stripe has no end-of-image marker".into())
}

/// Joins independently encoded stripes into one JPEG.
fn splice(stripes: &[Vec<u8>], height: u16, interval: u16) -> Result<Vec<u8>, String> {
    let first = &stripes[0];
    let (sof, _) = find_marker(first, SOF0).ok_or("JPEG stripe has no frame header")?;
    let (sos, sos_length) = find_marker(first, SOS).ok_or("JPEG stripe has no scan")?;
    let total: usize = stripes.iter().map(Vec::len).sum();
    let mut out = Vec::with_capacity(total + 16);
    out.extend_from_slice(&first[..sos]);
    // The frame header says how tall the first stripe was; say how tall the
    // frame is. Layout: marker, length, precision, height, width.
    out[sof + 5..sof + 7].copy_from_slice(&height.to_be_bytes());
    out.extend_from_slice(&[0xFF, DRI, 0x00, 0x04]);
    out.extend_from_slice(&interval.to_be_bytes());
    out.extend_from_slice(&first[sos..sos + sos_length]);
    for (index, stripe) in stripes.iter().enumerate() {
        if index > 0 {
            out.extend_from_slice(&[0xFF, RST0 + ((index - 1) % 8) as u8]);
        }
        out.extend_from_slice(scan_data(stripe)?);
    }
    out.extend_from_slice(&[0xFF, EOI]);
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_image(width: usize, height: usize) -> Vec<u8> {
        (0..width * height)
            .flat_map(|index| {
                let (x, y) = (index % width, index / width);
                let hash = crate::render::splitmix64(index as u64);
                [
                    ((x * 255) / width.max(1)) as u8 ^ (hash & 7) as u8,
                    ((y * 255) / height.max(1)) as u8,
                    (((x + y) * 3) % 256) as u8 ^ ((hash >> 8) & 15) as u8,
                ]
            })
            .collect()
    }

    fn decode(bytes: &[u8]) -> (u32, u32, Vec<u8>) {
        let image = image::load_from_memory_with_format(bytes, image::ImageFormat::Jpeg)
            .expect("stitched JPEG decodes")
            .into_rgb8();
        (image.width(), image.height(), image.into_raw())
    }

    fn plain(rgb: &[u8], width: usize, height: usize, quality: u8) -> Vec<u8> {
        let mut out = Vec::new();
        let mut encoder = Encoder::new(&mut out, quality);
        encoder.set_sampling_factor(SamplingFactor::F_2_2);
        encoder
            .encode(rgb, width as u16, height as u16, ColorType::Rgb)
            .unwrap();
        out
    }

    /// The whole point: a decoder sees the same picture from the spliced file
    /// as from one encoded in a single stream, including a last stripe that is
    /// not a whole number of MCU rows.
    #[test]
    fn striped_encode_decodes_to_the_plain_one() {
        for (width, height) in [(200, 150), (97, 333), (64, 16), (33, 17)] {
            let rgb = test_image(width, height);
            let striped = encode_rgb(&rgb, width, height, 90, &[]).unwrap();
            let (w, h, pixels) = decode(&striped);
            assert_eq!((w as usize, h as usize), (width, height), "{width}x{height}");
            let (_, _, expected) = decode(&plain(&rgb, width, height, 90));
            assert_eq!(pixels, expected, "{width}x{height} decoded differently");
        }
    }

    /// A restart marker between every stripe and a restart interval that says
    /// how long a stripe is, and the height of the whole frame in the header.
    #[test]
    fn spliced_file_declares_its_stripes() {
        let (width, height) = (160, 100);
        let rgb = test_image(width, height);
        let striped = encode_rgb(&rgb, width, height, 85, &[(2, b"ICC_PROFILE\0test".to_vec())]).unwrap();
        let (dri, _) = find_marker(&striped, DRI).expect("restart interval declared");
        let interval = u16::from_be_bytes([striped[dri + 4], striped[dri + 5]]) as usize;
        let stripes = height.div_ceil(MCU_ROWS).div_ceil(stripe_rows(10, 7, rayon::current_num_threads()));
        let restarts = striped
            .windows(2)
            .filter(|pair| pair[0] == 0xFF && (RST0..RST0 + 8).contains(&pair[1]))
            .count();
        assert_eq!(restarts, stripes - 1);
        assert_eq!(interval % 10, 0, "interval covers whole MCU rows");
        let (sof, _) = find_marker(&striped, SOF0).unwrap();
        assert_eq!(u16::from_be_bytes([striped[sof + 5], striped[sof + 6]]), 100);
        assert!(find_marker(&striped, 0xE2).is_some(), "the application segment survives");
    }

    #[test]
    fn a_frame_too_small_to_stripe_is_encoded_whole() {
        let rgb = test_image(20, 12);
        let striped = encode_rgb(&rgb, 20, 12, 90, &[]).unwrap();
        assert!(find_marker(&striped, DRI).is_none());
        assert_eq!(decode(&striped).2, decode(&plain(&rgb, 20, 12, 90)).2);
    }
}
