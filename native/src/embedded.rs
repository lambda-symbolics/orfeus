//! The camera's own JPEG, lifted out of a RAW container without developing
//! anything: the filmstrip's first thumbnail and the file picker's previews.
//!
//! Every Olympus body writes a 3200-pixel JPEG of the frame into the ORF
//! beside the sensor data, and the 80 MP composites carry the whole ORF, JPEG
//! included. Finding it by its own markers rather than through the maker
//! notes means one search serves every container the camera makes; the
//! maker notes are only consulted for the orientation, which a preview JPEG
//! does not carry itself.

use std::fs::File;
use std::io::Read;
use std::path::Path;

use image::ImageFormat;

use super::Error;
use super::jpeg;
use super::render::{self, RgbImage};

/// How far into a container the camera's preview is looked for first. An
/// ORF keeps it in the first two megabytes, the composites too, and a card
/// reader hands over four megabytes in the time a whole frame would take
/// twenty; only when nothing turns up is the rest of the file searched.
const PREVIEW_SEARCH_BYTES: usize = 4 << 20;
/// A JPEG narrower than this is the 160-pixel thumbnail, not the picture.
const MINIMUM_PREVIEW_EDGE: u32 = 640;
/// The TIFF tag that says which way up the camera was held.
const TIFF_ORIENTATION_TAG: u16 = 0x0112;

/// Reads the orientation out of a TIFF-shaped container's first directory —
/// ORF, DNG and TIFF alike — or nothing when the bytes are not one. Olympus
/// writes its own magic after the byte order, so only the byte order is
/// checked; the directory is where every TIFF puts it.
pub(crate) fn tiff_orientation(bytes: &[u8]) -> Option<u16> {
    if bytes.len() < 8 {
        return None;
    }
    let little = match &bytes[0..2] {
        b"II" => true,
        b"MM" => false,
        _ => return None,
    };
    let read16 = |at: usize| -> Option<u16> {
        let pair = bytes.get(at..at + 2)?;
        Some(if little {
            u16::from_le_bytes([pair[0], pair[1]])
        } else {
            u16::from_be_bytes([pair[0], pair[1]])
        })
    };
    let read32 = |at: usize| -> Option<u32> {
        let quad = bytes.get(at..at + 4)?;
        Some(if little {
            u32::from_le_bytes([quad[0], quad[1], quad[2], quad[3]])
        } else {
            u32::from_be_bytes([quad[0], quad[1], quad[2], quad[3]])
        })
    };
    let directory = read32(4)? as usize;
    let entries = read16(directory)? as usize;
    for index in 0..entries.min(512) {
        let entry = directory + 2 + index * 12;
        if read16(entry)? == TIFF_ORIENTATION_TAG {
            // A SHORT's value sits in the first two bytes of the value field.
            let value = read16(entry + 8)?;
            return (1..=8).contains(&value).then_some(value);
        }
    }
    None
}

/// One JPEG stream found inside a container, by byte range and size.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct EmbeddedJpeg {
    pub(crate) start: usize,
    /// One past the end-of-image marker.
    pub(crate) end: usize,
    pub(crate) width: u32,
    pub(crate) height: u32,
}

/// Walks the marker segments of a JPEG that starts at START and returns its
/// extent and dimensions, or nothing when the bytes there are not a whole
/// JPEG. Segments are skipped by their declared length, so a thumbnail nested
/// inside an APP1 segment is passed over rather than mistaken for the end.
fn jpeg_at(bytes: &[u8], start: usize) -> Option<EmbeddedJpeg> {
    let mut position = start + 2;
    let mut dimensions = None;
    loop {
        if position + 4 > bytes.len() || bytes[position] != 0xFF {
            return None;
        }
        let marker = bytes[position + 1];
        match marker {
            // Padding and standalone markers carry no length.
            0xFF => {
                position += 1;
                continue;
            }
            0x01 | 0xD0..=0xD7 => {
                position += 2;
                continue;
            }
            _ => {}
        }
        let length = usize::from(u16::from_be_bytes([bytes[position + 2], bytes[position + 3]]));
        if length < 2 || position + 2 + length > bytes.len() {
            return None;
        }
        match marker {
            // Baseline, extended and progressive frame headers.
            0xC0 | 0xC1 | 0xC2 => {
                let header = &bytes[position + 4..position + 2 + length];
                if header.len() < 5 {
                    return None;
                }
                let height = u32::from(u16::from_be_bytes([header[1], header[2]]));
                let width = u32::from(u16::from_be_bytes([header[3], header[4]]));
                dimensions = Some((width, height));
            }
            // Start of scan: entropy-coded data follows until the end of
            // image. Inside it every 0xFF is either stuffed (0xFF 0x00) or a
            // restart marker, so the first other marker ends the picture.
            0xDA => {
                let (width, height) = dimensions?;
                let mut scan = position + 2 + length;
                while scan + 1 < bytes.len() {
                    if bytes[scan] == 0xFF {
                        let next = bytes[scan + 1];
                        if next == 0xD9 {
                            return Some(EmbeddedJpeg {
                                start,
                                end: scan + 2,
                                width,
                                height,
                            });
                        }
                        if next != 0x00 && !(0xD0..=0xD7).contains(&next) && next != 0xFF {
                            // Another marker before the end of image: a
                            // progressive scan boundary. Keep walking; the
                            // end of image is still ahead.
                        }
                    }
                    scan += 1;
                }
                return None;
            }
            _ => {}
        }
        position += 2 + length;
    }
}

/// Finds every JPEG in BYTES by its markers and returns the one with the most
/// pixels, or nothing when there is no whole JPEG of a useful size.
pub(crate) fn largest_embedded_jpeg(bytes: &[u8]) -> Option<EmbeddedJpeg> {
    let mut best: Option<EmbeddedJpeg> = None;
    let mut position = 0;
    while position + 3 <= bytes.len() {
        let Some(offset) = bytes[position..]
            .iter()
            .position(|&byte| byte == 0xFF)
        else {
            break;
        };
        let start = position + offset;
        if start + 3 > bytes.len() {
            break;
        }
        if bytes[start + 1] == 0xD8 && bytes[start + 2] == 0xFF {
            if let Some(found) = jpeg_at(bytes, start) {
                if found.width.max(found.height) >= MINIMUM_PREVIEW_EDGE
                    && best.is_none_or(|current| {
                        u64::from(found.width) * u64::from(found.height)
                            > u64::from(current.width) * u64::from(current.height)
                    })
                {
                    best = Some(found);
                }
                // Nothing inside a whole JPEG is a second preview.
                position = found.end;
                continue;
            }
        }
        position = start + 1;
    }
    best
}

/// Decodes the JPEG in BYTES, bounds it to MAX_EDGE pixels on its longer
/// side by a box filter, and turns it the way ORIENTATION says the camera
/// was held, so it matches what a develop of the same file shows.
pub(crate) fn thumbnail_from_jpeg(
    bytes: &[u8],
    max_edge: u32,
    orientation: u16,
) -> Result<RgbImage, Error> {
    let decoded = image::load_from_memory_with_format(bytes, ImageFormat::Jpeg)
        .map_err(|e| Error::Render(format!("embedded preview: {e}")))?
        .into_rgb8();
    let (width, height) = decoded.dimensions();
    let longest = width.max(height).max(1);
    let scaled = if max_edge > 0 && longest > max_edge {
        let scale = f64::from(max_edge) / f64::from(longest);
        let new_width = ((f64::from(width) * scale).round() as u32).max(1);
        let new_height = ((f64::from(height) * scale).round() as u32).max(1);
        image::imageops::thumbnail(&decoded, new_width, new_height)
    } else {
        decoded
    };
    let (width, height) = scaled.dimensions();
    let data: Vec<f32> = scaled
        .as_raw()
        .iter()
        .map(|&value| f32::from(value) / 255.0)
        .collect();
    let image = RgbImage {
        width: width as usize,
        height: height as usize,
        data,
    };
    Ok(render::orient(image, orientation))
}

/// Encodes IMAGE, display-referred in 0..1, as a JPEG of QUALITY.
fn encode_thumbnail(image: &RgbImage, quality: u8) -> Result<Vec<u8>, Error> {
    let rgb: Vec<u8> = image
        .data
        .iter()
        .map(|&value| (value.clamp(0.0, 1.0) * 255.0).round() as u8)
        .collect();
    jpeg::encode_rgb(&rgb, image.width, image.height, quality, &[]).map_err(Error::Render)
}

/// Writes the camera's preview of INPUT to OUTPUT as a JPEG no longer than
/// MAX_EDGE pixels on a side, oriented like a develop of INPUT would be.
/// Returns the written width and height.
///
/// No sensor data is decoded: the file is read, its maker notes parsed for
/// the orientation, and one JPEG decoded and box-filtered down, which is a
/// few tens of milliseconds where a develop is a second.
pub(crate) fn embedded_preview_file(
    input: &Path,
    output: &Path,
    max_edge: u32,
    quality: u8,
) -> Result<(u32, u32), Error> {
    // Only the head of the file is read: the preview sits in the first two
    // megabytes of an ORF, and a card of five hundred frames is ten
    // gigabytes that a picker should not have to pull through the reader.
    let mut file = File::open(input)
        .map_err(|e| Error::Render(format!("opening {}: {e}", input.display())))?;
    let mut bytes = Vec::with_capacity(PREVIEW_SEARCH_BYTES.min(4 << 20));
    file.by_ref()
        .take(PREVIEW_SEARCH_BYTES as u64)
        .read_to_end(&mut bytes)
        .map_err(|e| Error::Render(format!("reading {}: {e}", input.display())))?;
    let mut range = largest_embedded_jpeg(&bytes);
    if range.is_none() {
        // Nothing near the front: read the rest and look everywhere.
        file.read_to_end(&mut bytes)
            .map_err(|e| Error::Render(format!("reading {}: {e}", input.display())))?;
        range = largest_embedded_jpeg(&bytes);
    }
    let range = range.ok_or_else(|| {
        Error::Render("the RAW file carries no embedded JPEG preview".to_string())
    })?;
    // The orientation is the container's, not the JPEG's; a container that
    // is not TIFF-shaped gets its picture unturned.
    let orientation = tiff_orientation(&bytes).unwrap_or(1);
    let image = thumbnail_from_jpeg(&bytes[range.start..range.end], max_edge, orientation)?;
    let encoded = encode_thumbnail(&image, quality)?;
    std::fs::write(output, encoded)
        .map_err(|e| Error::Render(format!("writing {}: {e}", output.display())))?;
    Ok((image.width as u32, image.height as u32))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A flat-coloured JPEG of the given size, as a camera might embed one.
    fn flat_jpeg(width: usize, height: usize, shade: u8) -> Vec<u8> {
        let rgb = vec![shade; width * height * 3];
        jpeg::encode_rgb(&rgb, width, height, 80, &[]).expect("test jpeg")
    }

    /// A JPEG carrying an APP1 segment with a whole smaller JPEG inside it,
    /// the way an EXIF thumbnail rides inside a preview.
    fn jpeg_with_nested_thumbnail(width: usize, height: usize) -> Vec<u8> {
        let inner = flat_jpeg(16, 16, 40);
        let payload = inner;
        let outer = flat_jpeg(width, height, 200);
        // Splice the APP1 segment right after the start-of-image marker.
        let mut spliced = Vec::with_capacity(outer.len() + payload.len() + 4);
        spliced.extend_from_slice(&outer[..2]);
        spliced.extend_from_slice(&[0xFF, 0xE1]);
        let length = u16::try_from(payload.len() + 2).expect("segment length");
        spliced.extend_from_slice(&length.to_be_bytes());
        spliced.extend_from_slice(&payload);
        spliced.extend_from_slice(&outer[2..]);
        spliced
    }

    /// The largest JPEG in a container is the preview, however many smaller
    /// ones sit around and inside it, and its extent is exact.
    #[test]
    fn the_largest_jpeg_in_a_container_is_the_preview() {
        let thumbnail = flat_jpeg(160, 120, 90);
        let preview = jpeg_with_nested_thumbnail(800, 600);
        let mut container = vec![0x49, 0x49, 0x52, 0x4F, 0x08, 0x00, 0x00, 0x00];
        container.extend(std::iter::repeat_n(0xFF, 300)); // a run of padding bytes
        container.extend_from_slice(&thumbnail);
        container.extend(std::iter::repeat_n(0x00, 1000));
        let preview_start = container.len();
        container.extend_from_slice(&preview);
        container.extend(std::iter::repeat_n(0xAB, 5000));
        let found = largest_embedded_jpeg(&container).expect("a preview");
        assert_eq!(found.start, preview_start);
        assert_eq!(found.end, preview_start + preview.len());
        assert_eq!((found.width, found.height), (800, 600));
        // The slice decodes on its own, nested thumbnail and all.
        let image = thumbnail_from_jpeg(&container[found.start..found.end], 320, 1).expect("decode");
        assert_eq!((image.width, image.height), (320, 240));
    }

    /// Without a JPEG of picture size there is no preview: a 160-pixel
    /// thumbnail alone does not count, and neither does noise.
    #[test]
    fn a_thumbnail_alone_is_no_preview() {
        let mut container = vec![0u8; 4096];
        container[100] = 0xFF;
        container[101] = 0xD8;
        container[102] = 0xFF;
        assert!(largest_embedded_jpeg(&container).is_none());
        let mut with_thumbnail = container.clone();
        with_thumbnail.extend_from_slice(&flat_jpeg(160, 120, 90));
        assert!(largest_embedded_jpeg(&with_thumbnail).is_none());
    }

    /// The orientation is read straight out of the container's first
    /// directory, whichever byte order it uses, and Olympus's own magic
    /// number after the byte order does not get in the way.
    #[test]
    fn the_orientation_is_read_from_the_container() {
        // Little-endian, ORF magic "RO", directory at 8 with two entries.
        let mut orf = vec![b'I', b'I', b'R', b'O', 8, 0, 0, 0];
        orf.extend_from_slice(&2u16.to_le_bytes());
        // Tag 0x010f Make, ASCII, count 3, value offset (ignored).
        orf.extend_from_slice(&0x010fu16.to_le_bytes());
        orf.extend_from_slice(&2u16.to_le_bytes());
        orf.extend_from_slice(&3u32.to_le_bytes());
        orf.extend_from_slice(&100u32.to_le_bytes());
        // Tag 0x0112 Orientation, SHORT, count 1, value 8.
        orf.extend_from_slice(&0x0112u16.to_le_bytes());
        orf.extend_from_slice(&3u16.to_le_bytes());
        orf.extend_from_slice(&1u32.to_le_bytes());
        orf.extend_from_slice(&8u16.to_le_bytes());
        orf.extend_from_slice(&0u16.to_le_bytes());
        assert_eq!(tiff_orientation(&orf), Some(8));
        // Big-endian TIFF with the orientation as its only entry.
        let mut tiff = vec![b'M', b'M', 0, 42, 0, 0, 0, 8];
        tiff.extend_from_slice(&1u16.to_be_bytes());
        tiff.extend_from_slice(&0x0112u16.to_be_bytes());
        tiff.extend_from_slice(&3u16.to_be_bytes());
        tiff.extend_from_slice(&1u32.to_be_bytes());
        tiff.extend_from_slice(&6u16.to_be_bytes());
        tiff.extend_from_slice(&0u16.to_be_bytes());
        assert_eq!(tiff_orientation(&tiff), Some(6));
        assert_eq!(tiff_orientation(b"\xff\xd8\xff\xe0 not a tiff"), None);
    }

    /// A portrait frame's preview is stored the way the sensor saw it and
    /// turned on the way out, so the thumbnail stands the way the develop does.
    #[test]
    fn the_thumbnail_is_turned_the_way_the_camera_was_held() {
        let preview = flat_jpeg(640, 480, 128);
        let upright = thumbnail_from_jpeg(&preview, 320, 1).expect("decode");
        assert_eq!((upright.width, upright.height), (320, 240));
        let turned = thumbnail_from_jpeg(&preview, 320, 6).expect("decode");
        assert_eq!((turned.width, turned.height), (240, 320));
    }
}
