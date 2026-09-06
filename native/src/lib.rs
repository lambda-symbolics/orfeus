//! Native acceleration boundary for Orfeus.

/// Swap the system allocator for mimalloc with `--features mimalloc-allocator`.
///
/// Left off by default: measured on a 13700H it moved a full-resolution export
/// by less than the run-to-run variance, and the render's allocations are a
/// handful of whole-image buffers rather than the many small ones a faster
/// allocator would help with.
#[cfg(feature = "mimalloc-allocator")]
#[global_allocator]
static ALLOCATOR: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// The same question asked of rpmalloc, with `--features rpmalloc-allocator`.
#[cfg(all(feature = "rpmalloc-allocator", not(feature = "mimalloc-allocator")))]
#[global_allocator]
static ALLOCATOR: rpmalloc::RpMalloc = rpmalloc::RpMalloc;

use std::collections::HashSet;
use std::ffi::{CStr, c_char};
use std::fmt;
use std::fs;
use std::io::Read;
use std::os::unix::ffi::OsStrExt;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::Path;
use std::ptr;

use flate2::read::ZlibDecoder;
use rayon::prelude::*;
use md5::{Digest, Md5};

mod analyze;
mod demosaic;
mod embedded;
mod focus;
mod color;
mod gpu;
mod graphex;
mod jpeg;
mod nn;
mod render;
mod tone;

const TAG_ORIGINAL_RAW_FILE_NAME: u16 = 0xc68b;
const TAG_ORIGINAL_RAW_FILE_DATA: u16 = 0xc68c;
const TAG_ORIGINAL_RAW_FILE_DIGEST: u16 = 0xc71d;

/// The operation completed successfully.
pub const ORFEUS_STATUS_OK: i32 = 0;
/// A pointer, path, or buffer supplied by the caller was invalid.
pub const ORFEUS_STATUS_INVALID_ARGUMENT: i32 = 1;
/// Reading or writing a file failed.
pub const ORFEUS_STATUS_IO_ERROR: i32 = 2;
/// The DNG or embedded-original container was malformed.
pub const ORFEUS_STATUS_INVALID_DNG: i32 = 3;
/// The DNG does not contain all required embedded-original tags.
pub const ORFEUS_STATUS_NO_ORIGINAL: i32 = 4;
/// A compressed embedded-original block could not be decoded.
pub const ORFEUS_STATUS_DECOMPRESSION_ERROR: i32 = 5;
/// OriginalRawFileData did not match OriginalRawFileDigest.
pub const ORFEUS_STATUS_DIGEST_MISMATCH: i32 = 6;
/// A caller-provided result buffer was too small.
pub const ORFEUS_STATUS_BUFFER_TOO_SMALL: i32 = 7;
/// RAW decoding, processing, or encoding failed.
pub const ORFEUS_STATUS_RENDER_ERROR: i32 = 8;
/// Lens correction was requested but no conservative matching calibration exists.
pub const ORFEUS_STATUS_LENS_PROFILE_UNAVAILABLE: i32 = 9;
/// A Rust panic was contained at the ABI boundary.
pub const ORFEUS_STATUS_PANIC: i32 = 255;

#[derive(Debug)]
enum Error {
    InvalidArgument(&'static str),
    Io(std::io::Error),
    InvalidDng(&'static str),
    MissingOriginal(&'static str),
    Decompression(&'static str),
    DigestMismatch,
    BufferTooSmall { needed: usize },
    Color(color::ColorError),
    Render(String),
    LensProfileUnavailable(String),
}

impl Error {
    fn status(&self) -> i32 {
        match self {
            Self::InvalidArgument(_) => ORFEUS_STATUS_INVALID_ARGUMENT,
            Self::Io(_) => ORFEUS_STATUS_IO_ERROR,
            Self::InvalidDng(_) => ORFEUS_STATUS_INVALID_DNG,
            Self::MissingOriginal(_) => ORFEUS_STATUS_NO_ORIGINAL,
            Self::Decompression(_) => ORFEUS_STATUS_DECOMPRESSION_ERROR,
            Self::DigestMismatch => ORFEUS_STATUS_DIGEST_MISMATCH,
            Self::BufferTooSmall { .. } => ORFEUS_STATUS_BUFFER_TOO_SMALL,
            Self::Color(_) | Self::Render(_) => ORFEUS_STATUS_RENDER_ERROR,
            Self::LensProfileUnavailable(_) => ORFEUS_STATUS_LENS_PROFILE_UNAVAILABLE,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidArgument(message)
            | Self::InvalidDng(message)
            | Self::MissingOriginal(message)
            | Self::Decompression(message) => formatter.write_str(message),
            Self::Io(error) => write!(formatter, "I/O error: {error}"),
            Self::DigestMismatch => {
                formatter.write_str("OriginalRawFileDigest does not match OriginalRawFileData")
            }
            Self::BufferTooSmall { needed } => {
                write!(
                    formatter,
                    "result buffer too small (requires {needed} bytes)"
                )
            }
            Self::Color(error) => write!(formatter, "RAW color conversion: {error}"),
            Self::Render(message) | Self::LensProfileUnavailable(message) => {
                formatter.write_str(message)
            }
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Self::Io(error)
    }
}

#[derive(Clone, Copy)]
enum ByteOrder {
    Little,
    Big,
}

impl ByteOrder {
    fn u16(self, bytes: &[u8]) -> Result<u16, Error> {
        let bytes: [u8; 2] = bytes
            .try_into()
            .map_err(|_| Error::InvalidDng("truncated TIFF u16"))?;
        Ok(match self {
            Self::Little => u16::from_le_bytes(bytes),
            Self::Big => u16::from_be_bytes(bytes),
        })
    }

    fn u32(self, bytes: &[u8]) -> Result<u32, Error> {
        let bytes: [u8; 4] = bytes
            .try_into()
            .map_err(|_| Error::InvalidDng("truncated TIFF u32"))?;
        Ok(match self {
            Self::Little => u32::from_le_bytes(bytes),
            Self::Big => u32::from_be_bytes(bytes),
        })
    }
}

#[derive(Default)]
struct OriginalTags<'a> {
    name: Option<&'a [u8]>,
    data: Option<&'a [u8]>,
    digest: Option<&'a [u8]>,
}

fn checked_slice(data: &[u8], offset: usize, length: usize) -> Result<&[u8], Error> {
    let end = offset
        .checked_add(length)
        .ok_or(Error::InvalidDng("TIFF offset overflow"))?;
    data.get(offset..end)
        .ok_or(Error::InvalidDng("TIFF value outside file"))
}

fn tiff_type_size(field_type: u16) -> Result<usize, Error> {
    match field_type {
        1 | 2 | 6 | 7 => Ok(1),
        3 | 8 => Ok(2),
        4 | 9 | 11 | 13 => Ok(4),
        5 | 10 | 12 | 16 | 17 | 18 => Ok(8),
        _ => Err(Error::InvalidDng(
            "unsupported TIFF field type on original tag",
        )),
    }
}

fn entry_value<'a>(file: &'a [u8], entry: &'a [u8], order: ByteOrder) -> Result<&'a [u8], Error> {
    let field_type = order.u16(checked_slice(entry, 2, 2)?)?;
    let count = usize::try_from(order.u32(checked_slice(entry, 4, 4)?)?)
        .map_err(|_| Error::InvalidDng("TIFF value count is too large"))?;
    let length = tiff_type_size(field_type)?
        .checked_mul(count)
        .ok_or(Error::InvalidDng("TIFF value length overflow"))?;
    if length <= 4 {
        checked_slice(entry, 8, length)
    } else {
        let offset = usize::try_from(order.u32(checked_slice(entry, 8, 4)?)?)
            .map_err(|_| Error::InvalidDng("TIFF value offset is too large"))?;
        checked_slice(file, offset, length)
    }
}

fn parse_original_tags(file: &[u8]) -> Result<OriginalTags<'_>, Error> {
    let header = checked_slice(file, 0, 8)?;
    let order = match &header[0..2] {
        b"II" => ByteOrder::Little,
        b"MM" => ByteOrder::Big,
        _ => return Err(Error::InvalidDng("invalid TIFF byte-order marker")),
    };
    if order.u16(&header[2..4])? != 42 {
        return Err(Error::InvalidDng("not a classic TIFF/DNG file"));
    }

    let mut ifd_offset = usize::try_from(order.u32(&header[4..8])?)
        .map_err(|_| Error::InvalidDng("IFD offset is too large"))?;
    let mut visited = HashSet::new();
    let mut tags = OriginalTags::default();

    while ifd_offset != 0 {
        if !visited.insert(ifd_offset) || visited.len() > 64 {
            return Err(Error::InvalidDng("cyclic or excessive TIFF IFD chain"));
        }
        let count = usize::from(order.u16(checked_slice(file, ifd_offset, 2)?)?);
        let entries_start = ifd_offset
            .checked_add(2)
            .ok_or(Error::InvalidDng("IFD offset overflow"))?;
        let entries_length = count
            .checked_mul(12)
            .ok_or(Error::InvalidDng("IFD length overflow"))?;
        let entries = checked_slice(file, entries_start, entries_length)?;

        let (entries, remainder) = entries.as_chunks::<12>();
        debug_assert!(remainder.is_empty());
        for entry in entries {
            let tag = order.u16(&entry[0..2])?;
            match tag {
                TAG_ORIGINAL_RAW_FILE_NAME if tags.name.is_none() => {
                    tags.name = Some(entry_value(file, entry, order)?);
                }
                TAG_ORIGINAL_RAW_FILE_DATA if tags.data.is_none() => {
                    tags.data = Some(entry_value(file, entry, order)?);
                }
                TAG_ORIGINAL_RAW_FILE_DIGEST if tags.digest.is_none() => {
                    tags.digest = Some(entry_value(file, entry, order)?);
                }
                _ => {}
            }
        }

        let next_offset_position = entries_start
            .checked_add(entries_length)
            .ok_or(Error::InvalidDng("next IFD offset overflow"))?;
        ifd_offset = usize::try_from(order.u32(checked_slice(file, next_offset_position, 4)?)?)
            .map_err(|_| Error::InvalidDng("next IFD offset is too large"))?;
    }
    Ok(tags)
}

fn original_filename(file: &[u8]) -> Result<&[u8], Error> {
    let tags = parse_original_tags(file)?;
    let name = tags
        .name
        .ok_or(Error::MissingOriginal("OriginalRawFileName is absent"))?;
    let name = name.strip_suffix(&[0]).unwrap_or(name);
    if name.is_empty() {
        return Err(Error::InvalidDng("OriginalRawFileName is empty"));
    }
    if name.contains(&0) {
        return Err(Error::InvalidDng(
            "OriginalRawFileName contains an embedded NUL",
        ));
    }
    Ok(name)
}

fn be_u32(data: &[u8], offset: usize) -> Result<u32, Error> {
    let bytes: [u8; 4] = checked_slice(data, offset, 4)?
        .try_into()
        .map_err(|_| Error::InvalidDng("truncated OriginalRawFileData word"))?;
    Ok(u32::from_be_bytes(bytes))
}

fn decode_slot_zero(container: &[u8]) -> Result<Vec<u8>, Error> {
    let decoded_size = usize::try_from(be_u32(container, 0)?)
        .map_err(|_| Error::InvalidDng("decoded original is too large"))?;
    if decoded_size == 0 {
        return Err(Error::MissingOriginal(
            "OriginalRawFileData slot 0 is empty",
        ));
    }
    let block_count = decoded_size
        .checked_add(65_535)
        .ok_or(Error::InvalidDng("decoded original size overflow"))?
        / 65_536;
    let header_length = block_count
        .checked_add(2)
        .and_then(|count| count.checked_mul(4))
        .ok_or(Error::InvalidDng("OriginalRawFileData header overflow"))?;
    checked_slice(container, 0, header_length)?;

    let mut offsets = Vec::with_capacity(block_count + 1);
    for index in 0..=block_count {
        let table_position = index
            .checked_add(1)
            .and_then(|value| value.checked_mul(4))
            .ok_or(Error::InvalidDng("compressed block table overflow"))?;
        let offset = usize::try_from(be_u32(container, table_position)?)
            .map_err(|_| Error::InvalidDng("compressed block offset is too large"))?;
        offsets.push(offset);
    }
    if offsets[0] < header_length {
        return Err(Error::InvalidDng(
            "compressed block overlaps its offset table",
        ));
    }

    // Each block inflates independently into its own 64 KB slot, so the whole
    // container decompresses in parallel. Sequentially this was the larger half
    // of the 779 ms it took to unwrap a 116 MB DNG on Lukas's laptop, and that
    // cost lands on the first view of every high-resolution scan.
    let mut decoded = vec![0_u8; decoded_size];
    let blocks: Vec<&[u8]> = (0..block_count)
        .map(|index| {
            let start = offsets[index];
            let end = offsets[index + 1];
            if start >= end {
                return Err(Error::InvalidDng(
                    "compressed block offsets are not increasing",
                ));
            }
            checked_slice(container, start, end - start)
        })
        .collect::<Result<Vec<_>, Error>>()?;
    // BLOCK_COUNT is decoded_size rounded up to whole blocks, so the chunks and
    // the blocks pair off one to one and the last slot carries the remainder.
    decoded
        .par_chunks_mut(65_536)
        .zip(blocks.into_par_iter())
        .try_for_each(|(slot, compressed)| {
            let mut decoder = ZlibDecoder::new(compressed);
            let mut inflated = Vec::with_capacity(slot.len() + 1);
            decoder
                .by_ref()
                // One byte past the slot, so a block that decodes too much is
                // caught by the length check rather than silently truncated.
                .take(slot.len() as u64 + 1)
                .read_to_end(&mut inflated)
                .map_err(|_| Error::Decompression("failed to inflate embedded original block"))?;
            if inflated.len() != slot.len() || decoder.total_in() as usize != compressed.len() {
                return Err(Error::Decompression(
                    "embedded original block has an invalid decoded size or trailing data",
                ));
            }
            slot.copy_from_slice(&inflated);
            Ok(())
        })?;
    Ok(decoded)
}

/// The original RAW a DNG was converted from, verified against its digest.
///
/// Exposed to the renderer so a container rawler cannot decode can still be
/// developed from the file it was made from, without a temporary file.
pub(crate) fn embedded_original(file: &[u8]) -> Result<Vec<u8>, Error> {
    decode_and_verify(file)
}

fn decode_and_verify(file: &[u8]) -> Result<Vec<u8>, Error> {
    let tags = parse_original_tags(file)?;
    let container = tags
        .data
        .ok_or(Error::MissingOriginal("OriginalRawFileData is absent"))?;
    let digest = tags
        .digest
        .ok_or(Error::MissingOriginal("OriginalRawFileDigest is absent"))?;
    if digest.len() != 16 {
        return Err(Error::InvalidDng("OriginalRawFileDigest is not 16 bytes"));
    }
    let actual = Md5::digest(container);
    if actual.as_slice() != digest {
        return Err(Error::DigestMismatch);
    }
    decode_slot_zero(container)
}

unsafe fn path_from_c<'a>(path: *const c_char) -> Result<&'a Path, Error> {
    if path.is_null() {
        return Err(Error::InvalidArgument("path pointer is null"));
    }
    // SAFETY: The C API requires `path` to point to a readable NUL-terminated string.
    let bytes = unsafe { CStr::from_ptr(path) }.to_bytes();
    if bytes.is_empty() {
        return Err(Error::InvalidArgument("path is empty"));
    }
    Ok(Path::new(std::ffi::OsStr::from_bytes(bytes)))
}

unsafe fn write_c_buffer(bytes: &[u8], output: *mut c_char, capacity: usize) -> Result<(), Error> {
    let needed = bytes
        .len()
        .checked_add(1)
        .ok_or(Error::InvalidArgument("result length overflow"))?;
    if output.is_null() {
        return Err(Error::InvalidArgument("result buffer pointer is null"));
    }
    if capacity < needed {
        return Err(Error::BufferTooSmall { needed });
    }
    // SAFETY: The caller promises a writable buffer of `capacity` bytes; it was checked above.
    unsafe {
        ptr::copy_nonoverlapping(bytes.as_ptr(), output.cast::<u8>(), bytes.len());
        output.add(bytes.len()).write(0);
    }
    Ok(())
}

unsafe fn write_error(error: &str, buffer: *mut c_char, capacity: usize) {
    if buffer.is_null() || capacity == 0 {
        return;
    }
    let bytes = error.as_bytes();
    let count = bytes.len().min(capacity - 1);
    // SAFETY: The caller promises a writable buffer of `capacity` bytes; null and zero were checked.
    unsafe {
        ptr::copy_nonoverlapping(bytes.as_ptr(), buffer.cast::<u8>(), count);
        buffer.add(count).write(0);
    }
}

/// Keeps whole-image buffers on the heap instead of handing them back to the
/// kernel between renders.
///
/// glibc's malloc mmaps anything past its dynamic threshold, which is capped at
/// 32 MB, and unmaps it on free. A render allocates a handful of buffers a few
/// tens of megabytes each and frees them a moment later, so above that cap
/// every one of them was a fresh mapping faulted in a page at a time: a 2048 px
/// preview drag measured 43 ms a tick against 14 ms with the threshold raised,
/// while a 1600 px one — just under the cap — was already fast. The cost is
/// resident memory the process holds rather than returns, which is the right
/// trade for an editor that reuses the same few buffer sizes all session.
#[cfg(target_env = "gnu")]
fn tune_allocator() {
    use std::ffi::c_int;
    use std::sync::Once;
    // glibc's mallopt parameter numbers; there is no crate binding here.
    const M_TRIM_THRESHOLD: c_int = -1;
    const M_MMAP_THRESHOLD: c_int = -3;
    const LARGEST_POOLED_BUFFER: c_int = 512 * 1024 * 1024;
    unsafe extern "C" {
        fn mallopt(parameter: c_int, value: c_int) -> c_int;
    }
    static TUNED: Once = Once::new();
    TUNED.call_once(|| {
        // SAFETY: mallopt takes two plain integers and only adjusts allocator
        // policy for this process.
        unsafe {
            mallopt(M_MMAP_THRESHOLD, LARGEST_POOLED_BUFFER);
            mallopt(M_TRIM_THRESHOLD, LARGEST_POOLED_BUFFER);
        }
    });
}

#[cfg(not(target_env = "gnu"))]
fn tune_allocator() {}

unsafe fn ffi_result<F>(error_buffer: *mut c_char, error_capacity: usize, operation: F) -> i32
where
    F: FnOnce() -> Result<(), Error>,
{
    tune_allocator();
    match catch_unwind(AssertUnwindSafe(operation)) {
        Ok(Ok(())) => {
            // SAFETY: Forwarding the caller-provided optional error buffer.
            unsafe { write_error("", error_buffer, error_capacity) };
            ORFEUS_STATUS_OK
        }
        Ok(Err(error)) => {
            // SAFETY: Forwarding the caller-provided optional error buffer.
            unsafe { write_error(&error.to_string(), error_buffer, error_capacity) };
            error.status()
        }
        Err(_) => {
            // SAFETY: Forwarding the caller-provided optional error buffer.
            unsafe { write_error("internal Rust panic", error_buffer, error_capacity) };
            ORFEUS_STATUS_PANIC
        }
    }
}

/// Return the C ABI version implemented by this library.
///
/// Version 2 added `orfeus_raw_render_v2` with an opt-in decode cache.
/// Version 3 adds `orfeus_raw_render_v3`, which executes a serialized
/// processing node graph. Version 4 adds `orfeus_raw_decode_v1`, which fills
/// the decode cache without rendering. Version 10 adds
/// `orfeus_lens_profile_match_v1` and `orfeus_lens_profiles_v1`, which answer
/// about lens profiles without rendering. Every earlier entry point is
/// unchanged.
#[unsafe(no_mangle)]
pub extern "C" fn orfeus_bridge_abi_version() -> u32 {
    10
}

/// Write a 64-bit perceptual signature of the image at PATH.
///
/// A difference hash: the picture is reduced to a nine by eight grid of
/// brightness and each bit records whether one cell is brighter than the cell
/// to its right. Two frames of the same burst differ in a handful of bits;
/// two different subjects differ in dozens. Comparing brightness *gradients*
/// rather than brightness itself is what makes it survive the exposure and
/// white balance drift within a burst.
///
/// Any image the renderer can read will do, and the caller is expected to hand
/// over a thumbnail it already had rather than a photograph: the point of this
/// is to cost nothing.
///
/// # Safety
///
/// `path` must be a readable NUL-terminated path, `signature` writable, and the
/// error buffer, when non-null, writable for its stated capacity.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn orfeus_image_signature_v1(
    path: *const c_char,
    signature: *mut u64,
    error_buffer: *mut c_char,
    error_capacity: usize,
) -> i32 {
    // SAFETY: Pointer validation and dereferences remain inside the boundary.
    unsafe {
        ffi_result(error_buffer, error_capacity, || {
            if signature.is_null() {
                return Err(Error::InvalidArgument("signature pointer is null"));
            }
            let path = path_from_c(path)?;
            *signature = image_signature(path)?;
            Ok(())
        })
    }
}

/// Write a focus measurement of the RAW at PATH: blur radius, coverage, and
/// how much of the frame could be judged at all.
///
/// The three floats are, in order, the blur radius of the best-focused part of
/// the frame in pixels at a 1600-pixel long edge; the same for the middling
/// part of the frame, which separates a shallow plane of focus from a frame
/// with nothing sharp in it; and the fraction of the frame that carried enough
/// edge structure to judge. A radius near zero is as sharp as the sampling can
/// express. A judgeable fraction near zero means the frame
/// answered nothing — a sky, a wall, a lens cap — and the radius should be read
/// as unknown rather than as bad.
///
/// Measured on a draft develop, which bins whole sensor quads: two to three
/// thousand pixels on the long edge, which is the size a preview is looked at
/// and therefore the size the question is worth asking at. The decode is
/// deliberately not cached — a cull sweeps hundreds of frames and would evict
/// the photograph the interface is showing.
///
/// # Safety
///
/// `path` must be a readable NUL-terminated path, `focus` writable for three
/// floats, and the error buffer, when non-null, writable for its capacity.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn orfeus_image_focus_v1(
    path: *const c_char,
    focus: *mut f32,
    error_buffer: *mut c_char,
    error_capacity: usize,
) -> i32 {
    // SAFETY: Pointer validation and dereferences remain inside the boundary.
    unsafe {
        ffi_result(error_buffer, error_capacity, || {
            if focus.is_null() {
                return Err(Error::InvalidArgument("focus pointer is null"));
            }
            let path = path_from_c(path)?;
            let decoded = render::decode_linear_srgb(path, true, false)?;
            let report = focus::measure_frame(decoded.width, decoded.height, &decoded.data);
            let out = std::slice::from_raw_parts_mut(focus, 3);
            out[0] = report.blur_radius;
            out[1] = report.typical_blur;
            out[2] = report.judgeable;
            Ok(())
        })
    }
}

/// Report the lens profile a photograph would be corrected with.
///
/// Writes one line of tab-separated fields into `output`: the profile's
/// primary name, its display name, its maker, the letters of the calibrations
/// it offers at this focal length (D distortion, T chromatic aberration,
/// V vignetting), and the crop factor the correction would use. Returns the
/// lens-profile-unavailable status, with the same message a render would give,
/// when there is no profile — so a caller can know that before rendering
/// rather than by a render that fails.
///
/// # Safety
///
/// `make`, `model` and `lens_name` must be readable NUL-terminated strings,
/// `explicit_profile` null or one, and `output` and the error buffer writable
/// for their stated capacities.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn orfeus_lens_profile_match_v1(
    make: *const c_char,
    model: *const c_char,
    lens_name: *const c_char,
    focal: f32,
    explicit_profile: *const c_char,
    focal_reducer: f32,
    crop_factor: f32,
    output: *mut c_char,
    output_capacity: usize,
    error_buffer: *mut c_char,
    error_capacity: usize,
) -> i32 {
    // SAFETY: Pointer validation and dereferences remain inside the boundary.
    unsafe {
        ffi_result(error_buffer, error_capacity, || {
            let text = |pointer: *const c_char, what: &'static str| -> Result<&str, Error> {
                if pointer.is_null() {
                    return Err(Error::InvalidArgument(what));
                }
                CStr::from_ptr(pointer)
                    .to_str()
                    .map_err(|_| Error::InvalidArgument(what))
            };
            let options = render::LensCorrectionOptions {
                make: text(make, "camera make is not a string")?,
                model: text(model, "camera model is not a string")?,
                lens_name: text(lens_name, "lens name is not a string")?,
                focal,
                flags: render::FLAG_LENS_DISTORTION | render::FLAG_LENS_TCA,
                strength: 1.0,
                explicit_profile: if explicit_profile.is_null() {
                    None
                } else {
                    Some(text(explicit_profile, "lens profile is not a string")?)
                },
                focal_reducer: if focal_reducer > 0.0 { focal_reducer } else { 1.0 },
                crop_factor: crop_factor.max(0.0),
            };
            let description = render::describe_lens_match(&options)?;
            write_c_buffer(description.as_bytes(), output, output_capacity)
        })
    }
}

/// List lens profiles for a chooser: see `render::search_lens_profiles`.
///
/// Every string may be empty; `query` empty lists what the body can mount.
/// Writes newline-separated records of tab-separated fields into `output`, and
/// returns the buffer-too-small status naming the size needed when it does not
/// fit.
///
/// # Safety
///
/// `make`, `model` and `query` must be readable NUL-terminated strings, and
/// `output` and the error buffer writable for their stated capacities.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn orfeus_lens_profiles_v1(
    make: *const c_char,
    model: *const c_char,
    query: *const c_char,
    focal: f32,
    output: *mut c_char,
    output_capacity: usize,
    error_buffer: *mut c_char,
    error_capacity: usize,
) -> i32 {
    // SAFETY: Pointer validation and dereferences remain inside the boundary.
    unsafe {
        ffi_result(error_buffer, error_capacity, || {
            let text = |pointer: *const c_char| -> Result<&str, Error> {
                if pointer.is_null() {
                    return Ok("");
                }
                CStr::from_ptr(pointer)
                    .to_str()
                    .map_err(|_| Error::InvalidArgument("lens search text is not UTF-8"))
            };
            let listing =
                render::search_lens_profiles(text(make)?, text(model)?, text(query)?, focal)?;
            write_c_buffer(listing.as_bytes(), output, output_capacity)
        })
    }
}

/// Columns and rows the signature grid is reduced to. Nine columns give the
/// eight horizontal comparisons that fill one row of the hash.
const SIGNATURE_COLUMNS: u32 = 9;
const SIGNATURE_ROWS: u32 = 8;

fn image_signature(path: &Path) -> Result<u64, Error> {
    let image = image::ImageReader::open(path)
        .map_err(Error::Io)?
        .with_guessed_format()
        .map_err(Error::Io)?
        .decode()
        .map_err(|error| Error::Render(format!("signature decode: {error}")))?
        .resize_exact(
            SIGNATURE_COLUMNS,
            SIGNATURE_ROWS,
            image::imageops::FilterType::Triangle,
        )
        .into_luma8();
    let mut signature = 0_u64;
    let mut bit = 0;
    for row in 0..SIGNATURE_ROWS {
        for column in 0..SIGNATURE_COLUMNS - 1 {
            let left = image.get_pixel(column, row).0[0];
            let right = image.get_pixel(column + 1, row).0[0];
            if left > right {
                signature |= 1 << bit;
            }
            bit += 1;
        }
    }
    Ok(signature)
}

/// Report the colour temperature INPUT's camera balanced for, in kelvin.
///
/// Writes zero when the file's metadata does not identify one. A temperature
/// control reads this to start where the photograph actually is, and the
/// renderer treats the same value as the one that changes nothing.
///
/// # Safety
///
/// `input_path` must be a readable NUL-terminated path, `kelvin` writable, and
/// the error buffer, when non-null, writable for its stated capacity.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn orfeus_raw_as_shot_kelvin_v1(
    input_path: *const c_char,
    _unused_cache_mode: u32,
    kelvin: *mut f32,
    error_buffer: *mut c_char,
    error_capacity: usize,
) -> i32 {
    // SAFETY: Pointer validation and dereferences remain inside the boundary.
    unsafe {
        ffi_result(error_buffer, error_capacity, || {
            if kelvin.is_null() {
                return Err(Error::InvalidArgument("kelvin pointer is null"));
            }
            let input = path_from_c(input_path)?;
            *kelvin = render::as_shot_kelvin(input)?.unwrap_or(0.0);
            Ok(())
        })
    }
}

/// Decode INPUT into the cache without rendering anything.
///
/// A render needs the lens description before it can start, and reading that
/// out of the file costs half a second of ExifTool on a 116 MB DNG — while the
/// decode it precedes does not depend on it at all. A caller can run this on
/// another thread meanwhile and have the decode waiting when the description
/// arrives. Only worth doing with the cache on; without it the work is thrown
/// away and the render decodes again.
///
/// FLAGS, MAX_WIDTH and MAX_HEIGHT are the render frame's, so that a preview
/// warms the same draft entry it will go on to ask for.
///
/// # Safety
///
/// `input_path` must be a readable NUL-terminated path, and the error buffer,
/// when non-null, writable for its stated capacity.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn orfeus_raw_decode_v1(
    input_path: *const c_char,
    flags: u32,
    max_width: u32,
    max_height: u32,
    cache_mode: u32,
    error_buffer: *mut c_char,
    error_capacity: usize,
) -> i32 {
    // SAFETY: Pointer validation and dereferences remain inside the boundary.
    unsafe {
        ffi_result(error_buffer, error_capacity, || {
            let input = path_from_c(input_path)?;
            graphex::prewarm_decode(input, flags, max_width, max_height, cache_mode)
        })
    }
}

/// Read `OriginalRawFileName` from a classic TIFF/DNG into a caller buffer.
///
/// The returned filename is NUL-terminated. On failure, `error_buffer` receives
/// a NUL-terminated diagnostic when it is non-null and has nonzero capacity.
///
/// # Safety
///
/// `dng_path` must point to a readable NUL-terminated path. `filename_buffer`
/// and `error_buffer`, when non-null, must be writable for their stated capacities.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn orfeus_dng_original_filename(
    dng_path: *const c_char,
    filename_buffer: *mut c_char,
    filename_capacity: usize,
    error_buffer: *mut c_char,
    error_capacity: usize,
) -> i32 {
    // SAFETY: Pointer validation and all dereferences occur inside the operation.
    unsafe {
        ffi_result(error_buffer, error_capacity, || {
            let path = path_from_c(dng_path)?;
            let file = fs::read(path)?;
            write_c_buffer(
                original_filename(&file)?,
                filename_buffer,
                filename_capacity,
            )
        })
    }
}

/// Extract and verify DNG slot 0 to exactly the caller-specified output path.
///
/// The function verifies the decoded length and the MD5 digest of
/// `OriginalRawFileData` before creating or replacing the output file.
///
/// # Safety
///
/// Both path pointers must point to readable NUL-terminated paths.
/// `error_buffer`, when non-null, must be writable for `error_capacity` bytes.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn orfeus_dng_extract_original(
    dng_path: *const c_char,
    output_path: *const c_char,
    error_buffer: *mut c_char,
    error_capacity: usize,
) -> i32 {
    // SAFETY: Pointer validation and all dereferences occur inside the operation.
    unsafe {
        ffi_result(error_buffer, error_capacity, || {
            let input = path_from_c(dng_path)?;
            let output = path_from_c(output_path)?;
            let file = fs::read(input)?;
            let decoded = decode_and_verify(&file)?;
            fs::write(output, decoded)?;
            Ok(())
        })
    }
}

/// Begin building the Vulkan compute context on a background thread.
///
/// Initialization costs enough to show on the first frames a session renders.
/// Calling this once at startup moves that cost off the render path; calling it
/// again, or on a machine with no usable adapter, does nothing.
#[unsafe(no_mangle)]
pub extern "C" fn orfeus_gpu_warm_up() {
    gpu::warm_up();
}

/// Report version 1 render/export capabilities.
///
/// The returned bit set intentionally omits source EXIF/XMP preservation: the
/// selected encoders cannot accept arbitrary source metadata. Callers must not
/// claim metadata preservation when that bit is absent.
#[unsafe(no_mangle)]
pub extern "C" fn orfeus_raw_render_capabilities_v1() -> u32 {
    1 | 2 | 4 | 16 // orientation, 16-bit TIFF, sRGB ICC, lens tuning/overrides
}

/// Decode, process, and export one Olympus ORF using version 1 render settings.
///
/// Export applies source orientation physically and embeds an sRGB ICC profile,
/// but does not preserve source EXIF/XMP metadata. Output is created through a
/// same-directory temporary file, synced, then atomically renamed.
///
/// # Safety
///
/// Path pointers and a non-null `settings.lut_path` must be readable
/// NUL-terminated strings for the duration of this call. `settings` must point
/// to a readable `OrfeusRenderSettingsV1`. The error buffer, when non-null, must
/// be writable for its stated capacity.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn orfeus_raw_render_v1(
    input_path: *const c_char,
    output_path: *const c_char,
    settings: *const render::RenderSettingsV1,
    error_buffer: *mut c_char,
    error_capacity: usize,
) -> i32 {
    // SAFETY: The v2 entry point upholds the identical contract.
    unsafe {
        orfeus_raw_render_v2(
            input_path,
            output_path,
            settings,
            render::CACHE_NONE,
            error_buffer,
            error_capacity,
        )
    }
}

/// Render INPUT through a serialized processing node graph.
///
/// `frame` carries the output and lens-alias parameters shared by the whole
/// graph; `graph`/`graph_length` reference the little-endian node program
/// produced by the Orfeus core (magic "ORFG"). Cache semantics match
/// `orfeus_raw_render_v2`. Export behavior (orientation, ICC, atomic
/// publish) matches `orfeus_raw_render_v1`.
///
/// # Safety
///
/// Path pointers and a non-null `frame.lens_profile_model` must be readable
/// NUL-terminated strings, `frame` must point to a readable
/// `orfeus_render_frame_v1`, and `graph` must be readable for
/// `graph_length` bytes, all for the duration of this call. The error
/// buffer, when non-null, must be writable for its stated capacity.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn orfeus_raw_render_v3(
    input_path: *const c_char,
    output_path: *const c_char,
    frame: *const graphex::RenderFrameV1,
    graph: *const u8,
    graph_length: usize,
    cache_mode: u32,
    error_buffer: *mut c_char,
    error_capacity: usize,
) -> i32 {
    // SAFETY: Pointer validation and dereferences remain inside the panic boundary.
    unsafe {
        ffi_result(error_buffer, error_capacity, || {
            if frame.is_null() {
                return Err(Error::InvalidArgument("render frame pointer is null"));
            }
            if graph.is_null() {
                return Err(Error::InvalidArgument("graph pointer is null"));
            }
            let bytes = std::slice::from_raw_parts(graph, graph_length);
            let input = path_from_c(input_path)?;
            let output = path_from_c(output_path)?;
            graphex::render_graph(input, output, &*frame, bytes, cache_mode)
        })
    }
}

/// Render a node graph into a caller-provided RGB8 buffer.
///
/// The live-preview hot path: identical processing to
/// `orfeus_raw_render_v3`, but the oriented display image lands directly in
/// `buffer` (row-major RGB, one byte per channel) with no JPEG encode or
/// file write. Writes the image dimensions to `out_width`/`out_height`.
/// The frame's output format and JPEG quality fields are ignored.
///
/// # Safety
///
/// `input_path` must be a readable NUL-terminated path, `frame` and `graph`
/// valid for their stated sizes, `buffer` writable for `capacity` bytes,
/// `out_width`/`out_height` writable, and the error buffer, when non-null,
/// writable for its stated capacity.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn orfeus_raw_render_rgb_v1(
    input_path: *const c_char,
    frame: *const graphex::RenderFrameV1,
    graph: *const u8,
    graph_length: usize,
    buffer: *mut u8,
    capacity: usize,
    out_width: *mut u32,
    out_height: *mut u32,
    cache_mode: u32,
    error_buffer: *mut c_char,
    error_capacity: usize,
) -> i32 {
    // SAFETY: Pointer validation and dereferences remain inside the panic boundary.
    unsafe {
        ffi_result(error_buffer, error_capacity, || {
            if frame.is_null() {
                return Err(Error::InvalidArgument("render frame pointer is null"));
            }
            if graph.is_null() {
                return Err(Error::InvalidArgument("graph pointer is null"));
            }
            if buffer.is_null() || out_width.is_null() || out_height.is_null() {
                return Err(Error::InvalidArgument("output pointer is null"));
            }
            let bytes = std::slice::from_raw_parts(graph, graph_length);
            let destination = std::slice::from_raw_parts_mut(buffer, capacity);
            let input = path_from_c(input_path)?;
            let (width, height) =
                graphex::render_graph_rgb(input, &*frame, bytes, destination, cache_mode)?;
            *out_width = width as u32;
            *out_height = height as u32;
            Ok(())
        })
    }
}

/// Detect the central tile and film-base color of a scanned negative.
///
/// Writes eight floats: the crop rectangle (left, top, width, height) in
/// oriented normalized coordinates, the scene-linear base color (red, green,
/// blue) sampled from the border ring, and the straightening angle in
/// degrees. A full-frame rectangle with angle zero is reported when no
/// plausible tile exists. Cache semantics match `orfeus_raw_render_v2`.
///
/// # Safety
///
/// `input_path` must be a readable NUL-terminated path, `results` must be
/// writable for eight floats, and the error buffer, when non-null, writable
/// for its stated capacity.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn orfeus_analyze_negative_frame_v1(
    input_path: *const c_char,
    cache_mode: u32,
    results: *mut f32,
    error_buffer: *mut c_char,
    error_capacity: usize,
) -> i32 {
    // SAFETY: Pointer validation and dereferences remain inside the panic boundary.
    unsafe {
        ffi_result(error_buffer, error_capacity, || {
            if results.is_null() {
                return Err(Error::InvalidArgument("results pointer is null"));
            }
            let input = path_from_c(input_path)?;
            let frame = analyze::analyze_negative_frame_file(input, cache_mode)?;
            let values = [
                frame.rect[0],
                frame.rect[1],
                frame.rect[2],
                frame.rect[3],
                frame.base[0],
                frame.base[1],
                frame.base[2],
                frame.angle,
            ];
            ptr::copy_nonoverlapping(values.as_ptr(), results, values.len());
            Ok(())
        })
    }
}

/// Write the camera's embedded JPEG preview of a RAW file as a thumbnail.
///
/// No sensor data is developed: the preview JPEG the camera wrote into the
/// container is found by its markers, decoded, box-filtered down so its
/// longer side is at most `max_edge` pixels, turned the way the file's
/// orientation says, and written to `output_path` as a JPEG of `quality`.
/// `results`, when non-null, receives the written width and height.
///
/// # Safety
///
/// `input_path` and `output_path` must be readable NUL-terminated paths,
/// `results` null or writable for two unsigned integers, and the error
/// buffer, when non-null, writable for its stated capacity.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn orfeus_embedded_preview_v1(
    input_path: *const c_char,
    output_path: *const c_char,
    max_edge: u32,
    quality: u32,
    results: *mut u32,
    error_buffer: *mut c_char,
    error_capacity: usize,
) -> i32 {
    // SAFETY: Pointer validation and dereferences remain inside the panic boundary.
    unsafe {
        ffi_result(error_buffer, error_capacity, || {
            let input = path_from_c(input_path)?;
            let output = path_from_c(output_path)?;
            let quality = u8::try_from(quality.clamp(1, 100)).unwrap_or(82);
            let (width, height) =
                embedded::embedded_preview_file(input, output, max_edge, quality)?;
            if !results.is_null() {
                ptr::copy_nonoverlapping([width, height].as_ptr(), results, 2);
            }
            Ok(())
        })
    }
}

/// Read the tilt of a picture from its own straight edges.
///
/// Writes two floats: the crop angle, in degrees of the display convention
/// (positive turns the picture clockwise), that stands the strongest edges
/// upright, and a confidence — how many times the winning angle's strongest
/// line outweighs the run of angles'. Under about two there was no straight
/// edge worth trusting; zero says the tilt lies beyond the ten degrees
/// searched. Cache semantics match `orfeus_raw_render_v2`.
///
/// # Safety
///
/// `input_path` must be a readable NUL-terminated path, `results` must be
/// writable for two floats, and the error buffer, when non-null, writable for
/// its stated capacity.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn orfeus_analyze_level_v1(
    input_path: *const c_char,
    cache_mode: u32,
    results: *mut f32,
    error_buffer: *mut c_char,
    error_capacity: usize,
) -> i32 {
    // SAFETY: Pointer validation and dereferences remain inside the panic boundary.
    unsafe {
        ffi_result(error_buffer, error_capacity, || {
            if results.is_null() {
                return Err(Error::InvalidArgument("results pointer is null"));
            }
            let input = path_from_c(input_path)?;
            let estimate = analyze::analyze_level_file(input, cache_mode)?;
            let values = [estimate.angle, estimate.confidence];
            ptr::copy_nonoverlapping(values.as_ptr(), results, values.len());
            Ok(())
        })
    }
}

/// Average the scene-linear color around an oriented normalized point.
///
/// Writes three floats. The radius is normalized against the shorter image
/// edge and limited to 0.25. Cache semantics match `orfeus_raw_render_v2`.
///
/// # Safety
///
/// `input_path` must be a readable NUL-terminated path, `rgb` must be
/// writable for three floats, and the error buffer, when non-null, writable
/// for its stated capacity.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn orfeus_sample_linear_v1(
    input_path: *const c_char,
    cache_mode: u32,
    x: f32,
    y: f32,
    radius: f32,
    rgb: *mut f32,
    error_buffer: *mut c_char,
    error_capacity: usize,
) -> i32 {
    // SAFETY: Pointer validation and dereferences remain inside the panic boundary.
    unsafe {
        ffi_result(error_buffer, error_capacity, || {
            if rgb.is_null() {
                return Err(Error::InvalidArgument("rgb pointer is null"));
            }
            let input = path_from_c(input_path)?;
            let color = analyze::sample_linear_file(input, cache_mode, x, y, radius)?;
            ptr::copy_nonoverlapping(color.as_ptr(), rgb, color.len());
            Ok(())
        })
    }
}

/// Render like `orfeus_raw_render_v1`, optionally reusing decoded scene data.
///
/// `cache_mode` 0 always decodes the input fresh. Mode 1 serves repeated
/// renders of an unchanged input file from a small in-process cache of decoded
/// scene-linear images, which interactive frontends use to re-render quickly
/// while adjusting settings. The cache key covers path, size, and modification
/// time.
///
/// # Safety
///
/// Identical to `orfeus_raw_render_v1`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn orfeus_raw_render_v2(
    input_path: *const c_char,
    output_path: *const c_char,
    settings: *const render::RenderSettingsV1,
    cache_mode: u32,
    error_buffer: *mut c_char,
    error_capacity: usize,
) -> i32 {
    // SAFETY: Pointer validation and dereferences remain inside the panic boundary.
    unsafe {
        ffi_result(error_buffer, error_capacity, || {
            if settings.is_null() {
                return Err(Error::InvalidArgument("render settings pointer is null"));
            }
            let input = path_from_c(input_path)?;
            let output = path_from_c(output_path)?;
            render::render(input, output, &*settings, cache_mode)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use flate2::Compression;
    use flate2::write::ZlibEncoder;
    use std::ffi::CString;
    use std::io::Write;

    fn embedded_container(original: &[u8]) -> Vec<u8> {
        let blocks: Vec<&[u8]> = original.chunks(65_536).collect();
        let header_length = 4 * (blocks.len() + 2);
        let mut compressed_blocks = Vec::new();
        for block in &blocks {
            let mut encoder = ZlibEncoder::new(Vec::new(), Compression::default());
            encoder.write_all(block).unwrap();
            compressed_blocks.push(encoder.finish().unwrap());
        }

        let mut result = Vec::new();
        result.extend_from_slice(&(original.len() as u32).to_be_bytes());
        let mut offset = header_length;
        result.extend_from_slice(&(offset as u32).to_be_bytes());
        for block in &compressed_blocks {
            offset += block.len();
            result.extend_from_slice(&(offset as u32).to_be_bytes());
        }
        for block in compressed_blocks {
            result.extend_from_slice(&block);
        }
        result
    }

    /// A container rawler cannot decode is developed from the original it was
    /// made from, without anything being written to disk on the way.
    #[test]
    fn an_undecodable_container_falls_back_to_its_embedded_original() {
        let original = b"an ORF rawler would understand, if this were one".repeat(40);
        let file = synthetic_dng(&original, Md5::digest(embedded_container(&original)).into());
        let path = std::env::temp_dir().join(format!(
            "orfeus-fallback-{}.dng",
            std::process::id()
        ));
        fs::write(&path, &file).unwrap();
        // Neither half of this synthetic file is a real RAW, so the decode
        // fails — but its message has to show the embedded original was tried,
        // which is the only way to see the fallback happen without shipping a
        // photograph as a fixture.
        let message = crate::render::decode_linear_srgb(&path, false, false)
            .err()
            .expect("a synthetic container cannot decode")
            .to_string();
        assert!(
            message.contains("embedded original"),
            "the embedded original was not tried: {message}"
        );
        fs::remove_file(&path).ok();
    }

    /// An ordinary file that simply is not a RAW must say so, rather than
    /// complaining that it has no embedded original — which is true of every
    /// RAW file and explains nothing.
    #[test]
    fn a_file_that_is_not_raw_at_all_reports_the_decoder_failure() {
        let path = std::env::temp_dir().join(format!(
            "orfeus-not-raw-{}.dng",
            std::process::id()
        ));
        fs::write(&path, b"this is not a photograph").unwrap();
        let message = crate::render::decode_linear_srgb(&path, false, false)
            .err()
            .expect("an unreadable file must fail")
            .to_string();
        assert!(
            message.contains("RAW decoder"),
            "unhelpful message: {message}"
        );
        assert!(
            !message.contains("embedded original"),
            "an ordinary file has no original to blame: {message}"
        );
        fs::remove_file(&path).ok();
    }

    fn synthetic_dng(original: &[u8], digest: [u8; 16]) -> Vec<u8> {
        let name = b"sample.ORF\0";
        let container = embedded_container(original);
        let ifd_offset = 8usize;
        let ifd_length = 2 + 3 * 12 + 4;
        let name_offset = ifd_offset + ifd_length;
        let data_offset = name_offset + name.len();
        let digest_offset = data_offset + container.len();
        let mut file = Vec::new();
        file.extend_from_slice(b"II");
        file.extend_from_slice(&42u16.to_le_bytes());
        file.extend_from_slice(&(ifd_offset as u32).to_le_bytes());
        file.extend_from_slice(&3u16.to_le_bytes());
        for (tag, field_type, count, offset) in [
            (TAG_ORIGINAL_RAW_FILE_NAME, 2u16, name.len(), name_offset),
            (
                TAG_ORIGINAL_RAW_FILE_DATA,
                7u16,
                container.len(),
                data_offset,
            ),
            (TAG_ORIGINAL_RAW_FILE_DIGEST, 1u16, 16usize, digest_offset),
        ] {
            file.extend_from_slice(&tag.to_le_bytes());
            file.extend_from_slice(&field_type.to_le_bytes());
            file.extend_from_slice(&(count as u32).to_le_bytes());
            file.extend_from_slice(&(offset as u32).to_le_bytes());
        }
        file.extend_from_slice(&0u32.to_le_bytes());
        file.extend_from_slice(name);
        file.extend_from_slice(&container);
        file.extend_from_slice(&digest);
        file
    }

    fn original_data_digest(original: &[u8]) -> [u8; 16] {
        Md5::digest(embedded_container(original)).into()
    }

    #[test]
    fn reports_current_abi_version() {
        assert_eq!(orfeus_bridge_abi_version(), 10);
        assert_eq!(orfeus_raw_render_capabilities_v1(), 1 | 2 | 4 | 16);
    }

    #[test]
    fn color_errors_use_the_render_status_and_preserve_context() {
        let error = Error::Color(color::ColorError::MissingMatrix);
        assert_eq!(error.status(), ORFEUS_STATUS_RENDER_ERROR);
        assert_eq!(
            error.to_string(),
            "RAW color conversion: RAW has no camera color matrix"
        );
    }

    #[test]
    fn render_abi_rejects_same_input_without_modifying_it() {
        let path =
            std::env::temp_dir().join(format!("orfeus-abi-same-input-{}", std::process::id()));
        fs::write(&path, b"not a raw, but must survive").unwrap();
        let c_path = CString::new(path.as_os_str().as_bytes()).unwrap();
        let settings = render::RenderSettingsV1 {
            flags: 0,
            ..render::RenderSettingsV1::default()
        };
        let mut error = [0_i8; 256];
        let status = unsafe {
            orfeus_raw_render_v1(
                c_path.as_ptr(),
                c_path.as_ptr(),
                &settings,
                error.as_mut_ptr(),
                error.len(),
            )
        };
        assert_eq!(status, ORFEUS_STATUS_INVALID_ARGUMENT);
        assert_eq!(fs::read(&path).unwrap(), b"not a raw, but must survive");
        fs::remove_file(path).unwrap();
    }

    #[test]
    fn reads_filename_and_decodes_multiple_blocks() {
        let original: Vec<u8> = (0..140_000).map(|value| (value % 251) as u8).collect();
        let digest = original_data_digest(&original);
        let dng = synthetic_dng(&original, digest);
        assert_eq!(original_filename(&dng).unwrap(), b"sample.ORF");
        assert_eq!(decode_and_verify(&dng).unwrap(), original);
    }

    #[test]
    fn decodes_every_block_count_and_remainder_exactly() {
        // Blocks now inflate in parallel, paired off against 64 KB slots of the
        // output. If the slot count and the block count ever disagreed, the zip
        // would silently drop the tail rather than fail, so the sizes that sit
        // on the boundary are worth naming: empty remainder, one byte over, one
        // byte under, and a single short block.
        for size in [
            1_usize,
            65_535,
            65_536,
            65_537,
            131_072,
            131_073,
            140_000,
            262_144,
        ] {
            let original: Vec<u8> = (0..size).map(|value| (value % 251) as u8).collect();
            let digest = original_data_digest(&original);
            let dng = synthetic_dng(&original, digest);
            let decoded = decode_and_verify(&dng).expect("a well-formed container decodes");
            assert_eq!(decoded.len(), size, "wrong length for {size} bytes");
            assert_eq!(decoded, original, "wrong contents for {size} bytes");
        }
    }

    #[test]
    fn rejects_tiff_value_outside_file() {
        let original = b"raw bytes";
        let digest = original_data_digest(original);
        let mut dng = synthetic_dng(original, digest);
        let data_entry_offset_field = 8 + 2 + 12 + 8;
        dng[data_entry_offset_field..data_entry_offset_field + 4]
            .copy_from_slice(&u32::MAX.to_le_bytes());
        assert!(matches!(
            decode_and_verify(&dng),
            Err(Error::InvalidDng("TIFF value outside file"))
        ));
    }

    #[test]
    fn rejects_compressed_offset_outside_container() {
        let mut container = embedded_container(b"raw bytes");
        container[8..12].copy_from_slice(&u32::MAX.to_be_bytes());
        assert!(matches!(
            decode_slot_zero(&container),
            Err(Error::InvalidDng(_))
        ));
    }

    #[test]
    fn rejects_digest_mismatch_without_writing() {
        let dng = synthetic_dng(b"raw bytes", [0; 16]);
        assert!(matches!(
            decode_and_verify(&dng),
            Err(Error::DigestMismatch)
        ));
    }
}
