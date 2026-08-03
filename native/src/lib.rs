//! Native acceleration boundary for Orfeus.

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
use md5::{Digest, Md5};

mod analyze;
mod color;
mod gpu;
mod graphex;
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

    let mut decoded = Vec::with_capacity(decoded_size);
    for index in 0..block_count {
        let start = offsets[index];
        let end = offsets[index + 1];
        if start >= end {
            return Err(Error::InvalidDng(
                "compressed block offsets are not increasing",
            ));
        }
        let compressed = checked_slice(container, start, end - start)?;
        let expected = (decoded_size - decoded.len()).min(65_536);
        let mut decoder = ZlibDecoder::new(compressed);
        let before = decoded.len();
        decoder
            .by_ref()
            .take(65_537)
            .read_to_end(&mut decoded)
            .map_err(|_| Error::Decompression("failed to inflate embedded original block"))?;
        if decoded.len() - before != expected || decoder.total_in() as usize != compressed.len() {
            return Err(Error::Decompression(
                "embedded original block has an invalid decoded size or trailing data",
            ));
        }
    }
    if decoded.len() != decoded_size {
        return Err(Error::Decompression(
            "decoded original size does not match container",
        ));
    }
    Ok(decoded)
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

unsafe fn ffi_result<F>(error_buffer: *mut c_char, error_capacity: usize, operation: F) -> i32
where
    F: FnOnce() -> Result<(), Error>,
{
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
/// processing node graph. Every earlier entry point is unchanged.
#[unsafe(no_mangle)]
pub extern "C" fn orfeus_bridge_abi_version() -> u32 {
    3
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

/// Detect the central tile and film-base color of a scanned negative.
///
/// Writes seven floats: the crop rectangle (left, top, width, height) in
/// oriented normalized coordinates, then the scene-linear base color
/// (red, green, blue) sampled from the border ring. A full-frame rectangle
/// is reported when no plausible tile exists. Cache semantics match
/// `orfeus_raw_render_v2`.
///
/// # Safety
///
/// `input_path` must be a readable NUL-terminated path, `results` must be
/// writable for seven floats, and the error buffer, when non-null, writable
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
            ];
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
        assert_eq!(orfeus_bridge_abi_version(), 3);
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
