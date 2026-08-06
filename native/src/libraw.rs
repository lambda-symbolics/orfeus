//! Narrow LibRaw fallback for Olympus pixel-shift high-resolution files.

use std::ffi::{CStr, CString, c_char, c_void};
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::slice;

use rayon::prelude::*;

use super::Error;
use super::color::LinearSrgbImage;

#[repr(C)]
struct LibRawImage {
    width: usize,
    height: usize,
    data: *const u16,
    handle: *mut c_void,
}

unsafe extern "C" {
    fn orfeus_libraw_decode_linear_srgb(
        path: *const c_char,
        half_size: i32,
        output: *mut LibRawImage,
        error: *mut c_char,
        error_capacity: usize,
    ) -> i32;
    fn orfeus_libraw_free_image(handle: *mut c_void);
}

struct ImageGuard(*mut c_void);

impl Drop for ImageGuard {
    fn drop(&mut self) {
        unsafe { orfeus_libraw_free_image(self.0) };
    }
}

pub(crate) fn decode_linear_srgb(input: &Path, half_size: bool) -> Result<LinearSrgbImage, Error> {
    let path = CString::new(input.as_os_str().as_bytes())
        .map_err(|_| Error::Render("RAW path contains a NUL byte".into()))?;
    let mut image = LibRawImage {
        width: 0,
        height: 0,
        data: std::ptr::null(),
        handle: std::ptr::null_mut(),
    };
    let mut error = [0 as c_char; 512];
    let status = unsafe {
        orfeus_libraw_decode_linear_srgb(
            path.as_ptr(),
            i32::from(half_size),
            &mut image,
            error.as_mut_ptr(),
            error.len(),
        )
    };
    if status != 0 {
        let message = unsafe { CStr::from_ptr(error.as_ptr()) }
            .to_string_lossy()
            .into_owned();
        return Err(Error::Render(format!(
            "LibRaw high-resolution decode: {message}"
        )));
    }
    if image.handle.is_null() || image.data.is_null() || image.width == 0 || image.height == 0 {
        if !image.handle.is_null() {
            unsafe { orfeus_libraw_free_image(image.handle) };
        }
        return Err(Error::Render(
            "LibRaw high-resolution decode returned an empty image".into(),
        ));
    }
    let guard = ImageGuard(image.handle);
    let pixels = image
        .width
        .checked_mul(image.height)
        .and_then(|count| count.checked_mul(3))
        .ok_or_else(|| Error::Render("LibRaw high-resolution image is too large".into()))?;
    let source = unsafe { slice::from_raw_parts(image.data, pixels) };
    let mut data = vec![0.0_f32; pixels];
    data.par_iter_mut()
        .zip(source.par_iter())
        .for_each(|(output, input)| *output = *input as f32 / u16::MAX as f32);
    drop(guard);
    Ok(LinearSrgbImage {
        width: image.width,
        height: image.height,
        data,
    })
}
