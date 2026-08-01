#ifndef ORFEUS_NATIVE_H
#define ORFEUS_NATIVE_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

enum orfeus_status {
    ORFEUS_STATUS_OK = 0,
    ORFEUS_STATUS_INVALID_ARGUMENT = 1,
    ORFEUS_STATUS_IO_ERROR = 2,
    ORFEUS_STATUS_INVALID_DNG = 3,
    ORFEUS_STATUS_NO_ORIGINAL = 4,
    ORFEUS_STATUS_DECOMPRESSION_ERROR = 5,
    ORFEUS_STATUS_DIGEST_MISMATCH = 6,
    ORFEUS_STATUS_BUFFER_TOO_SMALL = 7,
    ORFEUS_STATUS_RENDER_ERROR = 8,
    ORFEUS_STATUS_LENS_PROFILE_UNAVAILABLE = 9,
    ORFEUS_STATUS_PANIC = 255
};

uint32_t orfeus_bridge_abi_version(void);

enum orfeus_render_format {
    ORFEUS_RENDER_JPEG = 1,
    ORFEUS_RENDER_TIFF = 2
};

enum orfeus_render_flags {
    ORFEUS_RENDER_LENS_DISTORTION = 1,
    ORFEUS_RENDER_LENS_TCA = 2
};

enum orfeus_render_capabilities_v1 {
    ORFEUS_RENDER_APPLIES_ORIENTATION = 1,
    ORFEUS_RENDER_TIFF_16_BIT = 2,
    ORFEUS_RENDER_EMBEDS_SRGB_ICC = 4,
    /* Deliberately absent: source EXIF/XMP metadata preservation. */
    ORFEUS_RENDER_PRESERVES_SOURCE_METADATA = 8
};

uint32_t orfeus_raw_render_capabilities_v1(void);

struct orfeus_render_settings_v1 {
    uint32_t struct_size;
    uint32_t version;
    uint32_t flags;
    uint32_t output_format;
    float kelvin; /* zero selects camera white balance */
    float tint;
    float exposure_ev;
    float chroma_noise_reduction;
    float luma_noise_reduction;
    float lut_strength;
    float grain_amount;
    float grain_size;
    uint64_t grain_seed;
    uint32_t max_width;
    uint32_t max_height;
    uint32_t jpeg_quality;
    /* When lut_strength is nonzero, this must remain a readable NUL-terminated
       UTF-8 path for the complete orfeus_raw_render_v1 call. */
    const char *lut_path;
};

/* Applies orientation and embeds sRGB ICC. TIFF output is true 16-bit RGB.
   Source EXIF/XMP is not preserved; query capabilities instead of claiming it.
   Unknown flag bits and all non-finite settings are invalid arguments.
   The destination must not identify the input through a path, symlink, or
   hardlink. Export uses a synced same-directory temporary and atomic rename. */
int32_t orfeus_raw_render_v1(const char *input_path,
                             const char *output_path,
                             const struct orfeus_render_settings_v1 *settings,
                             char *error_buffer,
                             size_t error_capacity);

int32_t orfeus_dng_original_filename(const char *dng_path,
                                     char *filename_buffer,
                                     size_t filename_capacity,
                                     char *error_buffer,
                                     size_t error_capacity);

int32_t orfeus_dng_extract_original(const char *dng_path,
                                    const char *output_path,
                                    char *error_buffer,
                                    size_t error_capacity);

#ifdef __cplusplus
}
#endif

#endif
