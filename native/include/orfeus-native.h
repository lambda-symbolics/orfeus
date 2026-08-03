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
    ORFEUS_RENDER_LENS_TUNING_AND_OVERRIDES = 16,
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
    float tone_blacks;
    float tone_shadows;
    float tone_dark_mids;
    float tone_midtones;
    float tone_light_mids;
    float tone_highlights;
    float tone_whites;
    float lut_strength;
    float grain_amount;
    float grain_size;
    uint64_t grain_seed;
    uint32_t max_width;
    uint32_t max_height;
    uint32_t jpeg_quality;
    float lens_correction_strength;
    float focal_reducer;
    float lens_crop_factor;
    /* String pointers must remain readable NUL-terminated UTF-8 for the complete
       orfeus_raw_render_v1 call. lens_profile_model may be NULL for automatic
       camera/mount matching. */
    const char *lut_path;
    const char *lens_profile_model;
    /* Since settings version 3: FFDNet neural denoiser strength in [0, 1]. */
    float neural_noise_reduction;
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

enum orfeus_render_cache_mode {
    ORFEUS_RENDER_CACHE_NONE = 0,
    ORFEUS_RENDER_CACHE_USE = 1
};

enum orfeus_graph_node_kind {
    ORFEUS_NODE_WHITE_BALANCE = 1,
    ORFEUS_NODE_EXPOSURE = 2,
    ORFEUS_NODE_NOISE_REDUCTION = 3,
    ORFEUS_NODE_TONE = 4,
    ORFEUS_NODE_OPTICS = 5,
    ORFEUS_NODE_FILM = 6,
    ORFEUS_NODE_BLEND = 7
};

struct orfeus_render_frame_v1 {
    uint32_t struct_size;
    uint32_t version; /* 1 */
    uint32_t output_format;
    uint32_t max_width;
    uint32_t max_height;
    uint32_t jpeg_quality;
    uint64_t grain_seed;
    float focal_reducer;
    float lens_crop_factor;
    /* NULL for automatic camera/mount matching; must stay readable for the
       complete call. */
    const char *lens_profile_model;
};

/* Renders through a little-endian node program: magic "ORFG" (0x4746524F),
   format version 1, node count, then per node: kind u32, input_a i32,
   input_b i32 (-1 unless blend), param count u32, params f32[], string
   length u32 + UTF-8 bytes (film LUT path only). Inputs reference earlier
   node ordinals; 0 is the decoded source. Blend nodes mix scene-linear
   branches; film nodes convert their branch to display space and only film
   nodes may consume them. Orientation, ICC, and atomic publish semantics
   match orfeus_raw_render_v1; cache semantics match orfeus_raw_render_v2. */
int32_t orfeus_raw_render_v3(const char *input_path,
                             const char *output_path,
                             const struct orfeus_render_frame_v1 *frame,
                             const uint8_t *graph,
                             size_t graph_length,
                             uint32_t cache_mode,
                             char *error_buffer,
                             size_t error_capacity);

/* Renders like orfeus_raw_render_v1. Cache mode ORFEUS_RENDER_CACHE_USE keeps
   a small in-process cache of decoded scene-linear images keyed by input path,
   size, and modification time so interactive setting changes skip RAW
   decoding. Batch work should pass ORFEUS_RENDER_CACHE_NONE. */
int32_t orfeus_raw_render_v2(const char *input_path,
                             const char *output_path,
                             const struct orfeus_render_settings_v1 *settings,
                             uint32_t cache_mode,
                             char *error_buffer,
                             size_t error_capacity);

/* Detects a scanned negative's central tile and film-base color. Writes
   seven floats: crop left, top, width, height in oriented normalized
   coordinates, then the scene-linear base red, green, blue. Falls back to
   the full frame when no tile is plausible. */
int32_t orfeus_analyze_negative_frame_v1(const char *input_path,
                                         uint32_t cache_mode,
                                         float *results,
                                         char *error_buffer,
                                         size_t error_capacity);

/* Averages scene-linear color around an oriented normalized point into
   three floats; radius is normalized to the shorter edge, at most 0.25. */
int32_t orfeus_sample_linear_v1(const char *input_path,
                                uint32_t cache_mode,
                                float x,
                                float y,
                                float radius,
                                float *rgb,
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
