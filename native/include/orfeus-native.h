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
    ORFEUS_STATUS_PANIC = 255
};

uint32_t orfeus_bridge_abi_version(void);

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
