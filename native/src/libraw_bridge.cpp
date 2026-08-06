#include <libraw/libraw.h>

#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <exception>

extern "C" {

struct OrfeusLibRawImage {
  std::size_t width;
  std::size_t height;
  const std::uint16_t *data;
  void *handle;
};

static void orfeus_libraw_set_error(char *buffer, std::size_t capacity,
                                    const char *message) {
  if (!buffer || capacity == 0) {
    return;
  }
  std::snprintf(buffer, capacity, "%s", message ? message : "unknown LibRaw error");
}

int orfeus_libraw_decode_linear_srgb(const char *path, int half_size,
                                     OrfeusLibRawImage *output,
                                     char *error, std::size_t error_capacity) {
  if (!path || !output) {
    orfeus_libraw_set_error(error, error_capacity, "invalid LibRaw decode arguments");
    return -1;
  }
  std::memset(output, 0, sizeof(*output));
  try {
    LibRaw processor;
    auto &params = processor.imgdata.params;
    params.use_camera_wb = 1;
    params.use_auto_wb = 0;
    params.output_color = 1; // linear sRGB after the gamma settings below
    params.output_bps = 16;
    params.no_auto_bright = 1;
    params.gamm[0] = 1.0;
    params.gamm[1] = 1.0;
    params.user_flip = 0; // Orfeus applies EXIF orientation later.
    params.half_size = half_size ? 1 : 0;

    int status = processor.open_file(path);
    if (status != LIBRAW_SUCCESS) {
      orfeus_libraw_set_error(error, error_capacity, libraw_strerror(status));
      return status;
    }
    status = processor.unpack();
    if (status != LIBRAW_SUCCESS) {
      orfeus_libraw_set_error(error, error_capacity, libraw_strerror(status));
      return status;
    }
    status = processor.dcraw_process();
    if (status != LIBRAW_SUCCESS) {
      orfeus_libraw_set_error(error, error_capacity, libraw_strerror(status));
      return status;
    }
    int image_status = LIBRAW_SUCCESS;
    libraw_processed_image_t *image = processor.dcraw_make_mem_image(&image_status);
    if (!image || image_status != LIBRAW_SUCCESS) {
      orfeus_libraw_set_error(error, error_capacity, libraw_strerror(image_status));
      if (image) {
        LibRaw::dcraw_clear_mem(image);
      }
      return image_status == LIBRAW_SUCCESS ? -1 : image_status;
    }
    if (image->type != LIBRAW_IMAGE_BITMAP || image->colors != 3 || image->bits != 16) {
      orfeus_libraw_set_error(error, error_capacity,
                              "LibRaw did not return a 16-bit RGB bitmap");
      LibRaw::dcraw_clear_mem(image);
      return -1;
    }
    output->width = image->width;
    output->height = image->height;
    output->data = reinterpret_cast<const std::uint16_t *>(image->data);
    output->handle = image;
    return 0;
  } catch (const std::exception &condition) {
    orfeus_libraw_set_error(error, error_capacity, condition.what());
    return -1;
  } catch (...) {
    orfeus_libraw_set_error(error, error_capacity, "unknown LibRaw exception");
    return -1;
  }
}

void orfeus_libraw_free_image(void *handle) {
  if (handle) {
    LibRaw::dcraw_clear_mem(
        static_cast<libraw_processed_image_t *>(handle));
  }
}

} // extern "C"
