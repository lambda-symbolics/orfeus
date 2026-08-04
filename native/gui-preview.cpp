#include <FL/Fl.H>
#include <FL/Fl_Image.H>
#include <FL/Fl_JPEG_Image.H>
#include <FL/Fl_Native_File_Chooser.H>
#include <FL/Fl_Widget.H>
#include <FL/fl_draw.H>
#include <algorithm>
#include <cmath>
#include <climits>
#include <csetjmp>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <jpeglib.h>
#include <memory>
#include <mutex>
#include <string>
#include <unordered_map>

namespace clfl_bridge {
Fl_Widget *find_widget(long long id);
}

namespace {
struct PreviewImage {
    std::unique_ptr<Fl_JPEG_Image> source;
    std::unique_ptr<Fl_Image> scaled;
    int scaled_width = 0;
    int scaled_height = 0;
    std::unique_ptr<Fl_Image> thumbnail;
    int thumbnail_width = 0;
    int thumbnail_height = 0;
};

std::unordered_map<std::string, PreviewImage> images;
// Native callbacks can load, draw, inspect, or evict the same FLTK image.
// Serialize both the ownership map and access to its image objects.
std::mutex images_mutex;

PreviewImage *find_image(const char *path) {
    if (!path || !*path) return nullptr;
    auto found = images.find(path);
    if (found == images.end()) {
        PreviewImage image;
        image.source = std::make_unique<Fl_JPEG_Image>(path);
        if (image.source->fail() || image.source->w() <= 0 || image.source->h() <= 0) {
            return nullptr;
        }
        found = images.emplace(path, std::move(image)).first;
    }
    return &found->second;
}

int scaled_dimension(int source, double scale) {
    return std::max(1, static_cast<int>(std::lround(source * scale)));
}

struct JpegErrorManager {
    jpeg_error_mgr base;
    std::jmp_buf jump_buffer;
};

void jpeg_error_exit(j_common_ptr info) {
    auto *error = reinterpret_cast<JpegErrorManager *>(info->err);
    std::longjmp(error->jump_buffer, 1);
}
}

extern "C" int orfeus_gui_preview_draw(long long widget_id,
                                        const char *path,
                                        double zoom,
                                        double center_x,
                                        double center_y) {
    std::lock_guard<std::mutex> lock(images_mutex);
    Fl_Widget *widget = clfl_bridge::find_widget(widget_id);
    PreviewImage *image = find_image(path);
    if (!widget || !image || widget->w() <= 0 || widget->h() <= 0 ||
        !std::isfinite(zoom) || !std::isfinite(center_x) || !std::isfinite(center_y)) {
        return 0;
    }

    Fl_JPEG_Image &source = *image->source;
    const double fit_scale = std::min(static_cast<double>(widget->w()) / source.w(),
                                      static_cast<double>(widget->h()) / source.h());
    const double maximum_scale = std::max(fit_scale, 2.0);
    const double effective_scale = std::clamp(fit_scale * std::max(1.0, zoom),
                                              fit_scale, maximum_scale);
    const int width = scaled_dimension(source.w(), effective_scale);
    const int height = scaled_dimension(source.h(), effective_scale);
    if (!image->scaled || image->scaled_width != width || image->scaled_height != height) {
        image->scaled.reset(source.copy(width, height));
        if (!image->scaled) return 0;
        image->scaled_width = width;
        image->scaled_height = height;
    }

    const double visible_x = std::min(1.0, static_cast<double>(widget->w()) / width);
    const double visible_y = std::min(1.0, static_cast<double>(widget->h()) / height);
    const double clamped_x = std::clamp(center_x, visible_x / 2.0, 1.0 - visible_x / 2.0);
    const double clamped_y = std::clamp(center_y, visible_y / 2.0, 1.0 - visible_y / 2.0);
    const int x = widget->x() + widget->w() / 2 -
                  static_cast<int>(std::lround(clamped_x * width));
    const int y = widget->y() + widget->h() / 2 -
                  static_cast<int>(std::lround(clamped_y * height));
    image->scaled->draw(x, y);
    return 1;
}

namespace {
// The live preview: one borrowed RGB8 buffer drawn without any JPEG or
// file round trip. The scaled copy is cached per generation and size.
struct LiveView {
    std::unique_ptr<Fl_Image> scaled;
    int generation = -1;
    int scaled_width = 0;
    int scaled_height = 0;
};
LiveView live_view;
}

extern "C" int orfeus_gui_preview_draw_buffer(long long widget_id,
                                              const unsigned char *rgb,
                                              int width,
                                              int height,
                                              int generation,
                                              double zoom,
                                              double center_x,
                                              double center_y) {
    std::lock_guard<std::mutex> lock(images_mutex);
    Fl_Widget *widget = clfl_bridge::find_widget(widget_id);
    if (!widget || !rgb || width <= 0 || height <= 0 ||
        widget->w() <= 0 || widget->h() <= 0 ||
        !std::isfinite(zoom) || !std::isfinite(center_x) ||
        !std::isfinite(center_y)) {
        return 0;
    }

    const double fit_scale = std::min(static_cast<double>(widget->w()) / width,
                                      static_cast<double>(widget->h()) / height);
    const double maximum_scale = std::max(fit_scale, 2.0);
    const double effective_scale = std::clamp(fit_scale * std::max(1.0, zoom),
                                              fit_scale, maximum_scale);
    const int scaled_width = scaled_dimension(width, effective_scale);
    const int scaled_height = scaled_dimension(height, effective_scale);
    if (!live_view.scaled || live_view.generation != generation ||
        live_view.scaled_width != scaled_width ||
        live_view.scaled_height != scaled_height) {
        Fl_RGB_Image source(rgb, width, height, 3, 0);
        live_view.scaled.reset(source.copy(scaled_width, scaled_height));
        if (!live_view.scaled) return 0;
        live_view.generation = generation;
        live_view.scaled_width = scaled_width;
        live_view.scaled_height = scaled_height;
    }

    const double visible_x =
        std::min(1.0, static_cast<double>(widget->w()) / scaled_width);
    const double visible_y =
        std::min(1.0, static_cast<double>(widget->h()) / scaled_height);
    const double clamped_x =
        std::clamp(center_x, visible_x / 2.0, 1.0 - visible_x / 2.0);
    const double clamped_y =
        std::clamp(center_y, visible_y / 2.0, 1.0 - visible_y / 2.0);
    const int x = widget->x() + widget->w() / 2 -
                  static_cast<int>(std::lround(clamped_x * scaled_width));
    const int y = widget->y() + widget->h() / 2 -
                  static_cast<int>(std::lround(clamped_y * scaled_height));
    live_view.scaled->draw(x, y);
    return 1;
}

extern "C" int orfeus_gui_preview_draw_rect(long long widget_id,
                                               const char *path,
                                               int x,
                                               int y,
                                               int width,
                                               int height) {
    std::lock_guard<std::mutex> lock(images_mutex);
    Fl_Widget *widget = clfl_bridge::find_widget(widget_id);
    PreviewImage *image = find_image(path);
    if (!widget || !image || width <= 0 || height <= 0) return 0;

    Fl_JPEG_Image &source = *image->source;
    const double scale = std::min(static_cast<double>(width) / source.w(),
                                  static_cast<double>(height) / source.h());
    const int scaled_width = scaled_dimension(source.w(), scale);
    const int scaled_height = scaled_dimension(source.h(), scale);
    if (!image->thumbnail || image->thumbnail_width != scaled_width ||
        image->thumbnail_height != scaled_height) {
        image->thumbnail.reset(source.copy(scaled_width, scaled_height));
        if (!image->thumbnail) return 0;
        image->thumbnail_width = scaled_width;
        image->thumbnail_height = scaled_height;
    }

    const int draw_x = x + (width - scaled_width) / 2;
    const int draw_y = y + (height - scaled_height) / 2;
    const int clip_x = std::max(x, widget->x());
    const int clip_y = std::max(y, widget->y());
    const int clip_right = std::min(x + width, widget->x() + widget->w());
    const int clip_bottom = std::min(y + height, widget->y() + widget->h());
    if (clip_right <= clip_x || clip_bottom <= clip_y) return 0;
    fl_push_clip(clip_x, clip_y, clip_right - clip_x, clip_bottom - clip_y);
    image->thumbnail->draw(draw_x, draw_y);
    fl_pop_clip();
    return 1;
}

extern "C" int orfeus_gui_preview_histogram(const char *path,
                                            int *bins,
                                            int bin_count) {
    if (!path || !*path || !bins || bin_count <= 0 || bin_count > INT_MAX / 3) {
        return 0;
    }

    FILE *file = std::fopen(path, "rb");
    if (!file) return 0;
    auto *decoder = static_cast<jpeg_decompress_struct *>(
        std::calloc(1, sizeof(jpeg_decompress_struct)));
    auto *error = static_cast<JpegErrorManager *>(
        std::calloc(1, sizeof(JpegErrorManager)));
    if (!decoder || !error) {
        std::free(decoder);
        std::free(error);
        std::fclose(file);
        return 0;
    }

    decoder->err = jpeg_std_error(&error->base);
    error->base.error_exit = jpeg_error_exit;
    if (setjmp(error->jump_buffer)) {
        jpeg_destroy_decompress(decoder);
        std::free(decoder);
        std::free(error);
        std::fclose(file);
        return 0;
    }

    jpeg_create_decompress(decoder);
    jpeg_stdio_src(decoder, file);
    if (jpeg_read_header(decoder, TRUE) != JPEG_HEADER_OK) {
        jpeg_destroy_decompress(decoder);
        std::free(decoder);
        std::free(error);
        std::fclose(file);
        return 0;
    }
    decoder->out_color_space = JCS_RGB;
    jpeg_start_decompress(decoder);
    if (decoder->output_components != 3) {
        jpeg_destroy_decompress(decoder);
        std::free(decoder);
        std::free(error);
        std::fclose(file);
        return 0;
    }

    const JDIMENSION row_size = decoder->output_width * decoder->output_components;
    JSAMPARRAY row = (*decoder->mem->alloc_sarray)(
        reinterpret_cast<j_common_ptr>(decoder), JPOOL_IMAGE, row_size, 1);
    const size_t plane_size = static_cast<size_t>(bin_count);
    std::fill(bins, bins + 3 * plane_size, 0);
    while (decoder->output_scanline < decoder->output_height) {
        jpeg_read_scanlines(decoder, row, 1);
        for (JDIMENSION x = 0; x < decoder->output_width; ++x) {
            const unsigned char *pixel = row[0] + x * decoder->output_components;
            for (size_t channel = 0; channel < 3; ++channel) {
                const size_t bin = static_cast<size_t>(pixel[channel]) * plane_size / 256;
                int &count = bins[channel * plane_size + bin];
                if (count < INT_MAX) ++count;
            }
        }
    }

    jpeg_finish_decompress(decoder);
    jpeg_destroy_decompress(decoder);
    std::free(decoder);
    std::free(error);
    std::fclose(file);
    return 1;
}

extern "C" int orfeus_gui_preview_size(const char *path, int *width, int *height) {
    std::lock_guard<std::mutex> lock(images_mutex);
    PreviewImage *image = find_image(path);
    if (!image || !width || !height) return 0;
    *width = image->source->w();
    *height = image->source->h();
    return 1;
}

extern "C" void orfeus_gui_preview_forget(const char *path) {
    std::lock_guard<std::mutex> lock(images_mutex);
    if (path && *path) images.erase(path);
}

extern "C" void orfeus_gui_preview_clear(void) {
    std::lock_guard<std::mutex> lock(images_mutex);
    images.clear();
}

extern "C" char *orfeus_gui_choose_files(const char *title,
                                           const char *filter,
                                           const char *preset_path) {
    // Avoid FLTK 1.4's optional GTK chooser driver, which can raise SIGFPE
    // while its nested GTK loop remains open. The built-in FLTK driver keeps
    // multi-selection and filter semantics without entering GTK.
    Fl::option(Fl::OPTION_FNFC_USES_GTK, false);
    Fl_Native_File_Chooser chooser(Fl_Native_File_Chooser::BROWSE_MULTI_FILE);
    if (title && *title) chooser.title(title);
    if (filter && *filter) chooser.filter(filter);
    if (preset_path && *preset_path) chooser.preset_file(preset_path);
    if (chooser.show() != 0 || chooser.count() <= 0) return nullptr;

    std::string result;
    for (int index = 0; index < chooser.count(); ++index) {
        const char *path = chooser.filename(index);
        if (!path || !*path) continue;
        if (!result.empty()) result.push_back('\n');
        result.append(path);
    }
    if (result.empty()) return nullptr;
    char *copy = static_cast<char *>(std::malloc(result.size() + 1));
    if (!copy) return nullptr;
    std::memcpy(copy, result.c_str(), result.size() + 1);
    return copy;
}

extern "C" void orfeus_gui_string_free(char *value) {
    std::free(value);
}
