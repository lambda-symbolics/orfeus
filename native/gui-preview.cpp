#include <FL/Fl.H>
#include <FL/Fl_Image.H>
#include <FL/Fl_JPEG_Image.H>
#include <FL/Fl_Native_File_Chooser.H>
#include <FL/Fl_Widget.H>
#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <cstring>
#include <memory>
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
};

std::unordered_map<std::string, PreviewImage> images;

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
}

extern "C" int orfeus_gui_preview_draw(long long widget_id,
                                        const char *path,
                                        double zoom,
                                        double center_x,
                                        double center_y) {
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

extern "C" int orfeus_gui_preview_size(const char *path, int *width, int *height) {
    PreviewImage *image = find_image(path);
    if (!image || !width || !height) return 0;
    *width = image->source->w();
    *height = image->source->h();
    return 1;
}

extern "C" void orfeus_gui_preview_forget(const char *path) {
    if (path && *path) images.erase(path);
}

extern "C" void orfeus_gui_preview_clear(void) {
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
