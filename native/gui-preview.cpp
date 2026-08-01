#include <FL/Fl.H>
#include <FL/Fl_JPEG_Image.H>
#include <FL/Fl_Widget.H>
#include <algorithm>
#include <memory>
#include <string>
#include <unordered_map>

#include "../../fltk-sun/native/cl_fltk_bridge.hpp"

namespace {
std::unordered_map<std::string, std::unique_ptr<Fl_JPEG_Image>> images;
}

extern "C" int orfeus_gui_preview_draw(long long widget_id, const char *path) {
    Fl_Widget *widget = clfl_bridge::find_widget(widget_id);
    if (!widget || !path || !*path) return 0;
    auto found = images.find(path);
    if (found == images.end()) {
        auto image = std::make_unique<Fl_JPEG_Image>(path);
        if (image->fail() || image->w() <= 0 || image->h() <= 0) return 0;
        found = images.emplace(path, std::move(image)).first;
    }
    const Fl_JPEG_Image &source = *found->second;
    const double scale = std::min(static_cast<double>(widget->w()) / source.w(),
                                  static_cast<double>(widget->h()) / source.h());
    const int width = std::max(1, static_cast<int>(source.w() * scale));
    const int height = std::max(1, static_cast<int>(source.h() * scale));
    std::unique_ptr<Fl_Image> scaled(source.copy(width, height));
    if (!scaled) return 0;
    scaled->draw(widget->x() + (widget->w() - width) / 2,
                 widget->y() + (widget->h() - height) / 2);
    return 1;
}

extern "C" void orfeus_gui_preview_forget(const char *path) {
    if (path && *path) images.erase(path);
}

extern "C" void orfeus_gui_preview_clear(void) {
    images.clear();
}
