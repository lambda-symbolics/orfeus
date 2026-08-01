#include <FL/Fl.H>
#include <FL/Fl_JPEG_Image.H>
#include <FL/Fl_Native_File_Chooser.H>
#include <FL/Fl_Widget.H>
#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <string>
#include <unordered_map>

namespace clfl_bridge {
Fl_Widget *find_widget(long long id);
}

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
    Fl_JPEG_Image &source = *found->second;
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

extern "C" char *orfeus_gui_choose_files(const char *title,
                                          const char *filter,
                                          const char *preset_path) {
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
