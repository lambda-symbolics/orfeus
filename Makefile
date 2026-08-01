FLTK_SUN ?= ../fltk-sun
FLTK_CONFIG ?= fltk-config
CXX ?= c++
GUI_PREVIEW := native/build/liborfeus_gui_preview.so
GUI_PREVIEW_SRC := native/gui-preview.cpp
GUI_CXXFLAGS := -std=c++17 -fPIC -O2 -Wall -Wextra $(shell $(FLTK_CONFIG) --cxxflags)
GUI_LDFLAGS := -shared $(shell $(FLTK_CONFIG) --use-images --ldflags) -L$(FLTK_SUN)/build -lcl_fltk_bridge -Wl,-rpath,$(abspath $(FLTK_SUN)/build)

.PHONY: gui-native clean-gui-native

gui-native: $(GUI_PREVIEW)

$(GUI_PREVIEW): $(GUI_PREVIEW_SRC) $(FLTK_SUN)/native/cl_fltk_bridge.hpp $(FLTK_SUN)/build/libcl_fltk_bridge.so
	mkdir -p native/build
	$(CXX) $(GUI_CXXFLAGS) $< -o $@ $(GUI_LDFLAGS)

$(FLTK_SUN)/build/libcl_fltk_bridge.so:
	$(MAKE) -C $(FLTK_SUN) cl-fltk-bridge

clean-gui-native:
	rm -rf native/build
