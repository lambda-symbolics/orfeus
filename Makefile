LIGHTFAST ?= ../lightfast
FLTK_CONFIG ?= fltk-config
CXX ?= c++
GUI_PREVIEW := native/build/liborfeus_gui_preview.so
GUI_PREVIEW_SRC := native/gui-preview.cpp
GUI_CXXFLAGS := -std=c++17 -fPIC -O2 -Wall -Wextra $(shell $(FLTK_CONFIG) --cxxflags)
GUI_LDFLAGS := -shared $(shell $(FLTK_CONFIG) --use-images --ldflags) -L$(LIGHTFAST)/build -llightfast -Wl,-rpath,$(abspath $(LIGHTFAST)/build)

.PHONY: gui-native clean-gui-native

gui-native: $(GUI_PREVIEW)

$(GUI_PREVIEW): $(GUI_PREVIEW_SRC) $(LIGHTFAST)/native/cl_fltk_bridge.hpp $(LIGHTFAST)/build/liblightfast.so
	mkdir -p native/build
	$(CXX) $(GUI_CXXFLAGS) $< -o $@ $(GUI_LDFLAGS)

# Named prerequisites, not just the file: a rule with none is considered current
# the moment the file exists, however old it is against the sources beside it.
LIGHTFAST_SOURCES := $(wildcard $(LIGHTFAST)/native/*)

$(LIGHTFAST)/build/liblightfast.so: $(LIGHTFAST_SOURCES)
	$(MAKE) -C $(LIGHTFAST) native

clean-gui-native:
	rm -rf native/build
