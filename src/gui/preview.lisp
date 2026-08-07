(in-package #:orfeus/gui)

(define-condition gui-preview-library-unavailable (error)
  ((path :initarg :path :reader gui-preview-library-path))
  (:report (lambda (condition stream)
             (format stream "Orfeus GUI preview library is missing at ~A. Run `make gui-native`."
                     (gui-preview-library-path condition)))))

(defvar *gui-preview-library-loaded-p* nil)

(defun gui-preview-library-pathname ()
  (merge-pathnames "native/build/liborfeus_gui_preview.so"
                   (asdf:system-source-directory :orfeus/gui)))

(defun load-gui-preview-library ()
  (unless *gui-preview-library-loaded-p*
    (lightfast:load-library)
    (let ((path (gui-preview-library-pathname)))
      (unless (probe-file path)
        (error 'gui-preview-library-unavailable :path path))
      (cffi:load-foreign-library path)
      (setf *gui-preview-library-loaded-p* t))))

(cffi:defcfun ("orfeus_gui_preview_draw" %gui-preview-draw) :int
  (widget-id :long-long) (path :string)
  (zoom :double) (center-x :double) (center-y :double))
(cffi:defcfun ("orfeus_gui_preview_draw_rect" %gui-preview-draw-rect) :int
  (widget-id :long-long) (path :string)
  (x :int) (y :int) (width :int) (height :int))
(cffi:defcfun ("orfeus_gui_preview_size" %gui-preview-size) :int
  (path :string) (width :pointer) (height :pointer))
(cffi:defcfun ("orfeus_gui_preview_histogram" %gui-preview-histogram) :int
  (path :string) (bins :pointer) (bin-count :int))
(cffi:defcfun ("orfeus_gui_preview_scopes" %gui-preview-scopes) :int
  (path :string) (bins :pointer) (bin-count :int)
  (rgb :pointer) (columns :int) (levels :int))
(cffi:defcfun ("orfeus_gui_preview_draw_rgb_rect" %gui-preview-draw-rgb-rect) :int
  (widget-id :long-long) (rgb :pointer) (width :int) (height :int)
  (x :int) (y :int) (rect-width :int) (rect-height :int))
(cffi:defcfun ("orfeus_gui_preview_draw_buffer" %gui-preview-draw-buffer) :int
  (widget-id :long-long) (rgb :pointer) (width :int) (height :int)
  (generation :int) (zoom :double) (center-x :double) (center-y :double))
(cffi:defcfun ("orfeus_gui_preview_forget" %gui-preview-forget) :void
  (path :string))
(cffi:defcfun ("orfeus_gui_preview_clear" %gui-preview-clear) :void)
(cffi:defcfun ("orfeus_gui_preview_set_cursor" %gui-preview-set-cursor) :int
  (widget-id :long-long) (cursor :int))
(defmacro with-preview-float-traps (&body body)
  "Run BODY with the float traps SBCL arms by default disabled.

Every call below crosses into the FLTK preview adapter, which computes scale
factors from image and widget dimensions. A dimension is briefly zero whenever a
preview is being replaced — adding a graph node bumps the preview generation and
drops the live buffer, so a redraw can land in exactly that window — and the
resulting division raised SIGFPE in foreign code, which SBCL cannot handle and
which killed the process outright. The C++ is entitled to produce an infinity
here; what it must not do is trap. Mirrors NATIVE.LISP, where the same rule
already applies to the render library."
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow
                                    :underflow :inexact)
     ,@body))

(defun choose-photo-files (&key (title "Open RAW photographs")
                                (filter "") (preset-path ""))
  "Return all photo pathnames selected by the canonical FLTK file chooser."
  ;; CHOOSE-FILES already returns an empty list when nothing is picked, so
  ;; there is nothing for an OR fallback to catch.
  (mapcar #'pathname
          (lightfast:choose-files :title title
                                  :filter filter
                                  :preset-file preset-path)))

(defun forget-preview-file (pathname)
  "Evict PATHNAME from the native preview cache after it is overwritten."
  (when *gui-preview-library-loaded-p*
    (with-preview-float-traps
      (%gui-preview-forget (namestring pathname)))))

(defun clear-preview-cache ()
  "Release all decoded images held by the native preview adapter."
  (when *gui-preview-library-loaded-p*
    (with-preview-float-traps (%gui-preview-clear))))

(defun preview-histogram (pathname &key (bins 64))
  "Return three vectors of BINS counts (red, green, blue) for PATHNAME."
  (load-gui-preview-library)
  (cffi:with-foreign-object (buffer :int (* 3 bins))
    (when (plusp (with-preview-float-traps
                   (%gui-preview-histogram (namestring pathname) buffer bins)))
      (flet ((plane (channel)
               (let ((vector (make-array bins)))
                 (dotimes (index bins vector)
                   (setf (aref vector index)
                         (cffi:mem-aref buffer :int
                                        (+ (* channel bins) index)))))))
        (values (plane 0) (plane 1) (plane 2))))))

(defparameter *waveform-columns* 256
  "Column bands across the frame's width in the waveform scope.")

(defparameter *waveform-levels* 128
  "Level rows in the waveform scope, row zero being the brightest.")

(defun waveform-buffer-size ()
  (* *waveform-columns* *waveform-levels* 3))

(defun allocate-waveform-buffer ()
  "Allocate one reusable RGB8 image for the waveform scope."
  (cffi:foreign-alloc :unsigned-char :count (waveform-buffer-size)
                                     :initial-element 0))

(defun preview-scopes (pathname buffer &key (bins 64))
  "Fill BUFFER with PATHNAME's waveform image; return its histogram planes.

BUFFER must hold WAVEFORM-BUFFER-SIZE bytes. Returns the red, green, and
blue level histograms, or NIL when the preview cannot be read."
  (load-gui-preview-library)
  (cffi:with-foreign-object (counts :int (* 3 bins))
    (when (plusp (with-preview-float-traps
                   (%gui-preview-scopes (namestring pathname) counts bins
                                        buffer *waveform-columns*
                                        *waveform-levels*)))
      (flet ((plane (channel)
               (let ((vector (make-array bins)))
                 (dotimes (index bins vector)
                   (setf (aref vector index)
                         (cffi:mem-aref counts :int
                                        (+ (* channel bins) index)))))))
        (values (plane 0) (plane 1) (plane 2))))))

(defun draw-waveform (canvas buffer x y width height)
  "Blit the waveform image in BUFFER into CANVAS's absolute rectangle."
  (load-gui-preview-library)
  (plusp (with-preview-float-traps
           (%gui-preview-draw-rgb-rect (lightfast:widget-id canvas) buffer
                                       *waveform-columns* *waveform-levels*
                                       x y width height))))

(defun preview-file-size (pathname)
  "Return the decoded width and height of preview PATHNAME."
  (load-gui-preview-library)
  (cffi:with-foreign-objects ((width :int) (height :int))
    (when (plusp (with-preview-float-traps
                   (%gui-preview-size (namestring pathname) width height)))
      (values (cffi:mem-ref width :int)
              (cffi:mem-ref height :int)))))

(defun draw-preview-file (canvas pathname &key (zoom 1d0)
                                                (center-x .5d0)
                                                (center-y .5d0))
  "Draw PATHNAME in CANVAS at relative ZOOM around normalized source center."
  (load-gui-preview-library)
  (plusp (with-preview-float-traps
           (%gui-preview-draw (lightfast:widget-id canvas) (namestring pathname)
                              zoom center-x center-y))))

(defun draw-preview-buffer (canvas pointer width height generation
                            &key (zoom 1d0) (center-x .5d0) (center-y .5d0))
  "Draw the borrowed live RGB8 buffer in CANVAS with fit, zoom, and pan.

GENERATION keys the cached scaled copy; bump it whenever the buffer's
contents change."
  (load-gui-preview-library)
  (plusp (with-preview-float-traps
           (%gui-preview-draw-buffer (lightfast:widget-id canvas) pointer
                                     width height generation
                                     zoom center-x center-y))))

(defparameter *cursor-codes*
  ;; FLTK's Fl_Cursor values; only the two this application switches between.
  '((:default . 0) (:cross . 66))
  "Mouse cursor shapes, by FLTK's own numbering.")

(defun set-widget-cursor (widget shape)
  "Set the mouse cursor over WIDGET's window to SHAPE; true when it took."
  (load-gui-preview-library)
  (let ((code (rest (assoc shape *cursor-codes*))))
    (when code
      (plusp (with-preview-float-traps
               (%gui-preview-set-cursor (lightfast:widget-id widget) code))))))

(defun draw-thumbnail-file (canvas pathname x y width height)
  "Draw PATHNAME fitted inside the absolute rectangle in CANVAS."
  (load-gui-preview-library)
  (plusp (with-preview-float-traps
           (%gui-preview-draw-rect (lightfast:widget-id canvas)
                                   (namestring pathname)
                                   x y width height))))
