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
    (cl-fltk:load-library)
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
(cffi:defcfun ("orfeus_gui_preview_forget" %gui-preview-forget) :void
  (path :string))
(cffi:defcfun ("orfeus_gui_preview_clear" %gui-preview-clear) :void)
(defun choose-photo-files (&key (title "Open RAW photographs")
                                (filter "") (preset-path ""))
  "Return all photo pathnames selected by the canonical FLTK file chooser."
  (mapcar #'pathname
          (or (cl-fltk:choose-files :title title
                                    :filter filter
                                    :preset-file preset-path)
              '())))

(defun forget-preview-file (pathname)
  "Evict PATHNAME from the native preview cache after it is overwritten."
  (when *gui-preview-library-loaded-p*
    (%gui-preview-forget (namestring pathname))))

(defun clear-preview-cache ()
  "Release all decoded images held by the native preview adapter."
  (when *gui-preview-library-loaded-p*
    (%gui-preview-clear)))

(defun preview-file-size (pathname)
  "Return the decoded width and height of preview PATHNAME."
  (load-gui-preview-library)
  (cffi:with-foreign-objects ((width :int) (height :int))
    (when (plusp (%gui-preview-size (namestring pathname) width height))
      (values (cffi:mem-ref width :int)
              (cffi:mem-ref height :int)))))

(defun draw-preview-file (canvas pathname &key (zoom 1d0)
                                                (center-x .5d0)
                                                (center-y .5d0))
  "Draw PATHNAME in CANVAS at relative ZOOM around normalized source center."
  (load-gui-preview-library)
  (plusp (%gui-preview-draw (cl-fltk:widget-id canvas) (namestring pathname)
                            zoom center-x center-y)))

(defun draw-thumbnail-file (canvas pathname x y width height)
  "Draw PATHNAME fitted inside the absolute rectangle in CANVAS."
  (load-gui-preview-library)
  (plusp (%gui-preview-draw-rect (cl-fltk:widget-id canvas) (namestring pathname)
                                 x y width height)))
