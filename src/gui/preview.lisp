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
  (widget-id :long-long) (path :string))
(cffi:defcfun ("orfeus_gui_preview_forget" %gui-preview-forget) :void
  (path :string))
(cffi:defcfun ("orfeus_gui_preview_clear" %gui-preview-clear) :void)
(cffi:defcfun ("orfeus_gui_choose_files" %gui-choose-files) :pointer
  (title :string) (filter :string) (preset-path :string))
(cffi:defcfun ("orfeus_gui_string_free" %gui-string-free) :void
  (value :pointer))

(defun choose-photo-files (&key (title "Open RAW photographs")
                                (filter "") (preset-path ""))
  "Return all photo pathnames selected by the native multi-file chooser."
  (load-gui-preview-library)
  (let ((pointer (%gui-choose-files title filter preset-path)))
    (unless (cffi:null-pointer-p pointer)
      (unwind-protect
           (mapcar #'pathname
                   (uiop:split-string (cffi:foreign-string-to-lisp pointer)
                                      :separator '(#\Newline)))
        (%gui-string-free pointer)))))

(defun forget-preview-file (pathname)
  "Evict PATHNAME from the native preview cache after it is overwritten."
  (when *gui-preview-library-loaded-p*
    (%gui-preview-forget (namestring pathname))))

(defun clear-preview-cache ()
  "Release all decoded images held by the native preview adapter."
  (when *gui-preview-library-loaded-p*
    (%gui-preview-clear)))

(defun draw-preview-file (canvas pathname)
  (load-gui-preview-library)
  (plusp (%gui-preview-draw (cl-fltk:widget-id canvas) (namestring pathname))))
