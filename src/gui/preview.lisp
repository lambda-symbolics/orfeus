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
(cffi:defcfun ("orfeus_gui_preview_clear" %gui-preview-clear) :void)

(defun draw-preview-file (canvas pathname)
  (load-gui-preview-library)
  (plusp (%gui-preview-draw (cl-fltk:widget-id canvas) (namestring pathname))))
