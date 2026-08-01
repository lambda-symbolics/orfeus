(in-package #:orfeus)

(defvar *native-library* nil
  "The loaded CFFI library handle for the Orfeus Rust bridge.")

(defcfun ("orfeus_bridge_abi_version" %native-bridge-abi-version) :uint32)

(defcfun ("orfeus_dng_original_filename" %dng-original-filename) :int32
  (dng-path :string)
  (filename-buffer :pointer)
  (filename-capacity :size)
  (error-buffer :pointer)
  (error-capacity :size))

(defcfun ("orfeus_dng_extract_original" %dng-extract-original) :int32
  (dng-path :string)
  (output-path :string)
  (error-buffer :pointer)
  (error-capacity :size))

(defparameter *native-error-buffer-size* 1024
  "Bytes reserved for a diagnostic returned by the Rust bridge.")

(defparameter *native-filename-buffer-size* 4096
  "Bytes reserved for an embedded original filename.")

(defun native-library-candidates ()
  (remove nil
          (list (uiop:getenv "ORFEUS_NATIVE_LIBRARY")
                (namestring
                 (asdf:system-relative-pathname
                  '#:orfeus "native/target/release/liborfeus_native.so"))
                (namestring
                 (asdf:system-relative-pathname
                  '#:orfeus "native/target/debug/liborfeus_native.so")))
))

(defun native-library-load ()
  (or *native-library*
      (let ((paths (native-library-candidates))
            (last-cause "no candidate exists"))
        (dolist (path paths)
          (when (probe-file path)
            (handler-case
                (return-from native-library-load
                  (setf *native-library* (load-foreign-library path)))
              (error (condition)
                (setf last-cause condition)))))
        (error 'native-library-unavailable
               :paths paths
               :cause last-cause))))

(defun native-bridge-available-p ()
  "Return true when the Rust native bridge can be loaded."
  (handler-case
      (progn
        (native-library-load)
        t)
    (native-library-unavailable () nil)))

(defun native-bridge-version ()
  "Return the loaded Rust bridge ABI version."
  (native-library-load)
  (%native-bridge-abi-version))

(defun native-error-message (buffer)
  (foreign-string-to-lisp buffer :encoding :utf-8))

(defun dng-original-filename (pathname)
  "Return the embedded original filename recorded by DNG PATHNAME."
  (native-library-load)
  (with-foreign-pointer (filename-buffer *native-filename-buffer-size*)
    (with-foreign-pointer (error-buffer *native-error-buffer-size*)
      (let ((status (%dng-original-filename
                     (namestring pathname)
                     filename-buffer *native-filename-buffer-size*
                     error-buffer *native-error-buffer-size*)))
        (unless (zerop status)
          (error 'dng-original-error
                 :pathname pathname
                 :status status
                 :message (native-error-message error-buffer)))
        (foreign-string-to-lisp filename-buffer :encoding :utf-8)))))

(defun dng-extract-original (dng-pathname output-pathname
                             &key (if-exists :error))
  "Extract and digest-verify the original RAW from DNG-PATHNAME.

OUTPUT-PATHNAME is never the input file. IF-EXISTS accepts :ERROR or
:SUPERSEDE. The output is written only after successful decompression and
verification by the Rust bridge."
  (when (probe-file output-pathname)
    (ecase if-exists
      (:error (restart-case
                  (error 'output-file-exists :pathname output-pathname)
                (overwrite ()
                  :report "Replace the existing output file.")))
      (:supersede nil)))
  (native-library-load)
  (with-foreign-pointer (error-buffer *native-error-buffer-size*)
    (let ((status (%dng-extract-original
                   (namestring dng-pathname)
                   (namestring output-pathname)
                   error-buffer *native-error-buffer-size*)))
      (unless (zerop status)
        (error 'dng-original-error
               :pathname dng-pathname
               :output-pathname output-pathname
               :status status
               :message (native-error-message error-buffer)))))
  output-pathname)
