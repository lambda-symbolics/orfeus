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

(defcstruct render-settings-v1
  (struct-size :uint32)
  (version :uint32)
  (flags :uint32)
  (output-format :uint32)
  (kelvin :float)
  (tint :float)
  (exposure-ev :float)
  (chroma-noise-reduction :float)
  (luma-noise-reduction :float)
  (tone-blacks :float)
  (tone-shadows :float)
  (tone-dark-mids :float)
  (tone-midtones :float)
  (tone-light-mids :float)
  (tone-highlights :float)
  (tone-whites :float)
  (lut-strength :float)
  (grain-amount :float)
  (grain-size :float)
  (grain-seed :uint64)
  (max-width :uint32)
  (max-height :uint32)
  (jpeg-quality :uint32)
  (lens-correction-strength :float)
  (focal-reducer :float)
  (lens-crop-factor :float)
  (lut-path :pointer)
  (lens-profile-model :pointer)
  (neural-noise-reduction :float))

(defcfun ("orfeus_raw_render_capabilities_v1"
          %raw-render-capabilities-v1) :uint32)

(defcfun ("orfeus_raw_render_v1" %raw-render-v1) :int32
  (input-path :string)
  (output-path :string)
  (settings (:pointer (:struct render-settings-v1)))
  (error-buffer :pointer)
  (error-capacity :size))

(defcfun ("orfeus_raw_render_v2" %raw-render-v2) :int32
  (input-path :string)
  (output-path :string)
  (settings (:pointer (:struct render-settings-v1)))
  (cache-mode :uint32)
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

(defparameter *required-render-capabilities* #b10111
  "Render features required by the Common Lisp core.")

(defun native-render-require-compatible ()
  (let ((version (native-bridge-version)))
    (unless (>= version 2)
      (error 'native-library-incompatible
             :message (format nil "bridge ABI ~D does not provide raw render v2"
                              version))))
  (let ((capabilities
          (handler-case
              (%raw-render-capabilities-v1)
            (error (condition)
              (error 'native-library-incompatible
                     :message (format nil
                                      "render capability query failed: ~A"
                                      condition))))))
    (unless (= *required-render-capabilities*
               (logand capabilities *required-render-capabilities*))
      (error 'native-library-incompatible
             :message (format nil "capabilities 0x~X lack required mask 0x~X"
                              capabilities *required-render-capabilities*)))
    capabilities))

(defun native-raw-render (input-pathname output-pathname
                          &key exposure kelvin tint noise-reduction
                            tone-blacks tone-shadows tone-dark-mids tone-midtones
                            tone-light-mids tone-highlights tone-whites
                            lens-correction-p lens-correction-strength
                            chromatic-aberration-correction-p
                            lens-profile-model focal-reducer lens-crop-factor
                            lut-path lut-strength grain-amount grain-size
                            (grain-seed 0) (max-width 0) (max-height 0)
                            (jpeg-quality 92) output-format cache-p
                            neural-noise-reduction)
  (native-library-load)
  (native-render-require-compatible)
  (labels ((invoke (lut-pointer lens-pointer)
             (with-foreign-object (settings '(:struct render-settings-v1))
               (flet ((setting (name value)
                        (setf (foreign-slot-value
                               settings '(:struct render-settings-v1) name)
                              value)))
                 (setting 'struct-size
                          (foreign-type-size '(:struct render-settings-v1)))
                 (setting 'version 3)
                 (setting 'flags
                          (logior (if lens-correction-p 1 0)
                                  (if chromatic-aberration-correction-p 2 0)))
                 (setting 'output-format (ecase output-format
                                           (:jpeg 1)
                                           (:tiff 2)))
                 (setting 'kelvin (float (or kelvin 0.0) 0.0))
                 (setting 'tint (float tint 0.0))
                 (setting 'exposure-ev (float exposure 0.0))
                 (setting 'chroma-noise-reduction
                          (float noise-reduction 0.0))
                 ;; Demosaiced chroma needs much stronger filtering than luma;
                 ;; retain fine luminance texture instead of making it waxy.
                 (setting 'luma-noise-reduction
                          (float (* 0.2 noise-reduction) 0.0))
                 (setting 'neural-noise-reduction
                          (float (or neural-noise-reduction 0.0) 0.0))
                 (setting 'tone-blacks (float tone-blacks 0.0))
                 (setting 'tone-shadows (float tone-shadows 0.0))
                 (setting 'tone-dark-mids (float tone-dark-mids 0.0))
                 (setting 'tone-midtones (float tone-midtones 0.0))
                 (setting 'tone-light-mids (float tone-light-mids 0.0))
                 (setting 'tone-highlights (float tone-highlights 0.0))
                 (setting 'tone-whites (float tone-whites 0.0))
                 (setting 'lut-strength
                          (float (if lut-path lut-strength 0.0) 0.0))
                 (setting 'grain-amount (float grain-amount 0.0))
                 (setting 'grain-size (float grain-size 0.0))
                 (setting 'grain-seed grain-seed)
                 (setting 'max-width max-width)
                 (setting 'max-height max-height)
                 (setting 'jpeg-quality jpeg-quality)
                 (setting 'lens-correction-strength
                          (float lens-correction-strength 0.0))
                 (setting 'focal-reducer (float (or focal-reducer 1.0) 0.0))
                 (setting 'lens-crop-factor
                          (float (or lens-crop-factor 0.0) 0.0))
                 (setting 'lut-path lut-pointer)
                 (setting 'lens-profile-model lens-pointer))
               (with-foreign-pointer (error-buffer *native-error-buffer-size*)
                 (let ((status
                         #+sbcl
                         (sb-int:with-float-traps-masked
                             (:invalid :divide-by-zero :overflow
                              :underflow :inexact)
                           (%raw-render-v2
                            (namestring input-pathname)
                            (namestring output-pathname)
                            settings (if cache-p 1 0) error-buffer
                            *native-error-buffer-size*))
                         #-sbcl
                         (%raw-render-v2
                          (namestring input-pathname)
                          (namestring output-pathname)
                          settings (if cache-p 1 0) error-buffer
                          *native-error-buffer-size*)))
                   (unless (zerop status)
                     (error 'raw-render-error
                            :input-pathname input-pathname
                            :output-pathname output-pathname
                            :status status
                            :message (native-error-message error-buffer)))))))
           (call-with-lens-pointer (lut-pointer)
             (if lens-profile-model
                 (with-foreign-string (lens-pointer lens-profile-model
                                                    :encoding :utf-8)
                   (invoke lut-pointer lens-pointer))
                 (invoke lut-pointer (null-pointer)))))
    (if lut-path
        (with-foreign-string (lut-pointer (namestring lut-path)
                                          :encoding :utf-8)
          (call-with-lens-pointer lut-pointer))
        (call-with-lens-pointer (null-pointer))))
  output-pathname)

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
