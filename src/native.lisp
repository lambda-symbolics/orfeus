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

(defcstruct render-frame-v1
  (struct-size :uint32)
  (version :uint32)
  (output-format :uint32)
  (max-width :uint32)
  (max-height :uint32)
  (jpeg-quality :uint32)
  (grain-seed :uint64)
  (focal-reducer :float)
  (lens-crop-factor :float)
  (lens-profile-model :pointer))

(defcfun ("orfeus_raw_render_v3" %raw-render-v3) :int32
  (input-path :string)
  (output-path :string)
  (frame (:pointer (:struct render-frame-v1)))
  (graph :pointer)
  (graph-length :size)
  (cache-mode :uint32)
  (error-buffer :pointer)
  (error-capacity :size))

(defcfun ("orfeus_raw_render_rgb_v1" %raw-render-rgb-v1) :int32
  (input-path :string)
  (frame (:pointer (:struct render-frame-v1)))
  (graph :pointer)
  (graph-length :size)
  (buffer :pointer)
  (capacity :size)
  (out-width :pointer)
  (out-height :pointer)
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

(defcfun ("orfeus_gpu_warm_up" %gpu-warm-up) :void)

(defun native-gpu-warm-up ()
  "Start building the Vulkan compute context on a background thread.

Vulkan initialization costs enough to show on a session's first renders. This
returns at once, so a caller can spend it during startup instead."
  (when (native-bridge-available-p)
    (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow
                                     :underflow :inexact)
      (%gpu-warm-up))
    t))

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

(defcfun ("orfeus_analyze_negative_frame_v1" %analyze-negative-frame-v1) :int32
  (input-path :string)
  (cache-mode :uint32)
  (results :pointer)
  (error-buffer :pointer)
  (error-capacity :size))

(defcfun ("orfeus_sample_linear_v1" %sample-linear-v1) :int32
  (input-path :string)
  (cache-mode :uint32)
  (x :float)
  (y :float)
  (radius :float)
  (rgb :pointer)
  (error-buffer :pointer)
  (error-capacity :size))

(defun analyze-negative-frame (input-pathname &key cache-p)
  "Detect a scanned negative's central tile, film-base color, and tilt.

Returns three values: the crop rectangle (left top width height) in oriented
normalized coordinates, the scene-linear base color (red green blue), and
the straightening angle in degrees for a crop node."
  (native-library-load)
  (with-foreign-pointer (results (* 8 4))
    (with-foreign-pointer (error-buffer *native-error-buffer-size*)
      (let ((status
              #+sbcl
              (sb-int:with-float-traps-masked
                  (:invalid :divide-by-zero :overflow :underflow :inexact)
                (%analyze-negative-frame-v1
                 (namestring input-pathname)
                 (if cache-p 1 0)
                 results error-buffer *native-error-buffer-size*))
              #-sbcl
              (%analyze-negative-frame-v1
               (namestring input-pathname)
               (if cache-p 1 0)
               results error-buffer *native-error-buffer-size*)))
        (unless (zerop status)
          (error 'raw-render-error
                 :input-pathname input-pathname
                 :output-pathname input-pathname
                 :status status
                 :message (native-error-message error-buffer)))
        (values (loop for index below 4
                      collect (mem-aref results :float index))
                (loop for index from 4 below 7
                      collect (mem-aref results :float index))
                (mem-aref results :float 7))))))

(defun sample-photo-linear-color (input-pathname x y
                                  &key (radius 0.01) cache-p)
  "Average the scene-linear color around oriented normalized point X, Y."
  (native-library-load)
  (with-foreign-pointer (rgb (* 3 4))
    (with-foreign-pointer (error-buffer *native-error-buffer-size*)
      (let ((status
              #+sbcl
              (sb-int:with-float-traps-masked
                  (:invalid :divide-by-zero :overflow :underflow :inexact)
                (%sample-linear-v1
                 (namestring input-pathname)
                 (if cache-p 1 0)
                 (float x 0.0) (float y 0.0) (float radius 0.0)
                 rgb error-buffer *native-error-buffer-size*))
              #-sbcl
              (%sample-linear-v1
               (namestring input-pathname)
               (if cache-p 1 0)
               (float x 0.0) (float y 0.0) (float radius 0.0)
               rgb error-buffer *native-error-buffer-size*)))
        (unless (zerop status)
          (error 'raw-render-error
                 :input-pathname input-pathname
                 :output-pathname input-pathname
                 :status status
                 :message (native-error-message error-buffer)))
        (loop for index below 3
              collect (mem-aref rgb :float index))))))

(defparameter *graph-node-kind-codes*
  '((:white-balance . 1) (:exposure . 2) (:noise-reduction . 3)
    (:tone . 4) (:optics . 5) (:film . 6) (:blend . 7)
    (:color-subtract . 8) (:crop . 9) (:curves . 10))
  "Wire codes of graph node kinds in the native program format.")

(defconstant +graph-program-magic+ #x4746524F
  "Little-endian magic of a serialized graph program, spelling ORFG.")

(defconstant +graph-program-version+ 2
  "Serialized graph program version; 2 added the curves node's luma channel.")

(defun graph-boolean-parameter (value)
  (if value 1.0 0.0))

(defun graph-node-program-parameters (node)
  "Return NODE's packed parameter list and its optional string payload."
  (flet ((parameter (key)
           (or (getf (graph-node-params node) key)
               (getf *stage-identity-plist* key))))
    (ecase (graph-node-kind node)
      (:white-balance
       (values (list (or (parameter :white-balance-temperature) 0.0)
                     (parameter :white-balance-tint))
               nil))
      (:exposure
       (values (list (parameter :exposure)) nil))
      (:noise-reduction
       (values (list (parameter :noise-reduction)
                     (parameter :neural-noise-reduction))
               nil))
      (:tone
       (values (list (parameter :tone-blacks) (parameter :tone-shadows)
                     (parameter :tone-dark-mids) (parameter :tone-midtones)
                     (parameter :tone-light-mids) (parameter :tone-highlights)
                     (parameter :tone-whites))
               nil))
      (:optics
       (values (list (graph-boolean-parameter (parameter :lens-correction-p))
                     (parameter :lens-correction-strength)
                     (graph-boolean-parameter
                      (parameter :chromatic-aberration-correction-p)))
               nil))
      (:film
       (let ((lut-path (parameter :lut-path))
             (lut-strength (parameter :lut-strength)))
         (values (list (if lut-path lut-strength 0.0)
                       (parameter :grain-amount)
                       (parameter :grain-size))
                 (when lut-path (namestring lut-path)))))
      (:blend
       (values (list (graph-node-opacity node)) nil))
      (:color-subtract
       (let ((params (graph-node-params node)))
         (values (list (getf params :red 1.0)
                       (getf params :green 1.0)
                       (getf params :blue 1.0))
                 nil)))
      (:crop
       (let ((params (graph-node-params node)))
         (values (list (getf params :left 0.0)
                       (getf params :top 0.0)
                       (getf params :width 1.0)
                       (getf params :height 1.0)
                       (getf params :angle 0.0))
                 nil)))
      (:curves
       (let ((params (graph-node-params node)))
         (values (loop for key in *curve-channel-keys*
                       append (copy-list
                               (or (getf params key)
                                   *identity-curve-points*)))
                 nil))))))

(defun graph->program-bytes (graph)
  "Serialize GRAPH's effective nodes into the native program format."
  (let ((nodes (graph-effective-nodes graph))
        (bytes (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer t)))
    (labels ((emit-u32 (value)
               (let ((value (logand value #xFFFFFFFF)))
                 (dotimes (shift 4)
                   (vector-push-extend (ldb (byte 8 (* shift 8)) value) bytes))))
             (emit-f32 (value)
               (emit-u32 (logand (sb-kernel:single-float-bits
                                  (float value 1.0f0))
                                 #xFFFFFFFF)))
             (emit-text (text)
               (if text
                   (let ((octets (sb-ext:string-to-octets
                                  text :external-format :utf-8)))
                     (emit-u32 (length octets))
                     (loop for octet across octets
                           do (vector-push-extend octet bytes)))
                   (emit-u32 0))))
      (emit-u32 +graph-program-magic+)
      (emit-u32 +graph-program-version+)
      (emit-u32 (length nodes))
      (let ((ordinals (make-hash-table)))
        (setf (gethash 0 ordinals) 0)
        (loop for node in nodes
              for ordinal from 1
              do (setf (gethash (graph-node-id node) ordinals) ordinal))
        (dolist (node nodes)
          (multiple-value-bind (parameters text)
              (graph-node-program-parameters node)
            (emit-u32 (rest (assoc (graph-node-kind node)
                                   *graph-node-kind-codes*)))
            (emit-u32 (gethash (first (graph-node-inputs node)) ordinals))
            (emit-u32 (if (graph-node-blend-p node)
                          (gethash (second (graph-node-inputs node)) ordinals)
                          #xFFFFFFFF))
            (emit-u32 (length parameters))
            (mapc #'emit-f32 parameters)
            (emit-text text)))))
    (coerce bytes '(simple-array (unsigned-byte 8) (*)))))

(defun native-raw-render-graph (input-pathname output-pathname graph
                                &key lens-profile-model focal-reducer
                                  lens-crop-factor (grain-seed 0)
                                  (max-width 0) (max-height 0)
                                  (jpeg-quality 92) output-format cache-p)
  "Render INPUT-PATHNAME through the node GRAPH via the version 3 bridge."
  (native-library-load)
  (native-render-require-compatible)
  (unless (>= (native-bridge-version) 3)
    (error 'native-library-incompatible
           :message "bridge ABI does not provide raw render v3"))
  (let ((program (graph->program-bytes graph)))
    (flet ((invoke (lens-pointer)
             (with-foreign-object (frame '(:struct render-frame-v1))
               (flet ((setting (name value)
                        (setf (foreign-slot-value
                               frame '(:struct render-frame-v1) name)
                              value)))
                 (setting 'struct-size
                          (foreign-type-size '(:struct render-frame-v1)))
                 (setting 'version 1)
                 (setting 'output-format (ecase output-format
                                           (:jpeg 1)
                                           (:tiff 2)))
                 (setting 'max-width max-width)
                 (setting 'max-height max-height)
                 (setting 'jpeg-quality jpeg-quality)
                 (setting 'grain-seed grain-seed)
                 (setting 'focal-reducer (float (or focal-reducer 1.0) 0.0))
                 (setting 'lens-crop-factor
                          (float (or lens-crop-factor 0.0) 0.0))
                 (setting 'lens-profile-model lens-pointer))
               (with-foreign-pointer (buffer (length program))
                 (loop for octet across program
                       for index from 0
                       do (setf (mem-aref buffer :uint8 index) octet))
                 (with-foreign-pointer (error-buffer
                                        *native-error-buffer-size*)
                   (let ((status
                           #+sbcl
                           (sb-int:with-float-traps-masked
                               (:invalid :divide-by-zero :overflow
                                :underflow :inexact)
                             (%raw-render-v3
                              (namestring input-pathname)
                              (namestring output-pathname)
                              frame buffer (length program)
                              (if cache-p 1 0) error-buffer
                              *native-error-buffer-size*))
                           #-sbcl
                           (%raw-render-v3
                            (namestring input-pathname)
                            (namestring output-pathname)
                            frame buffer (length program)
                            (if cache-p 1 0) error-buffer
                            *native-error-buffer-size*)))
                     (unless (zerop status)
                       (error 'raw-render-error
                              :input-pathname input-pathname
                              :output-pathname output-pathname
                              :status status
                              :message (native-error-message
                                        error-buffer)))))))))
      (if lens-profile-model
          (with-foreign-string (lens-pointer lens-profile-model
                                             :encoding :utf-8)
            (invoke lens-pointer))
          (invoke (null-pointer)))))
  output-pathname)

(defun native-raw-render-graph-rgb (input-pathname graph rgb-buffer capacity
                                    &key lens-profile-model focal-reducer
                                      lens-crop-factor (grain-seed 0)
                                      (max-width 0) (max-height 0) cache-p)
  "Render INPUT-PATHNAME through GRAPH into the foreign RGB-BUFFER.

The live-preview hot path: no JPEG encode and no file. Returns the oriented
image width and height as two values."
  (native-library-load)
  (native-render-require-compatible)
  (unless (>= (native-bridge-version) 3)
    (error 'native-library-incompatible
           :message "bridge ABI does not provide raw render v3"))
  (let ((program (graph->program-bytes graph)))
    (flet ((invoke (lens-pointer)
             (with-foreign-object (frame '(:struct render-frame-v1))
               (flet ((setting (name value)
                        (setf (foreign-slot-value
                               frame '(:struct render-frame-v1) name)
                              value)))
                 (setting 'struct-size
                          (foreign-type-size '(:struct render-frame-v1)))
                 (setting 'version 1)
                 (setting 'output-format 1)
                 (setting 'max-width max-width)
                 (setting 'max-height max-height)
                 (setting 'jpeg-quality 92)
                 (setting 'grain-seed grain-seed)
                 (setting 'focal-reducer (float (or focal-reducer 1.0) 0.0))
                 (setting 'lens-crop-factor
                          (float (or lens-crop-factor 0.0) 0.0))
                 (setting 'lens-profile-model lens-pointer))
               (with-foreign-pointer (program-buffer (length program))
                 (loop for octet across program
                       for index from 0
                       do (setf (mem-aref program-buffer :uint8 index) octet))
                 (with-foreign-object (out-width :uint32)
                   (with-foreign-object (out-height :uint32)
                     (with-foreign-pointer (error-buffer
                                            *native-error-buffer-size*)
                       (let ((status
                               #+sbcl
                               (sb-int:with-float-traps-masked
                                   (:invalid :divide-by-zero :overflow
                                    :underflow :inexact)
                                 (%raw-render-rgb-v1
                                  (namestring input-pathname)
                                  frame program-buffer (length program)
                                  rgb-buffer capacity
                                  out-width out-height
                                  (if cache-p 1 0) error-buffer
                                  *native-error-buffer-size*))
                               #-sbcl
                               (%raw-render-rgb-v1
                                (namestring input-pathname)
                                frame program-buffer (length program)
                                rgb-buffer capacity
                                out-width out-height
                                (if cache-p 1 0) error-buffer
                                *native-error-buffer-size*)))
                         (unless (zerop status)
                           (error 'raw-render-error
                                  :input-pathname input-pathname
                                  :output-pathname nil
                                  :status status
                                  :message (native-error-message
                                            error-buffer)))
                         (values
                          (cffi:mem-ref out-width :uint32)
                          (cffi:mem-ref out-height :uint32))))))))))
      (if lens-profile-model
          (with-foreign-string (lens-pointer lens-profile-model
                                             :encoding :utf-8)
            (invoke lens-pointer))
          (invoke (null-pointer))))))

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
