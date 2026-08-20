(in-package #:orfeus)

(defun render-output-format (pathname)
  (let ((type (string-downcase (or (pathname-type pathname) ""))))
    (cond
      ((member type '("jpg" "jpeg") :test #'string=) :jpeg)
      ((member type '("tif" "tiff") :test #'string=) :tiff)
      (t
       (error 'raw-render-error
              :input-pathname pathname
              :output-pathname pathname
              :status 1
              :message "output extension must be JPEG, JPG, TIFF, or TIF")))))

(defun render-temporary-pathname (output-pathname)
  (let ((directory (uiop:pathname-directory-pathname output-pathname))
        (name (or (pathname-name output-pathname) "render"))
        (type (pathname-type output-pathname)))
    (loop repeat 100
          for candidate =
             (merge-pathnames
              (make-pathname
               :name (format nil ".~A.orfeus-~D-~D"
                             name (get-universal-time)
                             (random most-positive-fixnum))
               :type type)
              directory)
          unless (probe-file candidate)
            return candidate
          finally
             (error 'raw-render-error
                    :input-pathname output-pathname
                    :output-pathname output-pathname
                    :status 2
                    :message "could not allocate a temporary output name"))))

(defun call-with-render-source (input-pathname function)
  "Call FUNCTION with the file INPUT-PATHNAME's pixels should be read from.

That used to mean unwrapping a DNG's embedded original into a temporary file
first, which wrote 131 MB and cost half a second on the first view of an 80 MP
scan, and left a DNG without an embedded original impossible to open. The
bridge reads the container itself now and only unwraps — in memory — when it
cannot, so nothing needs to be staged on the way in."
  (funcall function input-pathname))

(defun photo-extract-embedded-preview (input-pathname output-pathname
                                       &key (if-exists :error))
  "Extract INPUT-PATHNAME's largest available embedded JPEG to OUTPUT-PATHNAME.

ExifTool's PreviewImage is preferred, followed by JpgFromRaw and OtherImage.
The output is published atomically and INPUT-PATHNAME is never modified."
  (let* ((input-pathname (pathname input-pathname))
         (output-pathname (pathname output-pathname))
         (temporary (render-temporary-pathname output-pathname))
         (last-error "no embedded JPEG preview was found"))
    (when (pathname-same-file-p input-pathname output-pathname)
      (error 'raw-render-error
             :input-pathname input-pathname
             :output-pathname output-pathname
             :status 1
             :message "embedded preview output must not replace the input"))
    (ensure-directories-exist output-pathname)
    (when (and (probe-file output-pathname) (eq if-exists :error))
      (error 'output-file-exists :pathname output-pathname))
    (unwind-protect
         (progn
           (dolist (tag '("PreviewImage" "JpgFromRaw" "OtherImage"))
             (multiple-value-bind (standard-output error-output status)
                 (uiop:run-program
                  (list "exiftool" "-b" (format nil "-~A" tag)
                        (namestring input-pathname))
                  :output temporary
                  :if-output-exists :supersede
                  :element-type '(unsigned-byte 8)
                  :error-output :string
                  :ignore-error-status t)
               (declare (ignore standard-output))
               (setf last-error error-output)
               (when (and (zerop status)
                          (probe-file temporary)
                          (plusp (with-open-file
                                     (stream temporary
                                             :direction :input
                                             :element-type '(unsigned-byte 8))
                                   (file-length stream))))
                 (render-publish temporary output-pathname if-exists)
                 (return-from photo-extract-embedded-preview output-pathname))))
           (error 'raw-render-error
                  :input-pathname input-pathname
                  :output-pathname output-pathname
                  :status 1
                  :message (if (plusp (length last-error))
                               last-error
                               "no embedded JPEG preview was found")))
      (when (probe-file temporary)
        (delete-file temporary)))))

(defparameter *metadata-copy-exclusions*
  '("--ICC_Profile" "--Orientation"
    "--ImageWidth" "--ImageHeight" "--ExifImageWidth" "--ExifImageHeight"
    "--PixelXDimension" "--PixelYDimension"
    "--ThumbnailImage" "--PreviewImage" "--JpgFromRaw" "--OtherImage")
  "Source tags excluded because rendered pixels or profiles replace them.")

(defun pathname-same-file-p (first-pathname second-pathname)
  (handler-case
      (let ((first-stat (stat (namestring first-pathname)))
            (second-stat (stat (namestring second-pathname))))
        (and (= (stat-dev first-stat) (stat-dev second-stat))
             (= (stat-ino first-stat) (stat-ino second-stat))))
    (error () nil)))

(defun render-publish (temporary output-pathname if-exists)
  (ecase if-exists
    (:supersede
     (uiop:rename-file-overwriting-target temporary output-pathname))
    (:error
     (handler-case
         (progn
           (link (namestring temporary) (namestring output-pathname))
           (delete-file temporary))
       (syscall-error (condition)
         (if (= eexist (syscall-errno condition))
             (error 'output-file-exists :pathname output-pathname)
             (error 'raw-render-error
                    :input-pathname temporary
                    :output-pathname output-pathname
                    :status 2
                    :message (format nil "could not publish output: ~A"
                                     condition))))))))

(defun render-copy-metadata (input-pathname output-pathname)
  (labels ((run-exiftool (arguments operation)
             (multiple-value-bind (standard-output error-output status)
                 (uiop:run-program arguments
                                   :output :string
                                   :error-output :string
                                   :ignore-error-status t)
               (declare (ignore standard-output))
               (unless (zerop status)
                 (error 'raw-render-error
                        :input-pathname input-pathname
                        :output-pathname output-pathname
                        :status status
                        :message (format nil "~A failed: ~A"
                                         operation error-output))))))
    (run-exiftool
     (append (list "exiftool"
                   "-overwrite_original"
                   "-TagsFromFile" (namestring input-pathname)
                   "-all:all")
             *metadata-copy-exclusions*
             (list "-Orientation#=1"
                   "-ColorSpace#=1"
                   (namestring output-pathname)))
     "metadata copy")
    ;; ExifTool copies Olympus maker notes as one block, so normalize their
    ;; output-space tags in a second pass after the block exists in the export.
    (run-exiftool
     (list "exiftool"
           "-overwrite_original"
           "-Olympus:ColorSpace=sRGB"
           "-Olympus:RawDevColorSpace=sRGB"
           (namestring output-pathname))
     "metadata color-space normalization")))

(defun render-native-photo (input-pathname output-pathname settings
                            &key max-width max-height jpeg-quality grain-seed
                              cache-p
                              (report-input-pathname input-pathname))
  (multiple-value-bind (lens-profile focal-reducer lens-crop-factor)
      (resolve-lens-profile-alias
       (photo-lens-description report-input-pathname))
    (labels ((invoke (effective-settings)
               (native-raw-render
                input-pathname output-pathname
                :lens-name (photo-lens-name report-input-pathname)
              :cache-p cache-p
              :output-format (render-output-format output-pathname)
              :exposure (processing-settings-exposure effective-settings)
              :kelvin
              (processing-settings-white-balance-temperature effective-settings)
              :tint (processing-settings-white-balance-tint effective-settings)
              :noise-reduction
              (processing-settings-noise-reduction effective-settings)
              :neural-noise-reduction
              (processing-settings-neural-noise-reduction effective-settings)
              :tone-blacks (processing-settings-tone-blacks effective-settings)
              :tone-shadows (processing-settings-tone-shadows effective-settings)
              :tone-dark-mids
              (processing-settings-tone-dark-mids effective-settings)
              :tone-midtones (processing-settings-tone-midtones effective-settings)
              :tone-light-mids
              (processing-settings-tone-light-mids effective-settings)
              :tone-highlights
              (processing-settings-tone-highlights effective-settings)
              :tone-whites (processing-settings-tone-whites effective-settings)
                :lens-correction-p
                (processing-settings-lens-correction-p effective-settings)
                :lens-correction-strength
                (processing-settings-lens-correction-strength effective-settings)
                :lens-profile-model lens-profile
                :focal-reducer focal-reducer
                :lens-crop-factor lens-crop-factor
                :chromatic-aberration-correction-p
              (processing-settings-chromatic-aberration-correction-p
               effective-settings)
              :lut-path (processing-settings-lut-path effective-settings)
              :lut-strength (processing-settings-lut-strength effective-settings)
              :grain-amount (processing-settings-grain-amount effective-settings)
              :grain-size (processing-settings-grain-size effective-settings)
              :grain-seed grain-seed
              :max-width max-width
              :max-height max-height
              :jpeg-quality jpeg-quality)))
    (handler-case
        (invoke settings)
      (raw-render-error (condition)
        (if (and (= 9 (raw-render-error-status condition))
                 (or (processing-settings-lens-correction-p settings)
                     (processing-settings-chromatic-aberration-correction-p
                      settings)))
            (progn
              (warn 'lens-profile-unavailable
                    :input-pathname report-input-pathname
                    :message (raw-render-error-message condition))
              (let ((fallback (copy-processing-settings settings)))
                (setf (processing-settings-lens-correction-p fallback) nil
                      (processing-settings-chromatic-aberration-correction-p
                       fallback)
                      nil)
                (invoke fallback)))
            (error condition)))))))

(defun graph-active-optics-p (graph)
  "True when GRAPH's effective plan applies any lens correction."
  (loop for node in (graph-effective-nodes graph)
        thereis (and (eq :optics (graph-node-kind node))
                     (let ((params (graph-node-params node)))
                       (or (getf params :lens-correction-p)
                           (getf params :chromatic-aberration-correction-p))))))

(defun graph-without-optics (graph)
  "Return a copy of GRAPH with every optics node bypassed."
  (let ((copy (graph-copy graph)))
    (dolist (node (processing-graph-nodes copy))
      (when (eq :optics (graph-node-kind node))
        (setf (graph-node-bypassed-p node) t)))
    copy))

(defun render-native-photo-graph (input-pathname output-pathname graph
                                  &key max-width max-height jpeg-quality
                                    grain-seed cache-p draft-p viewport progress
                                    (report-input-pathname input-pathname))
  (multiple-value-bind (lens-profile focal-reducer lens-crop-factor)
      (resolve-lens-profile-alias
       (photo-lens-description report-input-pathname))
    (flet ((invoke (effective-graph)
             (native-raw-render-graph
              input-pathname output-pathname effective-graph
              :lens-name (photo-lens-name report-input-pathname)
              :cache-p cache-p
              :draft-p draft-p
              :viewport viewport
              :progress progress
              :output-format (render-output-format output-pathname)
              :lens-profile-model lens-profile
              :focal-reducer focal-reducer
              :lens-crop-factor lens-crop-factor
              :grain-seed grain-seed
              :max-width max-width
              :max-height max-height
              :jpeg-quality jpeg-quality)))
      (handler-case
          (invoke graph)
        (raw-render-error (condition)
          (if (and (= 9 (raw-render-error-status condition))
                   (graph-active-optics-p graph))
              (progn
                (warn 'lens-profile-unavailable
                      :input-pathname report-input-pathname
                      :message (raw-render-error-message condition))
                (invoke (graph-without-optics graph)))
              (error condition)))))))

(defun render-photo (input-pathname output-pathname settings
                     &key (if-exists :error)
                       (max-width 0) (max-height 0)
                       (jpeg-quality 92) (grain-seed 0)
                       (preserve-metadata-p t) cache-p graph draft-p viewport
                       progress)
  "Render INPUT-PATHNAME to JPEG or TIFF using PROCESSING-SETTINGS.

This frontend-independent operation is shared by the CLI and GUI. It renders
through the Rust bridge, optionally copies source metadata with ExifTool, and
publishes the completed file atomically. MAX-WIDTH and MAX-HEIGHT bound preview
output dimensions; zero leaves a dimension unconstrained. CACHE-P asks the
bridge to reuse decoded scene data across renders of the same unchanged input;
interactive frontends enable it for the photograph under adjustment. GRAPH,
when supplied, renders through the processing node graph instead of SETTINGS,
which may then be NIL.

VIEWPORT, when given, names the part of the frame to develop as four fractions
of the oriented frame: left, top, width, height. Only an interactive preview
gives one, and only when it is zoomed in far enough that most of the frame is
off screen; an export is the whole photograph by definition.

PROGRESS, when given, is called with a stage name and a fraction as the render
passes each stage, on this thread. A render takes anywhere from forty
milliseconds to half a minute depending on which nodes are in it, and this is
what lets a frontend say which one it is waiting on."
  (check-type settings (or null processing-settings))
  ;; A viewport is only implemented on the graph path, and the graph path
  ;; renders the same picture — the interface already develops every project
  ;; photograph through it. Converting here rather than at the call site keeps
  ;; the rule where the reason for it is, and keeps a caller from asking for a
  ;; window and silently getting a whole frame back: that mismatch is worse
  ;; than either answer, because the caller then draws a frame as though it were
  ;; the part of one it asked for.
  (when (and viewport (null graph) settings)
    (setf graph (settings->graph settings)
          settings nil))
  (check-type graph (or null processing-graph))
  (unless (or settings graph)
    (error 'raw-render-error
           :input-pathname input-pathname
           :output-pathname output-pathname
           :status 1
           :message "a render needs processing settings or a node graph"))
  (when (pathname-same-file-p input-pathname output-pathname)
    (error 'raw-render-error
           :input-pathname input-pathname
           :output-pathname output-pathname
           :status 1
           :message "input and output identify the same file"))
  (let ((publish-policy if-exists))
    (when (probe-file output-pathname)
      (ecase publish-policy
        (:error
         (restart-case
             (error 'output-file-exists :pathname output-pathname)
           (overwrite ()
             :report "Replace the existing output file."
             (setf publish-policy :supersede))))
        (:supersede nil)))
    (ensure-directories-exist output-pathname)
    (let ((temporary (render-temporary-pathname output-pathname)))
      (unwind-protect
           (progn
             (call-with-render-source
              input-pathname
              (lambda (render-input-pathname)
                (if graph
                    (render-native-photo-graph
                     render-input-pathname temporary graph
                     :report-input-pathname input-pathname
                     :grain-seed grain-seed
                     :max-width (or max-width 0)
                     :max-height (or max-height 0)
                     :jpeg-quality jpeg-quality
                     :cache-p cache-p
                     ;; Only the graph path can draft; a photograph still on
                     ;; flat settings goes through the older settings struct,
                     ;; which has no room to ask.
                     :draft-p draft-p
                     :viewport viewport
                     :progress progress)
                    (render-native-photo
                     render-input-pathname temporary settings
                     :report-input-pathname input-pathname
                     :grain-seed grain-seed
                     :max-width (or max-width 0)
                     :max-height (or max-height 0)
                     :jpeg-quality jpeg-quality
                     :cache-p cache-p))))
             (when preserve-metadata-p
               (render-copy-metadata input-pathname temporary))
             (render-publish temporary output-pathname publish-policy)
             output-pathname)
        (when (probe-file temporary)
          (delete-file temporary))))))

(defun render-preview (input-pathname output-pathname settings
                       &key (if-exists :error)
                         (max-width 1600) (max-height 1200)
                         (jpeg-quality 88) (grain-seed 0) cache-p graph
                         (draft-p t) viewport progress)
  "Render a bounded JPEG preview without metadata-copy overhead.

Develops a draft by default: this is what the editor looks at, not what it
delivers, and the bridge ignores the request when the bound is large enough that
binning would drop detail the downscale would have kept."
  (render-photo input-pathname output-pathname settings
                :draft-p draft-p
                :max-width max-width
                :max-height max-height
                :jpeg-quality jpeg-quality
                :grain-seed grain-seed
                :preserve-metadata-p nil
                :if-exists if-exists
                :cache-p cache-p
                :viewport viewport
                :progress progress
                :graph graph))

(defmacro with-decoded-while ((input-pathname &rest decode-arguments) &body body)
  "Run BODY while INPUT-PATHNAME decodes on another thread, and return its values.

The first view of a photograph reads its lens description out of the file
before it can render, which is half a second of ExifTool on a 116 MB DNG, and
the decode that follows does not depend on the answer. Running them at once
hides the shorter behind the longer.

Nothing waits for the thread. The render decodes for itself if it gets there
first — concurrent misses share one loader — and a failure here is not
reported, because that render will fail again and say so properly."
  (let ((path (gensym "PATH")))
    `(let ((,path ,input-pathname))
       (unless (photo-metadata-known-p ,path)
         (sb-thread:make-thread
          (lambda ()
            (ignore-errors (native-raw-decode ,path ,@decode-arguments)))
          :name "Orfeus decode warm-up"))
       ,@body)))

(defun render-preview-rgb (input-pathname graph rgb-buffer capacity
                           &key (max-width 2048) (max-height 2048)
                             (grain-seed 0) (cache-p t))
  "Render a bounded live preview through GRAPH into the foreign RGB-BUFFER.

The interactive hot path: no JPEG encode, no file, no metadata copy.
Returns the oriented image width and height as two values."
  (check-type graph processing-graph)
  (multiple-value-bind (lens-profile focal-reducer lens-crop-factor)
      (with-decoded-while
          (input-pathname :max-width max-width :max-height max-height
                          :draft-p t :cache-p cache-p)
        (resolve-lens-profile-alias (photo-lens-description input-pathname)))
    (call-with-render-source
     input-pathname
     (lambda (render-input-pathname)
       (flet ((invoke (effective-graph)
                (native-raw-render-graph-rgb
                 render-input-pathname effective-graph rgb-buffer capacity
                 :lens-name (photo-lens-name input-pathname)
                 :cache-p cache-p
                 :draft-p t
                 :lens-profile-model lens-profile
                 :focal-reducer focal-reducer
                 :lens-crop-factor lens-crop-factor
                 :grain-seed grain-seed
                 :max-width max-width
                 :max-height max-height)))
         (handler-case
             (invoke graph)
           (raw-render-error (condition)
             (if (and (= 9 (raw-render-error-status condition))
                      (graph-active-optics-p graph))
                 (progn
                   (warn 'lens-profile-unavailable
                         :input-pathname input-pathname
                         :message (raw-render-error-message condition))
                   (invoke (graph-without-optics graph)))
                 (error condition)))))))))
