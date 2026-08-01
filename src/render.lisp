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

(defun dng-input-pathname-p (pathname)
  (string-equal (or (pathname-type pathname) "") "dng"))

(defun render-source-temporary-pathname (input-pathname)
  (let* ((original-name (dng-original-filename input-pathname))
         (type (or (pathname-type (pathname original-name)) "orf")))
    (loop repeat 100
          for candidate =
             (merge-pathnames
              (make-pathname :name (format nil "orfeus-source-~D-~D"
                                            (get-universal-time)
                                            (random most-positive-fixnum))
                             :type type)
              (uiop:temporary-directory))
          unless (probe-file candidate)
            return candidate
          finally
             (error 'raw-render-error
                    :input-pathname input-pathname
                    :output-pathname input-pathname
                    :status 2
                    :message "could not allocate a temporary embedded original"))))

(defun call-with-render-source (input-pathname function)
  (if (dng-input-pathname-p input-pathname)
      (let ((temporary (render-source-temporary-pathname input-pathname)))
        (unwind-protect
             (progn
               (dng-extract-original input-pathname temporary)
               (funcall function temporary))
          (when (probe-file temporary)
            (delete-file temporary))))
      (funcall function input-pathname)))

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
                              (report-input-pathname input-pathname))
  (labels ((invoke (effective-settings)
             (native-raw-render
              input-pathname output-pathname
              :output-format (render-output-format output-pathname)
              :exposure (processing-settings-exposure effective-settings)
              :kelvin
              (processing-settings-white-balance-temperature effective-settings)
              :tint (processing-settings-white-balance-tint effective-settings)
              :noise-reduction
              (processing-settings-noise-reduction effective-settings)
              :lens-correction-p
              (processing-settings-lens-correction-p effective-settings)
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
            (error condition))))))

(defun render-photo (input-pathname output-pathname settings
                     &key (if-exists :error)
                       (max-width 0) (max-height 0)
                       (jpeg-quality 92) (grain-seed 0)
                       (preserve-metadata-p t))
  "Render INPUT-PATHNAME to JPEG or TIFF using PROCESSING-SETTINGS.

This frontend-independent operation is shared by the CLI and GUI. It renders
through the Rust bridge, optionally copies source metadata with ExifTool, and
publishes the completed file atomically. MAX-WIDTH and MAX-HEIGHT bound preview
output dimensions; zero leaves a dimension unconstrained."
  (check-type settings processing-settings)
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
                (render-native-photo
                 render-input-pathname temporary settings
                 :report-input-pathname input-pathname
                 :grain-seed grain-seed
                 :max-width max-width
                 :max-height max-height
                 :jpeg-quality jpeg-quality)))
             (when preserve-metadata-p
               (render-copy-metadata input-pathname temporary))
             (render-publish temporary output-pathname publish-policy)
             output-pathname)
        (when (probe-file temporary)
          (delete-file temporary))))))

(defun render-preview (input-pathname output-pathname settings
                       &key (if-exists :error)
                         (max-width 1600) (max-height 1200)
                         (jpeg-quality 88) (grain-seed 0))
  "Render a bounded JPEG preview without metadata-copy overhead."
  (render-photo input-pathname output-pathname settings
                :max-width max-width
                :max-height max-height
                :jpeg-quality jpeg-quality
                :grain-seed grain-seed
                :preserve-metadata-p nil
                :if-exists if-exists))
