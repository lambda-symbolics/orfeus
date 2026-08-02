(in-package #:orfeus/tests)

(defun test-temporary-pathname (suffix)
  (merge-pathnames
   (format nil "orfeus-test-~D-~D.~A"
           (get-universal-time) (random most-positive-fixnum) suffix)
   #P"/tmp/"))

(defun project-test-value ()
  (make-project
   :output-directory #P"exports/"
   :defaults (make-processing-settings :exposure 0.75 :grain-amount 0.12
                                       :lens-correction-strength 0.65)
   :photos (list (make-photo-job
                  :input-path #P"input/example.orf"
                  :overrides '(:exposure -0.25 :lut-strength 0.8)))))

(defun project-round-trip-p ()
  (let* ((original (project-test-value))
         (decoded (sexp->project (project->sexp original))))
    (and (= 0.75 (processing-settings-exposure
                  (project-defaults decoded)))
         (= 0.65 (processing-settings-lens-correction-strength
                  (project-defaults decoded)))
         (= 1 (length (project-photos decoded)))
         (equal '(:exposure -0.25 :lut-strength 0.8)
                (photo-job-overrides (first (project-photos decoded)))))))

(defun export-settings-round-trip-p ()
  (let* ((project (make-project
                   :output-directory #P"exports/"
                   :export-settings (make-export-settings
                                     :jpeg-quality 84
                                     :max-width 2400
                                     :max-height 1600
                                     :preserve-metadata-p nil)))
         (decoded (sexp->project (project->sexp project)))
         (settings (project-export-settings decoded)))
    (and (= 84 (export-settings-jpeg-quality settings))
         (= 2400 (export-settings-max-width settings))
         (= 1600 (export-settings-max-height settings))
         (null (export-settings-preserve-metadata-p settings)))))

(defun processing-presets-round-trip-p ()
  (let* ((project
           (make-project
            :output-directory #P"exports/"
            :presets (list (make-processing-preset
                            :name "Night"
                            :settings (make-processing-settings
                                       :exposure 1.25
                                       :noise-reduction 0.8)))))
         (decoded (sexp->project (project->sexp project)))
         (preset (first (project-presets decoded))))
    (and (= 1 (length (project-presets decoded)))
         (string= "Night" (processing-preset-name preset))
         (= 1.25 (processing-settings-exposure
                  (processing-preset-settings preset))))))

(defun old-project-export-defaults-p ()
  (let* ((decoded
           (sexp->project
            '(:orfeus-project 1
              :output-directory "exports/"
              :defaults (:exposure 0.0
                         :white-balance-temperature nil
                         :white-balance-tint 0.0
                         :noise-reduction 0.35
                         :lens-correction-p t
                         :lens-correction-strength 1.0
                         :chromatic-aberration-correction-p t
                         :lut-path nil :lut-strength 1.0
                         :grain-amount 0.0 :grain-size 1.0)
              :photos ())))
         (settings (project-export-settings decoded)))
    (and (= 92 (export-settings-jpeg-quality settings))
         (null (export-settings-max-width settings))
         (null (export-settings-max-height settings))
         (export-settings-preserve-metadata-p settings)
         (null (project-presets decoded)))))

(defun project-file-round-trip-p ()
  (let ((pathname (test-temporary-pathname "sexp")))
    (unwind-protect
         (progn
           (project-write (project-test-value) pathname)
           (and (not (search "#A(" (uiop:read-file-string pathname)))
                (search "\"exports/\"" (uiop:read-file-string pathname))
                (project-round-trip-p-from (project-read pathname))))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun project-round-trip-p-from (project)
  (and (= 0.75 (processing-settings-exposure (project-defaults project)))
       (= 1 (length (project-photos project)))))

(defun project-reader-evaluation-disabled-p ()
  (let ((pathname (test-temporary-pathname "sexp")))
    (unwind-protect
         (progn
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede)
             (write-string "#.(error \"reader evaluation escaped\")" stream))
           (handler-case
               (progn (project-read pathname) nil)
             (reader-error () t)))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun invalid-project-rejected-p ()
  (handler-case
      (progn
        (sexp->project '(:orfeus-project 99))
        nil)
    (invalid-project-data () t)))

(defun lens-description-selection-p ()
  (and (string= "Olympus M.Zuiko 12-45mm"
                (uiop:symbol-call '#:orfeus '#:preferred-lens-description
                                  (format nil "Unknown~%Olympus M.Zuiko 12-45mm~%")))
       (string= "Ultron 0.7x"
                (uiop:symbol-call '#:orfeus '#:preferred-lens-description
                                  (format nil "None~%Ultron 0.7x~%")))))

(defun capture-description-formatting-p ()
  (and (string= "OM-1 | ISO 200 | f/5.6 | 1/50"
                (uiop:symbol-call '#:orfeus '#:capture-description
                                  "OM-1" "200" "5.6" "1/50"))
       (string= "PEN-F | ISO 800"
                (uiop:symbol-call '#:orfeus '#:capture-description
                                  "PEN-F" "800" "-" "-"))
       (null (uiop:symbol-call '#:orfeus '#:capture-description
                               "-" "" "none" "unknown"))))

(defun adapted-lens-aliases-p ()
  (multiple-value-bind (model reducer crop-factor)
      (resolve-lens-profile-alias "Ultron 0.7x")
    (and (string= model "Voigtlander Ultron 40mm f/2 SLII Aspherical")
         (= reducer 0.71)
         (= crop-factor 2.0))))

(defun lens-alias-reader-evaluation-disabled-p ()
  (let ((pathname (test-temporary-pathname "sexp")))
    (unwind-protect
         (progn
           (with-open-file (stream pathname :direction :output
                                           :if-exists :supersede)
             (write-string "#.(error \"reader evaluation escaped\")" stream))
           (handler-case
               (progn (lens-profile-aliases-read pathname) nil)
             (reader-error () t)))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun processing-overrides-p ()
  (let ((settings
          (processing-settings-with-overrides
           (make-processing-settings :exposure 0.5 :grain-amount 0.0)
           '(:exposure -1.0 :grain-amount 0.2))))
    (and (= -1.0 (processing-settings-exposure settings))
         (= 0.2 (processing-settings-grain-amount settings)))))

(defun project-relative-paths-p ()
  (let ((pathname (test-temporary-pathname "sexp")))
    (unwind-protect
         (progn
           (project-write
            (make-project
             :output-directory #P"exports/"
             :photos (list (make-photo-job :input-path #P"input.orf")))
            pathname)
           (let* ((project (project-read pathname))
                  (base (uiop:pathname-directory-pathname pathname)))
             (and (equal (project-output-directory project)
                         (merge-pathnames #P"exports/" base))
                  (equal (photo-job-input-path (first (project-photos project)))
                         (merge-pathnames #P"input.orf" base)))))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun photo-job-render-output-semantics-p ()
  (let* ((project (make-project :output-directory #P"/tmp/orfeus-exports/"))
         (automatic (make-photo-job :input-path #P"source/photo.orf"))
         (relative (make-photo-job :input-path #P"source/photo.orf"
                                   :output-path #P"edited.tiff"))
         (absolute (make-photo-job :input-path #P"source/photo.orf"
                                   :output-path #P"/tmp/custom-output.jpg")))
    (and (equal (photo-job-render-output project automatic)
                #P"/tmp/orfeus-exports/photo.jpg")
         (equal (photo-job-render-output project relative)
                #P"/tmp/orfeus-exports/edited.tiff")
         (equal (photo-job-render-output project absolute)
                #P"/tmp/custom-output.jpg"))))

(defun render-rejects-input-as-output-p ()
  (let ((pathname (test-temporary-pathname "orf"))
        (contents "source pixels must survive"))
    (unwind-protect
         (progn
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede)
             (write-string contents stream))
           (let ((rejected
                   (handler-case
                       (progn
                         (render-photo pathname pathname
                                       (make-processing-settings)
                                       :if-exists :supersede)
                         nil)
                     (raw-render-error () t))))
             (and rejected
                  (with-open-file (stream pathname :direction :input)
                    (string= contents (read-line stream nil ""))))))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun render-accepts-unbounded-nil-dimensions-p ()
  (let ((input (test-temporary-pathname "orf"))
        (output (test-temporary-pathname "jpg")))
    (unwind-protect
         (progn
           (with-open-file (stream input :direction :output :if-exists :supersede)
             (write-string "not a raw" stream))
           (handler-case
               (progn
                 (render-photo input output (make-processing-settings)
                               :max-width nil :max-height nil
                               :preserve-metadata-p nil)
                 nil)
             (raw-render-error () t)
             (type-error () nil)))
      (dolist (pathname (list input output))
        (when (probe-file pathname)
          (delete-file pathname))))))

(defun preview-does-not-overwrite-p ()
  (let ((input (test-temporary-pathname "orf"))
        (output (test-temporary-pathname "jpg"))
        (contents "keep this export"))
    (unwind-protect
         (progn
           (with-open-file (stream input :direction :output
                                         :if-exists :supersede)
             (write-string "raw" stream))
           (with-open-file (stream output :direction :output
                                          :if-exists :supersede)
             (write-string contents stream))
           (let ((rejected
                   (handler-case
                       (progn
                         (render-preview input output
                                         (make-processing-settings))
                         nil)
                     (output-file-exists () t))))
             (and rejected
                  (with-open-file (stream output :direction :input)
                    (string= contents (read-line stream nil ""))))))
      (dolist (pathname (list input output))
        (when (probe-file pathname)
          (delete-file pathname))))))

(defun embedded-preview-rejects-missing-image-p ()
  (let ((input (test-temporary-pathname "txt"))
        (output (test-temporary-pathname "jpg")))
    (unwind-protect
         (progn
           (with-open-file (stream input :direction :output
                                         :if-exists :supersede)
             (write-string "not a photograph" stream))
           (and (handler-case
                    (progn
                      (photo-extract-embedded-preview input output
                                                      :if-exists :supersede)
                      nil)
                  (raw-render-error () t))
                (not (probe-file output))))
      (dolist (pathname (list input output))
        (when (probe-file pathname)
          (delete-file pathname))))))

(defun publish-race-does-not-clobber-p ()
  (let ((temporary (test-temporary-pathname "jpg"))
        (output (test-temporary-pathname "jpg")))
    (unwind-protect
         (progn
           (with-open-file (stream temporary :direction :output
                                             :if-exists :supersede)
             (write-string "new render" stream))
           (with-open-file (stream output :direction :output
                                          :if-exists :supersede)
             (write-string "racing writer" stream))
           (let ((rejected
                   (handler-case
                       (progn
                         (uiop:symbol-call '#:orfeus '#:render-publish
                                           temporary output :error)
                         nil)
                     (output-file-exists () t))))
             (and rejected
                  (with-open-file (stream output :direction :input)
                    (string= "racing writer" (read-line stream nil ""))))))
      (dolist (pathname (list temporary output))
        (when (probe-file pathname)
          (delete-file pathname))))))

(defun cli-version-p ()
  (let ((output (make-string-output-stream))
        (errors (make-string-output-stream)))
    (and (zerop (cli-run '("--version")
                         :output-stream output
                         :error-stream errors))
         (search (orfeus-version) (get-output-stream-string output))
         (string= "" (get-output-stream-string errors)))))

(defun cli-rejects-unknown-command-p ()
  (let ((output (make-string-output-stream))
        (errors (make-string-output-stream)))
    (and (= 2 (cli-run '("wat")
                       :output-stream output
                       :error-stream errors))
         (search "Unknown command" (get-output-stream-string errors)))))

(defun run-tests ()
  "Run the dependency-free Orfeus test suite."
  (let ((failures 0))
    (flet ((check (description predicate)
             (format t "~:[not ok~;ok~] - ~A~%" predicate description)
             (unless predicate
               (incf failures))))
      (check "system exposes a version"
             (and (stringp (orfeus-version))
                  (plusp (length (orfeus-version)))))
      (check "project S-expressions round trip" (project-round-trip-p))
      (check "project files round trip" (project-file-round-trip-p))
      (check "export settings round trip" (export-settings-round-trip-p))
      (check "processing presets round trip" (processing-presets-round-trip-p))
      (check "old projects receive export defaults" (old-project-export-defaults-p))
      (check "project-relative paths resolve beside the project"
             (project-relative-paths-p))
      (check "project reads disable reader evaluation"
             (project-reader-evaluation-disabled-p))
      (check "invalid project versions are rejected"
             (invalid-project-rejected-p))
      (check "lens metadata skips unidentified values"
             (lens-description-selection-p))
      (check "capture metadata formats compactly"
             (capture-description-formatting-p))
      (check "adapted lens nicknames resolve portable Lensfun mappings"
             (adapted-lens-aliases-p))
      (check "lens alias reads disable reader evaluation"
             (lens-alias-reader-evaluation-disabled-p))
      (check "per-photo overrides produce effective settings"
             (processing-overrides-p))
      (check "photo outputs follow project path semantics"
             (photo-job-render-output-semantics-p))
      (check "rendering never replaces its input"
             (render-rejects-input-as-output-p))
      (check "NIL export bounds mean unbounded dimensions"
             (render-accepts-unbounded-nil-dimensions-p))
      (check "preview does not overwrite an existing export"
             (preview-does-not-overwrite-p))
      (check "embedded preview extraction rejects files without an image"
             (embedded-preview-rejects-missing-image-p))
      (check "a racing writer is not clobbered at publish time"
             (publish-race-does-not-clobber-p))
      (check "CLI reports its version" (cli-version-p))
      (check "CLI rejects unknown commands" (cli-rejects-unknown-command-p)))
    (zerop failures)))
