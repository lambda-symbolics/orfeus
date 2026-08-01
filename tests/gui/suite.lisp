(defpackage #:orfeus/gui-tests
  (:use #:cl)
  (:export #:run-tests))

(in-package #:orfeus/gui-tests)

(defun check (condition control &rest arguments)
  (unless condition (error (apply #'format nil control arguments))))

(defun test-model-settings ()
  (let* ((job (orfeus:make-photo-job :input-path #P"one.orf" :overrides '(:exposure 1.0)))
         (project (orfeus:make-project :output-directory #P"exports/" :photos (list job)))
         (model (orfeus/gui:make-gui-model :project project)))
    (check (= 1.0 (orfeus/gui:gui-model-setting model :exposure)) "Effective override missing")
    (orfeus/gui:gui-model-set-setting model :grain-amount 0.4)
    (check (= 0.4 (getf (orfeus:photo-job-overrides job) :grain-amount)) "Photo edit missing")
    (setf (orfeus/gui:gui-model-edit-target model) :defaults)
    (orfeus/gui:gui-model-set-setting model :noise-reduction 0.7)
    (check (= 0.7 (orfeus:processing-settings-noise-reduction (orfeus:project-defaults project))) "Default edit missing")
    (orfeus/gui:gui-model-reset-selected model)
    (check (null (orfeus:photo-job-overrides job)) "Reset did not clear overrides")))

(defun test-output-path-semantics ()
  (let* ((relative (orfeus:make-photo-job :input-path #P"one.orf" :output-path #P"nested/one.jpg"))
         (automatic (orfeus:make-photo-job :input-path #P"two.orf"))
         (absolute (orfeus:make-photo-job :input-path #P"three.orf" :output-path #P"/var/tmp/three.jpg"))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos (list relative automatic absolute)))
         (model (orfeus/gui:make-gui-model :project project)))
    (check (equal #P"exports/nested/one.jpg" (orfeus/gui::gui-photo-output-path model relative))
           "Relative output path did not use project output directory")
    (check (equal #P"exports/two.jpg" (orfeus/gui::gui-photo-output-path model automatic))
           "Automatic output path differs from core batch semantics")
    (check (equal #P"/var/tmp/three.jpg" (orfeus/gui::gui-photo-output-path model absolute))
           "Absolute output path changed")
    (check (orfeus/gui::gui-preview-event-current-p
            model (list :preview 4 0 relative #P"preview.jpg") 4)
           "Matching preview completion was rejected")
    (check (not (orfeus/gui::gui-preview-event-current-p
                 model (list :preview 3 0 relative #P"preview.jpg") 4))
           "Stale preview generation was accepted")
    (setf (orfeus/gui:gui-model-selected-index model) 1)
    (check (not (orfeus/gui::gui-preview-event-current-p
                 model (list :preview 4 0 relative #P"preview.jpg") 4))
           "Preview for the wrong selection was accepted")))

(defun test-preview-job-identity ()
  (let* ((first (orfeus:make-photo-job :input-path #P"duplicate.orf"))
         (second (orfeus:make-photo-job :input-path #P"duplicate.orf"))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos (list first second)))
         (model (orfeus/gui:make-gui-model :project project :selected-index 1)))
    (check (orfeus/gui::gui-preview-event-current-p
            model (list :preview 7 1 second #P"preview.jpg") 7)
           "Selected duplicate-path job preview was rejected")
    (check (not (orfeus/gui::gui-preview-event-current-p
                 model (list :preview 7 0 first #P"preview.jpg") 7))
           "Preview for a different duplicate-path job was accepted")
    (check (not (orfeus/gui::gui-preview-event-current-p
                 model (list :preview 7 1 first #P"preview.jpg") 7))
           "Preview with a mismatched job identity was accepted")))

(defun test-preview-directory ()
  (let ((first (orfeus/gui::make-gui-preview-directory))
        (second (orfeus/gui::make-gui-preview-directory)))
    (unwind-protect
         (progn
           (check (not (equal first second)) "GUI runs shared a preview directory")
           (with-open-file (stream (merge-pathnames "preview.jpg" first)
                                   :direction :output :if-exists :error)
             (write-line "preview" stream))
           (orfeus/gui::delete-gui-preview-directory first)
           (setf first nil)
           (check (probe-file second)
                  "Cleaning one GUI preview directory removed another session's directory"))
      (when first
        (orfeus/gui::delete-gui-preview-directory first))
      (orfeus/gui::delete-gui-preview-directory second))))

(defun test-preview-status-percent ()
  (let* ((settings (orfeus:make-processing-settings
                    :lut-path #P"look.cube" :lut-strength 1.0))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :defaults settings))
         (model (orfeus/gui:make-gui-model :project project))
         (text (orfeus/gui::preview-status-text model)))
    (check (search "look.cube (100%)" text)
           "Preview status did not use one literal percent sign")
    (check (null (search "100%%" text))
           "Preview status contains a C-style doubled percent sign")))

(defun test-file-filter-syntax ()
  (let ((filter (orfeus/gui::fltk-file-filter "RAW photographs" "*.orf")))
    (check (char= #\Tab (char filter (length "RAW photographs")))
           "FLTK filter does not contain a real tab separator")
    (check (null (search "\\t" filter))
           "FLTK filter contains a literal backslash-t")))

(defun test-direct-open-workflow ()
  (check (eq :photo (orfeus/gui::gui-open-kind #P"photo.DNG"))
         "DNG was not classified as a photograph")
  (check (eq :project (orfeus/gui::gui-open-kind #P"project.sexp"))
         "S-expression was not classified as a project")
  (check (null (orfeus/gui::gui-open-kind #P"notes.txt"))
         "Unsupported extension was accepted")
  (let* ((inputs (list #P"/photos/one.orf" #P"/other/two.dng"))
         (project (orfeus/gui::gui-photos-project inputs))
         (jobs (orfeus:project-photos project)))
    (check (equal #P"/photos/orfeus-exports/"
                  (orfeus:project-output-directory project))
           "Direct photo output directory is not beside the first input")
    (check (equal inputs (mapcar #'orfeus:photo-job-input-path jobs))
           "Multi-photo project changed input order")
    (check (search "agfa_precisa_100.cube"
                   (orfeus:processing-settings-lut-path
                    (orfeus:project-defaults project)))
           "Direct photo project did not select the bundled Agfa LUT")))

(defun test-checkbox-normalization ()
  (let* ((job (orfeus:make-photo-job :input-path #P"one.orf"))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos (list job)))
         (model (orfeus/gui:make-gui-model :project project)))
    (orfeus/gui:gui-model-set-setting model :lens-correction-p 0)
    (check (null (getf (orfeus:photo-job-overrides job) :lens-correction-p))
           "Numeric checkbox zero was not normalized to NIL")
    (orfeus/gui:gui-model-set-setting model :lens-correction-p 1)
    (check (eq t (getf (orfeus:photo-job-overrides job) :lens-correction-p))
           "Numeric checkbox one was not normalized to T")))

(defun test-render-queue ()
  (let ((queue (orfeus/gui::make-gui-queue))
        (lock (sb-thread:make-mutex))
        (values '()))
    (unwind-protect
         (progn
           (orfeus/gui::enqueue-gui-task
            queue :export
            (lambda ()
              (sb-thread:with-mutex (lock) (push :export values))))
           (orfeus/gui::enqueue-gui-task
            queue :preview
            (lambda ()
              (sb-thread:with-mutex (lock) (push :preview values)))
            :replace-kind :preview)
           (loop repeat 100
                 until (sb-thread:with-mutex (lock) (member :export values))
                 do (sleep 0.01))
           (check (sb-thread:with-mutex (lock) (member :export values))
                  "Queue did not run export task"))
      (orfeus/gui::stop-gui-queue queue))))

(defun test-preview-cache-key ()
  (let* ((job (orfeus:make-photo-job :input-path #P"one.orf"))
         (first (orfeus:make-processing-settings :exposure 0.0))
         (same (orfeus:make-processing-settings :exposure 0.0))
         (changed (orfeus:make-processing-settings :exposure 1.0))
         (directory #P"/tmp/"))
    (check (equal (orfeus/gui::preview-pathname directory 0 job :after first)
                  (orfeus/gui::preview-pathname directory 0 job :after same))
           "Equivalent preview settings did not share a cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory 0 job :after first)
                       (orfeus/gui::preview-pathname directory 0 job :after changed)))
           "Edited preview settings reused a stale cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory 0 job :before first)
                       (orfeus/gui::preview-pathname directory 0 job :after first)))
           "Before and After previews shared one cache path")))

(defun test-discard-pending-tasks ()
  (let ((queue (orfeus/gui::make-gui-queue))
        (ran-p nil))
    (unwind-protect
         (progn
           (orfeus/gui::enqueue-gui-task queue :blocker (lambda () (sleep 0.1)))
           (orfeus/gui::enqueue-gui-task queue :background
                                         (lambda () (setf ran-p t)))
           (orfeus/gui::discard-gui-tasks queue :background)
           (sleep 0.2)
           (check (not ran-p) "Discarded background task still ran"))
      (orfeus/gui::stop-gui-queue queue))))

(defun run-tests ()
  (test-model-settings)
  (test-output-path-semantics)
  (test-preview-job-identity)
  (test-preview-directory)
  (test-preview-status-percent)
  (test-file-filter-syntax)
  (test-direct-open-workflow)
  (test-checkbox-normalization)
  (test-render-queue)
  (test-preview-cache-key)
  (test-discard-pending-tasks)
  t)
