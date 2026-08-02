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
    (orfeus/gui:gui-model-set-setting model :lens-correction-strength 0.6)
    (check (= 0.6 (orfeus:processing-settings-lens-correction-strength
                   (orfeus:project-defaults project)))
           "Lens correction strength edit missing")
    (orfeus/gui:gui-model-reset-selected model)
    (check (null (orfeus:photo-job-overrides job)) "Reset did not clear overrides")))

(defun test-preset-bulk-application ()
  (let* ((jobs (loop for name in '("one" "two" "three")
                     collect (orfeus:make-photo-job
                              :input-path (make-pathname :name name :type "orf"))))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos jobs))
         (model (orfeus/gui:make-gui-model :project project)))
    (orfeus/gui:gui-model-set-selected-indices model '(0 2))
    (orfeus/gui:gui-model-set-setting model :exposure 1.5)
    (orfeus/gui:gui-model-save-preset model "Bright")
    (check (= 1 (length (orfeus:project-presets project)))
           "Saving a preset did not persist it in the project")
    (check (= 2 (orfeus/gui:gui-model-apply-preset model "Bright"))
           "Preset did not report every selected photo")
    (check (= 1.5 (getf (orfeus:photo-job-overrides (first jobs)) :exposure))
           "Preset did not apply to the first selected photo")
    (check (null (orfeus:photo-job-overrides (second jobs)))
           "Preset changed an unselected photo")
    (check (= 1.5 (getf (orfeus:photo-job-overrides (third jobs)) :exposure))
           "Preset did not apply to the last selected photo")
    (orfeus/gui:gui-model-reset-selected model)
    (check (and (null (orfeus:photo-job-overrides (first jobs)))
                (null (orfeus:photo-job-overrides (third jobs))))
           "Bulk reset did not clear all selected photos")))

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
           "Preview status contains a C-style doubled percent sign")
    (check (search "NR: 35%" text)
           "Preview status does not report active noise reduction")))

(defun test-preview-priority-order ()
  (check (equal '(0 1 2 3 4)
                (orfeus/gui::preview-priority-indices 5 0))
         "Preview priority omitted photos at the project start")
  (check (equal '(2 1 3 0 4)
                (orfeus/gui::preview-priority-indices 5 2))
         "Preview priority is not selected-first and distance-ordered")
  (check (equal '(4 3 2 1 0)
                (orfeus/gui::preview-priority-indices 5 4))
         "Preview priority omitted photos at the project end")
  (check (null (orfeus/gui::preview-priority-indices 0 0))
         "Empty project produced preview work"))

(defun test-thumbnail-hit-testing ()
  (dolist (case '((0 0 104 0)
                  (103 0 104 0)
                  (104 0 104 1)
                  (207 0 104 1)
                  (0 104 104 1)))
    (destructuring-bind (event-y scroll row-height expected) case
      (check (= expected
                (orfeus/gui::thumbnail-row-at event-y scroll row-height))
             "Thumbnail hit test selected the wrong row for ~S" case))))

(defun test-thumbnail-multi-selection ()
  (multiple-value-bind (selection anchor)
      (orfeus/gui::thumbnail-selection-after-click '(1 4) 2 1 0)
    (check (equal '(2) selection) "Plain thumbnail click did not replace selection")
    (check (= 2 anchor) "Plain thumbnail click did not move the anchor"))
  (multiple-value-bind (selection anchor)
      (orfeus/gui::thumbnail-selection-after-click '(1 4) 2 1 4)
    (check (equal '(2 1 4) selection) "Control-click did not add a thumbnail")
    (check (= 2 anchor) "Control-click did not move the anchor"))
  (multiple-value-bind (selection anchor)
      (orfeus/gui::thumbnail-selection-after-click '(1 2 4) 2 1 4)
    (check (equal '(1 4) selection) "Control-click did not remove a thumbnail")
    (check (= 2 anchor) "Control-click removal did not move the anchor"))
  (multiple-value-bind (selection anchor)
      (orfeus/gui::thumbnail-selection-after-click '(1 4) 5 2 1)
    (check (equal '(2 3 4 5) selection) "Shift-click did not select an inclusive range")
    (check (= 2 anchor) "Shift-click did not preserve the anchor")))

(defun test-bundled-film-lut-menu ()
  (let ((names (mapcar #'file-namestring (orfeus/gui::gui-bundled-lut-paths))))
    (check (equal names
                  '("agfa_apx_100.cube"
                    "agfa_apx_25.cube"
                    "agfa_precisa_100.cube"
                    "agfa_ultra_color_100.cube"
                    "agfa_vista_200.cube"
                    "kodak_kodachrome_200.cube"
                    "kodak_kodachrome_25.cube"
                    "kodak_kodachrome_64.cube"
                    "kodak_kodachrome_64_generic.cube"))
           "Bundled LUT menu does not contain the pinned film set")))

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

(defun test-project-photo-mutations ()
  (let* ((first (orfeus:make-photo-job :input-path #P"/photos/one.orf"))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos (list first)))
         (model (orfeus/gui:make-gui-model :project project)))
    (multiple-value-bind (count first-index)
        (orfeus/gui:gui-model-add-photos
         model (list #P"/photos/one.orf" #P"/photos/two.orf" #P"/photos/three.dng"))
      (check (= count 2) "Adding photos did not skip the existing path")
      (check (= first-index 1) "First added photo index was incorrect")
      (check (= (orfeus/gui:gui-model-selected-index model) 1)
             "First added photo was not selected")
      (check (equal '(#P"/photos/one.orf" #P"/photos/two.orf" #P"/photos/three.dng")
                    (mapcar #'orfeus:photo-job-input-path
                            (orfeus:project-photos project)))
             "Adding photos changed order"))
    (check (equal '(1) (orfeus/gui:gui-model-selected-indices model))
           "First added photo was not the sole selection")
    (orfeus/gui:gui-model-set-selected-indices model '(1))
    (let ((removed (orfeus/gui:gui-model-remove-selected model)))
      (check (and (= 1 (length removed))
                  (equal #P"/photos/two.orf"
                         (orfeus:photo-job-input-path (first removed))))
             "Removing selected photo removed the wrong job")
      (check (= (orfeus/gui:gui-model-selected-index model) 1)
             "Selection was not clamped to the following photo"))
    (orfeus/gui:gui-model-set-selected-indices model '(0 1))
    (check (= 2 (length (orfeus/gui:gui-model-remove-selected model)))
           "Bulk removal did not remove every selected photo")
    (check (null (orfeus:project-photos project))
           "Removing all photos left a project entry")
    (check (= (orfeus/gui:gui-model-selected-index model) 0)
           "Empty project selection was not reset")))

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

(defun test-parallel-render-queue ()
  (let ((queue (orfeus/gui::make-gui-queue :workers 2))
        (release (sb-thread:make-semaphore :count 0))
        (lock (sb-thread:make-mutex))
        (started 0))
    (unwind-protect
         (progn
           (loop repeat 2
                 do (orfeus/gui::enqueue-gui-task
                     queue :preview
                     (lambda ()
                       (sb-thread:with-mutex (lock) (incf started))
                       (sb-thread:wait-on-semaphore release))))
           (loop repeat 100
                 until (sb-thread:with-mutex (lock) (= started 2))
                 do (sleep 0.01))
           (check (sb-thread:with-mutex (lock) (= started 2))
                  "Render queue did not execute two tasks concurrently"))
      (sb-thread:signal-semaphore release)
      (sb-thread:signal-semaphore release)
      (orfeus/gui::stop-gui-queue queue))))

(defun test-preview-cache-key ()
  (let* ((job (orfeus:make-photo-job :input-path #P"one.orf"))
         (first (orfeus:make-processing-settings :exposure 0.0))
         (same (orfeus:make-processing-settings :exposure 0.0))
         (changed (orfeus:make-processing-settings :exposure 1.0))
         (agfa (orfeus:make-processing-settings
                :lut-path "agfa_precisa_100.cube" :lut-strength 1.0))
         (portra (orfeus:make-processing-settings
                  :lut-path "Presetpro - Portra 160.cube" :lut-strength 1.0))
         (directory #P"/tmp/"))
    (check (equal (orfeus/gui::preview-pathname directory 0 job :after first)
                  (orfeus/gui::preview-pathname directory 0 job :after same))
           "Equivalent preview settings did not share a cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory 0 job :after first)
                       (orfeus/gui::preview-pathname directory 0 job :after changed)))
           "Edited preview settings reused a stale cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory 0 job :after first)
                       (orfeus/gui::preview-pathname directory 0 job :after agfa)))
           "Adding a LUT reused the no-LUT preview cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory 0 job :after agfa)
                       (orfeus/gui::preview-pathname directory 0 job :after portra)))
           "Distinct LUT paths reused one preview cache path")
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
  (test-preset-bulk-application)
  (test-output-path-semantics)
  (test-preview-job-identity)
  (test-preview-directory)
  (test-preview-status-percent)
  (test-preview-priority-order)
  (test-thumbnail-hit-testing)
  (test-thumbnail-multi-selection)
  (test-bundled-film-lut-menu)
  (test-file-filter-syntax)
  (test-direct-open-workflow)
  (test-project-photo-mutations)
  (test-checkbox-normalization)
  (test-render-queue)
  (test-parallel-render-queue)
  (test-preview-cache-key)
  (test-discard-pending-tasks)
  t)
