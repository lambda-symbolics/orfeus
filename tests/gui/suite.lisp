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
    (orfeus/gui:gui-model-toggle-stage model :film)
    (orfeus/gui:gui-model-save-preset model "Bright")
    (check (= 1 (length (orfeus:project-presets project)))
           "Saving a preset did not persist it in the project")
    (check (equal '(:film)
                  (orfeus:processing-preset-disabled-stages
                   (first (orfeus:project-presets project))))
           "Saving a preset lost its bypass state")
    (dolist (job jobs)
      (setf (orfeus:photo-job-disabled-stages job) '()))
    (check (= 2 (orfeus/gui:gui-model-apply-preset model "Bright"))
           "Preset did not report every selected photo")
    (check (= 1.5 (getf (orfeus:photo-job-overrides (first jobs)) :exposure))
           "Preset did not apply to the first selected photo")
    (check (null (orfeus:photo-job-overrides (second jobs)))
           "Preset changed an unselected photo")
    (check (= 1.5 (getf (orfeus:photo-job-overrides (third jobs)) :exposure))
           "Preset did not apply to the last selected photo")
    (check (and (equal '(:film)
                       (orfeus:photo-job-disabled-stages (first jobs)))
                (null (orfeus:photo-job-disabled-stages (second jobs)))
                (equal '(:film)
                       (orfeus:photo-job-disabled-stages (third jobs))))
           "Preset did not apply bypass state only to selected photos")
    (orfeus/gui:gui-model-reset-selected model)
    (check (and (null (orfeus:photo-job-overrides (first jobs)))
                (null (orfeus:photo-job-overrides (third jobs)))
                (null (orfeus:photo-job-disabled-stages (first jobs)))
                (null (orfeus:photo-job-disabled-stages (third jobs))))
           "Bulk reset did not clear all selected photos")))

(defun test-grade-workflow ()
  (let* ((jobs (loop for name in '("one" "two" "three")
                     collect (orfeus:make-photo-job
                              :input-path (make-pathname :name name :type "orf"))))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos jobs))
         (model (orfeus/gui:make-gui-model :project project)))
    (orfeus/gui:gui-model-set-setting model :exposure 1.25)
    (orfeus/gui:gui-model-set-setting model :grain-amount 0.5)
    (check (eq :bypassed (orfeus/gui:gui-model-toggle-stage model :film))
           "Toggling a fresh stage did not report a bypass")
    (check (orfeus/gui:gui-model-stage-bypassed-p model :film)
           "Bypassed stage is not reported as bypassed")
    (check (= 0.0 (orfeus:processing-settings-grain-amount
                   (orfeus/gui:gui-model-render-settings model)))
           "Bypassed film stage still renders grain")
    (check (= 0.5 (orfeus/gui:gui-model-setting model :grain-amount))
           "Bypass forgot the underlying grade value")
    (check (orfeus/gui:gui-model-stage-adjusted-p model :exposure)
           "Adjusted exposure stage is not flagged")
    (check (not (orfeus/gui:gui-model-stage-adjusted-p model :tone))
           "Untouched tone stage is flagged as adjusted")
    (multiple-value-bind (grade disabled)
        (orfeus/gui:gui-model-copy-grade model)
      (orfeus/gui:gui-model-set-selected-indices model '(1 2))
      (check (= 2 (orfeus/gui:gui-model-paste-grade model grade disabled))
             "Paste did not report every selected photo"))
    (check (= 1.25 (getf (orfeus:photo-job-overrides (second jobs)) :exposure))
           "Pasted grade lost the exposure value")
    (check (equal '(:film) (orfeus:photo-job-disabled-stages (third jobs)))
           "Pasted grade lost the bypass list")
    (orfeus/gui:gui-model-set-selected-indices model '(1))
    (let ((preset (orfeus/gui:gui-model-grab-still model)))
      (check (string= "Still 001 (two)" (orfeus:processing-preset-name preset))
             "Grabbed still name was ~S" (orfeus:processing-preset-name preset))
      (check (equal (orfeus:photo-job-input-path (second jobs))
                    (orfeus:processing-preset-source-photo preset))
             "Grabbed still forgot its source photograph")
      (check (= 0.0 (orfeus:processing-settings-grain-amount
                     (orfeus:processing-preset-settings preset)))
             "Grabbed still captured a bypassed stage as active"))
    (orfeus/gui:gui-model-reset-selected model)
    (check (null (orfeus:photo-job-disabled-stages (second jobs)))
           "Reset kept the stage bypass")))

(defun test-node-graph-model ()
  (let* ((jobs (list (orfeus:make-photo-job :input-path #P"one.orf"
                                            :overrides '(:exposure 1.0))
                     (orfeus:make-photo-job :input-path #P"two.orf")))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos jobs))
         (model (orfeus/gui:make-gui-model :project project)))
    (orfeus/gui:gui-model-ensure-graph model)
    (check (orfeus:photo-job-graph (first jobs))
           "Conversion did not store a graph")
    (check (null (orfeus:photo-job-overrides (first jobs)))
           "Conversion kept flat overrides")
    (check (= 1.0 (orfeus/gui:gui-model-setting model :exposure))
           "Converted graph lost the exposure value")
    ;; Editing a stage without a node creates one at the chain end.
    (orfeus/gui:gui-model-set-setting model :tone-shadows 0.4)
    (check (= 0.4 (orfeus/gui:gui-model-setting model :tone-shadows))
           "Tone edit did not round trip through its node")
    ;; Duplicate kinds: a second exposure node edited through selection.
    (let ((node (orfeus/gui:gui-model-add-node model :exposure)))
      (orfeus/gui:gui-model-set-node-params model node '(:exposure 2.0))
      (check (= 2.0 (orfeus/gui:gui-model-setting model :exposure))
             "Selected node did not win key routing")
      (check (= 2 (count :exposure
                         (mapcar #'orfeus:graph-node-kind
                                 (orfeus:processing-graph-nodes
                                  (orfeus:photo-job-graph (first jobs))))))
             "Stacked exposure node is missing"))
    ;; Paste the graph across the selection.
    (orfeus/gui:gui-model-set-selected-indices model '(0 1))
    (let ((graph (orfeus/gui:gui-model-copy-graph model)))
      (check (= 2 (orfeus/gui:gui-model-paste-graph model graph))
             "Graph paste did not cover the selection"))
    (check (orfeus:photo-job-graph (second jobs))
           "Graph paste skipped the second photo")
    ;; Stills carry graphs; smart apply bypasses chosen kinds.
    (orfeus/gui:gui-model-set-selected-indices model '(0))
    (let ((preset (orfeus/gui:gui-model-grab-still model)))
      (check (orfeus:processing-preset-graph preset)
             "Grabbed still lost its node graph")
      (orfeus/gui:gui-model-set-selected-indices model '(1))
      (check (= 1 (orfeus/gui:gui-model-apply-preset-graph
                   model preset :bypass-kinds (list :optics)))
             "Smart apply did not report the selection")
      (let* ((applied (orfeus:photo-job-graph (second jobs)))
             (optics (find :optics (orfeus:processing-graph-nodes applied)
                           :key #'orfeus:graph-node-kind)))
        (check (or (null optics) (orfeus:graph-node-bypassed-p optics))
               "Smart apply left optics active")))
    (orfeus/gui:gui-model-reset-selected model)
    (check (null (orfeus:photo-job-graph (second jobs)))
           "Reset kept the node graph")))

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
  ;; The persistent cache keys previews by content: renames keep the
  ;; fingerprint, and eviction clears out only stale files.
  (let* ((directory (merge-pathnames
                     (format nil "orfeus-preview-cache-~D-~D/"
                             (get-universal-time)
                             (random most-positive-fixnum))
                     (uiop:temporary-directory)))
         (original (merge-pathnames "photo-a.bin" directory))
         (renamed (merge-pathnames "photo-b.bin" directory)))
    (ensure-directories-exist directory)
    (unwind-protect
         (progn
           (with-open-file (stream original :direction :output
                                            :element-type '(unsigned-byte 8))
             (dotimes (index 200000)
               (write-byte (mod index 251) stream)))
           (let ((key (orfeus/gui::photo-content-key original)))
             (rename-file original renamed)
             (check (string= key (orfeus/gui::photo-content-key renamed))
                    "Renaming a photo changed its content key")
             (check (= 64 (length key)) "Content key is not full SHA-256")
             ;; A middle-only edit defeated the former head/tail sampler.
             (with-open-file (stream renamed :direction :io
                                             :if-exists :overwrite
                                             :element-type '(unsigned-byte 8))
               (file-position stream 100000)
               (write-byte (logxor #xff (read-byte stream)) stream))
             (check (not (string= key (orfeus/gui::photo-content-key renamed)))
                    "Middle-byte replacement reused a stale source digest"))
           (let ((old-file (merge-pathnames "old.jpg" directory))
                 (new-file (merge-pathnames "new.jpg" directory)))
             (with-open-file (stream old-file :direction :output)
               (write-line "old" stream))
             (with-open-file (stream new-file :direction :output)
               (write-line "new" stream))
             (sb-posix:utime (namestring old-file) 0 0)
             (orfeus/gui::evict-stale-previews directory)
             (check (null (probe-file old-file))
                    "Eviction kept an ancient preview")
             (check (probe-file new-file)
                    "Eviction deleted a fresh preview")))
      (uiop:delete-directory-tree directory :validate t
                                            :if-does-not-exist :ignore))))

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
  (let* ((first (orfeus:make-processing-settings :exposure 0.0))
         (same (orfeus:make-processing-settings :exposure 0.0))
         (changed (orfeus:make-processing-settings :exposure 1.0))
         (agfa (orfeus:make-processing-settings
                :lut-path "agfa_precisa_100.cube" :lut-strength 1.0))
         (portra (orfeus:make-processing-settings
                  :lut-path "Presetpro - Portra 160.cube" :lut-strength 1.0))
         (directory #P"/tmp/")
         (key "0123456789abcdef")
         (other "fedcba9876543210"))
    (check (equal (orfeus/gui::preview-pathname directory key :after first)
                  (orfeus/gui::preview-pathname directory key :after same))
           "Equivalent preview settings did not share a cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory key :after first)
                       (orfeus/gui::preview-pathname directory key :after changed)))
           "Edited preview settings reused a stale cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory key :after first)
                       (orfeus/gui::preview-pathname directory key :after agfa)))
           "Adding a LUT reused the no-LUT preview cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory key :after agfa)
                       (orfeus/gui::preview-pathname directory key :after portra)))
           "Distinct LUT paths reused one preview cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory key :before first)
                       (orfeus/gui::preview-pathname directory key :after first)))
           "Before and After previews shared one cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory key :after first)
                       (orfeus/gui::preview-pathname directory other :after first)))
           "Distinct photo content shared one preview cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory key :after first
                                                       :max-width 1024)
                       (orfeus/gui::preview-pathname directory key :after first
                                                       :max-width 2048)))
           "Distinct preview dimensions shared one cache path")
    (check (= 64 (length (orfeus/gui::preview-settings-key first)))
           "Preview recipe key is not full SHA-256")))

(defun test-preview-cache-dependencies-and-permissions ()
  (let* ((directory (merge-pathnames
                     (format nil "orfeus-preview-security-~D/" (random most-positive-fixnum))
                     (uiop:temporary-directory)))
         (lut (merge-pathnames "look.cube" directory))
         (output (merge-pathnames "preview.jpg" directory))
         (coalesced (merge-pathnames "coalesced.jpg" directory))
         (raced (merge-pathnames "raced.jpg" directory))
         (session (merge-pathnames "session/" directory)))
    (ensure-directories-exist directory)
    (orfeus/gui::make-secure-preview-directory session)
    (unwind-protect
         (progn
           (orfeus/gui::protect-preview-cache-path directory #o700)
           (with-open-file (stream lut :direction :output :if-exists :supersede)
             (write-line "first LUT" stream))
           (let* ((settings (orfeus:make-processing-settings
                             :lut-path lut :lut-strength 1.0))
                  (first-key (orfeus/gui::preview-settings-key settings)))
             (with-open-file (stream lut :direction :output :if-exists :supersede)
               (write-line "replacement LUT" stream))
             (check (not (string= first-key
                                  (orfeus/gui::preview-settings-key settings)))
                    "Same-path LUT replacement reused a stale recipe key"))
           (let* ((node (orfeus:make-graph-node :id 1 :kind :exposure
                                                :params '(:exposure 1.0)
                                                :inputs '(0) :position '(10 20)))
                  (graph (orfeus:make-processing-graph :nodes (list node) :output 1))
                  (first-key (orfeus/gui::preview-settings-key graph)))
             (setf (orfeus:graph-node-position node) '(900 700)
                   (orfeus::graph-node-kind-states node)
                   '((:film :params (:lut-path "inactive.cube"
                                     :lut-strength 1.0))))
             (check (string= first-key (orfeus/gui::preview-settings-key graph))
                    "Editor-only graph state changed the render recipe key"))
           (orfeus/gui::call-with-preview-cache-fill
            output
            (lambda (temporary)
              (check (string-equal "jpg" (pathname-type temporary))
                     "Cache fill temporary lost the renderer output type")
              (with-open-file (stream temporary :direction :output
                                               :if-exists :supersede)
                (write-line "jpeg" stream))))
           (check (= #o700 (logand #o777 (sb-posix:stat-mode
                                          (sb-posix:stat (namestring directory)))))
                  "Preview cache directory is not mode 0700")
           (check (= #o600 (logand #o777 (sb-posix:stat-mode
                                          (sb-posix:stat (namestring output)))))
                  "Preview cache file is not mode 0600")
           (let* ((symbol 'orfeus/gui::protect-preview-cache-path)
                  (original (symbol-function symbol)))
             (unwind-protect
                  (progn
                    (setf (symbol-function symbol)
                          (lambda (pathname mode)
                            (declare (ignore pathname mode))
                            nil))
                    (check (not (orfeus/gui::preview-cache-hit-p output))
                           "A cache hit survived failed permission verification")
                    (check (not (probe-file output))
                           "An insecure cache hit was not removed"))
               (setf (symbol-function symbol) original)))
           (orfeus/gui::call-with-preview-cache-fill
            output
            (lambda (temporary)
              (with-open-file (stream temporary :direction :output)
                (write-line "jpeg" stream))))
           (let ((fills 0)
                 (lock (sb-thread:make-mutex)))
             (flet ((fill-cache ()
                      (orfeus/gui::call-with-preview-cache-fill
                       coalesced
                       (lambda (temporary)
                         (sb-thread:with-mutex (lock) (incf fills))
                         (sleep 0.05)
                         (with-open-file (stream temporary :direction :output)
                           (write-line "one fill" stream))))))
               (let ((first (sb-thread:make-thread #'fill-cache))
                     (second (sb-thread:make-thread #'fill-cache)))
                 (sb-thread:join-thread first)
                 (sb-thread:join-thread second)))
             (check (= 1 fills) "Concurrent preview fills were not coalesced"))
           (with-open-file (stream lut :direction :output :if-exists :supersede)
             (write-line "stable LUT" stream))
           (let* ((settings (orfeus:make-processing-settings
                             :lut-path lut :lut-strength 1.0))
                  (key (orfeus/gui::preview-settings-key settings))
                  (current-p (lambda ()
                               (string= key
                                        (orfeus/gui::preview-settings-key settings))))
                  (rejected-p nil))
             (handler-case
                 (orfeus/gui::call-with-preview-cache-fill
                  raced
                  (lambda (temporary)
                    (with-open-file (stream temporary :direction :output)
                      (write-line "rendered" stream))
                    (with-open-file (stream lut :direction :output
                                                :if-exists :supersede)
                      (write-line "changed during render" stream)))
                  :validation-function current-p)
               (error () (setf rejected-p t)))
             (check rejected-p "A LUT race published a mismatched cache entry")
             (check (not (probe-file raced))
                    "A cache entry survived failed post-render LUT validation")
             (with-open-file (stream lut :direction :output :if-exists :supersede)
               (write-line "stable again" stream))
             (setf key (orfeus/gui::preview-settings-key settings))
             (orfeus/gui::call-with-preview-cache-fill
              raced
              (lambda (temporary)
                (with-open-file (stream temporary :direction :output)
                  (write-line "rendered" stream))))
             (setf rejected-p nil)
             (handler-case
                 (orfeus/gui::materialize-preview-cache-hit
                  raced session
                  :validation-function
                  (lambda ()
                    (with-open-file (stream lut :direction :output
                                                :if-exists :supersede)
                      (write-line "changed during hit" stream))
                    (funcall current-p)))
               (error () (setf rejected-p t)))
             (check rejected-p "A LUT race materialized a mismatched cache hit")
             (check (null (uiop:directory-files session))
                    "A failed LUT hit validation retained a display copy"))
           (sb-posix:utime (namestring output) 0 0)
           (orfeus/gui::call-with-active-preview-cache-file
            output (lambda () (orfeus/gui::evict-stale-previews directory)))
           (check (probe-file output) "Eviction deleted an active cache hit"))
      (uiop:delete-directory-tree directory :validate t
                                           :if-does-not-exist :ignore))))

(defun test-preview-cache-recovery-and-generation-load ()
  (let* ((directory (merge-pathnames
                     (format nil "orfeus-preview-lock-~D/" (random most-positive-fixnum))
                     (uiop:temporary-directory)))
         (cache (merge-pathnames "hit.jpg" directory))
         (session (merge-pathnames "session/" directory))
         (lock (orfeus/gui::preview-lock-directory cache)))
    (ensure-directories-exist directory)
    (orfeus/gui::make-secure-preview-directory session)
    (unwind-protect
         (progn
           (sb-posix:mkdir (namestring lock) #o700)
           (with-open-file (stream (orfeus/gui::preview-lock-owner-pathname lock)
                                  :direction :output :if-exists :supersede)
             (write (list :pid 99999999
                          :created (get-universal-time)
                          :token "dead")
                    :stream stream))
           (let ((held (orfeus/gui::acquire-preview-lock cache :ignore-hit-p t
                                                               :timeout 1.0d0)))
             (check held "A fresh lock owned by a dead process was not reclaimed")
             (orfeus/gui::release-preview-lock held))
           (flet ((check-abandoned-lock (owner description)
                    (sb-posix:mkdir (namestring lock) #o700)
                    (when owner
                      (with-open-file
                          (stream (orfeus/gui::preview-lock-owner-pathname lock)
                                  :direction :output :if-exists :supersede)
                        (write owner :stream stream)))
                    (sb-posix:utime (namestring lock) 0 0)
                    (let* ((orfeus/gui::*preview-lock-owner-grace-seconds* 0)
                           (held (orfeus/gui::acquire-preview-lock
                                  cache :ignore-hit-p t :timeout 1.0d0)))
                      (check held "~A was not reclaimed after its grace" description)
                      (orfeus/gui::release-preview-lock held))))
             (check-abandoned-lock nil "An ownerless preview lock")
             (check-abandoned-lock '(:pid "bad" :created nil)
                                   "A malformed preview lock"))
           (with-open-file (stream cache :direction :output :if-exists :supersede)
             (write-line "jpeg" stream))
           (orfeus/gui::protect-preview-cache-path cache #o600)
           (let ((display (orfeus/gui::materialize-preview-cache-hit cache session)))
             (orfeus/gui::call-with-preview-key-lock
              cache (lambda () (delete-file cache)))
             (check (probe-file display)
                    "Materialized cache hit was lost during concurrent eviction")))
      (uiop:delete-directory-tree directory :validate t
                                            :if-does-not-exist :ignore)))
  (let ((queue (orfeus/gui::make-gui-queue))
        (release (sb-thread:make-semaphore)))
    (unwind-protect
         (progn
           (orfeus/gui::enqueue-gui-task queue :after
                                         (lambda () (sb-thread:wait-on-semaphore release))
                                         :generation 10)
           (orfeus/gui::enqueue-gui-task queue :after (lambda () nil)
                                         :generation 11)
           (sleep 0.02)
           (check (= 1 (orfeus/gui::gui-queue-load queue :generation 10))
                  "Generation progress omitted current foreground work")
           (check (= 1 (orfeus/gui::gui-queue-load queue :generation 11))
                  "Generation progress counted stale work"))
      (sb-thread:signal-semaphore release)
      (orfeus/gui::stop-gui-queue queue))))

(defun test-preview-progress-cancellation ()
  (multiple-value-bind (percent total generation)
      (orfeus/gui::preview-progress-state 2 10 8 7)
    (check (= 2 percent) "New preview generation inherited stale progress")
    (check (= 2 total) "New preview generation retained the old batch total")
    (check (= 8 generation) "New preview generation was not recorded"))
  (multiple-value-bind (percent total generation)
      (orfeus/gui::preview-progress-state 0 7 9 8)
    (declare (ignore generation))
    (check (and (zerop percent) (zerop total))
           "Cancelled preview generation did not reset progress")))

(defun test-preview-recipe-snapshot ()
  (let* ((points (list 0.0 0.0 0.33 0.25 0.66 0.75 1.0 1.0))
         (node (orfeus:make-graph-node :id 1 :kind :curves
                                      :params (list :red-points points)))
         (graph (orfeus:make-processing-graph :nodes (list node) :output 1))
         (snapshot (orfeus/gui::preview-recipe-snapshot graph))
         (snapshot-node (first (orfeus:processing-graph-nodes snapshot))))
    (setf (second points) 0.5
          (orfeus:graph-node-bypassed-p node) t)
    (check (= 0.0 (second (getf (orfeus:graph-node-params snapshot-node)
                                :red-points)))
           "Preview graph snapshot shared nested parameter storage")
    (check (not (orfeus:graph-node-bypassed-p snapshot-node))
           "Preview graph snapshot shared node state")
    (check (not (equal (orfeus/gui::preview-settings-key graph)
                       (orfeus/gui::preview-settings-key snapshot)))
           "Mutating the live graph also changed the preview snapshot"))
  (let* ((settings (orfeus:make-processing-settings :exposure 0.5))
         (snapshot (orfeus/gui::preview-recipe-snapshot settings)))
    (setf (orfeus:processing-settings-exposure settings) 1.5)
    (check (= 0.5 (orfeus:processing-settings-exposure snapshot))
           "Preview settings snapshot shared mutable state"))
  (check (orfeus/gui::crop-preview-current-p 7 7)
         "Current bypass preview generation was rejected")
  (check (not (orfeus/gui::crop-preview-current-p 8 7))
         "Stale cropped preview generation allowed crop interaction")
  (check (not (orfeus/gui::crop-preview-current-p 8 nil))
         "Missing bypass preview allowed crop interaction"))

(defun test-gallery-selection-provenance ()
  (let* ((project-preset (orfeus:make-processing-preset
                          :name "Shared" :settings
                          (orfeus:make-processing-settings :exposure 1.0)))
         (local-preset (orfeus:make-processing-preset
                        :name "Shared" :settings
                        (orfeus:make-processing-settings :exposure 2.0)))
         (project-still
           (orfeus/gui::make-project-gallery-still project-preset))
         (local-still (orfeus/gui::make-local-gallery-still local-preset))
         (stills (list project-still local-still))
         (local-key (orfeus/gui::gallery-selection-key stills 1)))
    (check (and (eq :local (first local-key))
                (equal (second local-key)
                       (second (orfeus/gui::gallery-still-key project-still))))
           "Gallery key lost same-name local provenance: ~S" local-key)
    (check (= 0 (orfeus/gui::gallery-selection-index
                 (list local-still project-still) local-key))
           "Gallery refresh did not re-find the selected local still")
    (check (null (orfeus/gui::gallery-selection-index
                  (list project-still) local-key))
           "Removed local still retargeted the same-name project still")
    (check (eq local-still
               (orfeus/gui::gallery-selected-still stills 1))
           "Visible gallery selection did not resolve to the local still")
    (let* ((job (orfeus:make-photo-job :input-path #P"target.orf"))
           (project (orfeus:make-project :output-directory #P"exports/"
                                         :photos (list job)
                                         :presets (list project-preset)))
           (model (orfeus/gui:make-gui-model :project project)))
      (orfeus/gui:gui-model-apply-preset-graph
       model (orfeus/gui::gallery-still-preset
              (orfeus/gui::gallery-selected-still stills 1)))
      (check (= 2.0 (orfeus/gui:gui-model-setting model :exposure))
             "Selected local still applied the same-name project preset"))
    (check (string= "local still"
                    (orfeus/gui::gallery-still-origin-description local-still))
           "Still status description lost provenance")))

(defun test-empty-graph-editor-geometry ()
  (check (>= orfeus/gui::*inspector-min-height* 378)
         "Minimum inspector height clips the Curves reset control")
  (multiple-value-bind (x y width height)
      (orfeus/gui::graph-output-box-position '())
    (check (equal (list 18 50 orfeus/gui::*graph-node-width* 22)
                  (list x y width height))
           "Empty graph OUT geometry failed: ~S"
           (list x y width height)))
  (let ((node (orfeus:make-graph-node :id 1 :kind :exposure
                                      :position '(30.0 70.0))))
    (multiple-value-bind (x y width height)
        (orfeus/gui::graph-output-box-position (list node))
      (declare (ignore x width height))
      (check (= 122 y) "Populated graph OUT geometry was ~D" y))))

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
  (test-grade-workflow)
  (test-node-graph-model)
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
  (test-preview-cache-dependencies-and-permissions)
  (test-preview-cache-recovery-and-generation-load)
  (test-preview-progress-cancellation)
  (test-preview-recipe-snapshot)
  (test-gallery-selection-provenance)
  (test-empty-graph-editor-geometry)
  (test-discard-pending-tasks)
  t)
