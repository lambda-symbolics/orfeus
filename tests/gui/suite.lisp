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

(defun test-key-events ()
  (check (eq :before-after (orfeus/gui::gui-key-action "98 0 b")) "B shortcut not parsed")
  (check (eq :preview (orfeus/gui::gui-key-action "114 0 r")) "R shortcut not parsed")
  (check (eq :render (orfeus/gui::gui-key-action "101 0 e")) "E shortcut not parsed")
  (check (eq :previous (orfeus/gui::gui-key-action "65362 0 ")) "Up key not parsed")
  (check (eq :next (orfeus/gui::gui-key-action "65363 0 ")) "Right key not parsed")
  (check (null (orfeus/gui::gui-key-action "malformed")) "Malformed key event should be ignored")
  (check (null (orfeus/gui::gui-key-action "98junk 0 b"))
         "Key code with a numeric prefix and junk was accepted"))

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

(defun run-tests ()
  (test-model-settings)
  (test-output-path-semantics)
  (test-preview-job-identity)
  (test-preview-directory)
  (test-key-events)
  (test-render-queue)
  t)
