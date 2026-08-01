(defpackage #:orfeus/gui-tests
  (:use #:cl)
  (:export #:run-tests))

(in-package #:orfeus/gui-tests)

(defun check (condition control &rest arguments)
  (unless condition (error (apply #'format nil control arguments))))

(defun run-tests ()
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
    (check (null (orfeus:photo-job-overrides job)) "Reset did not clear overrides")
    t))
