(in-package #:orfeus/gui)

(defstruct gui-model
  "Frontend state that does not belong in an Orfeus project."
  project
  (selected-index 0 :type fixnum)
  (edit-target :photo :type (member :photo :defaults))
  project-path)

(defun gui-model-selected-job (model)
  "Return MODEL's selected photo job, or NIL for an empty project."
  (nth (gui-model-selected-index model)
       (project-photos (gui-model-project model))))

(defun gui-model-selected-settings (model)
  "Return effective processing settings for MODEL's selected photograph."
  (let* ((project (gui-model-project model))
         (job (gui-model-selected-job model)))
    (if job
        (processing-settings-with-overrides (project-defaults project)
                                            (photo-job-overrides job))
        (project-defaults project))))

(defun setting-reader (key)
  (ecase key
    (:exposure #'orfeus:processing-settings-exposure)
    (:white-balance-temperature #'orfeus:processing-settings-white-balance-temperature)
    (:white-balance-tint #'orfeus:processing-settings-white-balance-tint)
    (:noise-reduction #'orfeus:processing-settings-noise-reduction)
    (:lens-correction-p #'orfeus:processing-settings-lens-correction-p)
    (:chromatic-aberration-correction-p #'orfeus:processing-settings-chromatic-aberration-correction-p)
    (:lut-path #'orfeus:processing-settings-lut-path)
    (:lut-strength #'orfeus:processing-settings-lut-strength)
    (:grain-amount #'orfeus:processing-settings-grain-amount)
    (:grain-size #'orfeus:processing-settings-grain-size)))

(defun gui-model-setting (model key)
  "Read one effective setting from MODEL."
  (funcall (setting-reader key) (gui-model-selected-settings model)))

(defun plist-put (plist key value)
  (let ((copy (copy-list plist)))
    (if (member key copy)
        (setf (getf copy key) value)
        (setf copy (list* key value copy)))
    copy))

(defun set-default-setting (settings key value)
  (ecase key
    (:exposure (setf (orfeus:processing-settings-exposure settings) value))
    (:white-balance-temperature (setf (orfeus:processing-settings-white-balance-temperature settings) value))
    (:white-balance-tint (setf (orfeus:processing-settings-white-balance-tint settings) value))
    (:noise-reduction (setf (orfeus:processing-settings-noise-reduction settings) value))
    (:lens-correction-p (setf (orfeus:processing-settings-lens-correction-p settings) value))
    (:chromatic-aberration-correction-p (setf (orfeus:processing-settings-chromatic-aberration-correction-p settings) value))
    (:lut-path (setf (orfeus:processing-settings-lut-path settings) value))
    (:lut-strength (setf (orfeus:processing-settings-lut-strength settings) value))
    (:grain-amount (setf (orfeus:processing-settings-grain-amount settings) value))
    (:grain-size (setf (orfeus:processing-settings-grain-size settings) value)))
  value)

(defun gui-model-set-setting (model key value)
  "Set KEY to VALUE at MODEL's current :PHOTO or :DEFAULTS edit target."
  (if (eq (gui-model-edit-target model) :defaults)
      (set-default-setting (project-defaults (gui-model-project model)) key value)
      (let ((job (or (gui-model-selected-job model)
                     (error "Cannot edit a photo in an empty project."))))
        (setf (photo-job-overrides job)
              (plist-put (photo-job-overrides job) key value))))
  value)

(defun gui-model-reset-selected (model)
  "Clear all overrides from MODEL's selected photo."
  (let ((job (gui-model-selected-job model)))
    (when job (setf (photo-job-overrides job) '())))
  model)
