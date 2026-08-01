(in-package #:orfeus/gui)

(defstruct gui-model
  "Frontend state that does not belong in an Orfeus project."
  project
  (selected-index 0 :type fixnum)
  (edit-target :photo :type (member :photo :defaults))
  project-path)

(defun gui-bundled-lut-paths ()
  "Return bundled CUBE LUT pathnames in stable display order."
  (sort (directory (merge-pathnames #P"*.cube"
                                    (asdf:system-relative-pathname
                                     "orfeus" #P"data/luts/")))
        #'string-lessp :key #'file-namestring))

(defun gui-default-lut-path ()
  "Return the bundled Agfa Precisa 100 LUT pathname, when installed."
  (find "agfa_precisa_100" (gui-bundled-lut-paths)
        :test #'string-equal :key #'pathname-name))

(defun gui-default-processing-settings ()
  (make-processing-settings
   :lut-path (let ((path (gui-default-lut-path)))
               (and path (namestring path)))))

(defun gui-empty-project ()
  "Return an empty in-memory project suitable for a newly opened GUI."
  (make-project :output-directory #P"exports/"
                :defaults (gui-default-processing-settings)
                :photos '()))

(defun gui-photo-project (pathname)
  "Return a one-photo project for PATHNAME with a sibling Orfeus export directory."
  (let* ((input (pathname pathname))
         (directory (uiop:pathname-directory-pathname input))
         (output-directory (merge-pathnames #P"orfeus-exports/" directory)))
    (make-project :output-directory output-directory
                  :defaults (gui-default-processing-settings)
                  :photos (list (make-photo-job :input-path input)))))

(defun gui-open-kind (pathname)
  "Classify PATHNAME as :PROJECT, :PHOTO, or NIL for an unsupported extension."
  (let ((type (string-downcase (or (pathname-type (pathname pathname)) ""))))
    (cond ((member type '("sexp" "lisp") :test #'string=) :project)
          ((member type '("orf" "dng") :test #'string=) :photo))))

(defun gui-model-replace-project (model project &optional project-path)
  "Replace MODEL's project and reset its transient selection state."
  (setf (gui-model-project model) project
        (gui-model-project-path model) project-path
        (gui-model-selected-index model) 0
        (gui-model-edit-target model) :photo)
  model)

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

(defun gui-boolean-value (value)
  "Normalize a frontend checkbox VALUE to a Common Lisp boolean."
  (not (or (null value)
           (eql value 0)
           (and (stringp value)
                (member value '("" "0" "false" "nil")
                        :test #'string-equal)))))

(defun gui-model-set-setting (model key value)
  "Set KEY to VALUE at MODEL's current :PHOTO or :DEFAULTS edit target."
  (when (member key '(:lens-correction-p
                      :chromatic-aberration-correction-p))
    (setf value (gui-boolean-value value)))
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

(defun gui-photo-output-path (model job)
  "Return JOB's render output using the shared core project semantics."
  (photo-job-render-output (gui-model-project model) job))

(defun gui-preview-event-current-p (model event generation)
  "Return true when preview completion EVENT still matches MODEL and GENERATION."
  (let ((job (gui-model-selected-job model)))
    (and job
         (= (second event) generation)
         (= (third event) (gui-model-selected-index model))
         (eq (fourth event) job))))
