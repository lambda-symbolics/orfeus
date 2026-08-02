(in-package #:orfeus/gui)

(defstruct gui-model
  "Frontend state that does not belong in an Orfeus project."
  project
  (selected-index 0 :type fixnum)
  (selected-indices '() :type list)
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

(defun gui-photos-project (pathnames)
  "Return one project containing PATHNAMES in selection order."
  (let* ((inputs (mapcar #'pathname pathnames))
         (first-input (or (first inputs)
                          (error "At least one photograph is required.")))
         (directory (uiop:pathname-directory-pathname first-input))
         (output-directory (merge-pathnames #P"orfeus-exports/" directory)))
    (make-project :output-directory output-directory
                  :defaults (gui-default-processing-settings)
                  :photos (mapcar (lambda (input)
                                    (make-photo-job :input-path input))
                                  inputs))))

(defun gui-photo-project (pathname)
  "Return a one-photo project for PATHNAME."
  (gui-photos-project (list pathname)))

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
        (gui-model-selected-indices model)
        (if (project-photos project) '(0) '())
        (gui-model-edit-target model) :photo)
  model)

(defun gui-model-set-selected-indices (model indices)
  "Set MODEL's valid selected row INDICES and preview anchor."
  (let* ((count (length (project-photos (gui-model-project model))))
         (selected (sort (remove-duplicates
                          (remove-if-not (lambda (index)
                                           (and (integerp index)
                                                (<= 0 index)
                                                (< index count)))
                                         indices)
                          :test #'=)
                         #'<)))
    (setf (gui-model-selected-indices model) selected)
    (when selected
      (setf (gui-model-selected-index model) (first selected)))
    selected))

(defun gui-model-selected-jobs (model)
  "Return MODEL's selected photo jobs in project order."
  (let ((photos (project-photos (gui-model-project model))))
    (mapcar (lambda (index) (nth index photos))
            (gui-model-selected-indices model))))

(defun gui-model-add-photos (model pathnames)
  "Append unique PATHNAMES to MODEL's project and select the first addition."
  (let* ((project (gui-model-project model))
         (photos (project-photos project))
         (existing (mapcar #'photo-job-input-path photos))
         (inputs (remove-duplicates (mapcar #'pathname pathnames) :test #'equal))
         (new-inputs (remove-if (lambda (path) (member path existing :test #'equal))
                                inputs))
         (first-index (length photos)))
    (when new-inputs
      (when (null photos)
        (setf (project-output-directory project)
              (merge-pathnames #P"orfeus-exports/"
                               (uiop:pathname-directory-pathname
                                (first new-inputs)))))
      (setf (project-photos project)
            (append photos
                    (mapcar (lambda (input)
                              (make-photo-job :input-path input))
                            new-inputs))
            (gui-model-selected-index model) first-index
            (gui-model-selected-indices model) (list first-index)
            (gui-model-edit-target model) :photo))
    (values (length new-inputs) first-index)))

(defun gui-model-remove-selected (model)
  "Remove and return MODEL's selected photo jobs, safely clamping selection."
  (let* ((project (gui-model-project model))
         (photos (project-photos project))
         (indices (or (gui-model-selected-indices model)
                      (list (gui-model-selected-index model))))
         (removed (loop for photo in photos
                        for index from 0
                        when (member index indices) collect photo))
         (remaining (loop for photo in photos
                          for index from 0
                          unless (member index indices) collect photo)))
    (when removed
      (let ((next-index (max 0 (min (first indices) (1- (length remaining))))))
        (setf (project-photos project) remaining
              (gui-model-selected-index model) next-index
              (gui-model-selected-indices model)
              (if remaining (list next-index) '()))))
    removed))

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

(defun gui-model-save-preset (model name)
  "Save selected effective settings under NAME, replacing a same-named preset."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) name))
         (project (gui-model-project model)))
    (unless (plusp (length trimmed))
      (error "Preset name cannot be empty."))
    (let* ((preset (make-processing-preset
                    :name trimmed
                    :settings (orfeus::copy-processing-settings
                               (gui-model-selected-settings model))))
           (existing (project-presets project))
           (position (position trimmed existing :test #'string-equal
                                                :key #'processing-preset-name)))
      (if position
          (setf (nth position existing) preset)
          (setf (project-presets project) (append existing (list preset))))
      preset)))

(defun gui-model-apply-preset (model name)
  "Apply named preset NAME to all selected photos and return the changed count."
  (let* ((project (gui-model-project model))
         (preset (find name (project-presets project)
                       :test #'string-equal :key #'processing-preset-name)))
    (unless preset
      (error "Unknown preset ~S." name))
    (let ((overrides (orfeus::processing-settings->sexp
                      (processing-preset-settings preset)))
          (jobs (or (gui-model-selected-jobs model)
                    (let ((job (gui-model-selected-job model)))
                      (and job (list job))))))
      (dolist (job jobs)
        (setf (photo-job-overrides job) (copy-list overrides)))
      (length jobs))))

(defun setting-reader (key)
  (ecase key
    (:exposure #'orfeus:processing-settings-exposure)
    (:white-balance-temperature #'orfeus:processing-settings-white-balance-temperature)
    (:white-balance-tint #'orfeus:processing-settings-white-balance-tint)
    (:noise-reduction #'orfeus:processing-settings-noise-reduction)
    (:tone-blacks #'orfeus:processing-settings-tone-blacks)
    (:tone-shadows #'orfeus:processing-settings-tone-shadows)
    (:tone-dark-mids #'orfeus:processing-settings-tone-dark-mids)
    (:tone-midtones #'orfeus:processing-settings-tone-midtones)
    (:tone-light-mids #'orfeus:processing-settings-tone-light-mids)
    (:tone-highlights #'orfeus:processing-settings-tone-highlights)
    (:tone-whites #'orfeus:processing-settings-tone-whites)
    (:lens-correction-p #'orfeus:processing-settings-lens-correction-p)
    (:lens-correction-strength
     #'orfeus:processing-settings-lens-correction-strength)
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
    (:tone-blacks (setf (orfeus:processing-settings-tone-blacks settings) value))
    (:tone-shadows (setf (orfeus:processing-settings-tone-shadows settings) value))
    (:tone-dark-mids (setf (orfeus:processing-settings-tone-dark-mids settings) value))
    (:tone-midtones (setf (orfeus:processing-settings-tone-midtones settings) value))
    (:tone-light-mids (setf (orfeus:processing-settings-tone-light-mids settings) value))
    (:tone-highlights (setf (orfeus:processing-settings-tone-highlights settings) value))
    (:tone-whites (setf (orfeus:processing-settings-tone-whites settings) value))
    (:lens-correction-p (setf (orfeus:processing-settings-lens-correction-p settings) value))
    (:lens-correction-strength
     (setf (orfeus:processing-settings-lens-correction-strength settings) value))
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
  "Clear overrides from all selected photos."
  (dolist (job (or (gui-model-selected-jobs model)
                   (let ((job (gui-model-selected-job model)))
                     (and job (list job)))))
    (setf (photo-job-overrides job) '()))
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
