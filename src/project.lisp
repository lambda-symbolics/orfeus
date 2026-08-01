(in-package #:orfeus)

(defparameter *processing-setting-keys*
  '(:exposure :white-balance-temperature :white-balance-tint
    :noise-reduction :lens-correction-p :lens-correction-strength
    :chromatic-aberration-correction-p :lut-path :lut-strength
    :grain-amount :grain-size)
  "Keys accepted in processing setting S-expressions.")

(defstruct processing-settings
  "Frontend-independent controls for one rendering operation."
  (exposure 0.0)
  (white-balance-temperature nil)
  (white-balance-tint 0.0)
  (noise-reduction 0.35)
  (lens-correction-p t)
  (lens-correction-strength 1.0)
  (chromatic-aberration-correction-p t)
  (lut-path nil)
  (lut-strength 1.0)
  (grain-amount 0.0)
  (grain-size 1.0))

(defstruct export-settings
  "Frontend-independent options for encoded photo exports."
  (jpeg-quality 92 :type (integer 1 100))
  (max-width nil :type (or null (integer 1)))
  (max-height nil :type (or null (integer 1)))
  (preserve-metadata-p t :type boolean))

(defstruct photo-job
  "One input photograph and its optional per-photo setting overrides."
  input-path
  output-path
  (overrides '()))

(defstruct project
  "A batch of photographs sharing processing defaults and an output directory."
  output-directory
  (defaults (make-processing-settings))
  (export-settings (make-export-settings))
  (photos '()))

(defun project-invalid (datum control &rest arguments)
  (error 'invalid-project-data
         :datum datum
         :reason (apply #'format nil control arguments)))

(defun plist-known-keys-p (plist keys)
  (and (evenp (length plist))
       (loop for key in plist by #'cddr
             always (member key keys))))

(defun processing-value-valid-p (key value)
  (case key
    ((:exposure :white-balance-tint :noise-reduction :lut-strength
      :grain-amount :grain-size)
     (realp value))
    (:lens-correction-strength
     (and (realp value) (<= 0 value 2)))
    (:white-balance-temperature
     (or (null value) (and (realp value) (plusp value))))
    ((:lens-correction-p :chromatic-aberration-correction-p)
     (typep value 'boolean))
    (:lut-path
     (or (null value) (stringp value)))
    (otherwise nil)))

(defun processing-plist-validate (plist)
  (unless (and (listp plist)
               (plist-known-keys-p plist *processing-setting-keys*))
    (project-invalid plist "expected a property list of processing settings"))
  (loop for (key value) on plist by #'cddr
        unless (processing-value-valid-p key value)
          do (project-invalid plist "invalid value ~S for ~S" value key))
  plist)

(defun processing-settings->sexp (settings)
  (list :exposure (processing-settings-exposure settings)
        :white-balance-temperature
        (processing-settings-white-balance-temperature settings)
        :white-balance-tint (processing-settings-white-balance-tint settings)
        :noise-reduction (processing-settings-noise-reduction settings)
        :lens-correction-p (processing-settings-lens-correction-p settings)
        :lens-correction-strength
        (processing-settings-lens-correction-strength settings)
        :chromatic-aberration-correction-p
        (processing-settings-chromatic-aberration-correction-p settings)
        :lut-path (processing-settings-lut-path settings)
        :lut-strength (processing-settings-lut-strength settings)
        :grain-amount (processing-settings-grain-amount settings)
        :grain-size (processing-settings-grain-size settings)))

(defun sexp->processing-settings (sexp)
  (processing-plist-validate sexp)
  (apply #'make-processing-settings sexp))

(defun export-settings->sexp (settings)
  (list :jpeg-quality (export-settings-jpeg-quality settings)
        :max-width (export-settings-max-width settings)
        :max-height (export-settings-max-height settings)
        :preserve-metadata-p (export-settings-preserve-metadata-p settings)))

(defun sexp->export-settings (sexp)
  (unless (and (listp sexp)
               (plist-known-keys-p
                sexp '(:jpeg-quality :max-width :max-height :preserve-metadata-p)))
    (project-invalid sexp "expected a property list of export settings"))
  (let ((quality (getf sexp :jpeg-quality 92))
        (max-width (getf sexp :max-width))
        (max-height (getf sexp :max-height))
        (preserve-metadata-p (getf sexp :preserve-metadata-p t)))
    (unless (and (integerp quality) (<= 1 quality 100))
      (project-invalid sexp ":jpeg-quality must be an integer from 1 to 100"))
    (dolist (entry (list (cons :max-width max-width)
                         (cons :max-height max-height)))
      (unless (or (null (rest entry))
                  (and (integerp (rest entry)) (plusp (rest entry))))
        (project-invalid sexp "~S must be NIL or a positive integer" (first entry))))
    (unless (typep preserve-metadata-p 'boolean)
      (project-invalid sexp ":preserve-metadata-p must be a boolean"))
    (make-export-settings :jpeg-quality quality
                          :max-width max-width
                          :max-height max-height
                          :preserve-metadata-p preserve-metadata-p)))

(defun photo-job->sexp (photo)
  (list :input (namestring (photo-job-input-path photo))
        :output (when (photo-job-output-path photo)
                  (namestring (photo-job-output-path photo)))
        :overrides (copy-list (photo-job-overrides photo))))

(defun sexp->photo-job (sexp)
  (unless (and (listp sexp)
               (plist-known-keys-p sexp '(:input :output :overrides)))
    (project-invalid sexp "expected a photo property list"))
  (let ((input (getf sexp :input))
        (output (getf sexp :output))
        (overrides (getf sexp :overrides '())))
    (unless (stringp input)
      (project-invalid sexp ":input must be a pathname string"))
    (unless (or (null output) (stringp output))
      (project-invalid sexp ":output must be NIL or a pathname string"))
    (processing-plist-validate overrides)
    (make-photo-job :input-path (pathname input)
                    :output-path (when output (pathname output))
                    :overrides overrides)))

(defun processing-settings-with-overrides (settings overrides)
  "Return a copy of SETTINGS with validated OVERRIDES applied."
  (processing-plist-validate overrides)
  (let ((result (copy-processing-settings settings)))
    (loop for (key value) on overrides by #'cddr
          do (ecase key
               (:exposure
                (setf (processing-settings-exposure result) value))
               (:white-balance-temperature
                (setf (processing-settings-white-balance-temperature result)
                      value))
               (:white-balance-tint
                (setf (processing-settings-white-balance-tint result) value))
               (:noise-reduction
                (setf (processing-settings-noise-reduction result) value))
               (:lens-correction-p
                (setf (processing-settings-lens-correction-p result) value))
               (:lens-correction-strength
                (setf (processing-settings-lens-correction-strength result)
                      value))
               (:chromatic-aberration-correction-p
                (setf (processing-settings-chromatic-aberration-correction-p
                       result)
                      value))
               (:lut-path
                (setf (processing-settings-lut-path result) value))
               (:lut-strength
                (setf (processing-settings-lut-strength result) value))
               (:grain-amount
                (setf (processing-settings-grain-amount result) value))
               (:grain-size
                (setf (processing-settings-grain-size result) value))))
    result))

(defun project->sexp (project)
  "Convert PROJECT to its portable, versioned S-expression representation."
  (list :orfeus-project 1
        :output-directory (namestring (project-output-directory project))
        :defaults (processing-settings->sexp (project-defaults project))
         :export-settings (export-settings->sexp (project-export-settings project))
         :photos (mapcar #'photo-job->sexp (project-photos project))))

(defun sexp->project (sexp)
  "Validate and convert a project S-expression into a PROJECT."
  (unless (and (listp sexp)
               (eq (first sexp) :orfeus-project)
               (eql (second sexp) 1)
               (plist-known-keys-p (cddr sexp)
                                   '(:output-directory :defaults :export-settings :photos)))
    (project-invalid sexp "expected (:ORFEUS-PROJECT 1 ...)"))
  (let ((output-directory (getf (cddr sexp) :output-directory))
        (defaults (getf (cddr sexp) :defaults))
        (export-settings (getf (cddr sexp) :export-settings))
        (photos (getf (cddr sexp) :photos)))
    (unless (stringp output-directory)
      (project-invalid sexp ":output-directory must be a pathname string"))
    (unless (listp photos)
      (project-invalid sexp ":photos must be a list"))
    (make-project :output-directory (pathname output-directory)
                  :defaults (sexp->processing-settings defaults)
                  :export-settings (sexp->export-settings export-settings)
                  :photos (mapcar #'sexp->photo-job photos))))

(defun project-resolve-pathname (pathname base-directory)
  (if (uiop:absolute-pathname-p pathname)
      pathname
      (merge-pathnames pathname base-directory)))

(defun project-resolve-lut-path (settings base-directory)
  (let ((lut-path (processing-settings-lut-path settings)))
    (when lut-path
      (setf (processing-settings-lut-path settings)
            (namestring
             (project-resolve-pathname (pathname lut-path) base-directory)))))
  settings)

(defun project-resolve-relative-paths (project base-directory)
  (setf (project-output-directory project)
        (uiop:ensure-directory-pathname
         (project-resolve-pathname (project-output-directory project)
                                   base-directory)))
  (project-resolve-lut-path (project-defaults project) base-directory)
  (dolist (photo (project-photos project))
    (setf (photo-job-input-path photo)
          (project-resolve-pathname (photo-job-input-path photo)
                                    base-directory))
    (let ((overrides (copy-list (photo-job-overrides photo))))
      (when (getf overrides :lut-path)
        (setf (getf overrides :lut-path)
              (namestring
               (project-resolve-pathname
                (pathname (getf overrides :lut-path)) base-directory))))
      (setf (photo-job-overrides photo) overrides)))
  project)

(defun project-write (project pathname)
  "Write PROJECT readably to PATHNAME, replacing an existing file."
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*print-pretty* t)
            (*print-readably* nil)
            (*print-escape* t))
        (write (project->sexp project) :stream stream)
        (terpri stream))))
  pathname)

(defun project-read (pathname)
  "Read and validate one project, resolving relative paths beside PATHNAME."
  (let* ((absolute (truename pathname))
         (base-directory (uiop:pathname-directory-pathname absolute)))
    (with-open-file (stream absolute :direction :input)
      (let ((*read-eval* nil))
        (project-resolve-relative-paths
         (sexp->project (read stream t nil nil))
         base-directory)))))
