(in-package #:orfeus)

(defparameter *processing-setting-keys*
  '(:exposure :white-balance-temperature :white-balance-tint
    :noise-reduction :lens-correction-p
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
  (chromatic-aberration-correction-p t)
  (lut-path nil)
  (lut-strength 1.0)
  (grain-amount 0.0)
  (grain-size 1.0))

(defstruct photo-job
  "One input photograph and its optional per-photo setting overrides."
  input-path
  output-path
  (overrides '()))

(defstruct project
  "A batch of photographs sharing processing defaults and an output directory."
  output-directory
  (defaults (make-processing-settings))
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
        :chromatic-aberration-correction-p
        (processing-settings-chromatic-aberration-correction-p settings)
        :lut-path (processing-settings-lut-path settings)
        :lut-strength (processing-settings-lut-strength settings)
        :grain-amount (processing-settings-grain-amount settings)
        :grain-size (processing-settings-grain-size settings)))

(defun sexp->processing-settings (sexp)
  (processing-plist-validate sexp)
  (apply #'make-processing-settings sexp))

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

(defun project->sexp (project)
  "Convert PROJECT to its portable, versioned S-expression representation."
  (list :orfeus-project 1
        :output-directory (namestring (project-output-directory project))
        :defaults (processing-settings->sexp (project-defaults project))
        :photos (mapcar #'photo-job->sexp (project-photos project))))

(defun sexp->project (sexp)
  "Validate and convert a project S-expression into a PROJECT."
  (unless (and (listp sexp)
               (eq (first sexp) :orfeus-project)
               (eql (second sexp) 1)
               (plist-known-keys-p (cddr sexp)
                                   '(:output-directory :defaults :photos)))
    (project-invalid sexp "expected (:ORFEUS-PROJECT 1 ...)"))
  (let ((output-directory (getf (cddr sexp) :output-directory))
        (defaults (getf (cddr sexp) :defaults))
        (photos (getf (cddr sexp) :photos)))
    (unless (stringp output-directory)
      (project-invalid sexp ":output-directory must be a pathname string"))
    (unless (listp photos)
      (project-invalid sexp ":photos must be a list"))
    (make-project :output-directory (pathname output-directory)
                  :defaults (sexp->processing-settings defaults)
                  :photos (mapcar #'sexp->photo-job photos))))

(defun project-write (project pathname)
  "Write PROJECT readably to PATHNAME, replacing an existing file."
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (with-standard-io-syntax
      (let ((*print-pretty* t))
        (write (project->sexp project) :stream stream)
        (terpri stream))))
  pathname)

(defun project-read (pathname)
  "Read and validate one project from PATHNAME with reader evaluation disabled."
  (with-open-file (stream pathname :direction :input)
    (let ((*read-eval* nil))
      (sexp->project (read stream t nil nil)))))
