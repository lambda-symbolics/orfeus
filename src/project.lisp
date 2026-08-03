(in-package #:orfeus)

(defparameter *processing-setting-keys*
  '(:exposure :white-balance-temperature :white-balance-tint
    :noise-reduction :neural-noise-reduction
    :tone-blacks :tone-shadows :tone-dark-mids
    :tone-midtones :tone-light-mids :tone-highlights :tone-whites
    :lens-correction-p :lens-correction-strength
    :chromatic-aberration-correction-p :lut-path :lut-strength
    :grain-amount :grain-size)
  "Keys accepted in processing setting S-expressions.")

(defstruct processing-settings
  "Frontend-independent controls for one rendering operation."
  (exposure 0.0)
  (white-balance-temperature nil)
  (white-balance-tint 0.0)
  (noise-reduction 0.35)
  (neural-noise-reduction 0.0)
  (tone-blacks 0.0)
  (tone-shadows 0.0)
  (tone-dark-mids 0.0)
  (tone-midtones 0.0)
  (tone-light-mids 0.0)
  (tone-highlights 0.0)
  (tone-whites 0.0)
  (lens-correction-p t)
  (lens-correction-strength 1.0)
  (chromatic-aberration-correction-p t)
  (lut-path nil)
  (lut-strength 1.0)
  (grain-amount 0.0)
  (grain-size 1.0))

(defparameter *grade-stages*
  '((:white-balance (:white-balance-temperature :white-balance-tint))
    (:exposure (:exposure))
    (:noise-reduction (:noise-reduction :neural-noise-reduction))
    (:tone (:tone-blacks :tone-shadows :tone-dark-mids :tone-midtones
            :tone-light-mids :tone-highlights :tone-whites))
    (:optics (:lens-correction-p :lens-correction-strength
              :chromatic-aberration-correction-p))
    (:film (:lut-path :lut-strength :grain-amount :grain-size)))
  "The fixed processing pipeline as named stages over setting keys.
Together the stages partition *PROCESSING-SETTING-KEYS*; frontends present
them as a copyable node chain.")

(defparameter *stage-identity-plist*
  '(:white-balance-temperature nil :white-balance-tint 0.0
    :exposure 0.0
    :noise-reduction 0.0 :neural-noise-reduction 0.0
    :tone-blacks 0.0 :tone-shadows 0.0 :tone-dark-mids 0.0 :tone-midtones 0.0
    :tone-light-mids 0.0 :tone-highlights 0.0 :tone-whites 0.0
    :lens-correction-p nil :lens-correction-strength 1.0
    :chromatic-aberration-correction-p nil
    :lut-path nil :lut-strength 0.0 :grain-amount 0.0 :grain-size 1.0)
  "Setting values under which every stage passes pixels through unchanged.")

(defun grade-stage-keys (stage)
  "Return the setting keys belonging to pipeline STAGE."
  (or (second (assoc stage *grade-stages*))
      (error "Unknown grade stage ~S." stage)))

(defun grade-stages ()
  "Return the pipeline stage names in processing order."
  (mapcar #'first *grade-stages*))

(defstruct processing-preset
  "A named, portable snapshot of processing settings.

A preset grabbed from a specific photograph remembers that photograph in
SOURCE-PHOTO so galleries can render a representative still thumbnail.
DISABLED-STAGES preserves pipeline bypass state without discarding its settings."
  (name "" :type string)
  (settings (make-processing-settings) :type processing-settings)
  (source-photo nil)
  (disabled-stages '())
  (graph nil))

(defstruct export-settings
  "Frontend-independent options for encoded photo exports.

TIMESTAMP-FILENAMES-P prefixes automatic output names with the capture time,
as in 20260802-183512-PB020123.jpg, keeping the original name intact."
  (jpeg-quality 92 :type (integer 1 100))
  (max-width nil :type (or null (integer 1)))
  (max-height nil :type (or null (integer 1)))
  (preserve-metadata-p t :type boolean)
  (timestamp-filenames-p nil :type boolean))

(defstruct photo-job
  "One input photograph and its optional per-photo setting overrides.

DISABLED-STAGES lists pipeline stages bypassed for this photograph; their
settings are remembered but render as identity, like a disabled node. GRAPH,
when present, replaces the flat pipeline with an explicit node graph."
  input-path
  output-path
  (overrides '())
  (disabled-stages '())
  (graph nil))

(defstruct project
  "A batch of photographs sharing processing defaults and an output directory."
  output-directory
  (defaults (make-processing-settings))
  (export-settings (make-export-settings))
  (presets '())
  (photos '()))

(declaim (ftype (function (t) t) graph->sexp sexp->graph))

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
    (:neural-noise-reduction
     (and (realp value) (<= 0 value 1)))
    ((:tone-blacks :tone-shadows :tone-dark-mids :tone-midtones
      :tone-light-mids :tone-highlights :tone-whites)
     (and (realp value)
          (ignore-errors (<= -2 value 2))))
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
        :neural-noise-reduction
        (processing-settings-neural-noise-reduction settings)
        :tone-blacks (processing-settings-tone-blacks settings)
        :tone-shadows (processing-settings-tone-shadows settings)
        :tone-dark-mids (processing-settings-tone-dark-mids settings)
        :tone-midtones (processing-settings-tone-midtones settings)
        :tone-light-mids (processing-settings-tone-light-mids settings)
        :tone-highlights (processing-settings-tone-highlights settings)
        :tone-whites (processing-settings-tone-whites settings)
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

(defun processing-preset->sexp (preset)
  (append
   (list :name (processing-preset-name preset)
         :settings (processing-settings->sexp
                    (processing-preset-settings preset)))
   (let ((source (processing-preset-source-photo preset)))
     (when source
       (list :source-photo (namestring source))))
   (when (processing-preset-disabled-stages preset)
     (list :disabled-stages
           (copy-list (processing-preset-disabled-stages preset))))
   (when (processing-preset-graph preset)
     (list :graph (graph->sexp (processing-preset-graph preset))))))

(defun sexp->processing-preset (sexp)
  (unless (and (listp sexp)
               (plist-known-keys-p sexp
                                   '(:name :settings :source-photo
                                     :disabled-stages :graph)))
    (project-invalid sexp "expected a preset property list"))
  (let ((name (getf sexp :name))
        (settings (getf sexp :settings))
        (source-photo (getf sexp :source-photo))
        (disabled-stages (getf sexp :disabled-stages '()))
        (graph (getf sexp :graph)))
    (unless (and (stringp name)
                 (plusp (length (string-trim '(#\Space #\Tab) name))))
      (project-invalid sexp "preset :name must be a nonempty string"))
    (unless (or (null source-photo) (stringp source-photo))
      (project-invalid sexp "preset :source-photo must be NIL or a pathname string"))
    (disabled-stages-validate disabled-stages)
    (make-processing-preset :name name
                            :settings (sexp->processing-settings settings)
                            :source-photo (when source-photo
                                            (pathname source-photo))
                            :disabled-stages disabled-stages
                            :graph (when graph (sexp->graph graph)))))

(defun sexp->processing-presets (sexp)
  (unless (listp sexp)
    (project-invalid sexp ":presets must be a list"))
  (let ((presets (mapcar #'sexp->processing-preset sexp)))
    (unless (= (length presets)
               (length (remove-duplicates presets :test #'string-equal
                                                  :key #'processing-preset-name)))
      (project-invalid sexp "preset names must be unique"))
    presets))

(defun export-settings->sexp (settings)
  (list :jpeg-quality (export-settings-jpeg-quality settings)
        :max-width (export-settings-max-width settings)
        :max-height (export-settings-max-height settings)
        :preserve-metadata-p (export-settings-preserve-metadata-p settings)
        :timestamp-filenames-p
        (export-settings-timestamp-filenames-p settings)))

(defun sexp->export-settings (sexp)
  (unless (and (listp sexp)
               (plist-known-keys-p
                sexp '(:jpeg-quality :max-width :max-height :preserve-metadata-p
                       :timestamp-filenames-p)))
    (project-invalid sexp "expected a property list of export settings"))
  (let ((quality (getf sexp :jpeg-quality 92))
        (max-width (getf sexp :max-width))
        (max-height (getf sexp :max-height))
        (timestamp-filenames-p (getf sexp :timestamp-filenames-p))
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
    (unless (typep timestamp-filenames-p 'boolean)
      (project-invalid sexp ":timestamp-filenames-p must be a boolean"))
    (make-export-settings :jpeg-quality quality
                          :max-width max-width
                          :max-height max-height
                          :preserve-metadata-p preserve-metadata-p
                          :timestamp-filenames-p timestamp-filenames-p)))

(defun disabled-stages-validate (stages)
  (unless (and (listp stages)
               (every (lambda (stage) (assoc stage *grade-stages*)) stages)
               (= (length stages)
                  (length (remove-duplicates stages))))
    (project-invalid stages
                     "disabled stages must be unique pipeline stage names"))
  stages)

(defun photo-job->sexp (photo)
  (append
   (list :input (namestring (photo-job-input-path photo))
         :output (when (photo-job-output-path photo)
                   (namestring (photo-job-output-path photo)))
         :overrides (copy-list (photo-job-overrides photo)))
   (when (photo-job-disabled-stages photo)
     (list :disabled-stages
           (copy-list (photo-job-disabled-stages photo))))
   (when (photo-job-graph photo)
     (list :graph (graph->sexp (photo-job-graph photo))))))

(defun sexp->photo-job (sexp)
  (unless (and (listp sexp)
               (plist-known-keys-p
                sexp '(:input :output :overrides :disabled-stages :graph)))
    (project-invalid sexp "expected a photo property list"))
  (let ((input (getf sexp :input))
        (output (getf sexp :output))
        (overrides (getf sexp :overrides '()))
        (disabled-stages (getf sexp :disabled-stages '()))
        (graph (getf sexp :graph)))
    (unless (stringp input)
      (project-invalid sexp ":input must be a pathname string"))
    (unless (or (null output) (stringp output))
      (project-invalid sexp ":output must be NIL or a pathname string"))
    (processing-plist-validate overrides)
    (disabled-stages-validate disabled-stages)
    (make-photo-job :input-path (pathname input)
                    :output-path (when output (pathname output))
                    :overrides overrides
                    :disabled-stages disabled-stages
                    :graph (when graph (sexp->graph graph)))))

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
               (:neural-noise-reduction
                (setf (processing-settings-neural-noise-reduction result) value))
               (:tone-blacks
                (setf (processing-settings-tone-blacks result) value))
               (:tone-shadows
                (setf (processing-settings-tone-shadows result) value))
               (:tone-dark-mids
                (setf (processing-settings-tone-dark-mids result) value))
               (:tone-midtones
                (setf (processing-settings-tone-midtones result) value))
               (:tone-light-mids
                (setf (processing-settings-tone-light-mids result) value))
               (:tone-highlights
                (setf (processing-settings-tone-highlights result) value))
               (:tone-whites
                (setf (processing-settings-tone-whites result) value))
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

(defun settings-apply-stage-bypass (settings disabled-stages)
  "Return a copy of SETTINGS whose DISABLED-STAGES render as identity."
  (disabled-stages-validate disabled-stages)
  (let ((identity-overrides
          (loop for stage in disabled-stages
                append (loop for key in (grade-stage-keys stage)
                             collect key
                             collect (getf *stage-identity-plist* key)))))
    (if identity-overrides
        (processing-settings-with-overrides settings identity-overrides)
        (copy-processing-settings settings))))

(defun photo-render-settings (project photo)
  "Return PHOTO's settings as rendered: overrides applied, bypasses honored."
  (settings-apply-stage-bypass
   (processing-settings-with-overrides (project-defaults project)
                                       (photo-job-overrides photo))
   (photo-job-disabled-stages photo)))

(defun grade-key-inert-p (key plist)
  "True when KEY cannot affect rendering given the other values in PLIST."
  (case key
    (:lut-strength (null (getf plist :lut-path)))
    (:grain-size (let ((amount (getf plist :grain-amount)))
                   (or (null amount) (not (plusp amount)))))
    (:lens-correction-strength (not (getf plist :lens-correction-p)))
    (t nil)))

(defun settings-grade-plist (settings &optional (stages (grade-stages)))
  "Return the grade of SETTINGS as a plist restricted to STAGES."
  (let ((complete (processing-settings->sexp settings)))
    (loop for stage in stages
          append (loop for key in (grade-stage-keys stage)
                       collect key
                       collect (getf complete key)))))

(defun plist-merge (base additions)
  "Return BASE with every key of ADDITIONS added or replaced."
  (let ((merged (copy-list base)))
    (loop for (key value) on additions by #'cddr
          do (if (member key merged)
                 (setf (getf merged key) value)
                 (setf merged (list* key value merged))))
    merged))

(defun photo-job-apply-grade (photo grade &optional (disabled-stages nil
                                                     disabled-supplied-p))
  "Merge the validated GRADE plist into PHOTO's overrides.

When DISABLED-STAGES is supplied it replaces the photograph's bypass list,
so pasting a grade also carries which nodes were switched off."
  (processing-plist-validate grade)
  (setf (photo-job-overrides photo)
        (plist-merge (photo-job-overrides photo) grade))
  (when disabled-supplied-p
    (setf (photo-job-disabled-stages photo)
          (copy-list (disabled-stages-validate disabled-stages))))
  photo)

(defun next-still-preset-name (project photo)
  "Return an unused gallery name like \"Still 003 (photo)\" for PROJECT."
  (let ((taken (mapcar #'processing-preset-name (project-presets project)))
        (source (pathname-name (photo-job-input-path photo))))
    (loop for index from 1
          for candidate = (format nil "Still ~3,'0D (~A)" index source)
          unless (member candidate taken :test #'string-equal)
            return candidate)))

(defun project->sexp (project)
  "Convert PROJECT to its portable, versioned S-expression representation."
  (list :orfeus-project 3
        :output-directory (namestring (project-output-directory project))
        :defaults (processing-settings->sexp (project-defaults project))
        :export-settings (export-settings->sexp (project-export-settings project))
        :presets (mapcar #'processing-preset->sexp (project-presets project))
        :photos (mapcar #'photo-job->sexp (project-photos project))))

(defun sexp->project (sexp)
  "Validate and convert a project S-expression into a PROJECT."
  (unless (and (listp sexp)
               (eq (first sexp) :orfeus-project)
               (member (second sexp) '(1 2 3))
               (plist-known-keys-p
                (cddr sexp)
                '(:output-directory :defaults :export-settings :presets :photos)))
    (project-invalid sexp "expected (:ORFEUS-PROJECT 1|2|3 ...)"))
  (let ((output-directory (getf (cddr sexp) :output-directory))
        (defaults (getf (cddr sexp) :defaults))
        (export-settings (getf (cddr sexp) :export-settings))
        (presets (getf (cddr sexp) :presets '()))
        (photos (getf (cddr sexp) :photos)))
    (unless (stringp output-directory)
      (project-invalid sexp ":output-directory must be a pathname string"))
    (unless (listp photos)
      (project-invalid sexp ":photos must be a list"))
    (make-project :output-directory (pathname output-directory)
                  :defaults (sexp->processing-settings defaults)
                  :export-settings (sexp->export-settings export-settings)
                  :presets (sexp->processing-presets presets)
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
  (dolist (preset (project-presets project))
    (project-resolve-lut-path (processing-preset-settings preset) base-directory)
    (when (processing-preset-source-photo preset)
      (setf (processing-preset-source-photo preset)
            (project-resolve-pathname
             (processing-preset-source-photo preset) base-directory))))
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
