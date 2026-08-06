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
as in 20260802-183512-PB020123.jpg, keeping the original name intact. FORMAT
chooses the encoder for automatic output names; an explicit per-photo output
path still decides its own format from its extension."
  (format :jpeg :type (member :jpeg :tiff))
  (jpeg-quality 92 :type (integer 1 100))
  (max-width nil :type (or null (integer 1)))
  (max-height nil :type (or null (integer 1)))
  (preserve-metadata-p t :type boolean)
  (timestamp-filenames-p nil :type boolean))

(defun export-format-extension (format)
  "Return the filename extension FORMAT's encoder writes."
  (ecase format
    (:jpeg "jpg")
    (:tiff "tif")))

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

(declaim (ftype (function (t) t) graph->sexp graph->render-sexp sexp->graph))
(declaim (ftype (function (t) t) graph-copy))

(defun copy-photo-job-deep (job)
  "Return a PHOTO-JOB sharing no mutable structure with JOB."
  (make-photo-job
   :input-path (photo-job-input-path job)
   :output-path (photo-job-output-path job)
   :overrides (copy-list (photo-job-overrides job))
   :disabled-stages (copy-list (photo-job-disabled-stages job))
   :graph (let ((graph (photo-job-graph job)))
            (when graph (graph-copy graph)))))

(defun copy-processing-preset-deep (preset)
  "Return a PROCESSING-PRESET sharing no mutable structure with PRESET."
  (make-processing-preset
   :name (processing-preset-name preset)
   :settings (copy-processing-settings (processing-preset-settings preset))
   :source-photo (processing-preset-source-photo preset)
   :disabled-stages (copy-list (processing-preset-disabled-stages preset))
   :graph (let ((graph (processing-preset-graph preset)))
            (when graph (graph-copy graph)))))

(defun copy-project-deep (project)
  "Return a PROJECT sharing no mutable structure with PROJECT.

Every setting value is a number, boolean, string, or pathname, so copying the
structs and lists that hold them is enough to make the result independent.
Editing one project can never be seen through the other, which is what lets a
frontend keep undo snapshots."
  (make-project
   :output-directory (project-output-directory project)
   :defaults (copy-processing-settings (project-defaults project))
   :export-settings (copy-export-settings (project-export-settings project))
   :presets (mapcar #'copy-processing-preset-deep (project-presets project))
   :photos (mapcar #'copy-photo-job-deep (project-photos project))))

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
  (list :format (export-settings-format settings)
        :jpeg-quality (export-settings-jpeg-quality settings)
        :max-width (export-settings-max-width settings)
        :max-height (export-settings-max-height settings)
        :preserve-metadata-p (export-settings-preserve-metadata-p settings)
        :timestamp-filenames-p
        (export-settings-timestamp-filenames-p settings)))

(defun sexp->export-settings (sexp)
  (unless (and (listp sexp)
               (plist-known-keys-p
                sexp '(:format :jpeg-quality :max-width :max-height
                       :preserve-metadata-p :timestamp-filenames-p)))
    (project-invalid sexp "expected a property list of export settings"))
  (let ((format (getf sexp :format :jpeg))
        (quality (getf sexp :jpeg-quality 92))
        (max-width (getf sexp :max-width))
        (max-height (getf sexp :max-height))
        (timestamp-filenames-p (getf sexp :timestamp-filenames-p))
        (preserve-metadata-p (getf sexp :preserve-metadata-p t)))
    (unless (member format '(:jpeg :tiff))
      (project-invalid sexp ":format must be :JPEG or :TIFF"))
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
    (make-export-settings :format format
                          :jpeg-quality quality
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
  (list :orfeus-project 4
        :output-directory (namestring (project-output-directory project))
        :defaults (processing-settings->sexp (project-defaults project))
        :export-settings (export-settings->sexp (project-export-settings project))
        :presets (mapcar #'processing-preset->sexp (project-presets project))
        :photos (mapcar #'photo-job->sexp (project-photos project))))

(defun sexp->project (sexp)
  "Validate and convert a project S-expression into a PROJECT."
  (unless (and (listp sexp)
               (eq (first sexp) :orfeus-project)
               (member (second sexp) '(1 2 3 4))
               (plist-known-keys-p
                (cddr sexp)
                '(:output-directory :defaults :export-settings :presets :photos)))
    (project-invalid sexp "expected (:ORFEUS-PROJECT 1|2|3|4 ...)"))
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

(declaim (ftype (function (t t) t) project-resolve-graph-lut-paths)
                  (ftype (function (t) t) still-store-canonicalize-graph-lut-paths))

(defun project-resolve-relative-paths (project base-directory)
  (setf (project-output-directory project)
        (uiop:ensure-directory-pathname
         (project-resolve-pathname (project-output-directory project)
                                   base-directory)))
  (project-resolve-lut-path (project-defaults project) base-directory)
  (dolist (preset (project-presets project))
    (project-resolve-lut-path (processing-preset-settings preset) base-directory)
    (project-resolve-graph-lut-paths (processing-preset-graph preset)
                                     base-directory)
    (when (processing-preset-source-photo preset)
      (setf (processing-preset-source-photo preset)
            (project-resolve-pathname
             (processing-preset-source-photo preset) base-directory))))
  (dolist (photo (project-photos project))
    (project-resolve-graph-lut-paths (photo-job-graph photo) base-directory)
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

;;; The local still store.
;;;
;;; Grabbed stills are copied into a per-user gallery under the XDG data
;;; directory, so a grade survives removable source media and unsaved
;;; projects. Each still is one readable sidecar plus an optional thumbnail.

(defvar *still-store-lock*
  (sb-thread:make-mutex :name "Orfeus still store")
  "Process-local serialization around the store's interprocess lock.")

(defun still-store-protect (pathname mode)
  "Apply POSIX MODE to PATHNAME when the host supports it."
  (ignore-errors (sb-posix:chmod (namestring pathname) mode))
  pathname)

(defun still-store-directory ()
  "Return the private per-user still gallery directory, creating it when missing."
  (let ((directory (uiop:xdg-data-home "orfeus/stills/")))
    (ensure-directories-exist directory)
    (still-store-protect directory #o700)
    directory))

(defun call-with-still-store-lock (directory thunk)
  (ensure-directories-exist directory)
  (still-store-protect directory #o700)
  (sb-thread:with-mutex (*still-store-lock*)
    (let ((lock-path (merge-pathnames ".lock" directory)))
      (with-open-file (stream lock-path
                              :direction :io
                              :if-exists :overwrite
                              :if-does-not-exist :create)
        (still-store-protect lock-path #o600)
        (let ((descriptor (sb-sys:fd-stream-fd stream)))
          (unwind-protect
               (progn
                 (sb-posix:lockf descriptor sb-posix:f-lock 0)
                 (funcall thunk))
            (ignore-errors
              (sb-posix:lockf descriptor sb-posix:f-ulock 0))))))))

(defun still-store-identity (name)
  "Return the collision-resistant filesystem identity of exact still NAME."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256 (sb-ext:string-to-octets name :external-format :utf-8))))

(defun still-store-sexp-pathname (name &key (directory
                                             (still-store-directory)))
  (merge-pathnames (make-pathname :name (still-store-identity name) :type "sexp")
                   directory))

(defun still-store-thumbnail-pathname (name &key (directory
                                                  (still-store-directory)))
  (let ((pathname
          (merge-pathnames
           (make-pathname :name (still-store-identity name) :type "jpg")
           directory)))
    (when (probe-file pathname)
      (still-store-protect pathname #o600))
    pathname))

(defun still-store-canonical-pathname (path)
  (when path
    (or (ignore-errors (truename path))
        (uiop:ensure-absolute-pathname path (uiop:getcwd)))))

(defun still-store-canonical-preset (preset)
  "Copy PRESET and canonicalize every stored external path."
  (let ((copy (sexp->processing-preset (processing-preset->sexp preset))))
    (when (processing-preset-source-photo copy)
      (setf (processing-preset-source-photo copy)
            (still-store-canonical-pathname
             (processing-preset-source-photo copy))))
    (let ((lut-path (processing-settings-lut-path
                     (processing-preset-settings copy))))
      (when lut-path
        (setf (processing-settings-lut-path
               (processing-preset-settings copy))
              (namestring (still-store-canonical-pathname lut-path)))))
    (still-store-canonicalize-graph-lut-paths
     (processing-preset-graph copy))
    copy))

(defun still-store-write-unlocked (preset directory if-exists)
  (ensure-directories-exist directory)
  (still-store-protect directory #o700)
  (let* ((pathname (still-store-sexp-pathname
                    (processing-preset-name preset) :directory directory))
         (temporary (merge-pathnames
                     (make-pathname
                      :name (format nil ".~A-~36R-~36R"
                                    (pathname-name pathname)
                                    (get-internal-real-time)
                                    (random most-positive-fixnum))
                      :type "tmp")
                     directory))
         (stored (still-store-canonical-preset preset)))
    (unless (member if-exists '(:error :supersede))
      (error "Unknown still-store :if-exists policy ~S" if-exists))
    (when (and (eq if-exists :error) (probe-file pathname))
      (error "A local still named ~A already exists"
             (processing-preset-name preset)))
    (unwind-protect
         (progn
           (with-open-file (stream temporary
                                   :direction :output
                                   :if-exists nil
                                   :if-does-not-exist :create)
             (with-standard-io-syntax
               (let ((*print-pretty* t)
                     (*print-readably* nil)
                     (*print-escape* t))
                 (write (list :orfeus-still 1
                              (processing-preset->sexp stored))
                        :stream stream)
                 (terpri stream)
                 (finish-output stream))))
           (still-store-protect temporary #o600)
           (sb-posix:rename (namestring temporary) (namestring pathname))
           (still-store-protect pathname #o600)
           pathname)
      (when (probe-file temporary)
        (delete-file temporary)))))

(defun still-store-write (preset &key (directory (still-store-directory))
                                       (if-exists :supersede))
  "Atomically persist PRESET in the local still gallery; return its sidecar.
IF-EXISTS is either :SUPERSEDE or :ERROR."
  (call-with-still-store-lock
   directory
   (lambda () (still-store-write-unlocked preset directory if-exists))))

(defun still-store-read (pathname)
  "Read one still sidecar, validating like project data."
  (with-open-file (stream pathname :direction :input)
    (let ((*read-eval* nil))
      (let ((sexp (read stream t nil nil)))
        (unless (and (listp sexp)
                     (eq (first sexp) :orfeus-still)
                     (eql (second sexp) 1))
          (project-invalid sexp "expected an (:orfeus-still 1 ...) file"))
        (sexp->processing-preset (third sexp))))))

(defun still-store-list (&key (directory (still-store-directory)))
  "Return every readable still in the local gallery, oldest first."
  (call-with-still-store-lock
   directory
   (lambda ()
     (loop for file in (sort (uiop:directory-files directory "*.sexp")
                             #'< :key #'file-write-date)
           for preset = (handler-case (still-store-read file)
                          (error () nil))
           when preset collect preset))))

(defun still-store-delete (name &key (directory (still-store-directory)))
  "Remove NAME's sidecar and thumbnail from the local gallery."
  (call-with-still-store-lock
   directory
   (lambda ()
     (let ((sexp (still-store-sexp-pathname name :directory directory))
           (thumbnail (still-store-thumbnail-pathname name :directory directory)))
       (when (probe-file sexp) (delete-file sexp))
       (when (probe-file thumbnail) (delete-file thumbnail))))))

(defun still-store-write-thumbnail (name source
                                    &key (directory (still-store-directory)))
  "Atomically copy SOURCE into NAME's private local gallery thumbnail."
  (call-with-still-store-lock
   directory
   (lambda ()
     (let* ((destination (still-store-thumbnail-pathname
                          name :directory directory))
            (temporary (merge-pathnames
                        (make-pathname
                         :name (format nil ".~A-~36R-~36R"
                                       (pathname-name destination)
                                       (get-internal-real-time)
                                       (random most-positive-fixnum))
                         :type "tmp")
                        directory)))
       (unwind-protect
            (progn
              (uiop:copy-file source temporary)
              (still-store-protect temporary #o600)
              (sb-posix:rename (namestring temporary)
                               (namestring destination))
              (still-store-protect destination #o600)
              destination)
         (when (probe-file temporary)
           (delete-file temporary)))))))

(defun still-store-rename (preset old-name &key (directory
                                                 (still-store-directory)))
  "Rename exactly OLD-NAME's local files and atomically rewrite its sidecar."
  (call-with-still-store-lock
   directory
   (lambda ()
     (let ((new-name (processing-preset-name preset)))
       (if (string= old-name new-name)
           (still-store-write-unlocked preset directory :supersede)
           (let ((old-thumbnail (still-store-thumbnail-pathname
                                 old-name :directory directory))
                 (new-thumbnail (still-store-thumbnail-pathname
                                 new-name :directory directory))
                 (old-sexp (still-store-sexp-pathname old-name
                                                      :directory directory))
                 (new-sexp (still-store-sexp-pathname new-name
                                                      :directory directory)))
             (when (probe-file new-sexp)
               (error "A local still named ~A already exists" new-name))
             ;; Publish the new sidecar first. If the process stops mid-rename,
             ;; at worst both readable names remain; no sidecar is paired with
             ;; another still's thumbnail.
             (still-store-write-unlocked preset directory :supersede)
             (when (probe-file old-thumbnail)
               (sb-posix:rename (namestring old-thumbnail)
                                (namestring new-thumbnail))
               (still-store-protect new-thumbnail #o600))
             (when (probe-file old-sexp)
               (delete-file old-sexp))))))))

;;; Interned RAW files.
;;;
;;; A project normally points straight at the card the photographs were shot on,
;;; which stops working the moment the card comes out. Interning copies a RAW
;;; into a per-user store and repoints the project at the copy, so the card is
;;; only needed once. The store sits beside the still gallery for the same
;;; reason: it must exist whether or not the project has ever been saved.

(defparameter *interned-raw-directory-name* "orfeus-raw/"
  "Leaf directory holding interned RAW files inside the per-user data store.")

(defun interned-raw-directory ()
  "Return the per-user interned RAW directory, creating it when missing."
  (let ((directory (uiop:xdg-data-home
                    (concatenate 'string "orfeus/"
                                 *interned-raw-directory-name*))))
    (ensure-directories-exist directory)
    (still-store-protect directory #o700)
    directory))

(defun file-content-identity (pathname)
  "Return a collision-resistant digest of PATHNAME's contents."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-file :sha256 pathname)))

(defun interned-raw-pathname (source &key (directory (interned-raw-directory)))
  "Return where SOURCE would live once interned.

The original filename is kept so the store stays readable, with a short content
digest ahead of it: two cards both hold an _6040106.ORF, and they are not the
same photograph."
  (let ((name (or (pathname-name source) "photograph"))
        (digest (subseq (file-content-identity source) 0 12)))
    (merge-pathnames (make-pathname :name (format nil "~A-~A" digest name)
                                    :type (pathname-type source))
                     directory)))

(defun photo-interned-p (pathname &key (directory (interned-raw-directory)))
  "Return true when PATHNAME already lives in the interned RAW store.

Compared by directory rather than by a recorded flag, so the answer stays right
across saves, reloads, and files removed from the store behind our back."
  (and pathname
       (equal (uiop:pathname-directory-pathname (truename* pathname))
              (uiop:pathname-directory-pathname (truename* directory)))))

(defun truename* (pathname)
  "Resolve PATHNAME when it exists, else return it unchanged."
  (or (ignore-errors (truename pathname)) pathname))

(defun intern-raw-file (source &key (directory (interned-raw-directory)))
  "Copy SOURCE into the interned RAW store and return the interned pathname.

Idempotent: a photograph already in the store, or already copied by an earlier
intern, is returned without being copied again. The copy lands through a
temporary file in the same directory and an atomic rename, so an interrupted
intern never leaves a half-written RAW that looks complete."
  (let ((source (pathname source)))
    (unless (probe-file source)
      (error 'invalid-project-data
             :datum source
             :reason "the photograph to intern does not exist"))
    (if (photo-interned-p source :directory directory)
        source
        (let* ((target (interned-raw-pathname source :directory directory))
               (temporary (merge-pathnames
                           (make-pathname
                            :name (format nil ".~A-~36R"
                                          (pathname-name target)
                                          (random most-positive-fixnum))
                            :type "tmp")
                           directory)))
          (when (probe-file target)
            (return-from intern-raw-file target))
          (unwind-protect
               (progn
                 (uiop:copy-file source temporary)
                 (still-store-protect temporary #o600)
                 (rename-file temporary target)
                 target)
            (when (probe-file temporary)
              (ignore-errors (delete-file temporary))))))))

(defun intern-photo-job (job &key (directory (interned-raw-directory)))
  "Intern JOB's input and repoint it at the copy. Returns the new pathname.

Repointing is the point: leaving the job on the card would mean the intern
bought nothing."
  (let ((interned (intern-raw-file (photo-job-input-path job)
                                   :directory directory)))
    (setf (photo-job-input-path job) interned)
    interned))
