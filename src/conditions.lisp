(in-package #:orfeus)

(define-condition orfeus-error (error)
  ()
  (:documentation "Base condition for errors reported by Orfeus."))

(define-condition native-library-unavailable (orfeus-error)
  ((paths
    :initarg :paths
    :reader native-library-unavailable-paths)
   (cause
    :initarg :cause
    :reader native-library-unavailable-cause))
  (:report
   (lambda (condition stream)
     (format stream "Could not load the Orfeus native bridge from ~{~A~^, ~}: ~A"
             (native-library-unavailable-paths condition)
             (native-library-unavailable-cause condition))))
  (:documentation "Signalled when the Rust native bridge cannot be loaded."))

(define-condition invalid-project-data (orfeus-error)
  ((datum
    :initarg :datum
    :reader invalid-project-data-datum)
   (reason
    :initarg :reason
    :reader invalid-project-data-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid Orfeus project data ~S: ~A"
             (invalid-project-data-datum condition)
             (invalid-project-data-reason condition))))
  (:documentation "Signalled when project S-expression data fails validation."))

(define-condition dng-original-error (orfeus-error)
  ((pathname
    :initarg :pathname
    :reader dng-original-error-pathname)
   (output-pathname
    :initarg :output-pathname
    :initform nil
    :reader dng-original-error-output-pathname)
   (status
    :initarg :status
    :reader dng-original-error-status)
   (message
    :initarg :message
    :reader dng-original-error-message))
  (:report
   (lambda (condition stream)
     (format stream "Could not extract the original from ~A~@[ to ~A~] (~D): ~A"
             (dng-original-error-pathname condition)
             (dng-original-error-output-pathname condition)
             (dng-original-error-status condition)
             (dng-original-error-message condition))))
  (:documentation "Signalled when DNG original metadata or extraction fails."))

(define-condition output-file-exists (orfeus-error)
  ((pathname
    :initarg :pathname
    :reader output-file-exists-pathname))
  (:report
   (lambda (condition stream)
     (format stream "Output file already exists: ~A"
             (output-file-exists-pathname condition))))
  (:documentation "Signalled before an existing output file would be replaced."))

(define-condition raw-render-error (orfeus-error)
  ((input-pathname
    :initarg :input-pathname
    :reader raw-render-error-input-pathname)
   (output-pathname
    :initarg :output-pathname
    :reader raw-render-error-output-pathname)
   (status
    :initarg :status
    :reader raw-render-error-status)
   (message
    :initarg :message
    :reader raw-render-error-message))
  (:report
   (lambda (condition stream)
     (format stream "Could not render ~A to ~A (~D): ~A"
             (raw-render-error-input-pathname condition)
             (raw-render-error-output-pathname condition)
             (raw-render-error-status condition)
             (raw-render-error-message condition))))
  (:documentation "Signalled when native RAW rendering fails."))

(define-condition lens-profile-unavailable (warning)
  ((input-pathname
    :initarg :input-pathname
    :reader lens-profile-unavailable-input-pathname)
   (message
    :initarg :message
    :reader lens-profile-unavailable-message))
  (:report
   (lambda (condition stream)
     (format stream "Lens correction skipped for ~A: ~A"
             (lens-profile-unavailable-input-pathname condition)
             (lens-profile-unavailable-message condition))))
  (:documentation "Warns that requested automatic lens correction is unavailable."))

(define-condition native-library-incompatible (orfeus-error)
  ((message
    :initarg :message
    :reader native-library-incompatible-message))
  (:report
   (lambda (condition stream)
     (format stream "The loaded Orfeus native bridge is incompatible: ~A"
             (native-library-incompatible-message condition))))
  (:documentation "Signalled when the native bridge lacks required render ABI features."))
