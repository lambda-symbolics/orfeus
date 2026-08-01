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
