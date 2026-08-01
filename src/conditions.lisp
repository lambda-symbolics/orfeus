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
