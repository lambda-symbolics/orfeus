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
