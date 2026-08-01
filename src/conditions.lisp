(in-package #:orfeus)

(define-condition orfeus-error (error)
  ()
  (:documentation "Base condition for errors reported by Orfeus."))
