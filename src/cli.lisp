(in-package #:orfeus)

(defun cli-print-help (stream)
  (format stream "Orfeus ~A~%~%Usage: orfeus [--help | --version]~%"
          (orfeus-version)))

(defun main ()
  "Run the Orfeus command-line frontend."
  (let ((arguments (uiop:command-line-arguments)))
    (cond
      ((or (null arguments)
           (member "--help" arguments :test #'string=))
       (cli-print-help *standard-output*)
       0)
      ((member "--version" arguments :test #'string=)
       (format t "~A~%" (orfeus-version))
       0)
      (t
       (format *error-output* "Unknown arguments: ~{~A~^ ~}~%" arguments)
       (cli-print-help *error-output*)
       2))))
