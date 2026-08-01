(in-package #:orfeus)

(defun cli-print-help (stream)
  (format stream
          "Orfeus ~A~%~%Usage:~%  orfeus info DNG~%  orfeus extract DNG [OUTPUT.ORF]~%  orfeus --version~%"
          (orfeus-version)))

(defun cli-extract (arguments output-stream)
  (unless (member (length arguments) '(1 2))
    (error "extract expects DNG and optional OUTPUT.ORF"))
  (let* ((dng-pathname (pathname (first arguments)))
         (output-pathname
           (if (second arguments)
               (pathname (second arguments))
               (merge-pathnames (dng-original-filename dng-pathname)
                                (uiop:getcwd)))))
    (dng-extract-original dng-pathname output-pathname)
    (format output-stream "~A~%" (namestring output-pathname))
    0))

(defun cli-info (arguments output-stream)
  (unless (= 1 (length arguments))
    (error "info expects one DNG"))
  (let ((pathname (pathname (first arguments))))
    (format output-stream "DNG: ~A~%Embedded original: ~A~%"
            (namestring pathname)
            (dng-original-filename pathname)))
  0)

(defun cli-run (arguments &key
                            (output-stream *standard-output*)
                            (error-stream *error-output*))
  "Run a CLI request from ARGUMENTS and return its process status."
  (handler-case
      (cond
        ((or (null arguments)
             (member "--help" arguments :test #'string=))
         (cli-print-help output-stream)
         0)
        ((equal arguments '("--version"))
         (format output-stream "~A~%" (orfeus-version))
         0)
        ((string= (first arguments) "extract")
         (cli-extract (rest arguments) output-stream))
        ((string= (first arguments) "info")
         (cli-info (rest arguments) output-stream))
        (t
         (format error-stream "Unknown command: ~{~A~^ ~}~%" arguments)
         (cli-print-help error-stream)
         2))
    (orfeus-error (condition)
      (format error-stream "orfeus: ~A~%" condition)
      1)
    (error (condition)
      (format error-stream "orfeus: ~A~%" condition)
      2)))

(defun main ()
  "Run the Orfeus command-line frontend."
  (uiop:quit (cli-run (uiop:command-line-arguments))))
