(in-package #:orfeus)

(defun cli-print-help (stream)
  (format stream
          "Orfeus ~A~%~%Usage:~%  orfeus info DNG~%  orfeus extract DNG [OUTPUT.ORF]~%  orfeus preview INPUT.ORF OUTPUT.JPG [PROJECT.sexp]~%  orfeus render INPUT.ORF OUTPUT.(JPG|TIFF) [PROJECT.sexp]~%  orfeus batch PROJECT.sexp~%  orfeus init PROJECT.sexp OUTPUT-DIR INPUT...~%  orfeus --version~%"
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

(defun cli-render-settings (project-argument)
  (if project-argument
      (project-defaults (project-read (pathname project-argument)))
      (make-processing-settings)))

(defun cli-render (arguments output-stream preview-p)
  (unless (member (length arguments) '(2 3))
    (error "~A expects INPUT, OUTPUT, and optional PROJECT.sexp"
           (if preview-p "preview" "render")))
  (let ((input (pathname (first arguments)))
        (output (pathname (second arguments)))
        (settings (cli-render-settings (third arguments))))
    (if preview-p
        (render-preview input output settings)
        (render-photo input output settings))
    (format output-stream "~A~%" (namestring output))
    0))

(defun cli-batch-progress (stream)
  (lambda (index total photo output)
    (format stream "[~D/~D] ~A -> ~A~%"
            index total
            (namestring (photo-job-input-path photo))
            (namestring output))))

(defun cli-batch (arguments output-stream error-stream)
  (unless (= 1 (length arguments))
    (error "batch expects PROJECT.sexp"))
  (multiple-value-bind (completed failures)
      (project-render (project-read (pathname (first arguments)))
                      :on-error :continue
                      :progress-callback (cli-batch-progress output-stream))
    (dolist (failure failures)
      (format error-stream "Failed ~A: ~A~%"
              (namestring (photo-job-input-path (first failure)))
              (rest failure)))
    (format output-stream "Rendered ~D file~:P; ~D failure~:P.~%"
            (length completed) (length failures))
    (if failures 1 0)))

(defun cli-init-project (arguments output-stream)
  (unless (>= (length arguments) 3)
    (error "init expects PROJECT.sexp, OUTPUT-DIR, and at least one INPUT"))
  (destructuring-bind (project-path output-directory &rest inputs) arguments
    (let ((project
            (make-project
             :output-directory (uiop:ensure-directory-pathname output-directory)
             :photos (mapcar (lambda (input)
                               (make-photo-job :input-path (pathname input)))
                             inputs))))
      (project-write project (pathname project-path))
      (format output-stream "~A~%" project-path)))
  0)

(defun cli-run (arguments &key
                            (output-stream *standard-output*)
                            (error-stream *error-output*))
  "Run a CLI request from ARGUMENTS and return its process status."
  (unwind-protect
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
             ((string= (first arguments) "preview")
              (cli-render (rest arguments) output-stream t))
             ((string= (first arguments) "render")
              (cli-render (rest arguments) output-stream nil))
             ((string= (first arguments) "batch")
              (cli-batch (rest arguments) output-stream error-stream))
             ((string= (first arguments) "init")
              (cli-init-project (rest arguments) output-stream))
             (t
              (format error-stream "Unknown command: ~{~A~^ ~}~%" arguments)
              (cli-print-help error-stream)
              2))
         (orfeus-error (condition)
           (format error-stream "orfeus: ~A~%" condition)
           1)
         (error (condition)
           (format error-stream "orfeus: ~A~%" condition)
           2))
    (clear-render-source-cache)))

(defun main ()
  "Run the Orfeus command-line frontend."
  (uiop:quit (cli-run (uiop:command-line-arguments))))
