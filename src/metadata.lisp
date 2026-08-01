(in-package #:orfeus)

(defun usable-lens-description-p (value)
  (and (plusp (length value))
       (not (string= value "-"))
       (not (string-equal value "none"))
       (not (search "unknown" value :test #'char-equal))))

(defun preferred-lens-description (output)
  (find-if #'usable-lens-description-p
           (mapcar (lambda (line)
                     (string-trim '(#\Space #\Tab #\Return) line))
                   (uiop:split-string output :separator '(#\Newline)))))

(defun photo-lens-description (pathname)
  "Return PATHNAME's best electronic lens description, or NIL if unavailable."
  (multiple-value-bind (output error-output status)
      (uiop:run-program
       (list "exiftool" "-s3" "-Lens" "-LensModel" "-LensType"
             (namestring (pathname pathname)))
       :output :string :error-output :string :ignore-error-status t)
    (declare (ignore error-output))
    (and (zerop status) (preferred-lens-description output))))
