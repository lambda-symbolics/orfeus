(in-package #:orfeus)

(defun usable-metadata-value-p (value)
  (and value
       (plusp (length value))
       (not (string= value "-"))
       (not (string-equal value "none"))
       (not (search "unknown" value :test #'char-equal))))

(defun usable-lens-description-p (value)
  (usable-metadata-value-p value))

(defun preferred-lens-description (output)
  (find-if #'usable-lens-description-p
           (mapcar (lambda (line)
                     (string-trim '(#\Space #\Tab #\Return) line))
                   (uiop:split-string output :separator '(#\Newline)))))

(defvar *photo-lens-description-cache*
  (make-hash-table :test #'equal #+sbcl :synchronized #+sbcl t)
  "ExifTool lens lookups keyed by path and write date; a spawn per render is
otherwise the slowest step of an interactive preview.")

(defun read-photo-lens-description (pathname)
  (multiple-value-bind (output error-output status)
      (uiop:run-program
       (list "exiftool" "-s3" "-Lens" "-LensModel" "-LensType"
             (namestring (pathname pathname)))
       :output :string :error-output :string :ignore-error-status t)
    (declare (ignore error-output))
    (and (zerop status) (preferred-lens-description output))))

(defun photo-lens-description (pathname)
  "Return PATHNAME's best electronic lens description, or NIL if unavailable."
  (let* ((key (cons (namestring (pathname pathname))
                    (ignore-errors (file-write-date pathname))))
         (cached (gethash key *photo-lens-description-cache* :missing)))
    (if (eq cached :missing)
        (setf (gethash key *photo-lens-description-cache*)
              (read-photo-lens-description pathname))
        cached)))

(defun capture-description (model iso aperture shutter)
  (let ((parts
          (remove nil
                  (list (and (usable-metadata-value-p model) model)
                        (and (usable-metadata-value-p iso)
                             (format nil "ISO ~A" iso))
                        (and (usable-metadata-value-p aperture)
                             (format nil "f/~A" aperture))
                        (and (usable-metadata-value-p shutter) shutter)))))
    (when parts
      (format nil "~{~A~^ | ~}" parts))))

(defun photo-capture-description (pathname)
  "Return PATHNAME's camera, ISO, aperture, and shutter summary, or NIL."
  (multiple-value-bind (output error-output status)
      (uiop:run-program
       (list "exiftool" "-f" "-p" "$Model|$ISO|$FNumber|$ExposureTime"
             (namestring (pathname pathname)))
       :output :string :error-output :string :ignore-error-status t)
    (declare (ignore error-output))
    (when (zerop status)
      (let ((fields (uiop:split-string
                     (string-trim '(#\Space #\Tab #\Newline #\Return) output)
                     :separator '(#\|))))
        (when (= (length fields) 4)
          (apply #'capture-description
                 (mapcar (lambda (value)
                           (string-trim '(#\Space #\Tab #\Return) value))
                         fields)))))))
