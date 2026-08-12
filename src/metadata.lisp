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

(defparameter *photo-lens-tags*
  '("-Lens" "-LensModel" "-LensType" "-LensID")
  "ExifTool lens tags, most specific to the photographer's own naming first.

The first three name the lens as the camera wrote it, which is what an adapted
lens is recognized by. The last two are ExifTool's decoding of the maker note's
lens identifier, which is the name a lens profile is looked up under.")

(defvar *photo-lens-cache*
  (make-hash-table :test #'equal #+sbcl :synchronized #+sbcl t)
  "ExifTool lens lookups keyed by path and write date; a spawn per render is
otherwise the slowest step of an interactive preview.")

(defun read-photo-lens-tags (pathname)
  "Return (VALUES DESCRIPTION NAME) read from PATHNAME in one ExifTool run.

DESCRIPTION is how the camera wrote the lens down, which adapted-lens mappings
are keyed by. NAME is ExifTool's decoded lens name, which is the same string
rawler reports for an ORF and the one a Lensfun profile is found under. Asked
for with -f so an absent tag keeps its place in the output as a dash."
  (multiple-value-bind (output error-output status)
      (uiop:run-program
       (append (list "exiftool" "-f" "-s3") *photo-lens-tags*
               (list (namestring (pathname pathname))))
       :output :string :error-output :string :ignore-error-status t)
    (declare (ignore error-output))
    (if (zerop status)
        (let ((lines (mapcar (lambda (line)
                               (string-trim '(#\Space #\Tab #\Return) line))
                             (uiop:split-string output :separator '(#\Newline)))))
          (values (find-if #'usable-lens-description-p (subseq lines 0 (min 3 (length lines))))
                  (find-if #'usable-lens-description-p (nthcdr 2 lines))))
        (values nil nil))))

(defun photo-lens-tags (pathname)
  (let* ((key (cons (namestring (pathname pathname))
                    (ignore-errors (file-write-date pathname))))
         (cached (gethash key *photo-lens-cache* :missing)))
    (if (eq cached :missing)
        (setf (gethash key *photo-lens-cache*)
              (multiple-value-bind (description name)
                  (read-photo-lens-tags pathname)
                (cons description name)))
        cached)))

(defun photo-lens-description (pathname)
  "Return PATHNAME's best electronic lens description, or NIL if unavailable."
  (first (photo-lens-tags pathname)))

(defun photo-lens-name (pathname)
  "Return the lens name PATHNAME's maker note decodes to, or NIL.

Handed to the renderer for containers that carry no lens metadata it can read:
rawler names the lens in an ORF but not in the DNG converted from it."
  (rest (photo-lens-tags pathname)))

(defvar *photo-capture-timestamp-cache*
  (make-hash-table :test #'equal #+sbcl :synchronized #+sbcl t)
  "Capture timestamps keyed by path and write date.")

(defun timestamp-token (raw)
  "Normalize an EXIF date like \"2026:08:02 18:35:12\" to 20260802-183512."
  (let ((digits (remove-if-not #'digit-char-p raw)))
    (when (>= (length digits) 14)
      (format nil "~A-~A" (subseq digits 0 8) (subseq digits 8 14)))))

(defun read-photo-capture-timestamp (pathname)
  (multiple-value-bind (output error-output status)
      (uiop:run-program
       (list "exiftool" "-s3" "-DateTimeOriginal" "-CreateDate"
             (namestring (pathname pathname)))
       :output :string :error-output :string :ignore-error-status t)
    (declare (ignore error-output))
    (when (zerop status)
      (loop for line in (uiop:split-string output :separator '(#\Newline))
            for token = (timestamp-token line)
            when token return token))))

(defun photo-capture-timestamp (pathname)
  "Return PATHNAME's capture time as a 20260802-183512 token, or NIL."
  (let* ((key (cons (namestring (pathname pathname))
                    (ignore-errors (file-write-date pathname))))
         (cached (gethash key *photo-capture-timestamp-cache* :missing)))
    (if (eq cached :missing)
        (setf (gethash key *photo-capture-timestamp-cache*)
              (read-photo-capture-timestamp pathname))
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
