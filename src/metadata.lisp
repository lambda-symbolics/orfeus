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

(defun timestamp-token (raw)
  "Normalize an EXIF date like \"2026:08:02 18:35:12\" to 20260802-183512."
  (let ((digits (remove-if-not #'digit-char-p raw)))
    (when (>= (length digits) 14)
      (format nil "~A-~A" (subseq digits 0 8) (subseq digits 8 14)))))

(defun capture-universal-time (raw)
  "Return an EXIF date like \"2026:08:02 18:35:12\" as a universal time.

Read as local time throughout: only differences between two frames matter to
the grouping this feeds, and a shoot does not cross a time zone mid-burst."
  (let ((digits (remove-if-not #'digit-char-p raw)))
    (when (>= (length digits) 14)
      (flet ((part (start end) (parse-integer digits :start start :end end)))
        (ignore-errors
          (encode-universal-time (part 12 14) (part 10 12) (part 8 10)
                                 (part 6 8) (part 4 6) (part 0 4)))))))

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

(defun parsed-rating (value)
  "Return VALUE as a star rating in 1..5, or NIL when it says nothing.

A camera writes zero for an unrated frame, which is not a rating of zero out of
five — nobody rates anything zero — so it reads as unrated."
  (let ((number (and (usable-metadata-value-p value)
                     (ignore-errors (parse-integer value :junk-allowed t)))))
    (when (and number (<= 1 number 5))
      number)))

(defparameter *photo-metadata-tags*
  '("-Lens" "-LensModel" "-LensType" "-LensID"
    "-Rating" "-XMP:Rating"
    "-DateTimeOriginal" "-CreateDate"
    "-Model" "-ISO" "-FNumber" "-ExposureTime")
  "Everything read out of a photograph, in one ExifTool run.

Asked for together because the subprocess is the cost, not the tags: the lens,
the time and the exposure summary used to be three separate reads, which meant
three interpreter startups and three passes over a container that can be a
hundred megabytes. Asked for with -f so an absent tag holds its place in the
output as a dash, which is what keeps the answers positional.")

(defstruct (photo-metadata (:constructor %make-photo-metadata))
  "What a photograph says about itself."
  (lens-description nil)
  (lens-name nil)
  (rating nil)
  (timestamp nil)
  (seconds nil)
  (capture-summary nil))

(defvar *photo-metadata-cache*
  (make-hash-table :test #'equal #+sbcl :synchronized #+sbcl t)
  "Photograph metadata keyed by path and write date; a spawn per render is
otherwise the slowest step of an interactive preview.")

(defun read-photo-metadata (pathname)
  "Run ExifTool once over PATHNAME and return everything it said."
  (multiple-value-bind (output error-output status)
      (uiop:run-program
       (append (list "exiftool" "-f" "-s3") *photo-metadata-tags*
               (list (namestring (pathname pathname))))
       :output :string :error-output :string :ignore-error-status t)
    (declare (ignore error-output))
    (if (zerop status)
        (let* ((lines (mapcar (lambda (line)
                                (string-trim '(#\Space #\Tab #\Return) line))
                              (uiop:split-string output :separator
                                                 '(#\Newline))))
               (dates (remove-if-not #'usable-metadata-value-p
                                     (list (nth 6 lines) (nth 7 lines)))))
          (flet ((field (index)
                   (let ((value (nth index lines)))
                     (and (usable-metadata-value-p value) value))))
            (%make-photo-metadata
             :lens-description (find-if #'usable-lens-description-p
                                        (subseq lines 0 (min 3 (length lines))))
             :lens-name (find-if #'usable-lens-description-p
                                 (subseq lines 2 (min 4 (length lines))))
             :rating (or (parsed-rating (nth 4 lines))
                         (parsed-rating (nth 5 lines)))
             :timestamp (some #'timestamp-token dates)
             :seconds (some #'capture-universal-time dates)
             :capture-summary (capture-description (field 8) (field 9)
                                                   (field 10) (field 11)))))
        (%make-photo-metadata))))

(defun photo-metadata (pathname)
  "Return PATHNAME's memoized metadata."
  (let* ((key (cons (namestring (pathname pathname))
                    (ignore-errors (file-write-date pathname))))
         (cached (gethash key *photo-metadata-cache* :missing)))
    (if (eq cached :missing)
        (setf (gethash key *photo-metadata-cache*)
              (read-photo-metadata pathname))
        cached)))

(defun photo-metadata-known-p (pathname)
  "Whether PATHNAME's metadata has already been read.

Callers that arrange to spend the ExifTool wait on other work need to know
whether there is a wait at all: on a drag there is not, and the arranging would
cost more than it saves."
  (let ((key (cons (namestring (pathname pathname))
                   (ignore-errors (file-write-date pathname)))))
    (nth-value 1 (gethash key *photo-metadata-cache*))))

(defun photo-lens-description (pathname)
  "Return PATHNAME's best electronic lens description, or NIL if unavailable."
  (photo-metadata-lens-description (photo-metadata pathname)))

(defun photo-lens-name (pathname)
  "Return the lens name PATHNAME's maker note decodes to, or NIL.

Handed to the renderer for containers that carry no lens metadata it can read:
rawler names the lens in an ORF but not in the DNG converted from it."
  (photo-metadata-lens-name (photo-metadata pathname)))

(defun photo-rating (pathname)
  "Return the star rating the photographer gave PATHNAME, 1 to 5, or NIL."
  (photo-metadata-rating (photo-metadata pathname)))

(defun photo-capture-timestamp (pathname)
  "Return PATHNAME's capture time as a 20260802-183512 token, or NIL."
  (photo-metadata-timestamp (photo-metadata pathname)))

(defun photo-capture-seconds (pathname)
  "Return PATHNAME's capture time as a universal time, or NIL."
  (photo-metadata-seconds (photo-metadata pathname)))

(defun photo-capture-description (pathname)
  "Return PATHNAME's camera, ISO, aperture, and shutter summary, or NIL."
  (photo-metadata-capture-summary (photo-metadata pathname)))

(defvar *photo-as-shot-kelvin-cache*
  (make-hash-table :test #'equal #+sbcl :synchronized #+sbcl t)
  "As-shot colour temperatures keyed by path and write date.")

(defun photo-as-shot-kelvin (pathname)
  "Return the colour temperature PATHNAME's camera balanced for, or NIL.

Read from the camera's own white balance through the bridge, and memoized: a
temperature control asks for it whenever the selection changes."
  (let* ((key (cons (namestring (pathname pathname))
                    (ignore-errors (file-write-date pathname))))
         (cached (gethash key *photo-as-shot-kelvin-cache* :missing)))
    (if (eq cached :missing)
        (setf (gethash key *photo-as-shot-kelvin-cache*)
              (ignore-errors (native-as-shot-kelvin pathname)))
        cached)))

(defvar *photo-signature-cache*
  (make-hash-table :test #'equal #+sbcl :synchronized #+sbcl t)
  "Image signatures keyed by the file read and its write date.")

(defun photo-signature (pathname)
  "Return a 64-bit perceptual signature of the image at PATHNAME, or NIL.

Given a thumbnail the caller already had rather than a photograph: signing is
meant to cost nothing beyond the preview a filmstrip drew anyway."
  (let* ((key (cons (namestring (pathname pathname))
                    (ignore-errors (file-write-date pathname))))
         (cached (gethash key *photo-signature-cache* :missing)))
    (if (eq cached :missing)
        (setf (gethash key *photo-signature-cache*)
              (ignore-errors (native-image-signature pathname)))
        cached)))
