(in-package #:orfeus)

;;; Which photographs are out of focus.
;;;
;;; A shoot comes home with frames the photographer never meant to keep: the
;;; camera hunted, the subject moved, the shutter was too slow for the light.
;;; Finding them by hand is the dullest part of the work, and the usual
;;; automatic answer — the variance of the Laplacian over the whole frame —
;;; does not survive contact with real pictures. It is an absolute number with
;;; no units, so its threshold has to be re-guessed per camera and per ISO, and
;;; it averages the entire frame, so a macro shot wide open scores like a
;;; mistake because most of it is deliberately soft.
;;;
;;; The bridge answers a different question: how wide, in pixels, the blur is at
;;; the best-focused part of the frame, measured at the size a preview is looked
;;; at. That is a number with a meaning, so a threshold can be stated once and
;;; hold. See `native/src/focus.rs` for how it is measured.

(defparameter *blur-radius-threshold* 1.1
  "Blur radius, in pixels at a 1600-pixel long edge, past which a frame reads
as out of focus.

Measured against forty-six real photographs from four shoots — two bodies,
four lenses, f/1.4 to f/4, ISO 200 to 6400 — every one of which a human would
call sharp. They came in between 0.00 and 0.72, so this sits half again above
the softest of them. A frame blurred by a whole pixel at this scale reads 1.10,
and a whole pixel here is three at the sensor's own resolution, which is
softness anybody would see.

Set deliberately high. This pre-selects frames for a photographer to throw
away, and a keeper wrongly marked costs far more than a dud that slips through.")

(defparameter *focus-minimum-judgeable* 0.08
  "How much of a frame must carry edges before its focus means anything.

A frame of sky, a wall, or a lens cap has nothing in it to be sharp or soft, and
saying `blurry' about one is a guess dressed as a measurement.")

(defstruct (photo-focus-report (:constructor %make-photo-focus-report))
  "What the bridge measured about one frame's focus."
  (blur-radius nil)
  (typical-blur nil)
  (judgeable nil))

(defvar *photo-focus-cache*
  (make-hash-table :test #'equal #+sbcl :synchronized #+sbcl t)
  "Focus measurements keyed by path and write date.")

(defparameter *focus-cache-directory* nil
  "Where measurements are kept between runs, or NIL to keep none.

Measuring means developing the frame, which is half a second of work, and a
card holds hundreds. Recomputing all of it on every launch would make the
feature not worth having, so a caller that has somewhere durable to put them —
the interface does — points this at it.")

(defun focus-cache-file (pathname)
  "Return where PATHNAME's measurement is kept, or NIL if nowhere is."
  (when *focus-cache-directory*
    (let ((key (ignore-errors (file-content-key pathname))))
      (when key
        (merge-pathnames (format nil "~A.focus" key)
                         (pathname *focus-cache-directory*))))))

(defun read-focus-cache (file)
  "Return the measurement stored in FILE, or NIL."
  (ignore-errors
    (with-open-file (stream file :if-does-not-exist nil)
      (when stream
        (let ((blur (read stream nil))
              (typical (read stream nil))
              (judgeable (read stream nil)))
          (when (and (realp blur) (realp typical) (realp judgeable))
            (%make-photo-focus-report :blur-radius (float blur 1.0)
                                      :typical-blur (float typical 1.0)
                                      :judgeable (float judgeable 1.0))))))))

(defun write-focus-cache (file report)
  "Store REPORT in FILE, ignoring any reason it cannot be stored."
  (ignore-errors
    (ensure-directories-exist file)
    (with-open-file (stream file :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
      (format stream "~F ~F ~F~%"
              (photo-focus-report-blur-radius report)
              (photo-focus-report-typical-blur report)
              (photo-focus-report-judgeable report)))))

(defun measure-photo-focus (pathname)
  "Measure PATHNAME's focus through the bridge, or return NIL."
  (multiple-value-bind (blur typical judgeable)
      (ignore-errors (native-image-focus pathname))
    (when blur
      (%make-photo-focus-report :blur-radius blur
                                :typical-blur typical
                                :judgeable judgeable))))

(defun photo-focus (pathname)
  "Return PATHNAME's focus measurement, or NIL when it cannot be measured.

Memoized in this image and, when `*focus-cache-directory*' says where, on disk
as well. Costs a draft develop the first time and nothing after."
  (let* ((key (cons (namestring (pathname pathname))
                    (ignore-errors (file-write-date pathname))))
         (cached (gethash key *photo-focus-cache* :missing)))
    (if (not (eq cached :missing))
        cached
        (let* ((file (focus-cache-file pathname))
               (stored (and file (read-focus-cache file)))
               (report (or stored (measure-photo-focus pathname))))
          (when (and file report (not stored))
            (write-focus-cache file report))
          (setf (gethash key *photo-focus-cache*) report)))))

(defun photo-focus-known-p (pathname)
  "Whether PATHNAME's focus has already been measured in this image."
  (let ((key (cons (namestring (pathname pathname))
                   (ignore-errors (file-write-date pathname)))))
    (nth-value 1 (gethash key *photo-focus-cache*))))

(defun focus-verdict (report &key (threshold *blur-radius-threshold*)
                               (judgeable *focus-minimum-judgeable*))
  "Return :SHARP, :SOFT, or :UNKNOWN for REPORT.

:UNKNOWN covers both frames that were never measured and frames that were
measured and had nothing to say."
  (cond ((null report) :unknown)
        ((null (photo-focus-report-blur-radius report)) :unknown)
        ((< (or (photo-focus-report-judgeable report) 0) judgeable) :unknown)
        ((> (photo-focus-report-blur-radius report) threshold) :soft)
        (t :sharp)))

(defun blurry-photo-p (pathname &key (threshold *blur-radius-threshold*))
  "Whether PATHNAME is out of focus. NIL when it is sharp or cannot be judged."
  (eq :soft (focus-verdict (photo-focus pathname) :threshold threshold)))

(defun focus-description (report)
  "Return a phrase describing REPORT, or NIL when there is nothing to say.

Deliberately says nothing about the middling reading, tempting as it is to call
a frame a `sharp subject' when its sharpest part is crisp and its middle is
soft. On real photographs that reading is dominated by whatever parts of the
frame were dark and textureless, not by what was out of focus, so the phrase
would be wrong about half the time it appeared."
  (when report
    (let ((blur (photo-focus-report-blur-radius report)))
      (case (focus-verdict report)
        (:unknown "focus unclear")
        (:soft (format nil "soft · blur ~,1F px" blur))
        (t (format nil "sharp · blur ~,2F px" blur))))))
