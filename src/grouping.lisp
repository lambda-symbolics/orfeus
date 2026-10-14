(in-package #:orfeus)

;;; Grouping photographs taken in one burst.
;;;
;;; A photographer shooting a moving subject comes home with a dozen frames of
;;; it, and a filmstrip that lists them one after another buries everything
;;; else in the shoot. Two frames belong together when they were taken close
;;; together in time *and* look alike: time alone would join two different
;;; subjects photographed a second apart, and likeness alone would join every
;;; frame of a studio session shot over an hour.

(defparameter *group-within-seconds* 3
  "Longest gap between consecutive frames of one burst.

Three seconds rather than one: a photographer following a subject shoots in
short bursts with a breath between them, and those belong to the same moment.")

(defparameter *group-signature-distance* 12
  "Most bits two frames' signatures may differ by and still be one burst.

Out of sixty-four. A moving subject against a steady background changes a
handful of them; turning to photograph something else changes far more.")

(defun signature-distance (first second)
  "Return how many bits differ between two image signatures.

Signatures are unsigned 64-bit; NIL means the frame has not been signed yet,
which is not evidence of anything, so it reads as zero distance and leaves the
decision to the clock."
  (if (and first second)
      (logcount (logxor first second))
      0))

(defun capture-group-boundary-p (previous current
                                 &key (within-seconds *group-within-seconds*)
                                   (distance *group-signature-distance*))
  "Whether CURRENT starts a new group rather than continuing PREVIOUS's.

Each frame is compared against the one before it rather than against the first
of the group: a burst that pans across a scene drifts a long way from where it
started while every step of it is the same moment. A frame with no capture time
cannot be shown to belong with anything, so it stands alone."
  (destructuring-bind (previous-seconds previous-signature) previous
    (destructuring-bind (current-seconds current-signature) current
      (or (null previous-seconds)
          (null current-seconds)
          (> (abs (- current-seconds previous-seconds)) within-seconds)
          (> (signature-distance previous-signature current-signature)
             distance)))))

(defun group-captures (entries &key (within-seconds *group-within-seconds*)
                                 (distance *group-signature-distance*))
  "Partition ENTRIES into bursts, keeping their given order.

Each entry is (KEY SECONDS SIGNATURE): SECONDS is a universal time or NIL, and
SIGNATURE is a 64-bit image signature or NIL. Returns a list of lists of keys,
in the order the entries arrived — the caller has already decided what order to
show them in, and regrouping would move photographs around under the cursor."
  (let ((groups '())
        (current '())
        (previous nil))
    (dolist (entry entries)
      (destructuring-bind (key seconds signature) entry
        (when (and previous
                   (capture-group-boundary-p previous (list seconds signature)
                                             :within-seconds within-seconds
                                             :distance distance))
          (push (nreverse current) groups)
          (setf current '()))
        (push key current)
        (setf previous (list seconds signature))))
    (when current
      (push (nreverse current) groups))
    (nreverse groups)))

(defparameter *bracket-within-seconds* 4
  "Longest gap between consecutive frames of one exposure bracket.

A five-frame bracket whose longest frame is a quarter of a second takes about a
second; the clock only has whole seconds, so two frames a second apart can read
two seconds apart, and the margin above that covers a slow card.")

(defun group-brackets (entries &key (within-seconds *bracket-within-seconds*))
  "Find exposure brackets in ENTRIES, which are in capture order.

Each entry is (KEY SECONDS SHOT): SHOT is the frame's number in its bracket as
the camera recorded it, or NIL. A bracket is a run that starts at shot 1 and
counts up by one, frame after frame, each within WITHIN-SECONDS of the last.
Anything else — a run broken by a frame from outside it, a bracket whose first
frame was deleted — is not merged: a merge of the wrong frames is worse than
none. Returns a list of brackets, each a list of keys in shot order, of two
frames or more."
  (let ((brackets '())
        (current '())
        (previous nil))
    (flet ((close-current ()
             (when (rest current)
               (push (nreverse current) brackets))
             (setf current '())))
      (dolist (entry entries)
        (destructuring-bind (key seconds shot) entry
          (cond ((and shot (= shot 1))
                 (close-current)
                 (push key current))
                ((and shot current previous
                      (= shot (1+ (third previous)))
                      (second previous) seconds
                      (<= (abs (- seconds (second previous))) within-seconds))
                 (push key current))
                (t (close-current)))
          (setf previous (if current entry nil))))
      (close-current))
    (nreverse brackets)))

(defun group-index-of (groups)
  "Return a hash table from each key to (GROUP-NUMBER POSITION SIZE).

The filmstrip draws one row per photograph and needs to know, for that row,
which burst it belongs to and where it sits inside it."
  (let ((index (make-hash-table :test #'equal)))
    (loop for group in groups
          for number from 0
          do (loop for key in group
                   for position from 0
                   do (setf (gethash key index)
                            (list number position (length group)))))
    index))
