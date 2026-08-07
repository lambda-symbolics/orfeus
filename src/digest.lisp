(in-package #:orfeus)

;;; Content keys for the render and preview caches.
;;;
;;; These name cached artefacts; they are not signatures, so they do not need a
;;; cryptographic hash — and they must not cost what one costs, because a key has
;;; to be computed before the cache it guards can be consulted.

(defconstant +fnv-prime+ 1099511628211
  "The 64-bit FNV-1a multiplier.")

(defconstant +digest-chunk-words+ 131072
  "Words per read. Part of the digest: the second lane mixes in each word's
index within its chunk, so changing this changes every key.")

(defun file-fast-digest (pathname)
  "Return a 128-bit hex digest of every byte of PATHNAME.

FNV-1a over 64-bit words rather than SHA-256, because this keys a preview cache
and is not a signature. Measured in SBCL: ironclad's SHA-256 manages 91 MB/s, so
one 20 MB frame costs 220 ms and an 80 MP high-resolution frame nearly a second.

Words come out of a byte buffer through a pinned SAP rather than from a stream of
`(unsigned-byte 64)`. That is not a micro-optimization: reading a 116 MB DNG as
words cost 330 ms while reading the same file as bytes cost 10 ms, so SBCL's
word-element `read-sequence` was doing the work an element at a time and the
digest was almost entirely stream overhead. The arithmetic was never the limit.

Still every byte, not a sample: a sampled key once let an edit in the middle of a
file go unnoticed. Two lanes give 128 bits, which is far more than a cache of
preview files needs."
  (let ((low 14695981039346656037)
        (high 1099511628211)
        (buffer (make-array (* 8 +digest-chunk-words+)
                            :element-type '(unsigned-byte 8)))
        (total 0))
    (declare (type (unsigned-byte 64) low high)
             (type (simple-array (unsigned-byte 8) (*)) buffer)
             (type unsigned-byte total)
             (optimize (speed 3)))
    (with-open-file (stream pathname :element-type '(unsigned-byte 8))
      (loop
        (let ((count (read-sequence buffer stream)))
          (declare (type fixnum count))
          (when (zerop count) (return))
          (incf total count)
          (let ((words (ash count -3)))
            (declare (type fixnum words))
            (sb-sys:with-pinned-objects (buffer)
              (let ((sap (sb-sys:vector-sap buffer)))
                (loop for index of-type fixnum below words
                      do (let ((word (sb-sys:sap-ref-64 sap (ash index 3))))
                           (declare (type (unsigned-byte 64) word))
                           (setf low (ldb (byte 64 0)
                                          (* (logxor low word) +fnv-prime+))
                                 high (ldb (byte 64 0)
                                           (* (logxor high (logxor word index))
                                              +fnv-prime+)))))))
            ;; Bytes past the last whole word exist only at end of file, so the
            ;; tail is folded here in the same order a separate pass would.
            (loop for offset of-type fixnum from (ash words 3) below count
                  do (setf low (ldb (byte 64 0)
                                    (* (logxor low (aref buffer offset))
                                       +fnv-prime+)))))
          ;; A short read means end of file, and only whole chunks may mix
          ;; indices 0..CHUNK-1, so stopping here keeps the key stable.
          (when (< count (length buffer)) (return)))))
    (setf high (ldb (byte 64 0) (* (logxor high total) +fnv-prime+)))
    (format nil "~16,'0x~16,'0x" low high)))

(defvar *content-key-memo* (make-hash-table :test #'equal)
  "Cached content digests, keyed by path, size and modification time.")

(defvar *content-key-memo-lock* (sb-thread:make-mutex :name "orfeus content keys"))

(defparameter *content-key-memo-capacity* 1024)

(defun file-stat-identity (pathname)
  "Return PATHNAME's size and modification time, or NIL when it cannot stat."
  (let ((stat (ignore-errors (sb-posix:stat (namestring pathname)))))
    (when stat
      (list (namestring pathname)
            (sb-posix:stat-size stat)
            (sb-posix:stat-mtime stat)))))

(defparameter *content-key-settle-seconds* 2
  "How recently a file may have changed before its key is left unmemoized.

`stat` reports whole seconds on this platform, so a file edited in place without
changing size could otherwise keep a stale key for the rest of that second.
Anything untouched for longer than this cannot change without moving its mtime,
which the memo key already includes — so refusing to memoize the recent past
closes the window completely rather than living with it.")

(defun file-settled-p (identity)
  "True when IDENTITY's file has been unmodified long enough to memoize."
  (let ((mtime (third identity)))
    (and (integerp mtime)
         (> (- (sb-ext:get-time-of-day) mtime) *content-key-settle-seconds*))))

(defun file-content-key (pathname)
  "Return a content digest of PATHNAME, memoized against its size and mtime.

The digest keys every rendered preview, so it used to be recomputed on each
click — twice, since the render also re-read it to check the source had not
changed underneath. At 220 ms a time that made revisiting an already-rendered
photograph as slow as rendering it. Memoizing on the stat costs a syscall.

Derived from contents rather than from the stat itself so that copying a RAW —
interning it off a card — keeps every preview already rendered for it."
  (let* ((identity (file-stat-identity pathname))
         (memoizable (and identity (file-settled-p identity))))
    (or (when memoizable
          (sb-thread:with-mutex (*content-key-memo-lock*)
            (gethash identity *content-key-memo*)))
        (let ((digest (file-fast-digest pathname)))
          (when memoizable
            (sb-thread:with-mutex (*content-key-memo-lock*)
              (when (> (hash-table-count *content-key-memo*)
                       *content-key-memo-capacity*)
                (clrhash *content-key-memo*))
              (setf (gethash identity *content-key-memo*) digest)))
          digest))))

(defun forget-content-key (pathname)
  "Drop PATHNAME's memoized content key, forcing the next read to digest it.

Used when a render has just detected that a source changed underneath it, where
the stat may not have caught up."
  (let ((identity (file-stat-identity pathname)))
    (when identity
      (sb-thread:with-mutex (*content-key-memo-lock*)
        (remhash identity *content-key-memo*)))
    identity))
