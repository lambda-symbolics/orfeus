;;;; Where the first view of a photograph spends its time.
;;;;
;;;; The interactive loop is fast once a photograph is decoded; what remains is
;;;; the wait after the first click. For a DNG that is three full passes over a
;;;; 121 MB container — digest, inflate, write — before any pixel is developed,
;;;; so they are timed apart from the render rather than inside it.
;;;;
;;;;   sbcl --non-interactive --load scripts/bench-cold.lisp FILE [FILE...]

(require :asdf)
(asdf:load-system "orfeus")

(defpackage #:orfeus-cold-bench
  (:use #:common-lisp))
(in-package #:orfeus-cold-bench)

(defun milliseconds (start)
  (/ (* 1000.0 (- (get-internal-real-time) start))
     internal-time-units-per-second))

(defmacro timed (label &body body)
  `(let* ((start (get-internal-real-time))
          (value (progn ,@body)))
     (format t "  ~34A ~8,1F ms~%" ,label (milliseconds start))
     (finish-output)
     value))

(defun bench-file (path)
  (format t "~&~%=== ~A (~,1F MB) ===~%"
          (file-namestring path)
          (/ (with-open-file (stream path :element-type '(unsigned-byte 8))
               (file-length stream))
             1048576.0))
  ;; Each of these is a cold measurement, so the memo has to be dropped first.
  (orfeus:forget-content-key path)
  (timed "content key, cold" (orfeus:file-content-key path))
  (timed "content key, memoized" (orfeus:file-content-key path))
  (when (string-equal "dng" (or (pathname-type path) ""))
    (let ((target (merge-pathnames (format nil "cold-bench-~A.raw" (gensym))
                                   #P"/tmp/")))
      (unwind-protect
           (timed "extract embedded original"
             (orfeus::dng-extract-original path target :if-exists :supersede))
        (when (probe-file target) (delete-file target)))))
  (let* ((capacity (* 3 12000 12000))
         (buffer (cffi:foreign-alloc :unsigned-char :count capacity))
         (graph (orfeus:settings->graph (orfeus:make-processing-settings))))
    (unwind-protect
         (progn
           (timed "first render at 1600"
             (orfeus:render-preview-rgb path graph buffer capacity
                                        :max-width 1600 :max-height 1600))
           (timed "second render at 1600"
             (orfeus:render-preview-rgb path graph buffer capacity
                                        :max-width 1600 :max-height 1600)))
      (cffi:foreign-free buffer))))

(let ((files (rest sb-ext:*posix-argv*)))
  (when (null files)
    (format t "usage: bench-cold.lisp FILE [FILE...]~%")
    (sb-ext:quit :unix-status 2))
  (dolist (path files)
    (handler-case (bench-file (pathname path))
      (error (condition) (format t "  failed: ~A~%" condition)))))
