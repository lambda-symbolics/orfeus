;;;; Measure the interactive edit loop the way the GUI actually drives it.
;;;;
;;;; The viewport render and a curve drag are separate paths with separate
;;;; costs, and a drag's second tick is the one that matters: it should resume
;;;; from a checkpoint instead of developing the RAW again. Reporting only a
;;;; cold render hides exactly the regression this measures.
;;;;
;;;;   sbcl --non-interactive --load scripts/bench-interactive.lisp FILE [FILE...]

(require :asdf)
(asdf:load-system "orfeus")

(defpackage #:orfeus-bench
  (:use #:common-lisp))
(in-package #:orfeus-bench)

(defun milliseconds (start end)
  (/ (* 1000.0 (- end start)) internal-time-units-per-second))

(defmacro timing (&body body)
  (let ((start (gensym)) (value (gensym)))
    `(let* ((,start (get-internal-real-time))
            (,value (progn ,@body)))
       (values ,value (milliseconds ,start (get-internal-real-time))))))

(defun curve-graph (lift)
  "A graph ending in a curves node whose red shadow point sits at LIFT."
  (let* ((graph (orfeus:settings->graph (orfeus:make-processing-settings)))
         (node (orfeus:graph-insert-node
                graph (orfeus:processing-graph-output graph) :curves
                :params (list :red-points
                              (list 0.0 0.0 0.33 lift 0.67 0.67 1.0 1.0)))))
    (declare (ignore node))
    graph))

(defun bench-file (path bounds)
  (let ((capacity (* 3 12000 12000)))
    (format t "~&~%=== ~A ===~%" (file-namestring path))
    (let ((buffer (cffi:foreign-alloc :unsigned-char :count capacity)))
      (unwind-protect
           (dolist (bound bounds)
             (format t "  --- bound ~D ---~%" bound)
             ;; Cold: nothing cached. Then a second identical render, then two
             ;; that move one curve point, which is what a drag does.
             (multiple-value-bind (size cold)
                 (timing (multiple-value-list
                          (orfeus:render-preview-rgb
                           path (curve-graph 0.33) buffer capacity
                           :max-width bound :max-height bound)))
               (format t "    cold        ~,1F ms  -> ~{~A~^x~}~%" cold size))
             (multiple-value-bind (size warm)
                 (timing (multiple-value-list
                          (orfeus:render-preview-rgb
                           path (curve-graph 0.33) buffer capacity
                           :max-width bound :max-height bound)))
               (declare (ignore size))
               (format t "    same again  ~,1F ms~%" warm))
             (loop for lift in '(0.30 0.36 0.40 0.44 0.48)
                   for index from 1
                   do (multiple-value-bind (size drag)
                          (timing (multiple-value-list
                                   (orfeus:render-preview-rgb
                                    path (curve-graph lift) buffer capacity
                                    :max-width bound :max-height bound)))
                        (declare (ignore size))
                        (format t "    drag tick ~D ~,1F ms~%" index drag))))
        (cffi:foreign-free buffer)))))

(let ((files (rest sb-ext:*posix-argv*))
      (bounds (list 1600 2048 0)))
  (when (null files)
    (format t "usage: bench-interactive.lisp FILE [FILE...]~%")
    (sb-ext:quit :unix-status 2))
  (dolist (path files)
    (handler-case (bench-file (pathname path) bounds)
      (error (condition)
        (format t "  failed: ~A~%" condition)))))
