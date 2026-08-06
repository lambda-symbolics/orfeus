(in-package #:orfeus)

(defun photo-job-automatic-output-stem (project photo)
  (let* ((base (pathname-name (photo-job-input-path photo)))
         (timestamp (and (export-settings-timestamp-filenames-p
                          (project-export-settings project))
                         (photo-capture-timestamp
                          (photo-job-input-path photo)))))
    (values (if timestamp (format nil "~A-~A" timestamp base) base)
            timestamp)))

(defun allocate-automatic-output-stem (stem allocated)
  "Return an unused variant of STEM and record it in ALLOCATED."
  (let ((candidate stem)
        (suffix 2))
    (loop while (gethash candidate allocated)
          do (setf candidate (format nil "~A-~D" stem suffix))
             (incf suffix))
    (setf (gethash candidate allocated) t)
    candidate))

(defun photo-job-timestamp-output-stem (project photo stem)
  "Allocate PHOTO's stable automatic STEM among preceding project photos."
  (if (not (export-settings-timestamp-filenames-p
            (project-export-settings project)))
      stem
      (let ((position (position photo (project-photos project) :test #'eq)))
        (if (null position)
            stem
            (let ((allocated (make-hash-table :test #'equal)))
              (dolist (prior (subseq (project-photos project) 0 position))
                (unless (photo-job-output-path prior)
                  (allocate-automatic-output-stem
                   (photo-job-automatic-output-stem project prior)
                   allocated)))
              (allocate-automatic-output-stem stem allocated))))))

(defun photo-job-render-output (project photo)
  "Return PHOTO's output pathname using PROJECT's output-directory semantics.

Automatic names keep the original filename; with the project's
TIMESTAMP-FILENAMES-P export option they gain a capture-time prefix like
20260802-183512-PB020123.jpg. Repeated timestamp/name pairs gain a stable
one-based suffix. Explicit :output paths are never rewritten."
  (let ((specified (photo-job-output-path photo)))
    (if specified
        (if (uiop:absolute-pathname-p specified)
            specified
            (merge-pathnames specified (project-output-directory project)))
        (multiple-value-bind (stem timestamp)
            (photo-job-automatic-output-stem project photo)
          (declare (ignore timestamp))
          (merge-pathnames
           (make-pathname :name (photo-job-timestamp-output-stem
                                 project photo stem)
                          :type (export-format-extension
                                 (export-settings-format
                                  (project-export-settings project))))
           (project-output-directory project))))))

(defun render-photo-job (project photo &key (if-exists :error))
  "Render PHOTO using PROJECT's processing and export settings."
  (let ((export (project-export-settings project)))
    (render-photo
     (photo-job-input-path photo)
     (photo-job-render-output project photo)
     (photo-render-settings project photo)
     :graph (photo-job-graph photo)
     :if-exists if-exists
     :jpeg-quality (export-settings-jpeg-quality export)
     :max-width (export-settings-max-width export)
     :max-height (export-settings-max-height export)
     :preserve-metadata-p (export-settings-preserve-metadata-p export))))

(defparameter *render-concurrency* 2
  "Photographs rendered at once by PROJECT-RENDER.

One photograph already saturates every core through the middle of the pipeline,
but its decode and its JPEG encode are largely serial, so a second render
overlapping those raises throughput. Kept low deliberately: each render in
flight holds whole-image float buffers, hundreds of megabytes at export size.")

(defun render-concurrency (count)
  "Workers to use for COUNT photographs, never more than there is work for."
  (max 1 (min count *render-concurrency*)))

(defun project-render (project &key (if-exists :error) (on-error :abort)
                                  progress-callback)
  "Render every photo in PROJECT through the shared processing pipeline.

Returns two values: completed output pathnames and `(PHOTO . CONDITION)`
failures, both in project order. ON-ERROR accepts :ABORT or :CONTINUE.
PROGRESS-CALLBACK, when provided, receives index, total, photo job, and output
pathname before each render starts.

Renders run *RENDER-CONCURRENCY* at a time, so progress callbacks may arrive
out of order and more than one photograph may be in flight when a failure
aborts the batch."
  (check-type project project)
  (check-type on-error (member :abort :continue))
  (let* ((photos (project-photos project))
         (total (length photos))
         (outputs (make-array total))
         (results (make-array total :initial-element nil))
         (next 0)
         (aborted nil)
         (lock (sb-thread:make-mutex :name "orfeus project render"))
         (workers (render-concurrency total)))
    (loop for photo in photos
          for index from 0
          do (setf (aref outputs index) (photo-job-render-output project photo)))
    (labels ((claim ()
               ;; One queue, taken under a lock, so a worker that finishes early
               ;; picks up the next photograph rather than idling.
               (sb-thread:with-mutex (lock)
                 (unless (or aborted (>= next total))
                   (prog1 next (incf next)))))
             (report (index)
               (when progress-callback
                 (sb-thread:with-mutex (lock)
                   (funcall progress-callback (1+ index) total
                            (nth index photos) (aref outputs index)))))
             (work ()
               (loop for index = (claim)
                     while index
                     do (report index)
                        (handler-case
                            (progn
                              (render-photo-job project (nth index photos)
                                                :if-exists if-exists)
                              (setf (aref results index)
                                    (cons :done (aref outputs index))))
                          (error (condition)
                            (setf (aref results index) (cons :failed condition))
                            (when (eq on-error :abort)
                              (sb-thread:with-mutex (lock)
                                (setf aborted t))))))))
      (if (= workers 1)
          (work)
          (let ((threads
                  (loop repeat workers
                        collect (sb-thread:make-thread
                                 #'work :name "orfeus render worker"))))
            (mapc #'sb-thread:join-thread threads))))
    (let ((completed '())
          (failures '())
          (first-failure nil))
      (loop for photo in photos
            for index from 0
            for result = (aref results index)
            do (case (car result)
                 (:done (push (cdr result) completed))
                 (:failed (push (cons photo (cdr result)) failures)
                          (unless first-failure
                            (setf first-failure (cdr result))))))
      (when (and first-failure (eq on-error :abort))
        (error first-failure))
      (values (nreverse completed) (nreverse failures)))))
