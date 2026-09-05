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

(defun render-photo-job (project photo &key (if-exists :error)
                                            (finisher #'funcall))
  "Render PHOTO using PROJECT's processing and export settings.

FINISHER is passed on to RENDER-PHOTO: it decides when the metadata copy and
the publishing of the file happen."
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
     :preserve-metadata-p (export-settings-preserve-metadata-p export)
     :finisher finisher)))

(defun render-photo-jobs (project photos &key (if-exists :error)
                                              (on-error :abort)
                                              progress-callback)
  "Render PHOTOS of PROJECT through the shared processing pipeline, in order.

Returns two values: completed output pathnames and `(PHOTO . CONDITION)`
failures, both in the order of PHOTOS. ON-ERROR accepts :ABORT or :CONTINUE.
PROGRESS-CALLBACK, when provided, receives index, total, photo job, and output
pathname before each render starts.

Renders run one at a time on purpose. Overlapping them was measured on a
13700H over six 20 MP frames: one at a time 11.3 s, two 12.1 s, three 11.6 s.
One render already saturates the machine, and what it is short of is memory
bandwidth, which a second render competes for rather than adding to. An
apparent gain in a first round of measurements was the page cache warming
across runs, and disappeared when the order was reversed.

What does overlap is the finishing of one photograph with the rendering of the
next. Copying the metadata is an ExifTool process — most of a second on one
core for a 20 MP frame, against a render of about the same length that has
every other core busy — so it runs on a thread of its own while the next render
starts, one such thread at a time. A photograph's outcome is settled when its
finisher has been joined, which is before the one after it is spawned and
before this function returns."
  (check-type project project)
  (check-type on-error (member :abort :continue))
  (let* ((photos (coerce photos 'list))
         (total (length photos))
         (outcomes (make-array total :initial-element nil))
         (pending nil))
    (labels ((join-pending ()
               ;; The one finisher in flight, if any, and what became of it.
               (when pending
                 (destructuring-bind (index . thread) pending
                   (setf pending nil)
                   (let ((result (sb-thread:join-thread thread :default nil)))
                     (setf (aref outcomes index)
                           (if (typep result 'condition)
                               result
                               :done))
                     (when (and (typep result 'condition)
                                (eq on-error :abort))
                       (error result))))))
             (finish-later (index thunk)
               (join-pending)
               (setf pending
                     (cons index
                           (sb-thread:make-thread
                            (lambda ()
                              (handler-case (progn (funcall thunk) :done)
                                (error (condition) condition)))
                            :name "Orfeus export finish")))))
      (loop for photo in photos
            for index from 0
            for output = (photo-job-render-output project photo)
            do (when progress-callback
                 (funcall progress-callback (1+ index) total photo output))
               (handler-case
                   (render-photo-job project photo
                                     :if-exists if-exists
                                     :finisher (lambda (thunk)
                                                 (finish-later index thunk)))
                 (error (condition)
                   (setf (aref outcomes index) condition)
                   (when (eq on-error :abort)
                     (join-pending)
                     (error condition)))))
      (join-pending))
    (let ((completed '())
          (failures '()))
      (loop for photo in photos
            for index from 0
            for outcome = (aref outcomes index)
            do (if (typep outcome 'condition)
                   (push (cons photo outcome) failures)
                   (push (photo-job-render-output project photo) completed)))
      (values (nreverse completed) (nreverse failures)))))

(defun project-render (project &key (if-exists :error) (on-error :abort)
                                  progress-callback)
  "Render every photo in PROJECT; see RENDER-PHOTO-JOBS for the contract."
  (render-photo-jobs project (project-photos project)
                     :if-exists if-exists
                     :on-error on-error
                     :progress-callback progress-callback))
