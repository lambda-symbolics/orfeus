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

(defun project-render (project &key (if-exists :error) (on-error :abort)
                                  progress-callback)
  "Render every photo in PROJECT through the shared processing pipeline.

Returns two values: completed output pathnames and `(PHOTO . CONDITION)`
failures. ON-ERROR accepts :ABORT or :CONTINUE. PROGRESS-CALLBACK, when
provided, receives index, total, photo job, and output pathname before each
render starts."
  (check-type project project)
  (check-type on-error (member :abort :continue))
  (let ((completed '())
        (failures '())
        (photos (project-photos project)))
    (loop for photo in photos
          for index from 1
          for output = (photo-job-render-output project photo)
          do (when progress-callback
               (funcall progress-callback index (length photos) photo output))
             (handler-case
                 (progn
                   (render-photo-job project photo :if-exists if-exists)
                   (push output completed))
               (error (condition)
                 (push (cons photo condition) failures)
                 (when (eq on-error :abort)
                   (error condition)))))
    (values (nreverse completed) (nreverse failures))))
