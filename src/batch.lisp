(in-package #:orfeus)

(defun photo-job-render-output (project photo)
  "Return PHOTO's output pathname using PROJECT's output-directory semantics."
  (let ((specified (photo-job-output-path photo)))
    (if specified
        (if (uiop:absolute-pathname-p specified)
            specified
            (merge-pathnames specified (project-output-directory project)))
        (merge-pathnames
         (make-pathname :name (pathname-name (photo-job-input-path photo))
                        :type "jpg")
         (project-output-directory project)))))

(defun render-photo-job (project photo &key (if-exists :error))
  "Render PHOTO using PROJECT's processing and export settings."
  (let ((export (project-export-settings project)))
    (render-photo
     (photo-job-input-path photo)
     (photo-job-render-output project photo)
     (photo-render-settings project photo)
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
