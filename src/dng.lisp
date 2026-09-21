(in-package #:orfeus)

(defun dng-pathname-p (pathname)
  "Return whether PATHNAME names a DNG, without regard to extension case."
  (let ((type (pathname-type pathname)))
    (and type (string-equal type "dng"))))

(defun dng-directory-pathnames (directory)
  "Return all DNG files below DIRECTORY in a deterministic order."
  (labels ((collect (path)
             (append
              (remove-if-not #'dng-pathname-p (uiop:directory-files path))
              (mapcan #'collect (uiop:subdirectories path)))))
    (sort (collect (uiop:ensure-directory-pathname directory))
          #'string< :key #'namestring)))

(defun dng-original-output-pathname (dng-pathname original-filename)
  "Return the safe sibling output pathname for ORIGINAL-FILENAME in DNG-PATHNAME's directory."
  (unless (and (stringp original-filename)
               (plusp (length original-filename))
               (string= original-filename (file-namestring original-filename)))
    (error 'dng-original-error
           :pathname dng-pathname
           :status 1
           :message "Embedded original filename is not a plain filename"))
  (merge-pathnames original-filename
                   (uiop:pathname-directory-pathname dng-pathname)))

(defun dng-temporary-output-pathname (output-pathname)
  "Return an unused sibling pathname for staging OUTPUT-PATHNAME."
  (loop for attempt from 0
        for pathname =
          (make-pathname
           :name (format nil ".~A.orfeus-extract-~D-~D"
                         (pathname-name output-pathname)
                         (get-universal-time)
                         attempt)
           :type (pathname-type output-pathname)
           :defaults output-pathname)
        unless (probe-file pathname)
          return pathname))

(defun dng-install-extracted-original (temporary-pathname output-pathname)
  "Atomically install TEMPORARY-PATHNAME as a new OUTPUT-PATHNAME.

The hard link refuses to overwrite a concurrently created output.  Both files
are siblings, so this does not cross filesystems."
  (unwind-protect
       (progn
         (link (namestring temporary-pathname) (namestring output-pathname))
         (delete-file temporary-pathname)
         output-pathname)
    (when (probe-file temporary-pathname)
      (delete-file temporary-pathname))))

(defun dng-replace-directory-originals-using-functions
    (directory extract-original original-filename &key progress-callback)
  "Replace DNGs below DIRECTORY using injected extraction functions.

This internal seam keeps the destructive filesystem workflow testable without
a photograph fixture."
  (let ((completed '())
        (failures '())
        (dngs (dng-directory-pathnames directory)))
    (loop for dng-pathname in dngs
          for index from 1
          do (let ((output-pathname nil)
                   (condition nil))
               (handler-case
                   (progn
                     (setf output-pathname
                           (dng-original-output-pathname
                            dng-pathname
                            (funcall original-filename dng-pathname)))
                     (when (probe-file output-pathname)
                       (error 'output-file-exists :pathname output-pathname))
                     (let ((temporary-pathname
                             (dng-temporary-output-pathname output-pathname)))
                       (unwind-protect
                            (progn
                              (funcall extract-original dng-pathname temporary-pathname
                                       :if-exists :error)
                              (dng-install-extracted-original temporary-pathname
                                                               output-pathname))
                         (when (probe-file temporary-pathname)
                           (delete-file temporary-pathname))))
                     (delete-file dng-pathname)
                     (push (cons dng-pathname output-pathname) completed))
                 (error (caught)
                   (setf condition caught)
                   (push (cons dng-pathname caught) failures)))
               (when progress-callback
                 (funcall progress-callback index (length dngs) dng-pathname
                          output-pathname condition))))
    (values (nreverse completed) (nreverse failures))))

(defun dng-replace-directory-originals (directory &key progress-callback)
  "Replace every DNG below DIRECTORY with its verified embedded original.

The original is extracted to a sibling temporary file, atomically installed
under its embedded filename, and only then is the DNG deleted.  Existing output
files are left untouched and reported as failures.  PROGRESS-CALLBACK receives
INDEX, TOTAL, DNG-PATHNAME, OUTPUT-PATHNAME, and a condition or NIL.  Return
completed and failed entries in processing order."
  (dng-replace-directory-originals-using-functions
   directory #'dng-extract-original #'dng-original-filename
   :progress-callback progress-callback))
