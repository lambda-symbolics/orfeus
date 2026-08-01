(in-package #:orfeus/tests)

(defun test-temporary-pathname (suffix)
  (merge-pathnames
   (format nil "orfeus-test-~D-~D.~A"
           (get-universal-time) (random most-positive-fixnum) suffix)
   #P"/tmp/"))

(defun project-test-value ()
  (make-project
   :output-directory #P"exports/"
   :defaults (make-processing-settings :exposure 0.75 :grain-amount 0.12)
   :photos (list (make-photo-job
                  :input-path #P"input/example.orf"
                  :overrides '(:exposure -0.25 :lut-strength 0.8)))))

(defun project-round-trip-p ()
  (let* ((original (project-test-value))
         (decoded (sexp->project (project->sexp original))))
    (and (= 0.75 (processing-settings-exposure
                  (project-defaults decoded)))
         (= 1 (length (project-photos decoded)))
         (equal '(:exposure -0.25 :lut-strength 0.8)
                (photo-job-overrides (first (project-photos decoded)))))))

(defun project-file-round-trip-p ()
  (let ((pathname (test-temporary-pathname "sexp")))
    (unwind-protect
         (progn
           (project-write (project-test-value) pathname)
           (and (not (search "#A(" (uiop:read-file-string pathname)))
                (search "\"exports/\"" (uiop:read-file-string pathname))
                (project-round-trip-p-from (project-read pathname))))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun project-round-trip-p-from (project)
  (and (= 0.75 (processing-settings-exposure (project-defaults project)))
       (= 1 (length (project-photos project)))))

(defun project-reader-evaluation-disabled-p ()
  (let ((pathname (test-temporary-pathname "sexp")))
    (unwind-protect
         (progn
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede)
             (write-string "#.(error \"reader evaluation escaped\")" stream))
           (handler-case
               (progn (project-read pathname) nil)
             (reader-error () t)))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun invalid-project-rejected-p ()
  (handler-case
      (progn
        (sexp->project '(:orfeus-project 99))
        nil)
    (invalid-project-data () t)))

(defun processing-overrides-p ()
  (let ((settings
          (processing-settings-with-overrides
           (make-processing-settings :exposure 0.5 :grain-amount 0.0)
           '(:exposure -1.0 :grain-amount 0.2))))
    (and (= -1.0 (processing-settings-exposure settings))
         (= 0.2 (processing-settings-grain-amount settings)))))

(defun project-relative-paths-p ()
  (let ((pathname (test-temporary-pathname "sexp")))
    (unwind-protect
         (progn
           (project-write
            (make-project
             :output-directory #P"exports/"
             :photos (list (make-photo-job :input-path #P"input.orf")))
            pathname)
           (let* ((project (project-read pathname))
                  (base (uiop:pathname-directory-pathname pathname)))
             (and (equal (project-output-directory project)
                         (merge-pathnames #P"exports/" base))
                  (equal (photo-job-input-path (first (project-photos project)))
                         (merge-pathnames #P"input.orf" base)))))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun render-rejects-input-as-output-p ()
  (let ((pathname (test-temporary-pathname "orf"))
        (contents "source pixels must survive"))
    (unwind-protect
         (progn
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede)
             (write-string contents stream))
           (let ((rejected
                   (handler-case
                       (progn
                         (render-photo pathname pathname
                                       (make-processing-settings)
                                       :if-exists :supersede)
                         nil)
                     (raw-render-error () t))))
             (and rejected
                  (with-open-file (stream pathname :direction :input)
                    (string= contents (read-line stream nil ""))))))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun preview-does-not-overwrite-p ()
  (let ((input (test-temporary-pathname "orf"))
        (output (test-temporary-pathname "jpg"))
        (contents "keep this export"))
    (unwind-protect
         (progn
           (with-open-file (stream input :direction :output
                                         :if-exists :supersede)
             (write-string "raw" stream))
           (with-open-file (stream output :direction :output
                                          :if-exists :supersede)
             (write-string contents stream))
           (let ((rejected
                   (handler-case
                       (progn
                         (render-preview input output
                                         (make-processing-settings))
                         nil)
                     (output-file-exists () t))))
             (and rejected
                  (with-open-file (stream output :direction :input)
                    (string= contents (read-line stream nil ""))))))
      (dolist (pathname (list input output))
        (when (probe-file pathname)
          (delete-file pathname))))))

(defun publish-race-does-not-clobber-p ()
  (let ((temporary (test-temporary-pathname "jpg"))
        (output (test-temporary-pathname "jpg")))
    (unwind-protect
         (progn
           (with-open-file (stream temporary :direction :output
                                             :if-exists :supersede)
             (write-string "new render" stream))
           (with-open-file (stream output :direction :output
                                          :if-exists :supersede)
             (write-string "racing writer" stream))
           (let ((rejected
                   (handler-case
                       (progn
                         (uiop:symbol-call '#:orfeus '#:render-publish
                                           temporary output :error)
                         nil)
                     (output-file-exists () t))))
             (and rejected
                  (with-open-file (stream output :direction :input)
                    (string= "racing writer" (read-line stream nil ""))))))
      (dolist (pathname (list temporary output))
        (when (probe-file pathname)
          (delete-file pathname))))))

(defun cli-version-p ()
  (let ((output (make-string-output-stream))
        (errors (make-string-output-stream)))
    (and (zerop (cli-run '("--version")
                         :output-stream output
                         :error-stream errors))
         (search (orfeus-version) (get-output-stream-string output))
         (string= "" (get-output-stream-string errors)))))

(defun cli-rejects-unknown-command-p ()
  (let ((output (make-string-output-stream))
        (errors (make-string-output-stream)))
    (and (= 2 (cli-run '("wat")
                       :output-stream output
                       :error-stream errors))
         (search "Unknown command" (get-output-stream-string errors)))))

(defun run-tests ()
  "Run the dependency-free Orfeus test suite."
  (let ((failures 0))
    (flet ((check (description predicate)
             (format t "~:[not ok~;ok~] - ~A~%" predicate description)
             (unless predicate
               (incf failures))))
      (check "system exposes a version"
             (and (stringp (orfeus-version))
                  (plusp (length (orfeus-version)))))
      (check "project S-expressions round trip" (project-round-trip-p))
      (check "project files round trip" (project-file-round-trip-p))
      (check "project-relative paths resolve beside the project"
             (project-relative-paths-p))
      (check "project reads disable reader evaluation"
             (project-reader-evaluation-disabled-p))
      (check "invalid project versions are rejected"
             (invalid-project-rejected-p))
      (check "per-photo overrides produce effective settings"
             (processing-overrides-p))
      (check "rendering never replaces its input"
             (render-rejects-input-as-output-p))
      (check "preview does not overwrite an existing export"
             (preview-does-not-overwrite-p))
      (check "a racing writer is not clobbered at publish time"
             (publish-race-does-not-clobber-p))
      (check "CLI reports its version" (cli-version-p))
      (check "CLI rejects unknown commands" (cli-rejects-unknown-command-p)))
    (zerop failures)))
