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
           (project-round-trip-p-from (project-read pathname)))
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
      (check "project reads disable reader evaluation"
             (project-reader-evaluation-disabled-p))
      (check "invalid project versions are rejected"
             (invalid-project-rejected-p))
      (check "CLI reports its version" (cli-version-p))
      (check "CLI rejects unknown commands" (cli-rejects-unknown-command-p)))
    (zerop failures)))
