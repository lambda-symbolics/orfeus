(in-package #:orfeus)

(defun orfeus-version ()
  "Return the installed Orfeus version string."
  (or (asdf:component-version (asdf:find-system '#:orfeus nil))
      "unknown"))

(defvar *build-commit* :unresolved
  "Memoized build revision: a string, or NIL when it cannot be determined.")

(defun read-build-commit ()
  "Return the revision Orfeus was built from, or NIL.

Asked of git in the system's own directory, because Orfeus is normally run
straight from a checkout. A build with uncommitted changes says so: the whole
point of showing a revision is to answer \"is this the build with that fix in
it\", and a dirty tree means the revision alone does not answer it."
  (let ((directory (asdf:system-source-directory '#:orfeus)))
    (when directory
      (flet ((git (&rest arguments)
               (multiple-value-bind (output error-output status)
                   (ignore-errors
                     (uiop:run-program (append (list "git" "-C"
                                                     (namestring directory))
                                               arguments)
                                       :output '(:string :stripped t)
                                       :error-output nil
                                       :ignore-error-status t))
                 (declare (ignore error-output))
                 (when (and status (zerop status) (plusp (length output)))
                   output))))
        (let ((revision (git "rev-parse" "--short" "HEAD")))
          (when revision
            (if (git "status" "--porcelain")
                (format nil "~A with local changes" revision)
                revision)))))))

(defun orfeus-build-commit ()
  "Return the revision Orfeus was built from, or NIL when it is unknown.

Resolved once, on first use rather than at load time, so a session that never
asks never pays for the subprocess."
  (when (eq *build-commit* :unresolved)
    (setf *build-commit* (ignore-errors (read-build-commit))))
  *build-commit*)

(defun orfeus-build-description ()
  "Return a one-line version and revision string for the About box and the CLI."
  (let ((commit (orfeus-build-commit)))
    (if commit
        (format nil "~A (~A)" (orfeus-version) commit)
        (orfeus-version))))
