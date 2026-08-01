(in-package #:orfeus/tests)

(defun run-tests ()
  "Run the dependency-free Orfeus test suite."
  (let ((failures 0))
    (flet ((check (description predicate)
             (format t "~:[not ok~;ok~] - ~A~%" predicate description)
             (unless predicate
               (incf failures))))
      (check "system exposes a version"
             (and (stringp (orfeus-version))
                  (plusp (length (orfeus-version))))))
    (zerop failures)))
