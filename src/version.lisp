(in-package #:orfeus)

(defun orfeus-version ()
  "Return the installed Orfeus version string."
  (or (asdf:component-version (asdf:find-system '#:orfeus nil))
      "unknown"))
