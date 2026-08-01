(asdf:defsystem #:orfeus
  :description "Fast Olympus RAW processing core."
  :author "Lucius"
  :license "GPL-3.0-or-later"
  :version "0.1.0"
  :depends-on (#:cffi
               #:ironclad
               #:uiop)
  :serial t
  :components ((:file "src/package")
               (:file "src/conditions")
               (:file "src/native")
               (:file "src/project")
               (:file "src/version"))
  :in-order-to ((test-op (test-op "orfeus/tests"))))

(asdf:defsystem #:orfeus/cli
  :description "Command-line frontend for Orfeus."
  :depends-on (#:orfeus)
  :serial t
  :components ((:file "src/cli"))
  :build-operation "program-op"
  :build-pathname "orfeus"
  :entry-point "orfeus:main")

(asdf:defsystem #:orfeus/tests
  :description "Tests for Orfeus."
  :depends-on (#:orfeus/cli)
  :serial t
  :components ((:file "tests/package")
               (:file "tests/suite"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:orfeus/tests '#:run-tests)
               (error "Orfeus tests failed."))))
