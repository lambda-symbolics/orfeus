(asdf:defsystem #:orfeus
  :description "Fast Olympus RAW processing core."
  :author "Lucius"
  :license "GPL-3.0-or-later"
  :version "0.1.0"
  :depends-on (#:cffi
               #:ironclad
               #:sb-posix
               #:uiop)
  :serial t
  :components ((:file "src/package")
               (:file "src/conditions")
               (:file "src/native")
               (:file "src/project")
               (:file "src/render")
               (:file "src/batch")
               (:file "src/version"))
  :in-order-to ((test-op (test-op "orfeus/tests"))))

(asdf:defsystem #:orfeus/gui
  :description "FLTK frontend for Orfeus."
  :depends-on (#:orfeus #:cl-fltk)
  :components ((:module "src/gui"
                :serial t
                :components ((:file "package")
                             (:file "model")
                             (:file "preview")
                             (:file "application")))))

(asdf:defsystem #:orfeus/gui-tests
  :description "Noninteractive model tests for the Orfeus GUI."
  :depends-on (#:orfeus/gui)
  :components ((:module "tests/gui"
                :components ((:file "suite"))))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:orfeus/gui-tests '#:run-tests)
               (error "Orfeus GUI tests failed."))))

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
