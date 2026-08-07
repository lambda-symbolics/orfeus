;;; Ironclad comes from the implementation, not from ASDF.
;;;
;;; The SBCL image this builds against already provides it. Listing it in
;;; :DEPENDS-ON made ASDF compile a second copy and load it over the one in the
;;; image, redefining IRONCLAD:BLOCK-LENGTH and warning on every single build.
;;; REQUIRE uses whatever is already there, and still builds it from ASDF on an
;;; image that does not provide it.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :ironclad))

(asdf:defsystem #:orfeus
  :description "Fast Olympus RAW processing core."
  :author "Lucius"
  :license "COLL-Attribution"
  :version "1.0.0"
  :depends-on (#:cffi #:sb-posix)
  :serial t
  :components ((:file "src/package")
               (:file "src/conditions")
               ;; Data definitions come before anything that serializes or
               ;; copies them, so their accessors inline at those call sites.
               (:file "src/types")
               (:file "src/digest")
               (:file "src/native")
               (:file "src/metadata")
               (:file "src/project")
               (:file "src/graph")
               (:file "src/lens-aliases")
               (:file "src/render")
               (:file "src/batch")
               (:file "src/version"))
  :in-order-to ((test-op (test-op "orfeus/tests")
                         (test-op "orfeus/gui-tests"))))

(asdf:defsystem #:orfeus/gui
  :description "FLTK frontend for Orfeus."
  :depends-on (#:orfeus #:lightfast)
  :components ((:module "src/gui"
                :serial t
                :components ((:file "package")
                             (:file "model")
                             (:file "queue")
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
