(defpackage #:orfeus/tests
  (:use #:cl)
  (:import-from #:orfeus
                #:cli-run
                #:invalid-project-data
                #:make-photo-job
                #:make-processing-settings
                #:make-project
                #:orfeus-version
                #:output-file-exists
                #:photo-job-input-path
                #:photo-job-overrides
                #:photo-job-render-output
                #:processing-settings-exposure
                #:processing-settings-grain-amount
                #:processing-settings-with-overrides
                #:project->sexp
                #:project-defaults
                #:project-output-directory
                #:project-photos
                #:project-read
                #:project-write
                #:raw-render-error
                #:render-photo
                #:render-preview
                #:sexp->project)
  (:export #:run-tests))

(in-package #:orfeus/tests)
