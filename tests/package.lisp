(defpackage #:orfeus/tests
  (:use #:cl)
  (:import-from #:orfeus
                #:cli-run
                #:export-settings-jpeg-quality
                #:export-settings-max-height
                #:export-settings-max-width
                #:export-settings-preserve-metadata-p
                #:invalid-project-data
                #:lens-profile-aliases-read
                #:resolve-lens-profile-alias
                #:make-export-settings
                #:make-photo-job
                #:make-processing-preset
                #:make-processing-settings
                #:make-project
                #:orfeus-version
                #:output-file-exists
                #:photo-extract-embedded-preview
                #:photo-job-input-path
                #:photo-job-overrides
                #:photo-job-render-output
                #:processing-preset-name
                #:processing-preset-settings
                #:grade-stage-keys
                #:grade-stages
                #:next-still-preset-name
                #:photo-job-apply-grade
                #:photo-job-disabled-stages
                #:photo-render-settings
                #:processing-preset-source-photo
                #:processing-settings-exposure
                #:processing-settings-grain-amount
                #:processing-settings-lens-correction-strength
                #:processing-settings-lut-path
                #:processing-settings-lut-strength
                #:processing-settings-neural-noise-reduction
                #:processing-settings-noise-reduction
                #:processing-settings-tone-shadows
                #:processing-settings-with-overrides
                #:settings-grade-plist
                #:project->sexp
                #:project-defaults
                #:project-export-settings
                #:project-output-directory
                #:project-photos
                #:project-presets
                #:project-read
                #:project-write
                #:raw-render-error
                #:render-photo
                #:render-preview
                #:sexp->processing-settings
                #:sexp->project)
  (:export #:run-tests))

(in-package #:orfeus/tests)
