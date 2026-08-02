(in-package #:orfeus/gui)

(defparameter *preview-debounce-seconds* 0.15d0
  "Delay used to coalesce interactive control changes.")

(defparameter *gui-preview-max-width* 0
  "Maximum GUI preview width; zero preserves full source resolution.")

(defparameter *gui-preview-max-height* 0
  "Maximum GUI preview height; zero preserves full source resolution.")

(defparameter *gui-draft-preview-size* 2048
  "Bounding size of the fast draft preview rendered before the full one.")

(defparameter *thumbnail-preview-size* 320
  "Maximum width and height for orientation-correct thumbnail renders.")

(defun available-processor-count ()
  "Return the number of logical processors reported by Linux, or one."
  (handler-case
      (with-open-file (stream #P"/proc/cpuinfo" :direction :input)
        (max 1 (loop for line = (read-line stream nil)
                     while line
                     count (uiop:string-prefix-p "processor" line))))
    (error () 1)))

(defparameter *background-preview-workers* (available-processor-count)
  "Concurrent thumbnail and full-project preview workers, one per CPU.")

(defconstant +thumbnail-shift-mask+ 1)
(defconstant +thumbnail-control-mask+ 4)

(defun thumbnail-row-at (event-y scroll row-height)
  "Return the zero-based thumbnail row at local EVENT-Y."
  (floor (+ event-y scroll) row-height))

(defun preview-priority-indices (count selected)
  "Return every photo index ordered from SELECTED outward."
  (when (and (plusp count) (<= 0 selected) (< selected count))
    (stable-sort (loop for index below count collect index)
                 #'< :key (lambda (index) (abs (- index selected))))))

(defun thumbnail-selection-after-click (selected row anchor state)
  "Return the selection and anchor produced by clicking ROW with modifier STATE."
  (cond
    ((logtest +thumbnail-shift-mask+ state)
     (values (loop for index from (min anchor row) to (max anchor row)
                   collect index)
             anchor))
    ((logtest +thumbnail-control-mask+ state)
     (values (if (member row selected)
                 (remove row selected)
                 (cons row selected))
             row))
    (t (values (list row) row))))

(defun fltk-file-filter (label pattern)
  "Build FLTK's LABEL<TAB>PATTERN native file chooser syntax."
  (format nil "~A~C~A" label #\Tab pattern))

(defun display-number (value)
  (cond ((null value) "")
        ((integerp value) (format nil "~D" value))
        ((= value (round value)) (format nil "~D" (round value)))
        (t
         (string-right-trim
          "0"
          (string-right-trim "." (format nil "~,2F" value))))))

(defun parse-number (text &optional allow-empty)
  (if (and allow-empty (string= text ""))
      nil
      (let ((*read-eval* nil))
        (multiple-value-bind (value end) (read-from-string text nil nil)
          (unless (and (realp value) (= end (length text)))
            (error "Expected a number, got ~S." text))
          value))))

(defun make-gui-preview-directory ()
  "Create and return a private temporary directory for one GUI run."
  (uiop:ensure-directory-pathname
   (sb-posix:mkdtemp
    (namestring (merge-pathnames "orfeus-gui-XXXXXX"
                                 (uiop:temporary-directory))))))

(defun delete-gui-preview-directory (directory)
  "Delete the private preview DIRECTORY and all files created in it."
  (when directory
    (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore)))

(defun preview-settings-key (settings)
  "Return a content hash covering every processing setting for preview caching."
  (let* ((*print-readably* t)
         (text (prin1-to-string
                (orfeus::processing-settings->sexp settings)))
         (digest (ironclad:digest-sequence
                  :sha256
                  (sb-ext:string-to-octets text :external-format :utf-8))))
    (subseq (ironclad:byte-array-to-hex-string digest) 0 16)))

(defun preview-pathname (preview-directory index job role settings)
  (merge-pathnames
   (make-pathname :name (format nil "~(~A~)-~D-~A-~A"
                                role index (preview-settings-key settings)
                                (pathname-name (photo-job-input-path job)))
                  :type "jpg")
   preview-directory))

(defun thumbnail-pathname (preview-directory index job)
  (merge-pathnames
   (make-pathname :name (format nil "thumbnail-~D-~A"
                                index (pathname-name (photo-job-input-path job)))
                  :type "jpg")
   preview-directory))

(defun preview-status-text (model)
  (let ((temperature (gui-model-setting model :white-balance-temperature))
        (noise-reduction (gui-model-setting model :noise-reduction))
        (lut (gui-model-setting model :lut-path))
        (strength (gui-model-setting model :lut-strength)))
    (format nil "RAW preview  |  WB: ~A  |  NR: ~D%  |  LUT: ~A"
            (if temperature "Custom" "As shot")
            (round (* 100 noise-reduction))
            (if (and lut (plusp strength))
                (format nil "~A (~D%)" (file-namestring lut)
                        (round (* 100 strength)))
                "Off"))))

(defun initial-gui-project (project-or-path)
  (etypecase project-or-path
    (null (values (gui-empty-project) nil))
    (project (values project-or-path nil))
    ((or pathname string)
     (let ((path (pathname project-or-path)))
       (ecase (gui-open-kind path)
         (:project (values (project-read path) path))
         (:photo (values (gui-photo-project path) nil)))))))

(defun run-gui (&optional project-or-path)
  "Open Orfeus. PROJECT-OR-PATH may be NIL, a PROJECT, project file, ORF, or DNG."
  (multiple-value-bind (initial-project initial-path)
      (initial-gui-project project-or-path)
    ;; Load the CFFI bridge before render workers can race to initialize it.
    (orfeus:native-bridge-version)
    (let* ((project initial-project)
           (model (make-gui-model :project project
                                  :project-path initial-path))
           (queue (make-gui-queue :name "Orfeus foreground render worker"))
           (background-queue
             (make-gui-queue :workers *background-preview-workers*
                             :name "Orfeus background render worker"))
           (preview-directory (make-gui-preview-directory))
           (picker-directory
             (let ((photo (first (project-photos project))))
               (cond (initial-path
                      (uiop:pathname-directory-pathname initial-path))
                     (photo
                      (uiop:pathname-directory-pathname
                       (photo-job-input-path photo)))
                     (t (uiop:getcwd)))))
           (lens-cache (make-hash-table :test #'eq))
           (capture-cache (make-hash-table :test #'eq))
           (thumbnail-files (make-hash-table :test #'eq))
           (lut-paths (make-hash-table :test #'equal))
           window menu toolbar toolbar-bottom-rule main-tile left-pane center-pane
           thumbnail-canvas before-canvas after-canvas before-caption after-caption
           inspector tabs basic-page optics-page effects-page export-page presets-page
           status progress before-preview-file after-preview-file
           lens-name controls inspector-items tone-items lut-choice wb-choice target-choice
           export-quality export-max-width export-max-height export-metadata
           preset-browser preset-name-input preset-apply-button
           debounce-id poll-id comparison-p layout-initialized-p
           (thumbnail-scroll 0)
           (thumbnail-anchor 0)
           (preview-generation 0)
           (preview-zoom 1d0)
           (preview-center-x .5d0)
           (preview-center-y .5d0)
           preview-drag-p preview-drag-x preview-drag-y
           preview-drag-center-x preview-drag-center-y)
      (labels
          ((picker-preset ()
             (namestring picker-directory))
           (remember-picked-path (path)
             (when path
               (setf picker-directory
                     (uiop:pathname-directory-pathname (pathname path))))
             path)
           (selected-lens-description ()
             (let ((job (selected-job)))
               (if job
                   (multiple-value-bind (cached present-p)
                       (gethash job lens-cache)
                     (if present-p
                         cached
                         (setf (gethash job lens-cache)
                               (or (ignore-errors
                                     (photo-lens-description
                                      (photo-job-input-path job)))
                                   "Lens not identified"))))
                   "No photograph selected")))
           (selected-capture-description ()
             (let ((job (selected-job)))
               (when job
                 (multiple-value-bind (cached present-p)
                     (gethash job capture-cache)
                   (if present-p
                       cached
                       (setf (gethash job capture-cache)
                             (ignore-errors
                               (photo-capture-description
                                (photo-job-input-path job)))))))))
           (parse-preview-event (value)
             (let ((parts (remove "" (uiop:split-string (or value "")
                                                       :separator '(#\Space))
                                  :test #'string=)))
               (when (member (length parts) '(5 6))
                 (handler-case
                     (let ((values (mapcar #'parse-integer parts)))
                       (values-list (if (= (length values) 5)
                                        (append values '(0))
                                        values)))
                   (error () nil)))))
           (preview-path-for-canvas (canvas)
             (if (eq canvas before-canvas)
                 before-preview-file
                 after-preview-file))
           (redraw-previews ()
             (when before-canvas (cl-fltk:redraw before-canvas))
             (when after-canvas (cl-fltk:redraw after-canvas)))
           (reset-preview-view ()
             (setf preview-zoom 1d0
                   preview-center-x .5d0
                   preview-center-y .5d0)
             (redraw-previews)
             (set-status "Preview fitted to window"))
           (preview-scaled-size (canvas path zoom)
             (multiple-value-bind (source-width source-height)
                 (preview-file-size path)
               (when source-width
                 (let ((fit (min (/ (cl-fltk:widget-width canvas)
                                    (float source-width 1d0))
                                 (/ (cl-fltk:widget-height canvas)
                                    (float source-height 1d0)))))
                   (let ((scale (min (max fit 2d0) (* fit zoom))))
                     (values (* source-width scale)
                             (* source-height scale)
                             fit))))))
           (preview-zoom-limit (canvas path)
             (multiple-value-bind (scaled-width scaled-height fit)
                 (preview-scaled-size canvas path 1d0)
               (declare (ignore scaled-width scaled-height))
               (if fit (/ (max fit 2d0) fit) 32d0)))
           (zoom-preview (factor &optional canvas pointer-x pointer-y)
             (let* ((target-canvas (or canvas after-canvas before-canvas))
                    (path (and target-canvas
                               (preview-path-for-canvas target-canvas)))
                    (old-zoom preview-zoom)
                    (new-zoom (min (if path
                                       (preview-zoom-limit target-canvas path)
                                       32d0)
                                   (max 1d0 (* old-zoom factor)))))
               (when (and path canvas pointer-x pointer-y (/= old-zoom new-zoom))
                 (multiple-value-bind (old-width old-height)
                     (preview-scaled-size canvas path old-zoom)
                   (when old-width
                     (let* ((canvas-center-x (/ (cl-fltk:widget-width canvas) 2d0))
                            (canvas-center-y (/ (cl-fltk:widget-height canvas) 2d0))
                            (source-x (+ preview-center-x
                                         (/ (- pointer-x canvas-center-x) old-width)))
                            (source-y (+ preview-center-y
                                         (/ (- pointer-y canvas-center-y) old-height)))
                            (ratio (/ old-zoom new-zoom)))
                       (setf preview-center-x
                             (- source-x (* (- source-x preview-center-x) ratio))
                             preview-center-y
                             (- source-y (* (- source-y preview-center-y) ratio)))))))
               (setf preview-zoom new-zoom)
               (redraw-previews)
               (set-status (format nil "Preview zoom: ~,0F%" (* 100 new-zoom)))))
           (preview-one-to-one ()
             (let ((path (or after-preview-file before-preview-file)))
               (when path
                 (multiple-value-bind (scaled-width scaled-height fit)
                     (preview-scaled-size after-canvas path 1d0)
                   (declare (ignore scaled-width scaled-height))
                   (when fit
                     (setf preview-zoom (min 32d0 (max 1d0 (/ fit)))
                           preview-center-x .5d0
                           preview-center-y .5d0)
                     (redraw-previews)
                     (set-status "Preview at 1:1 pixels"))))))
           (handle-preview-mouse (canvas event value)
             (multiple-value-bind (x y button dx dy state)
                 (parse-preview-event value)
               (declare (ignore dx state))
               (when x
                 (case event
                   (#.cl-fltk:+event-push+
                    (when (or (= button 1) (= button 2))
                      (setf preview-drag-p t
                            preview-drag-x x
                            preview-drag-y y
                            preview-drag-center-x preview-center-x
                            preview-drag-center-y preview-center-y)))
                   (#.cl-fltk:+event-drag+
                    (when preview-drag-p
                      (let ((path (preview-path-for-canvas canvas)))
                        (when path
                          (multiple-value-bind (width height)
                              (preview-scaled-size canvas path preview-zoom)
                            (when width
                              (setf preview-center-x
                                    (- preview-drag-center-x
                                       (/ (- x preview-drag-x) width))
                                    preview-center-y
                                    (- preview-drag-center-y
                                       (/ (- y preview-drag-y) height)))
                              (redraw-previews)))))))
                   (#.cl-fltk:+event-release+
                    (setf preview-drag-p nil))
                   (#.cl-fltk:+event-wheel+
                    (zoom-preview (if (minusp dy) 1.25d0 .8d0)
                                  canvas x y))))))
           (set-status (text)
             (setf (cl-fltk:value status) text))
           (selected-job ()
             (gui-model-selected-job model))
           (thumbnail-row-height () 104)
           (thumbnail-scroll-limit ()
             (if thumbnail-canvas
                 (max 0 (- (* (length (project-photos project))
                              (thumbnail-row-height))
                           (cl-fltk:widget-height thumbnail-canvas)))
                 0))
           (clamp-thumbnail-scroll ()
             (setf thumbnail-scroll
                   (min (thumbnail-scroll-limit) (max 0 thumbnail-scroll))))
           (redraw-thumbnails ()
             (when thumbnail-canvas
               (clamp-thumbnail-scroll)
               (cl-fltk:redraw thumbnail-canvas)))
           (select-thumbnail-row (row state)
             (when (and (>= row 0) (< row (length (project-photos project))))
               (multiple-value-bind (selection anchor)
                   (thumbnail-selection-after-click
                    (gui-model-selected-indices model) row thumbnail-anchor state)
                 (setf thumbnail-anchor anchor)
                 (gui-model-set-selected-indices model selection)
                 (when selection
                   (setf (gui-model-selected-index model) row))
                 (clear-previews)
                 (sync-controls)
                 (when selection (schedule-initial-preview))
                 (redraw-thumbnails))))
           (handle-thumbnail-mouse (canvas event value)
             (declare (ignore canvas))
             (multiple-value-bind (x y button dx dy state)
                 (parse-preview-event value)
               (declare (ignore x button dx))
               (when y
                 (case event
                   (#.cl-fltk:+event-push+
                    (select-thumbnail-row
                     (thumbnail-row-at y thumbnail-scroll
                                       (thumbnail-row-height))
                     state))
                   (#.cl-fltk:+event-wheel+
                    (incf thumbnail-scroll (* dy 36))
                    (redraw-thumbnails))))))
           (clear-previews ()
             (setf preview-zoom 1d0
                   preview-center-x .5d0
                   preview-center-y .5d0
                   preview-drag-p nil)
             (dolist (path (list before-preview-file after-preview-file))
               (when path (forget-preview-file path)))
             (setf before-preview-file nil
                   after-preview-file nil)
             (when before-canvas (cl-fltk:redraw before-canvas))
             (when after-canvas (cl-fltk:redraw after-canvas)))
           (publish-preview (role path)
             (let ((old-path (ecase role
                               (:before before-preview-file)
                               (:after after-preview-file))))
               (when (and old-path (not (equal old-path path)))
                 (forget-preview-file old-path))
               (forget-preview-file path)
               (ecase role
                 (:before (setf before-preview-file path)
                          (cl-fltk:redraw before-canvas))
                 (:after (setf after-preview-file path
                               (gethash (selected-job) thumbnail-files) path)
                         (cl-fltk:redraw after-canvas)
                         (redraw-thumbnails)))))
           (sync-export-controls ()
             (when export-quality
               (let ((settings (project-export-settings project)))
                 (setf (cl-fltk:value export-quality)
                       (format nil "~D" (export-settings-jpeg-quality settings))
                       (cl-fltk:value export-max-width)
                       (format nil "~D" (or (export-settings-max-width settings) 0))
                       (cl-fltk:value export-max-height)
                       (format nil "~D" (or (export-settings-max-height settings) 0))
                       (cl-fltk:value export-metadata)
                       (if (export-settings-preserve-metadata-p settings) "1" "0")))))
           (export-setting-changed (key widget)
             (handler-case
                 (let ((settings (project-export-settings project)))
                   (ecase key
                     (:jpeg-quality
                      (let ((value (round (parse-number (cl-fltk:value widget)))))
                        (unless (<= 1 value 100)
                          (error "JPEG quality must be from 1 to 100."))
                        (setf (export-settings-jpeg-quality settings) value)))
                     (:max-width
                      (let ((value (round (parse-number (cl-fltk:value widget)))))
                        (when (minusp value) (error "Maximum width cannot be negative."))
                        (setf (export-settings-max-width settings)
                              (unless (zerop value) value))))
                     (:max-height
                      (let ((value (round (parse-number (cl-fltk:value widget)))))
                        (when (minusp value) (error "Maximum height cannot be negative."))
                        (setf (export-settings-max-height settings)
                              (unless (zerop value) value))))
                     (:preserve-metadata-p
                      (setf (export-settings-preserve-metadata-p settings)
                            (gui-boolean-value (cl-fltk:value widget)))))
                   (sync-export-controls)
                   (set-status "Export settings updated"))
               (error (condition)
                 (sync-export-controls)
                 (set-status (princ-to-string condition)))))
           (lut-choice-name (path)
             (when path
               (or (loop for name being the hash-keys of lut-paths
                           using (hash-value mapped-path)
                         when (string= mapped-path (namestring path))
                           return name)
                   (let ((name (namestring path)))
                     (setf (gethash name lut-paths) name)
                     (when lut-choice (cl-fltk:add-item lut-choice name))
                     name))))
           (selected-photo-count ()
             (length (or (gui-model-selected-indices model)
                         (and (selected-job) '(0)))))
           (refresh-preset-browser ()
             (when preset-browser
               (cl-fltk:clear preset-browser)
               (dolist (preset (project-presets project))
                 (cl-fltk:add-item preset-browser
                                   (processing-preset-name preset)))))
           (sync-preset-action-label ()
             (when preset-apply-button
               (setf (cl-fltk:label preset-apply-button)
                     (format nil "Apply to ~D photo~:P"
                             (selected-photo-count)))))
           (save-current-preset ()
             (handler-case
                 (let ((preset (gui-model-save-preset
                                model (cl-fltk:value preset-name-input))))
                   (refresh-preset-browser)
                   (setf (cl-fltk:value preset-name-input)
                         (processing-preset-name preset))
                   (set-status (format nil "Saved preset ~A"
                                       (processing-preset-name preset))))
               (error (condition)
                 (set-status (princ-to-string condition)))))
           (apply-current-preset ()
             (handler-case
                 (let* ((name (cl-fltk:value preset-name-input))
                        (count (gui-model-apply-preset model name)))
                   (sync-controls)
                   (schedule-edited-preview)
                   (set-status (format nil "Applied ~A to ~D photo~:P"
                                       name count)))
               (error (condition)
                 (set-status (princ-to-string condition)))))
           (sync-controls ()
             (dolist (entry controls)
               (let ((key (first entry)) (widget (second entry)))
                 (setf (cl-fltk:value widget)
                       (case key
                         ((:lens-correction-p :chromatic-aberration-correction-p)
                          (if (gui-model-setting model key) "1" "0"))
                         (otherwise (display-number (gui-model-setting model key)))))))
             (setf (cl-fltk:value wb-choice)
                   (if (gui-model-setting model :white-balance-temperature)
                       "Custom" "As shot")
                   (cl-fltk:value target-choice)
                   (if (eq (gui-model-edit-target model) :defaults)
                       "Defaults" "Photo"))
             (when lut-choice
               (let ((path (gui-model-setting model :lut-path)))
                 (setf (cl-fltk:value lut-choice)
                       (if path (lut-choice-name path) "None"))))
             (setf (cl-fltk:label lens-name)
                   (let ((capture (selected-capture-description)))
                     (if capture
                         (format nil "Lens: ~A   |   ~A"
                                 (selected-lens-description) capture)
                         (format nil "Lens: ~A" (selected-lens-description)))))
             (sync-export-controls)
             (sync-preset-action-label))
           (replace-project (new-project &optional path)
             (incf preview-generation)
             (clear-previews)
             (setf project new-project
                   thumbnail-scroll 0
                   thumbnail-anchor 0)
             (clrhash thumbnail-files)
             (gui-model-replace-project model new-project path)
             (refresh-preset-browser)
             (sync-controls)
             (if (selected-job)
                 (schedule-initial-preview)
                 (set-status "Open a photograph or project to begin")))
           (choose-photos (title)
             (choose-photo-files
              :title title
              :filter (fltk-file-filter
                       "RAW photographs" "*.{orf,ORF,dng,DNG}")
              :preset-path (picker-preset)))
           (open-photo ()
             (let ((paths (choose-photos "Open RAW photographs")))
               (when paths
                 (remember-picked-path (first paths))
                 (replace-project (gui-photos-project paths)))))
           (add-photos ()
             (let ((paths (choose-photos "Add RAW photographs to project")))
               (when paths
                 (remember-picked-path (first paths))
                 (multiple-value-bind (count first-index)
                     (gui-model-add-photos model paths)
                   (if (plusp count)
                       (progn
                         (gui-model-set-selected-indices model (list first-index))
                         (incf preview-generation)
                         (clear-previews)
                         (sync-controls)
                         (schedule-initial-preview)
                         (set-status (format nil "Added ~D photograph~:P" count)))
                       (set-status "All selected photographs are already in the project"))))))
           (remove-selected-photo ()
             (let ((removed (gui-model-remove-selected model)))
               (when removed
                 (incf preview-generation)
                 (dolist (job removed)
                   (remhash job lens-cache)
                   (remhash job capture-cache)
                   (remhash job thumbnail-files))
                 (clear-previews)
                 (sync-controls)
                 (if (selected-job)
                     (progn
                       (schedule-initial-preview)
                       (set-status (format nil "Removed ~D photograph~:P"
                                           (length removed))))
                     (set-status "Project contains no photographs")))))
           (open-project ()
             (let ((path (cl-fltk:choose-file
                          :title "Open Orfeus project"
                          :filter (fltk-file-filter "Orfeus project" "*.sexp")
                          :preset-file (picker-preset))))
               (when path
                 (remember-picked-path path)
                 (replace-project (project-read path) (pathname path)))))
           (save-project (&optional choose-p)
             (let ((path (or (and (not choose-p) (gui-model-project-path model))
                             (cl-fltk:choose-save-file
                              :title "Save Orfeus project"
                              :filter (fltk-file-filter "Orfeus project" "*.sexp")
                              :preset-file
                              (namestring
                               (merge-pathnames "project.sexp"
                                                picker-directory))))))
               (when path
                 (remember-picked-path path)
                 (project-write project path)
                 (setf (gui-model-project-path model) (pathname path))
                 (set-status "Project saved"))))
           (neutral-preview-settings ()
             (make-processing-settings
              :noise-reduction 0.0
              :lens-correction-p nil
              :chromatic-aberration-correction-p nil
              :lut-path nil
              :grain-amount 0.0))
           (settings-for-job (job)
             (processing-settings-with-overrides
              (project-defaults project)
              (photo-job-overrides job)))
           (current-settings ()
             (settings-for-job (selected-job)))
           (enqueue-render (target-queue role job index settings generation publish-p
                            &key front-p draft-p cache-p)
             (let* ((input (photo-job-input-path job))
                    (file-role (if draft-p :draft role))
                    (output (preview-pathname preview-directory index job
                                              file-role settings))
                    (max-width (if draft-p
                                   *gui-draft-preview-size*
                                   *gui-preview-max-width*))
                    (max-height (if draft-p
                                    *gui-draft-preview-size*
                                    *gui-preview-max-height*)))
               (if (probe-file output)
                   (when publish-p
                     (queue-event queue
                                  (list :preview generation index job role output)))
                   (enqueue-gui-task
                    target-queue role
                    (lambda ()
                      (when (or (not publish-p)
                                (= generation preview-generation))
                        (render-preview input output settings
                                        :max-width max-width
                                        :max-height max-height
                                        :cache-p cache-p
                                        :if-exists :supersede)
                        (when (and publish-p
                                   (= generation preview-generation))
                          (queue-event queue
                                       (list :preview generation index job role
                                             output)))))
                    :front-p front-p))))
           (enqueue-thumbnail (job index generation)
             (let ((output (thumbnail-pathname preview-directory index job)))
               (if (probe-file output)
                   (queue-event queue (list :thumbnail generation job output))
                   (enqueue-gui-task
                    background-queue :thumbnail
                    (lambda ()
                      (handler-case
                          (progn
                            (render-preview
                              (photo-job-input-path job) output
                              (neutral-preview-settings)
                              :max-width *thumbnail-preview-size*
                              :max-height *thumbnail-preview-size*
                              :jpeg-quality 82
                              :if-exists :supersede)
                            (queue-event queue
                                         (list :thumbnail generation job output)))
                        (error () nil)))))))
           (enqueue-background-previews (selected-before-p generation)
             (discard-gui-tasks background-queue :before)
             (discard-gui-tasks background-queue :after)
             (let* ((photos (project-photos project))
                    (selected-index (gui-model-selected-index model))
                    (selected (selected-job))
                    (indices (preview-priority-indices
                              (length photos) selected-index)))
               (when (and selected selected-before-p)
                 (enqueue-render background-queue :before selected
                                 selected-index
                                 (neutral-preview-settings) generation t
                                 :front-p t :cache-p t))
               (when selected-before-p
                 (dolist (index indices)
                   (enqueue-thumbnail (nth index photos) index generation)))
               (dolist (index indices)
                 (unless (= index selected-index)
                   (let ((job (nth index photos)))
                     (enqueue-render background-queue :after job index
                                     (settings-for-job job) nil nil))))))
           (enqueue-preview (initial-p)
             (let ((job (selected-job))
                   (index (gui-model-selected-index model))
                   (generation preview-generation))
               (when job
                 ;; A bounded draft lands quickly while the full-resolution
                 ;; preview renders behind it; both reuse the decoded RAW.
                 (let ((settings (current-settings)))
                   (unless (probe-file (preview-pathname preview-directory index
                                                         job :after settings))
                     (enqueue-render queue :after job index settings generation t
                                     :front-p t :draft-p t :cache-p t))
                   (enqueue-render queue :after job index settings generation t
                                   :cache-p t))
                 (enqueue-background-previews initial-p generation))))
           (schedule-initial-preview ()
             (when debounce-id
               (ignore-errors (cl-fltk:remove-timeout debounce-id))
               (setf debounce-id nil))
             (incf preview-generation)
             (discard-gui-tasks queue :before)
             (discard-gui-tasks queue :after)
             (enqueue-preview t))
           (schedule-edited-preview ()
             (incf preview-generation)
             (when debounce-id
               (ignore-errors (cl-fltk:remove-timeout debounce-id)))
             (setf debounce-id
                   (cl-fltk:add-timeout
                    *preview-debounce-seconds*
                    (lambda ()
                      (setf debounce-id nil)
                      (discard-gui-tasks queue :after)
                      (enqueue-preview nil)))))
           (setting-changed (key widget &optional allow-empty)
             (handler-case
                 (let ((new-value (parse-number (cl-fltk:value widget)
                                                allow-empty)))
                   (when (and (eq key :white-balance-tint)
                              (null (gui-model-setting
                                     model :white-balance-temperature)))
                     (gui-model-set-setting model
                                            :white-balance-temperature 5500.0))
                   (gui-model-set-setting model key new-value)
                   (sync-controls)
                   (when (member key '(:white-balance-temperature
                                       :white-balance-tint))
                     (setf (cl-fltk:value wb-choice) "Custom"))
                   (schedule-edited-preview))
               (error (condition) (set-status (princ-to-string condition)))))
           (render-selected ()
             (let ((job (selected-job)))
               (when job
                 (let ((output (gui-photo-output-path model job)))
                   (ensure-directories-exist output)
                   (enqueue-gui-task
                    queue :export
                    (lambda ()
                      (queue-event queue (list :status nil "Exporting current photo..."))
                      (handler-case
                          (progn
                            (render-photo-job project job :if-exists :supersede)
                            (queue-event queue (list :done output)))
                        (error (condition)
                          (queue-event queue
                                       (list :status nil
                                             (format nil "Export failed: ~A"
                                                     condition)))))))))))
           (render-all ()
             (when (project-photos project)
               (ensure-directories-exist
                (merge-pathnames "placeholder" (project-output-directory project)))
               (enqueue-gui-task
                queue :export
                (lambda ()
                  (queue-event queue (list :status nil "Exporting all photos..."))
                  (multiple-value-bind (completed failures)
                      (project-render project :if-exists :supersede :on-error :continue)
                    (if failures
                        (queue-event queue
                                     (list :error
                                           (format nil "Exported ~D; ~D failed"
                                                   (length completed)
                                                   (length failures))))
                        (queue-event queue
                                     (list :done
                                           (format nil "~D photos"
                                                   (length completed))))))))))
           (choose-lut ()
             (let ((path (cl-fltk:choose-file
                          :title "Choose 3D LUT"
                          :filter (fltk-file-filter "Cube LUT" "*.{cube,CUBE}")
                          :preset-file (picker-preset))))
               (when path
                 (remember-picked-path path)
                 (gui-model-set-setting model :lut-path path)
                 (sync-controls)
                 (schedule-edited-preview))))
           (clear-lut ()
             (gui-model-set-setting model :lut-path nil)
             (sync-controls)
             (schedule-edited-preview))
           (set-wb-mode (widget)
             (if (string-equal (cl-fltk:value widget) "As shot")
                 (progn
                   (gui-model-set-setting model :white-balance-temperature nil)
                   (gui-model-set-setting model :white-balance-tint 0.0))
                 (unless (gui-model-setting model :white-balance-temperature)
                   (gui-model-set-setting model :white-balance-temperature 5500.0)))
             (sync-controls)
             (schedule-edited-preview))
           (toggle-comparison ()
             (setf comparison-p (not comparison-p))
             (layout-ui)
             (set-status (if comparison-p
                             "Before and After comparison shown"
                             "Before and After comparison hidden")))
           (register-inspector (widget x y width-mode height
                                &optional (basis :root))
             (push (list widget x y width-mode height basis) inspector-items)
             widget)
           (register-field (field y &optional (basis :root))
             (register-inspector (cl-fltk:field-label field) 8 y 96 26 basis)
             (register-inspector (cl-fltk:field-control field) 110 y
                                 :control 26 basis)
             field)
           (make-number-field (key label minimum maximum step y parent)
             (let* ((label-widget
                      (cl-fltk:make-label :parent parent :x 8 :y y
                                          :width 96 :height 26 :label label))
                    (callback (lambda (widget event value)
                                (declare (ignore event value))
                                (setting-changed key widget)))
                    (spinner (cl-fltk:make-spinner
                              :parent parent :x 202 :y y
                              :width 78 :height 26 :callback callback))
                    (slider (unless (member key '(:white-balance-temperature
                                                  :grain-size))
                              (cl-fltk:make-slider
                               :parent parent :x 110 :y y
                               :width 84 :height 26 :callback callback))))
               (dolist (widget (remove nil (list slider spinner)))
                 (cl-fltk:set-range widget minimum maximum)
                 (cl-fltk:set-step widget step)
                 (push (list key widget) controls))
               (register-inspector label-widget 8 y 96 26 :page)
               (if slider
                   (progn
                     (register-inspector slider 110 y :slider 26 :page)
                     (register-inspector spinner 202 y :number 26 :page))
                   (register-inspector spinner 110 y :control 26 :page))
               spinner))
           (make-tone-band (key short-label full-label index)
             (let* ((callback (lambda (widget event value)
                                (declare (ignore event value))
                                (setting-changed key widget)))
                    (label (cl-fltk:make-label
                            :parent basic-page :x 0 :y 172 :width 32 :height 24
                            :label short-label))
                    (slider (cl-fltk:make-vertical-slider
                             :parent basic-page :x 0 :y 198 :width 20 :height 132
                             :callback callback))
                    (input (cl-fltk:make-value-input
                            :parent basic-page :x 0 :y 334 :width 32 :height 24
                            :callback callback)))
               (dolist (widget (list slider input))
                 (cl-fltk:set-range widget -2 2)
                 (cl-fltk:set-step widget 0.1)
                 (cl-fltk:set-tooltip widget full-label)
                 (push (list key widget) controls))
               (push (list label index :label) tone-items)
               (push (list slider index :slider) tone-items)
               (push (list input index :input) tone-items)))
           (layout-left-pane (&optional ignored)
             (declare (ignore ignored))
             (cl-fltk:resize-widget
              thumbnail-canvas :x 0 :y 0
              :width (cl-fltk:widget-width left-pane)
              :height (cl-fltk:widget-height left-pane))
             (redraw-thumbnails)
             (cl-fltk:redraw left-pane))
           (layout-center-pane (&optional ignored)
             (declare (ignore ignored))
             (let* ((center (cl-fltk:widget-width center-pane))
                    (main-height (cl-fltk:widget-height center-pane))
                    (caption-height 22)
                    (viewer-height (max 100 (- main-height caption-height)))
                    (gutter 6)
                    (pane-width (floor (- center gutter) 2)))
               (if comparison-p
                   (progn
                     (cl-fltk:show before-caption)
                     (cl-fltk:show before-canvas)
                     (cl-fltk:resize-widget before-caption :x 0 :y 0
                                            :width pane-width :height caption-height)
                     (cl-fltk:resize-widget before-canvas :x 0 :y caption-height
                                            :width pane-width :height viewer-height)
                     (cl-fltk:resize-widget after-caption
                                            :x (+ pane-width gutter) :y 0
                                            :width (- center pane-width gutter)
                                            :height caption-height)
                     (cl-fltk:resize-widget after-canvas
                                            :x (+ pane-width gutter) :y caption-height
                                            :width (- center pane-width gutter)
                                            :height viewer-height))
                   (progn
                     (cl-fltk:hide before-caption)
                     (cl-fltk:hide before-canvas)
                     (cl-fltk:resize-widget after-caption :x 0 :y 0
                                            :width center :height caption-height)
                     (cl-fltk:resize-widget after-canvas :x 0 :y caption-height
                                            :width center :height viewer-height)))
               (cl-fltk:redraw center-pane)))
           (layout-inspector-pane (&optional ignored)
             (declare (ignore ignored))
             (let ((right (cl-fltk:widget-width inspector))
                   (main-height (cl-fltk:widget-height inspector)))
               (cl-fltk:resize-widget tabs :x 4 :y 40
                                      :width (- right 8)
                                      :height (- main-height 80))
               (dolist (page (list basic-page optics-page effects-page
                                    export-page presets-page))
                 (cl-fltk:resize-widget page :x 2 :y 24
                                        :width (- right 12)
                                        :height (- main-height 108)))
               (dolist (item inspector-items)
                 (destructuring-bind
                     (widget x y width-mode item-height basis) item
                   (let* ((basis-width (if (eq basis :page)
                                           (- right 12)
                                           right))
                          (item-width
                            (case width-mode
                              (:scope-control (max 100 (- right 98)))
                              (:control (max 100 (- basis-width 118)))
                              (:slider (max 64 (- basis-width 204)))
                              (:number 78)
                              (:fill (max 100 (- basis-width 16)))
                              (:half-left (max 70 (floor (- right 30) 2)))
                              (:half-right (max 70 (floor (- right 30) 2)))
                              (otherwise width-mode)))
                          (item-x
                            (case width-mode
                              (:number (- basis-width 86))
                              (:half-right (+ 18 (floor (- right 30) 2)))
                              (otherwise x)))
                          (item-y (if (eq y :action-row)
                                      (- main-height 34)
                                      y)))
                     (cl-fltk:resize-widget
                      widget :x item-x :y item-y
                      :width item-width :height item-height))))
               (let* ((page-width (- right 12))
                      (column-width (max 34 (floor (- page-width 16) 7))))
                 (dolist (item tone-items)
                   (destructuring-bind (widget index role) item
                     (let* ((column-x (+ 8 (* index column-width)))
                            (item-width (ecase role
                                          (:label column-width)
                                          (:slider 20)
                                          (:input (max 30 (- column-width 4)))))
                            (item-x (ecase role
                                      (:label column-x)
                                      (:slider (+ column-x (floor (- column-width 20) 2)))
                                      (:input (+ column-x 2))))
                            (item-y (ecase role (:label 172) (:slider 198) (:input 334)))
                            (item-height (ecase role (:label 24) (:slider 132) (:input 24))))
                       (cl-fltk:resize-widget widget :x item-x :y item-y
                                              :width item-width :height item-height)))))
               (cl-fltk:redraw inspector)))
           (layout-ui (&optional ignored)
             (declare (ignore ignored))
             (let* ((width (cl-fltk:widget-width window))
                    (height (cl-fltk:widget-height window))
                    (top 64) (bottom 28)
                    (main-height (max 200 (- height top bottom)))
                    (left (min (max 180 (- width 580))
                               420
                               (max 180
                                    (if layout-initialized-p
                                        (cl-fltk:widget-width left-pane)
                                        (floor width 5)))))
                    (right (min (max 280 (- width left 300))
                                480
                                (max 280
                                     (if layout-initialized-p
                                         (cl-fltk:widget-width inspector)
                                         (floor width 3)))))
                    (center (- width left right)))
               (cl-fltk:resize-widget menu :x 0 :y 0 :width width :height 24)
               (cl-fltk:resize-widget toolbar :x 0 :y 24 :width width :height 40)
               (when toolbar-bottom-rule
                 (cl-fltk:resize-widget toolbar-bottom-rule :x 0 :y 38
                                        :width width :height 2))
               (when lens-name
                 (cl-fltk:resize-widget lens-name :x 342 :y 6
                                        :width (max 120 (- width 352)) :height 28))
               (cl-fltk:resize-widget main-tile :x 0 :y top
                                      :width width :height main-height)
               (cl-fltk:resize-widget left-pane :x 0 :y 0
                                      :width left :height main-height)
               (cl-fltk:resize-widget center-pane :x left :y 0
                                      :width center :height main-height)
               (cl-fltk:resize-widget inspector :x (+ left center) :y 0
                                      :width right :height main-height)
               (cl-fltk:resize-widget progress :x 0 :y (+ top main-height)
                                      :width 180 :height bottom)
               (cl-fltk:resize-widget status :x 180 :y (+ top main-height)
                                      :width (- width 180) :height bottom)
               (layout-left-pane)
               (layout-center-pane)
               (layout-inspector-pane)
               (unless layout-initialized-p
                 (cl-fltk:init-sizes main-tile))
               (setf layout-initialized-p t)))
           (poll ()
             (dolist (event (drain-events queue))
               (case (first event)
                 (:status
                  (when (or (null (second event))
                            (= (second event) preview-generation))
                    (set-status (third event))))
                 (:preview
                  (when (gui-preview-event-current-p model event preview-generation)
                    (publish-preview (fifth event) (sixth event))
                    (set-status (preview-status-text model))))
                 (:thumbnail
                  (when (and (member (third event) (project-photos project) :test #'eq)
                             (null (gethash (third event) thumbnail-files)))
                    (setf (gethash (third event) thumbnail-files) (fourth event))
                    (redraw-thumbnails)))
                 (:done
                  (set-status (format nil "Exported ~A" (second event))))
                 (:error
                  (set-status (format nil "Error: ~A" (second event))))))
             (setf (cl-fltk:value progress)
                   (if (gui-queue-busy-p queue) "55" "0"))))
        (setf window (cl-fltk:make-window :width 1280 :height 800
                                          :label "Orfeus"
                                          :app-id "org.orfeus.Orfeus"))
        (cl-fltk:apply-classic-theme)
        (cl-fltk:set-size-range window :min-width 960 :min-height 700)
        (setf menu (cl-fltk:make-menu-bar :parent window :x 0 :y 0
                                          :width 1280 :height 24))
        (cl-fltk:add-menu-item menu "File/Open Photo" (lambda (&rest ignored)
                                                          (declare (ignore ignored))
                                                          (open-photo)))
        (cl-fltk:add-menu-item menu "File/Add Photos to Project"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (add-photos)))
        (cl-fltk:add-menu-item menu "File/Open Project" (lambda (&rest ignored)
                                                            (declare (ignore ignored))
                                                            (open-project)))
        (cl-fltk:add-menu-item menu "File/Save Project" (lambda (&rest ignored)
                                                            (declare (ignore ignored))
                                                            (save-project)))
        (cl-fltk:add-menu-item menu "File/Save Project As" (lambda (&rest ignored)
                                                               (declare (ignore ignored))
                                                               (save-project t)))
        (cl-fltk:add-menu-item menu "File/Export Current Photo"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (render-selected)))
        (cl-fltk:add-menu-item menu "File/Export All Photos"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (render-all)))
        (cl-fltk:add-menu-item menu "File/Quit" (lambda (&rest ignored)
                                                    (declare (ignore ignored))
                                                    (cl-fltk:quit)))
        (cl-fltk:add-menu-item menu "Edit/Remove Selected Photos"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (remove-selected-photo)))
        (cl-fltk:add-menu-item menu "Edit/Reset Selected Photos"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (gui-model-reset-selected model)
                                 (sync-controls)
                                              (schedule-edited-preview)))
        (cl-fltk:add-menu-item menu "Edit/Reset Defaults"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (setf (project-defaults project)
                                       (gui-default-processing-settings))
                                 (sync-controls)
                                              (schedule-edited-preview)))
        (cl-fltk:add-menu-item menu "View/Before and After"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (toggle-comparison)))
        (cl-fltk:add-menu-item menu "View/Refresh Preview"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (schedule-initial-preview)))
        (cl-fltk:add-menu-item menu "Process/Export Current Photo"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (render-selected)))
        (cl-fltk:add-menu-item menu "Process/Export All Photos"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (render-all)))
        (cl-fltk:add-menu-item menu "Help/About Orfeus"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (cl-fltk:message-box
                                  "Orfeus RAW processor\nOlympus PEN-F and OM-1")))
        (setf toolbar (cl-fltk:make-panel :parent window :x 0 :y 24
                                          :width 1280 :height 40 :label ""))
        (cl-fltk:set-box toolbar cl-fltk:+box-flat-box+)
        (flet ((rule (x y width height red green blue)
                 (let ((box (cl-fltk:make-box :parent toolbar :x x :y y
                                              :width width :height height
                                              :label "")))
                   (cl-fltk:set-box box cl-fltk:+box-flat-box+)
                   (cl-fltk:set-color-rgb box :red red :green green :blue blue)
                   box))
               (toolbar-button (x icon tooltip action)
                 (let ((button (cl-fltk:make-button
                                :parent toolbar :x x :y 5 :width 28 :height 28
                                :label ""
                                :callback (lambda (&rest ignored)
                                            (declare (ignore ignored))
                                            (funcall action)))))
                   (cl-fltk:set-box button cl-fltk:+box-flat-box+)
                   (cl-fltk:set-stock-icon button icon)
                   (cl-fltk:set-tooltip button tooltip)
                   button))
               (toolbar-text-button (x width label tooltip action)
                 (let ((button (cl-fltk:make-button
                                :parent toolbar :x x :y 5 :width width :height 28
                                :label label
                                :callback (lambda (&rest ignored)
                                            (declare (ignore ignored))
                                            (funcall action)))))
                   (cl-fltk:set-box button cl-fltk:+box-flat-box+)
                   (cl-fltk:set-tooltip button tooltip)
                   button)))
          (rule 6 10 2 18 130 130 130)
          (rule 10 10 2 18 245 245 245)
          (toolbar-button 18 :open "Add RAW photographs to project" #'add-photos)
          (toolbar-button 48 :folder-open "Open project" #'open-project)
          (toolbar-button 78 :delete "Remove selected photographs" #'remove-selected-photo)
          (rule 112 7 1 24 150 150 150)
          (toolbar-button 120 :export "Export current photograph" #'render-selected)
          (toolbar-button 150 :pipeline "Show or hide Before and After" #'toggle-comparison)
          (rule 184 7 1 24 150 150 150)
          (toolbar-text-button 192 28 "−" "Zoom out" (lambda () (zoom-preview .8d0)))
          (toolbar-text-button 222 38 "Fit" "Fit preview" #'reset-preview-view)
          (toolbar-text-button 262 28 "+" "Zoom in" (lambda () (zoom-preview 1.25d0)))
          (toolbar-text-button 292 38 "1:1" "Show image pixels at 1:1" #'preview-one-to-one)
          (rule 334 7 1 24 150 150 150)
          (setf toolbar-bottom-rule (rule 0 38 1280 2 145 145 145)))
        (setf lens-name (cl-fltk:make-label :parent toolbar :x 342 :y 6
                                            :width 912 :height 28
                                            :label "Lens: No photograph selected"))
        (cl-fltk:set-label-font lens-name 1)
        (setf main-tile (cl-fltk:make-tile :parent window :x 0 :y 64
                                           :width 1280 :height 708)
              left-pane (cl-fltk:make-panel :parent main-tile :x 0 :y 0
                                            :width 240 :height 708 :label "")
              center-pane (cl-fltk:make-panel :parent main-tile :x 240 :y 0
                                              :width 720 :height 708 :label ""))
        (setf thumbnail-canvas
              (cl-fltk:make-canvas
               :parent left-pane :x 0 :y 0 :width 240 :height 708
               :callback
               (lambda (widget event value)
                 (declare (ignore event value))
                 (let ((x (cl-fltk:widget-x widget))
                       (y (cl-fltk:widget-y widget))
                       (width (cl-fltk:widget-width widget))
                       (height (cl-fltk:widget-height widget))
                       (row-height (thumbnail-row-height)))
                   (cl-fltk:draw-color-rgb :red 42 :green 44 :blue 46)
                   (cl-fltk:draw-filled-rect x y width height)
                   (loop for job in (project-photos project)
                         for row from 0
                         for row-y = (+ y (* row row-height) (- thumbnail-scroll))
                         when (and (< row-y (+ y height))
                                   (> (+ row-y row-height) y))
                           do (when (member row (gui-model-selected-indices model))
                                (cl-fltk:draw-color-rgb :red 38 :green 74 :blue 122)
                                (cl-fltk:draw-filled-rect x row-y width row-height))
                              (let ((path (gethash job thumbnail-files)))
                                (if path
                                    (draw-thumbnail-file widget path
                                                         (+ x 6) (+ row-y 6)
                                                         88 (- row-height 12))
                                    (progn
                                      (cl-fltk:draw-color-rgb :red 62 :green 64 :blue 66)
                                      (cl-fltk:draw-filled-rect
                                       (+ x 6) (+ row-y 6) 88 (- row-height 12)))))
                              (cl-fltk:draw-color-rgb :red 235 :green 235 :blue 235)
                              (cl-fltk:draw-text
                               (file-namestring (photo-job-input-path job))
                               (+ x 102) (+ row-y 30))
                              (cl-fltk:draw-color-rgb :red 125 :green 127 :blue 129)
                              (cl-fltk:draw-filled-rect x (+ row-y row-height -1)
                                                       width 1))))))
        (dolist (event (list cl-fltk:+event-push+ cl-fltk:+event-wheel+))
          (cl-fltk:on thumbnail-canvas #'handle-thumbnail-mouse :event event))
        (setf before-caption
              (cl-fltk:make-label :parent center-pane :x 0 :y 0
                                  :width 357 :height 22
                                  :label "Before · neutral RAW")
              after-caption
              (cl-fltk:make-label :parent center-pane :x 363 :y 0
                                  :width 357 :height 22
                                  :label "After · current adjustments"))
        (flet ((make-preview-canvas (role x)
                 (cl-fltk:make-canvas
                  :parent center-pane :x x :y 22 :width 357 :height 686
                  :callback
                  (lambda (widget event value)
                    (declare (ignore event value))
                    (cl-fltk:draw-color-rgb :red 30 :green 32 :blue 34)
                    (cl-fltk:draw-filled-rect
                     (cl-fltk:widget-x widget) (cl-fltk:widget-y widget)
                     (cl-fltk:widget-width widget) (cl-fltk:widget-height widget))
                    (let ((path (ecase role
                                  (:before before-preview-file)
                                  (:after after-preview-file))))
                      (if path
                          (draw-preview-file widget path
                                             :zoom preview-zoom
                                             :center-x preview-center-x
                                             :center-y preview-center-y)
                          (progn
                            (cl-fltk:draw-color-rgb :red 205 :green 208 :blue 210)
                            (cl-fltk:draw-text "Developing RAW preview..."
                                               (+ (cl-fltk:widget-x widget) 20)
                                               (+ (cl-fltk:widget-y widget) 36)))))))))
          (setf before-canvas (make-preview-canvas :before 0)
                after-canvas (make-preview-canvas :after 363))
          (dolist (canvas (list before-canvas after-canvas))
            (dolist (event (list cl-fltk:+event-push+
                                 cl-fltk:+event-drag+
                                 cl-fltk:+event-release+
                                 cl-fltk:+event-wheel+))
              (cl-fltk:on canvas
                          (lambda (widget callback-event value)
                            (handle-preview-mouse widget callback-event value))
                          :event event))))
        (setf inspector (cl-fltk:make-panel :parent main-tile :x 960 :y 0
                                            :width 320 :height 708
                                            :label ""))
        (cl-fltk:set-box inspector cl-fltk:+box-no-box+)
        (let ((scope-field
                (cl-fltk:make-labeled-choice
                 :parent inspector :x 8 :y 8 :width 300 :height 26
                 :label "Apply to" :label-width 96
                 :items '("Photo" "Defaults")
                 :callback (lambda (widget event value)
                             (declare (ignore event value))
                             (setf (gui-model-edit-target model)
                                   (if (string-equal (cl-fltk:value widget)
                                                     "Defaults")
                                       :defaults :photo))
                             (sync-controls)))))
          (register-field scope-field 8)
          (setf target-choice (cl-fltk:field-control scope-field)))
        (setf tabs (cl-fltk:make-tabs :parent inspector :x 4 :y 40
                                      :width 312 :height 628)
              basic-page (cl-fltk:make-tab-page :parent tabs :x 2 :y 24
                                                :width 308 :height 600
                                                :label "Basic")
              optics-page (cl-fltk:make-tab-page :parent tabs :x 2 :y 24
                                                 :width 308 :height 600
                                                 :label "Optics")
              effects-page (cl-fltk:make-tab-page :parent tabs :x 2 :y 24
                                                  :width 308 :height 600
                                                  :label "Effects")
               export-page (cl-fltk:make-tab-page :parent tabs :x 2 :y 24
                                                  :width 308 :height 600
                                                  :label "Export")
               presets-page (cl-fltk:make-tab-page :parent tabs :x 2 :y 24
                                                   :width 308 :height 600
                                                   :label "Presets"))
        (flet ((export-integer-field (key label y)
                 (cl-fltk:field-control
                  (register-field
                   (cl-fltk:make-labeled-control
                    :int :parent export-page :x 8 :y y :width 292 :height 26
                    :label label :label-width 110
                    :callback (lambda (widget event value)
                                (declare (ignore event value))
                                (export-setting-changed key widget)))
                   y :page))))
          (setf export-quality (export-integer-field :jpeg-quality "JPEG quality" 12)
                export-max-width (export-integer-field :max-width "Maximum width" 48)
                export-max-height (export-integer-field :max-height "Maximum height" 84)
                export-metadata
                (register-inspector
                 (cl-fltk:make-check-button
                  :parent export-page :x 110 :y 120 :width 182 :height 26
                  :label "Preserve metadata"
                  :callback (lambda (widget event value)
                              (declare (ignore event value))
                              (export-setting-changed :preserve-metadata-p widget)))
                 110 120 :control 26 :page)))
        (setf preset-browser
              (register-inspector
               (cl-fltk:make-browser
                :parent presets-page :x 8 :y 12 :width 292 :height 230
                :items nil
                :callback (lambda (widget event value)
                            (declare (ignore event value))
                            (let ((name (cl-fltk:value widget)))
                              (when (plusp (length name))
                                (setf (cl-fltk:value preset-name-input) name)))))
               8 12 :fill 230 :page))
        (let ((name-field
                (register-field
                 (cl-fltk:make-labeled-input
                  :parent presets-page :x 8 :y 252 :width 292 :height 26
                  :label "Preset name" :label-width 96)
                 252 :page)))
          (setf preset-name-input (cl-fltk:field-control name-field)))
        (register-inspector
         (cl-fltk:make-button
          :parent presets-page :x 8 :y 288 :width 142 :height 26
          :label "Save current"
          :callback (lambda (&rest ignored)
                      (declare (ignore ignored))
                      (save-current-preset)))
         8 288 :half-left 26 :page)
        (setf preset-apply-button
              (register-inspector
               (cl-fltk:make-button
                :parent presets-page :x 158 :y 288 :width 142 :height 26
                :label "Apply to 1 photo"
                :callback (lambda (&rest ignored)
                            (declare (ignore ignored))
                            (apply-current-preset)))
               158 288 :half-right 26 :page))
        (refresh-preset-browser)
        (setf wb-choice
              (cl-fltk:field-control
               (register-field
                (cl-fltk:make-labeled-choice
                 :parent basic-page :x 8 :y 12 :width 292 :height 26
                 :label "White balance" :label-width 96
                 :items '("As shot" "Custom")
                 :callback (lambda (widget event value)
                             (declare (ignore event value))
                             (set-wb-mode widget)))
                12 :page)))
        (make-number-field :white-balance-temperature "Temperature (K)"
                           2000 15000 50 44 basic-page)
        (make-number-field :white-balance-tint "Tint" -20 20 0.1 76 basic-page)
        (make-number-field :exposure "Exposure EV" -10 10 0.1 108 basic-page)
        (make-number-field :noise-reduction "Noise reduction" 0 1 0.05 140
                           basic-page)
        (make-tone-band :tone-blacks "Blk" "Blacks" 0)
        (make-tone-band :tone-shadows "Shd" "Shadows" 1)
        (make-tone-band :tone-dark-mids "DkM" "Dark mids" 2)
        (make-tone-band :tone-midtones "Mid" "Midtones" 3)
        (make-tone-band :tone-light-mids "LtM" "Light mids" 4)
        (make-tone-band :tone-highlights "Hi" "Highlights" 5)
        (make-tone-band :tone-whites "Wht" "Whites" 6)
        (let ((lens (register-inspector
                     (cl-fltk:make-check-button
                      :parent optics-page :x 8 :y 12 :width 292 :height 26
                      :label "Apply lens distortion correction"
                      :callback (lambda (widget event value)
                                  (declare (ignore event value))
                                  (gui-model-set-setting
                                   model :lens-correction-p
                                   (string/= "0" (cl-fltk:value widget)))
                                  (schedule-edited-preview)))
                     8 12 :fill 26 :page)))
          (push (list :lens-correction-p lens) controls))
        (make-number-field :lens-correction-strength "Lens strength"
                           0 2 0.05 44 optics-page)
        (let ((tca (register-inspector
                    (cl-fltk:make-check-button
                     :parent optics-page :x 8 :y 76 :width 292 :height 26
                     :label "Remove chromatic aberration"
                     :callback (lambda (widget event value)
                                 (declare (ignore event value))
                                 (gui-model-set-setting
                                  model :chromatic-aberration-correction-p
                                  (string/= "0" (cl-fltk:value widget)))
                                 (schedule-edited-preview)))
                    8 76 :fill 26 :page)))
          (push (list :chromatic-aberration-correction-p tca) controls))
        (register-inspector
         (cl-fltk:make-label :parent effects-page :x 8 :y 12
                             :width 96 :height 26 :label "3D LUT")
         8 12 96 26 :page)
        (let ((items '("None")))
          (dolist (path (gui-bundled-lut-paths))
            (let ((name (file-namestring path)))
              (setf (gethash name lut-paths) (namestring path)
                    items (append items (list name)))))
          (setf items (append items '("Browse..."))
                lut-choice
                (register-inspector
                 (cl-fltk:make-choice
                  :parent effects-page :x 110 :y 12 :width 190 :height 26
                  :items items
                  :callback
                  (lambda (widget event value)
                    (declare (ignore event value))
                    (let ((selection (cl-fltk:value widget)))
                      (cond ((string= selection "Browse...")
                             (choose-lut)
                             (sync-controls))
                            ((string= selection "None") (clear-lut))
                            (t
                             (gui-model-set-setting
                              model :lut-path (gethash selection lut-paths))
                             (schedule-edited-preview))))))
                 110 12 :control 26 :page)))
        (make-number-field :lut-strength "LUT strength" 0 1 0.05 44
                           effects-page)
        (make-number-field :grain-amount "Grain amount" 0 1 0.05 76
                           effects-page)
        (make-number-field :grain-size "Grain size" 0.25 16 0.25 108
                           effects-page)
        (register-inspector
         (cl-fltk:make-button :parent inspector :x 12 :y 674
                              :width 140 :height 26 :label "Reset selected"
                              :callback (lambda (&rest ignored)
                                          (declare (ignore ignored))
                                          (gui-model-reset-selected model)
                                          (sync-controls)
                                                                (schedule-edited-preview)))
         12 :action-row :half-left 26)
        (register-inspector
         (cl-fltk:make-button :parent inspector :x 166 :y 674
                              :width 142 :height 26 :label "Export current"
                              :callback (lambda (&rest ignored)
                                          (declare (ignore ignored))
                                          (render-selected)))
         166 :action-row :half-right 26)
        (setf progress (cl-fltk:make-progress :parent window :x 0 :y 772
                                              :width 180 :height 28 :value "0")
              status (cl-fltk:make-status-bar :parent window :x 180 :y 772
                                               :width 1100 :height 28
                                               :value "Ready"))
        (cl-fltk:tile-size-range main-tile left-pane
                                 :min-width 180 :max-width 420)
        (cl-fltk:tile-size-range main-tile center-pane :min-width 300)
        (cl-fltk:tile-size-range main-tile inspector
                                 :min-width 280 :max-width 480)
        (cl-fltk:on-resize window #'layout-ui)
        (cl-fltk:on-resize left-pane #'layout-left-pane)
        (cl-fltk:on-resize center-pane #'layout-center-pane)
        (cl-fltk:on-resize inspector #'layout-inspector-pane)
        (gui-model-set-selected-indices
         model (if (project-photos project) '(0) '()))
        (sync-controls)
        (layout-ui)
        (setf poll-id (cl-fltk:add-timeout 0.08d0 #'poll :repeat t))
        (when (selected-job)
          (schedule-initial-preview))
        (unwind-protect
             (progn (cl-fltk:show window) (cl-fltk:run))
          (when poll-id (ignore-errors (cl-fltk:remove-timeout poll-id)))
          (when debounce-id (ignore-errors (cl-fltk:remove-timeout debounce-id)))
          (stop-gui-queue queue)
          (stop-gui-queue background-queue)
          (clear-preview-cache)
          (ignore-errors (delete-gui-preview-directory preview-directory))
          (when window (ignore-errors (cl-fltk:destroy window))))))))

(defun main (&optional pathname)
  "Launch Orfeus with optional PHOTO or PROJECT PATHNAME."
  (let ((argument (or pathname (first (uiop:command-line-arguments)))))
    (run-gui argument)))
