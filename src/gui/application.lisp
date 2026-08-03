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

(defconstant +menu-shift+ #x00010000
  "FLTK FL_SHIFT modifier for menu shortcuts.")
(defconstant +menu-ctrl+ #x00040000
  "FLTK FL_CTRL modifier for menu shortcuts.")
(defconstant +key-f5+ #xffc2
  "FLTK key code for the F5 function key.")

(defparameter *stage-node-labels*
  '((:white-balance . "WB")
    (:exposure . "Expo")
    (:noise-reduction . "NR")
    (:tone . "Tone")
    (:optics . "Optics")
    (:film . "Film"))
  "Short node captions for the pipeline strip, in processing order.")

(defparameter *node-strip-height* 46
  "Height of the pipeline node strip under the preview.")

(defparameter *gallery-cell-width* 96
  "Width of one still cell in the gallery grid.")

(defparameter *gallery-cell-height* 92
  "Height of one still cell in the gallery grid.")

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

(defun preview-settings-key (recipe)
  "Return a content hash covering a settings or graph render RECIPE."
  (let* ((*print-readably* t)
         (text (prin1-to-string
                (etypecase recipe
                  (orfeus:processing-graph (orfeus:graph->sexp recipe))
                  (orfeus:processing-settings
                   (orfeus::processing-settings->sexp recipe)))))
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
        (neural (gui-model-setting model :neural-noise-reduction))
        (lut (gui-model-setting model :lut-path))
        (strength (gui-model-setting model :lut-strength)))
    (format nil "RAW preview  |  WB: ~A  |  NR: ~D%~@[  |  Neural: ~D%~]  |  LUT: ~A"
            (if temperature "Custom" "As shot")
            (round (* 100 noise-reduction))
            (when (plusp neural) (round (* 100 neural)))
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
           thumbnail-canvas thumbnail-scrollbar
           before-canvas after-canvas before-caption after-caption
           node-strip still-button copy-grade-button paste-grade-button
           grade-clipboard
           inspector tabs basic-page optics-page effects-page export-page presets-page
           status progress before-preview-file after-preview-file
           lens-name controls inspector-items tone-items lut-choice wb-choice target-choice
           export-quality export-max-width export-max-height export-metadata
           gallery-canvas preset-name-input preset-apply-button
           (gallery-scroll 0)
           (gallery-selected nil)
           (gallery-thumbs (make-hash-table :test #'equal))
           (gallery-click (cons 0 -1))
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
               (when thumbnail-scrollbar
                 (cl-fltk:set-range thumbnail-scrollbar
                                    0 (thumbnail-scroll-limit))
                 (setf (cl-fltk:value thumbnail-scrollbar)
                       (format nil "~D" thumbnail-scroll)))
               (cl-fltk:redraw thumbnail-canvas)))
           (select-thumbnail-row (row state)
             (when (and (>= row 0) (< row (length (project-photos project))))
               (multiple-value-bind (selection anchor)
                   (thumbnail-selection-after-click
                    (gui-model-selected-indices model) row thumbnail-anchor state)
                 (setf thumbnail-anchor anchor)
                 (gui-model-set-selected-indices model selection)
                 (when (member row selection)
                   (setf (gui-model-selected-index model) row))
                 (clear-previews)
                 (sync-controls)
                 (when node-strip (cl-fltk:redraw node-strip))
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
           (node-strip-metrics ()
             (let* ((width (cl-fltk:widget-width node-strip))
                    (count (length *stage-node-labels*))
                    (wire 14)
                    (node-width (max 44 (min 96 (floor (- width 16
                                                          (* (1- count) wire))
                                                       count)))))
               (values node-width wire)))
           (node-index-at (x)
             (multiple-value-bind (node-width wire) (node-strip-metrics)
               (let* ((pitch (+ node-width wire))
                      (index (floor (- x 8) pitch))
                      (offset (- x 8 (* index pitch))))
                 (when (and (>= x 8)
                            (>= index 0)
                            (< index (length *stage-node-labels*))
                            (< offset node-width))
                   index))))
           (draw-node-strip (widget)
             (multiple-value-bind (node-width wire) (node-strip-metrics)
               (let* ((x (cl-fltk:widget-x widget))
                      (y (cl-fltk:widget-y widget))
                      (width (cl-fltk:widget-width widget))
                      (height (cl-fltk:widget-height widget))
                      (node-height 30)
                      (top (+ y (floor (- height node-height) 2)))
                      (middle (+ top (floor node-height 2))))
                 (cl-fltk:draw-color-rgb :red 192 :green 192 :blue 192)
                 (cl-fltk:draw-filled-rect x y width height)
                 (loop for (stage . label) in *stage-node-labels*
                       for index from 0
                       for node-x = (+ x 8 (* index (+ node-width wire)))
                       do (let ((bypassed (gui-model-stage-bypassed-p model
                                                                      stage))
                                (adjusted (gui-model-stage-adjusted-p model
                                                                      stage)))
                            (when (plusp index)
                              (cl-fltk:draw-color-rgb :red 90 :green 90
                                                      :blue 90)
                              (cl-fltk:draw-filled-rect (- node-x wire) middle
                                                        wire 2))
                            (if bypassed
                                (cl-fltk:draw-color-rgb :red 168 :green 168
                                                        :blue 168)
                                (cl-fltk:draw-color-rgb :red 210 :green 210
                                                        :blue 210))
                            (cl-fltk:draw-filled-rect node-x top node-width
                                                      node-height)
                            (cl-fltk:draw-color-rgb :red 248 :green 248
                                                    :blue 248)
                            (cl-fltk:draw-filled-rect node-x top node-width 1)
                            (cl-fltk:draw-filled-rect node-x top 1 node-height)
                            (cl-fltk:draw-color-rgb :red 105 :green 105
                                                    :blue 105)
                            (cl-fltk:draw-filled-rect
                             node-x (+ top node-height -1) node-width 1)
                            (cl-fltk:draw-filled-rect
                             (+ node-x node-width -1) top 1 node-height)
                            (if adjusted
                                (cl-fltk:draw-color-rgb :red 40 :green 150
                                                        :blue 40)
                                (cl-fltk:draw-color-rgb :red 150 :green 150
                                                        :blue 150))
                            (cl-fltk:draw-filled-rect (+ node-x node-width -11)
                                                      (+ top 4) 7 7)
                            (if bypassed
                                (cl-fltk:draw-color-rgb :red 110 :green 110
                                                        :blue 110)
                                (cl-fltk:draw-color-rgb :red 0 :green 0
                                                        :blue 0))
                            (cl-fltk:draw-font :size 11)
                            (cl-fltk:draw-text label (+ node-x 6)
                                               (+ top 20))
                            (cl-fltk:draw-font :size 12)
                            (when bypassed
                              (cl-fltk:draw-color-rgb :red 165 :green 40
                                                      :blue 40)
                              (cl-fltk:draw-line node-x (+ top node-height -1)
                                                 (+ node-x node-width -1)
                                                 top)))))))
           (handle-node-strip-mouse (widget event value)
             (declare (ignore widget event))
             (multiple-value-bind (x y) (parse-preview-event value)
               (declare (ignore y))
               (when x
                 (let ((index (node-index-at x)))
                   (when index
                     (destructuring-bind (stage . label)
                         (nth index *stage-node-labels*)
                       (let ((result (gui-model-toggle-stage model stage)))
                         (unless (eq result :none)
                           (cl-fltk:redraw node-strip)
                           (schedule-edited-preview)
                           (set-status
                            (format nil "~A ~A for ~D photo~:P"
                                    (if (eq result :bypassed)
                                        "Bypassed"
                                        "Enabled")
                                    label
                                    (selected-photo-count)))))))))))
           (copy-grade ()
             (multiple-value-bind (grade disabled)
                 (gui-model-copy-grade model)
               (if grade
                   (progn
                     (setf grade-clipboard (list grade disabled))
                     (set-status "Grade copied"))
                   (set-status "No photograph selected"))))
           (paste-grade ()
             (cond
               ((null grade-clipboard)
                (set-status "Copy a grade first"))
               (t
                (let ((count (gui-model-paste-grade
                              model
                              (first grade-clipboard)
                              (second grade-clipboard))))
                  (if (plusp count)
                      (progn
                        (sync-controls)
                        (when node-strip (cl-fltk:redraw node-strip))
                        (schedule-edited-preview)
                        (set-status (format nil "Grade pasted to ~D photo~:P"
                                            count)))
                      (set-status "No photograph selected"))))))
           (grab-still ()
             (let ((preset (gui-model-grab-still model)))
               (if preset
                   (progn
                     (refresh-gallery)
                     (setf (cl-fltk:value preset-name-input)
                           (processing-preset-name preset))
                     (sync-preset-action-label)
                     (set-status (format nil "Grabbed ~A"
                                         (processing-preset-name preset))))
                   (set-status "No photograph selected"))))
           (still-thumbnail-pathname (preset)
             (merge-pathnames
              (make-pathname
               :name (format nil "still-~A-~A"
                             (preview-settings-key
                              (orfeus::settings-apply-stage-bypass
                               (processing-preset-settings preset)
                               (processing-preset-disabled-stages preset)))
                             (or (pathname-name
                                  (processing-preset-source-photo preset))
                                 "none"))
               :type "jpg")
              preview-directory))
           (request-still-thumbnail (preset)
             (let ((name (processing-preset-name preset))
                   (source (processing-preset-source-photo preset)))
               (when (and source (null (gethash name gallery-thumbs)))
                 (let ((output (still-thumbnail-pathname preset))
                       (settings
                         (orfeus::settings-apply-stage-bypass
                          (processing-preset-settings preset)
                          (processing-preset-disabled-stages preset))))
                   (if (probe-file output)
                       (setf (gethash name gallery-thumbs) output)
                       (enqueue-gui-task
                        background-queue :still
                        (lambda ()
                          (handler-case
                              (progn
                                (render-preview
                                 source output settings
                                 :max-width *thumbnail-preview-size*
                                 :max-height *thumbnail-preview-size*
                                 :jpeg-quality 82
                                 :if-exists :supersede)
                                (queue-event queue
                                             (list :still-thumb name output)))
                            (error () nil)))))))))
           (refresh-gallery ()
             (when gallery-canvas
               (dolist (preset (project-presets project))
                 (request-still-thumbnail preset))
               (cl-fltk:redraw gallery-canvas)))
           (gallery-columns ()
             (max 1 (floor (- (cl-fltk:widget-width gallery-canvas) 8)
                           *gallery-cell-width*)))
           (gallery-index-at (x y)
             (let* ((columns (gallery-columns))
                    (column (floor (- x 4) *gallery-cell-width*))
                    (row (floor (+ (- y 4) gallery-scroll)
                                *gallery-cell-height*))
                    (index (+ (* row columns) (min column (1- columns)))))
               (when (and (>= column 0) (< column columns) (>= row 0)
                          (< index (length (project-presets project))))
                 index)))
           (gallery-scroll-limit ()
             (let ((columns (gallery-columns)))
               (max 0 (- (* (ceiling (length (project-presets project))
                                     columns)
                            *gallery-cell-height*)
                         (- (cl-fltk:widget-height gallery-canvas) 8)))))
           (draw-gallery (widget)
             (let ((x (cl-fltk:widget-x widget))
                   (y (cl-fltk:widget-y widget))
                   (width (cl-fltk:widget-width widget))
                   (height (cl-fltk:widget-height widget))
                   (columns (gallery-columns)))
               (cl-fltk:draw-color-rgb :red 255 :green 255 :blue 255)
               (cl-fltk:draw-filled-rect x y width height)
               (loop for preset in (project-presets project)
                     for index from 0
                     for column = (mod index columns)
                     for row = (floor index columns)
                     for cell-x = (+ x 4 (* column *gallery-cell-width*))
                     for cell-y = (+ y 4 (* row *gallery-cell-height*)
                                    (- gallery-scroll))
                     when (and (< cell-y (+ y height))
                               (> (+ cell-y *gallery-cell-height*) y))
                       do (let ((name (processing-preset-name preset))
                                (selected (eql index gallery-selected)))
                            (when selected
                              (cl-fltk:draw-color-rgb :red 0 :green 0 :blue 128)
                              (cl-fltk:draw-filled-rect
                               cell-x cell-y (- *gallery-cell-width* 6)
                               (- *gallery-cell-height* 6)))
                            (let ((thumb (gethash name gallery-thumbs)))
                              (if thumb
                                  (draw-thumbnail-file
                                   widget thumb (+ cell-x 3) (+ cell-y 3)
                                   (- *gallery-cell-width* 12) 62)
                                  (progn
                                    (cl-fltk:draw-color-rgb
                                     :red 205 :green 205 :blue 205)
                                    (cl-fltk:draw-filled-rect
                                     (+ cell-x 3) (+ cell-y 3)
                                     (- *gallery-cell-width* 12) 62))))
                            (if selected
                                (cl-fltk:draw-color-rgb :red 255 :green 255
                                                        :blue 255)
                                (cl-fltk:draw-color-rgb :red 0 :green 0
                                                        :blue 0))
                            (cl-fltk:draw-font :size 10)
                            (cl-fltk:draw-text
                             (if (> (length name) 14)
                                 (subseq name 0 14)
                                 name)
                             (+ cell-x 3) (+ cell-y 80))
                            (cl-fltk:draw-font :size 12)))))
           (gallery-select (index)
             (setf gallery-selected index)
             (let ((preset (nth index (project-presets project))))
               (when preset
                 (setf (cl-fltk:value preset-name-input)
                       (processing-preset-name preset))))
             (cl-fltk:redraw gallery-canvas))
           (handle-gallery-mouse (widget event value)
             (declare (ignore widget))
             (multiple-value-bind (x y button dx dy state)
                 (parse-preview-event value)
               (declare (ignore button dx state))
               (when x
                 (case event
                   (#.cl-fltk:+event-push+
                    (let ((index (gallery-index-at x y))
                          (now (get-internal-real-time)))
                      (when index
                        (gallery-select index)
                        (if (and (eql index (cdr gallery-click))
                                 (< (- now (car gallery-click))
                                    (* 0.4 internal-time-units-per-second)))
                            (apply-current-preset)
                            (setf gallery-click (cons now index))))))
                   (#.cl-fltk:+event-wheel+
                    (setf gallery-scroll
                          (min (gallery-scroll-limit)
                               (max 0 (+ gallery-scroll (* dy 32)))))
                    (cl-fltk:redraw gallery-canvas))))))
           (sync-preset-action-label ()
             (when preset-apply-button
               (setf (cl-fltk:label preset-apply-button)
                     (format nil "Apply to ~D photo~:P"
                             (selected-photo-count)))))
           (save-current-preset ()
             (handler-case
                 (let ((preset (gui-model-save-preset
                                model (cl-fltk:value preset-name-input))))
                   (remhash (processing-preset-name preset) gallery-thumbs)
                   (refresh-gallery)
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
             (clrhash gallery-thumbs)
             (setf gallery-selected nil
                   gallery-scroll 0)
             (gui-model-replace-project model new-project path)
             (refresh-gallery)
             (when node-strip (cl-fltk:redraw node-strip))
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
             ;; The render recipe: the photo's node graph when it has one,
             ;; otherwise flat settings with overrides and bypasses applied,
             ;; exactly as the CLI batch renders them.
             (or (orfeus:photo-job-graph job)
                 (orfeus:photo-render-settings project job)))
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
                        (if (typep settings 'orfeus:processing-graph)
                            (render-preview input output nil
                                            :graph settings
                                            :max-width max-width
                                            :max-height max-height
                                            :cache-p cache-p
                                            :if-exists :supersede)
                            (render-preview input output settings
                                            :max-width max-width
                                            :max-height max-height
                                            :cache-p cache-p
                                            :if-exists :supersede))
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
             (let ((label-x (if (eq basis :page) 12 8))
                   (label-width (if (eq basis :page) 88 96)))
               (register-inspector (cl-fltk:field-label field)
                                   label-x y label-width 26 basis)
               (register-inspector (cl-fltk:field-control field) 110 y
                                   :control 26 basis))
             field)
           (section-frame (parent title y height)
             ;; A classic engraved group frame whose title interrupts the
             ;; frame line, drawn behind the controls it surrounds.
             (let ((frame (cl-fltk:make-box :parent parent :x 4 :y y
                                            :width 300 :height height
                                            :label "")))
               (cl-fltk:set-box frame cl-fltk:+box-engraved-frame+)
               (register-inspector frame 4 y :frame height :page))
             (let ((title-width (+ 14 (* 7 (length title))))
                   (title-label (cl-fltk:make-label
                                 :parent parent :x 14 :y (- y 8)
                                 :width 80 :height 16 :label title)))
               (cl-fltk:set-box title-label cl-fltk:+box-flat-box+)
               (cl-fltk:set-label-font title-label
                                       cl-fltk:+font-helvetica-bold+)
               (register-inspector title-label 14 (- y 8) title-width 16
                                   :page)))
           (make-number-field (key label minimum maximum step y parent)
             (let* ((label-widget
                      (cl-fltk:make-label :parent parent :x 12 :y y
                                          :width 88 :height 26 :label label))
                    (callback (lambda (widget event value)
                                (declare (ignore event value))
                                (setting-changed key widget)))
                    (spinner (cl-fltk:make-spinner
                              :parent parent :x 202 :y y
                              :width 78 :height 26 :callback callback))
                    (slider (cl-fltk:make-slider
                             :parent parent :x 110 :y y
                             :width 84 :height 26 :callback callback)))
               (dolist (widget (list slider spinner))
                 (cl-fltk:set-range widget minimum maximum)
                 (cl-fltk:set-step widget step)
                 (push (list key widget) controls))
               (register-inspector label-widget 12 y 88 26 :page)
               (register-inspector slider 110 y :slider 26 :page)
               (register-inspector spinner 202 y :number 26 :page)
               spinner))
           (make-tone-band (key short-label full-label index)
             (let* ((callback (lambda (widget event value)
                                (declare (ignore event value))
                                (setting-changed key widget)))
                    (label (cl-fltk:make-label
                            :parent basic-page :x 0 :y 306 :width 32 :height 22
                            :label short-label))
                    (slider (cl-fltk:make-vertical-slider
                             :parent basic-page :x 0 :y 330 :width 20 :height 120
                             :callback callback))
                    (input (cl-fltk:make-value-input
                            :parent basic-page :x 0 :y 454 :width 32 :height 24
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
             (let ((width (cl-fltk:widget-width left-pane))
                   (height (cl-fltk:widget-height left-pane)))
               (cl-fltk:resize-widget
                thumbnail-canvas :x 0 :y 0
                :width (max 40 (- width 16)) :height height)
               (when thumbnail-scrollbar
                 (cl-fltk:resize-widget
                  thumbnail-scrollbar :x (max 40 (- width 16)) :y 0
                  :width 16 :height height)))
             (redraw-thumbnails)
             (cl-fltk:redraw left-pane))
           (layout-center-pane (&optional ignored)
             (declare (ignore ignored))
             (let* ((center (cl-fltk:widget-width center-pane))
                    (main-height (cl-fltk:widget-height center-pane))
                    (caption-height 22)
                    (strip-height *node-strip-height*)
                    (viewer-height (max 100 (- main-height caption-height
                                               strip-height)))
                    (strip-y (+ caption-height viewer-height))
                    (button-width 52)
                    (buttons-width (+ (* 3 button-width) 16))
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
               (when node-strip
                 (cl-fltk:resize-widget node-strip :x 0 :y strip-y
                                        :width (max 120 (- center buttons-width))
                                        :height strip-height)
                 (let ((button-y (+ strip-y (floor (- strip-height 24) 2))))
                   (cl-fltk:resize-widget still-button
                                          :x (- center (* 3 button-width) 12)
                                          :y button-y
                                          :width button-width :height 24)
                   (cl-fltk:resize-widget copy-grade-button
                                          :x (- center (* 2 button-width) 8)
                                          :y button-y
                                          :width button-width :height 24)
                   (cl-fltk:resize-widget paste-grade-button
                                          :x (- center button-width 4)
                                          :y button-y
                                          :width button-width :height 24)))
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
                              (:frame (max 120 (- basis-width 8)))
                              (:fill (max 100 (- basis-width 24)))
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
                      (column-width (max 34 (floor (- page-width 32) 7))))
                 (dolist (item tone-items)
                   (destructuring-bind (widget index role) item
                     (let* ((column-x (+ 14 (* index column-width)))
                            (item-width (ecase role
                                          (:label column-width)
                                          (:slider 20)
                                          (:input (max 30 (- column-width 6)))))
                            (item-x (ecase role
                                      (:label (+ column-x 4))
                                      (:slider (+ column-x (floor (- column-width 20) 2)))
                                      (:input (+ column-x 2))))
                            (item-y (ecase role (:label 306) (:slider 330) (:input 454)))
                            (item-height (ecase role (:label 22) (:slider 120) (:input 24))))
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
                 (:still-thumb
                  (let ((preset (find (second event) (project-presets project)
                                      :test #'string-equal
                                      :key #'processing-preset-name)))
                    (when (and preset
                               (equal (third event)
                                      (still-thumbnail-pathname preset)))
                      (setf (gethash (second event) gallery-thumbs) (third event))
                      (when gallery-canvas (cl-fltk:redraw gallery-canvas)))))
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
                                                          (open-photo))
                               :shortcut (logior +menu-ctrl+ (char-code #\o)))
        (cl-fltk:add-menu-item menu "File/Add Photos to Project"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (add-photos))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\a)))
        (cl-fltk:add-menu-item menu "File/Open Project" (lambda (&rest ignored)
                                                            (declare (ignore ignored))
                                                            (open-project))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\o)))
        (cl-fltk:add-menu-item menu "File/Save Project" (lambda (&rest ignored)
                                                            (declare (ignore ignored))
                                                            (save-project))
                               :shortcut (logior +menu-ctrl+ (char-code #\s)))
        (cl-fltk:add-menu-item menu "File/Save Project As" (lambda (&rest ignored)
                                                               (declare (ignore ignored))
                                                               (save-project t))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\s)))
        (cl-fltk:add-menu-item menu "File/Export Current Photo"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (render-selected))
                               :shortcut (logior +menu-ctrl+ (char-code #\e)))
        (cl-fltk:add-menu-item menu "File/Export All Photos"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (render-all))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\e)))
        (cl-fltk:add-menu-item menu "File/Quit" (lambda (&rest ignored)
                                                    (declare (ignore ignored))
                                                    (cl-fltk:quit))
                               :shortcut (logior +menu-ctrl+ (char-code #\q)))
        (cl-fltk:add-menu-item menu "Edit/Copy Grade"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (copy-grade))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\c)))
        (cl-fltk:add-menu-item menu "Edit/Paste Grade to Selected"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (paste-grade))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\v)))
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
                                 (toggle-comparison))
                               :shortcut (logior +menu-ctrl+ (char-code #\b)))
        (cl-fltk:add-menu-item menu "View/Refresh Preview"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (schedule-initial-preview))
                               :shortcut +key-f5+)
        (cl-fltk:add-menu-item menu "Process/Grab Still"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (grab-still))
                               :shortcut (logior +menu-ctrl+ (char-code #\g)))
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
                                (cl-fltk:draw-color-rgb :red 0 :green 0 :blue 128)
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
        (setf thumbnail-scrollbar
              (cl-fltk:make-scrollbar
               :parent left-pane :x 224 :y 0 :width 16 :height 708
               :value "0"
               :callback
               (lambda (widget event value)
                 (declare (ignore event))
                 (let ((position (ignore-errors
                                   (round (parse-number
                                           (if (plusp (length (or value "")))
                                               value
                                               (cl-fltk:value widget)))))))
                   (when position
                     (setf thumbnail-scroll position)
                     (clamp-thumbnail-scroll)
                     (cl-fltk:redraw thumbnail-canvas))))))
        (cl-fltk:scrollbar-set-orientation thumbnail-scrollbar :vertical)
        (cl-fltk:set-step thumbnail-scrollbar 36)
        (setf before-caption
              (cl-fltk:make-label :parent center-pane :x 0 :y 0
                                  :width 357 :height 22
                                  :label "  Before · neutral RAW")
              after-caption
              (cl-fltk:make-label :parent center-pane :x 363 :y 0
                                  :width 357 :height 22
                                  :label "  After · current adjustments"))
        (dolist (caption (list before-caption after-caption))
          (cl-fltk:set-box caption cl-fltk:+box-thin-up-box+)
          (cl-fltk:set-label-font caption cl-fltk:+font-helvetica-bold+))
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
        (setf node-strip
              (cl-fltk:make-canvas
               :parent center-pane :x 0 :y 662 :width 540 :height 46
               :callback (lambda (widget event value)
                           (declare (ignore event value))
                           (draw-node-strip widget))))
        (cl-fltk:set-box node-strip cl-fltk:+box-flat-box+)
        (cl-fltk:set-tooltip node-strip
                             "Pipeline nodes: click one to bypass its stage")
        (cl-fltk:on node-strip #'handle-node-strip-mouse
                    :event cl-fltk:+event-push+)
        (flet ((grade-button (label tooltip action)
                 (let ((button (cl-fltk:make-button
                                :parent center-pane :x 560 :y 672
                                :width 52 :height 24 :label label
                                :callback (lambda (&rest ignored)
                                            (declare (ignore ignored))
                                            (funcall action)))))
                   (cl-fltk:set-tooltip button tooltip)
                   button)))
          (setf still-button
                (grade-button "Still" "Grab a still of the current grade"
                              #'grab-still)
                copy-grade-button
                (grade-button "Copy" "Copy the current photo's grade"
                              #'copy-grade)
                paste-grade-button
                (grade-button "Paste" "Paste the copied grade to the selection"
                              #'paste-grade)))
        (setf inspector (cl-fltk:make-panel :parent main-tile :x 960 :y 0
                                            :width 320 :height 708
                                            :label ""))
        ;; The inspector lays its children out manually, so it must paint its
        ;; own background; a boxless group smears stale pixels during tile
        ;; drags and window resizes.
        (cl-fltk:set-box inspector cl-fltk:+box-flat-box+)
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
                                                   :label "Gallery"))
        (section-frame export-page "Output" 16 140)
        (flet ((export-integer-field (key label y)
                 (cl-fltk:field-control
                  (register-field
                   (cl-fltk:make-labeled-control
                    :int :parent export-page :x 12 :y y :width 292 :height 26
                    :label label :label-width 96
                    :callback (lambda (widget event value)
                                (declare (ignore event value))
                                (export-setting-changed key widget)))
                   y :page))))
          (setf export-quality (export-integer-field :jpeg-quality "JPEG quality" 26)
                export-max-width (export-integer-field :max-width "Maximum width" 58)
                export-max-height (export-integer-field :max-height "Maximum height" 90)
                export-metadata
                (register-inspector
                 (cl-fltk:make-check-button
                  :parent export-page :x 110 :y 122 :width 182 :height 26
                  :label "Preserve metadata"
                  :callback (lambda (widget event value)
                              (declare (ignore event value))
                              (export-setting-changed :preserve-metadata-p widget)))
                 110 122 :control 26 :page)))
        (section-frame presets-page "Stills Gallery" 16 244)
        (setf gallery-canvas
              (register-inspector
               (cl-fltk:make-canvas
                :parent presets-page :x 12 :y 28 :width 292 :height 190
                :callback (lambda (widget event value)
                            (declare (ignore event value))
                            (draw-gallery widget)))
               12 28 :fill 190 :page))
        (dolist (event (list cl-fltk:+event-push+ cl-fltk:+event-wheel+))
          (cl-fltk:on gallery-canvas #'handle-gallery-mouse :event event))
        (let ((name-field
                (register-field
                 (cl-fltk:make-labeled-input
                  :parent presets-page :x 12 :y 226 :width 292 :height 26
                  :label "Preset name" :label-width 96)
                 226 :page)))
          (setf preset-name-input (cl-fltk:field-control name-field)))
        (register-inspector
         (cl-fltk:make-button
          :parent presets-page :x 8 :y 270 :width 142 :height 26
          :label "Save current"
          :callback (lambda (&rest ignored)
                      (declare (ignore ignored))
                      (save-current-preset)))
         8 270 :half-left 26 :page)
        (setf preset-apply-button
              (register-inspector
               (cl-fltk:make-button
                :parent presets-page :x 158 :y 270 :width 142 :height 26
                :label "Apply to 1 photo"
                :callback (lambda (&rest ignored)
                            (declare (ignore ignored))
                            (apply-current-preset)))
               158 270 :half-right 26 :page))
        (refresh-gallery)
        (section-frame basic-page "White Balance" 16 110)
        (setf wb-choice
              (cl-fltk:field-control
               (register-field
                (cl-fltk:make-labeled-choice
                 :parent basic-page :x 12 :y 26 :width 292 :height 26
                 :label "Mode" :label-width 96
                 :items '("As shot" "Custom")
                 :callback (lambda (widget event value)
                             (declare (ignore event value))
                             (set-wb-mode widget)))
                26 :page)))
        (make-number-field :white-balance-temperature "Temperature (K)"
                           2000 15000 50 58 basic-page)
        (make-number-field :white-balance-tint "Tint" -20 20 0.1 90 basic-page)
        (section-frame basic-page "Exposure" 142 44)
        (make-number-field :exposure "Exposure EV" -10 10 0.1 152 basic-page)
        (section-frame basic-page "Noise Reduction" 202 76)
        (make-number-field :noise-reduction "Edge-aware" 0 1 0.05 212
                           basic-page)
        (make-number-field :neural-noise-reduction "Neural" 0 1 0.05 244
                           basic-page)
        (section-frame basic-page "Tone Equalizer" 294 194)
        (make-tone-band :tone-blacks "Blk" "Blacks" 0)
        (make-tone-band :tone-shadows "Shd" "Shadows" 1)
        (make-tone-band :tone-dark-mids "DkM" "Dark mids" 2)
        (make-tone-band :tone-midtones "Mid" "Midtones" 3)
        (make-tone-band :tone-light-mids "LtM" "Light mids" 4)
        (make-tone-band :tone-highlights "Hi" "Highlights" 5)
        (make-tone-band :tone-whites "Wht" "Whites" 6)
        (section-frame optics-page "Lens Corrections" 16 110)
        (let ((lens (register-inspector
                     (cl-fltk:make-check-button
                      :parent optics-page :x 12 :y 26 :width 292 :height 26
                      :label "Apply lens distortion correction"
                      :callback (lambda (widget event value)
                                  (declare (ignore event value))
                                  (gui-model-set-setting
                                   model :lens-correction-p
                                   (string/= "0" (cl-fltk:value widget)))
                                  (schedule-edited-preview)))
                     12 26 :fill 26 :page)))
          (push (list :lens-correction-p lens) controls))
        (make-number-field :lens-correction-strength "Strength"
                           0 2 0.05 58 optics-page)
        (let ((tca (register-inspector
                    (cl-fltk:make-check-button
                     :parent optics-page :x 12 :y 90 :width 292 :height 26
                     :label "Remove chromatic aberration"
                     :callback (lambda (widget event value)
                                 (declare (ignore event value))
                                 (gui-model-set-setting
                                  model :chromatic-aberration-correction-p
                                  (string/= "0" (cl-fltk:value widget)))
                                 (schedule-edited-preview)))
                    12 90 :fill 26 :page)))
          (push (list :chromatic-aberration-correction-p tca) controls))
        (section-frame effects-page "Film Emulation" 16 76)
        (register-inspector
         (cl-fltk:make-label :parent effects-page :x 12 :y 26
                             :width 88 :height 26 :label "3D LUT")
         12 26 88 26 :page)
        (let ((items '("None")))
          (dolist (path (gui-bundled-lut-paths))
            (let ((name (file-namestring path)))
              (setf (gethash name lut-paths) (namestring path)
                    items (append items (list name)))))
          (setf items (append items '("Browse..."))
                lut-choice
                (register-inspector
                 (cl-fltk:make-choice
                  :parent effects-page :x 110 :y 26 :width 190 :height 26
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
                 110 26 :control 26 :page)))
        (make-number-field :lut-strength "Strength" 0 1 0.05 58
                           effects-page)
        (section-frame effects-page "Film Grain" 108 76)
        (make-number-field :grain-amount "Amount" 0 1 0.05 118
                           effects-page)
        (make-number-field :grain-size "Size" 0.25 16 0.25 150
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
          (orfeus::clear-render-source-cache)
          (clear-preview-cache)
          (ignore-errors (delete-gui-preview-directory preview-directory))
          (when window (ignore-errors (cl-fltk:destroy window))))))))

(defun main (&optional pathname)
  "Launch Orfeus with optional PHOTO or PROJECT PATHNAME."
  (let ((argument (or pathname (first (uiop:command-line-arguments)))))
    (run-gui argument)))
