(in-package #:orfeus/gui)

(defparameter *preview-debounce-seconds* 0.25d0
  "Delay used to coalesce interactive control changes.")

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

(defun project-table-records (project)
  (loop for job in (project-photos project)
        collect (list :file (file-namestring (photo-job-input-path job))
                      :settings (if (photo-job-overrides job) "Adjusted" "Defaults")
                      :output (or (and (photo-job-output-path job)
                                       (file-namestring (photo-job-output-path job)))
                                  "Automatic"))))

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
  (logand #xffffffff
          (sxhash (orfeus::processing-settings->sexp settings))))

(defun preview-pathname (preview-directory index job role settings)
  (merge-pathnames
   (make-pathname :name (format nil "~(~A~)-~D-~8,'0X-~A"
                                role index (preview-settings-key settings)
                                (pathname-name (photo-job-input-path job)))
                  :type "jpg")
   preview-directory))

(defun preview-status-text (model)
  (let ((temperature (gui-model-setting model :white-balance-temperature))
        (lut (gui-model-setting model :lut-path))
        (strength (gui-model-setting model :lut-strength)))
    (format nil "RAW preview  |  WB: ~A  |  LUT: ~A"
            (if temperature "Custom" "As shot")
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
    (let* ((project initial-project)
           (model (make-gui-model :project project
                                  :project-path initial-path))
           (queue (make-gui-queue))
           (background-queue (make-gui-queue))
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
           window menu toolbar toolbar-bottom-rule table before-canvas after-canvas before-caption after-caption
           inspector tabs basic-page optics-page effects-page
           status progress before-preview-file after-preview-file
           lens-name controls inspector-items lut-name lut-menu wb-choice target-choice
           debounce-id poll-id comparison-p
           (preview-generation 0))
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
           (set-status (text)
             (setf (cl-fltk:value status) text))
           (selected-job ()
             (gui-model-selected-job model))
           (clear-previews ()
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
                 (:after (setf after-preview-file path)
                         (cl-fltk:redraw after-canvas)))))
           (update-table ()
             (cl-fltk:table-set-records
              table '((:file "File" 230) (:settings "Settings" 90)
                      (:output "Output" 150))
              (project-table-records project)))
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
                       "Defaults" "Photo")
                   (cl-fltk:value lut-name)
                   (let ((path (gui-model-setting model :lut-path)))
                     (if path (file-namestring path) "No LUT selected")))
             (setf (cl-fltk:label lens-name)
                   (format nil "Lens: ~A" (selected-lens-description))))
           (replace-project (new-project &optional path)
             (incf preview-generation)
             (clear-previews)
             (setf project new-project)
             (gui-model-replace-project model new-project path)
             (update-table)
             (sync-controls)
             (if (selected-job)
                 (progn
                   (cl-fltk:table-select-row table 0)
                   (schedule-initial-preview))
                 (set-status "Open a photograph or project to begin")))
           (open-photo ()
             (let ((paths (choose-photo-files
                           :title "Open RAW photographs"
                           :filter (fltk-file-filter
                                    "RAW photographs" "*.{orf,ORF,dng,DNG}")
                           :preset-path (picker-preset))))
               (when paths
                 (remember-picked-path (first paths))
                 (replace-project (gui-photos-project paths)))))
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
           (enqueue-render (target-queue role job index settings generation publish-p)
             (let* ((input (photo-job-input-path job))
                    (output (preview-pathname preview-directory index job role settings)))
               (if (probe-file output)
                   (when publish-p
                     (queue-event queue
                                  (list :preview generation index job role output)))
                   (enqueue-gui-task
                    target-queue role
                    (lambda ()
                      (when (or (not publish-p)
                                (= generation preview-generation))
                        (when publish-p
                          (queue-event queue
                                       (list :status generation
                                             (format nil "Developing ~A..."
                                                     (file-namestring input)))))
                        (render-preview input output settings :if-exists :supersede)
                        (when (and publish-p
                                   (= generation preview-generation))
                          (queue-event queue
                                       (list :preview generation index job role
                                             output)))))))))
           (enqueue-background-previews (selected-before-p generation)
             (discard-gui-tasks background-queue)
             (let ((selected (selected-job)))
               (when (and selected selected-before-p)
                 (enqueue-render background-queue :before selected
                                 (gui-model-selected-index model)
                                 (neutral-preview-settings) generation t))
               ;; Make every photograph's useful edited preview ready first;
               ;; neutral comparison renders follow after that hot path.
               (loop for job in (project-photos project)
                     for index from 0
                     unless (eq job selected)
                       do (enqueue-render background-queue :after job index
                                          (settings-for-job job) nil nil))
               (loop for job in (project-photos project)
                     for index from 0
                     unless (eq job selected)
                       do (enqueue-render background-queue :before job index
                                          (neutral-preview-settings) nil nil))))
           (enqueue-preview (initial-p)
             (let ((job (selected-job))
                   (index (gui-model-selected-index model))
                   (generation preview-generation))
               (when job
                 (enqueue-render queue :after job index
                                 (current-settings) generation t)
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
             (when after-preview-file
               (forget-preview-file after-preview-file)
               (setf after-preview-file nil)
               (cl-fltk:redraw after-canvas))
             (set-status "Preview update pending...")
             (when debounce-id
               (ignore-errors (cl-fltk:remove-timeout debounce-id)))
             (setf debounce-id
                   (cl-fltk:add-timeout
                    *preview-debounce-seconds*
                    (lambda ()
                      (setf debounce-id nil)
                      (discard-gui-tasks queue :after)
                      (when after-preview-file
                        (forget-preview-file after-preview-file)
                        (setf after-preview-file nil)
                        (cl-fltk:redraw after-canvas))
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
                   (update-table)
                   (schedule-edited-preview))
               (error (condition) (set-status (princ-to-string condition)))))
           (select-row ()
             (let ((row (cl-fltk:table-selected-row table)))
               (when (and (>= row 0) (< row (length (project-photos project))))
                 (setf (gui-model-selected-index model) row)
                 (clear-previews)
                 (sync-controls)
                 (schedule-initial-preview))))
           (render-selected ()
             (let ((job (selected-job)))
               (when job
                 (let ((output (gui-photo-output-path model job))
                       (input (photo-job-input-path job))
                       (settings (current-settings)))
                   (ensure-directories-exist output)
                   (enqueue-gui-task
                    queue :export
                    (lambda ()
                      (queue-event queue (list :status nil "Rendering export..."))
                      (render-photo input output settings :if-exists :supersede)
                      (queue-event queue (list :done output))))))))
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
           (layout-ui (&optional ignored)
             (declare (ignore ignored))
             (let* ((width (cl-fltk:widget-width window))
                    (height (cl-fltk:widget-height window))
                    (top 64) (bottom 28)
                    (main-height (max 200 (- height top bottom)))
                    (left (min 260 (max 190 (floor width 5))))
                    (right (min 400 (max 300 (floor width 3))))
                    (center (max 300 (- width left right)))
                    (caption-height 22)
                    (viewer-y (+ top caption-height))
                    (viewer-height (max 100 (- main-height caption-height)))
                    (gutter 6)
                    (pane-width (floor (- center gutter) 2)))
               (cl-fltk:resize-widget menu :x 0 :y 0 :width width :height 24)
               (cl-fltk:resize-widget toolbar :x 0 :y 24 :width width :height 40)
                (when toolbar-bottom-rule
                  (cl-fltk:resize-widget toolbar-bottom-rule :x 0 :y 38
                                         :width width :height 2))
               (when lens-name
                 (cl-fltk:resize-widget lens-name :x 160 :y 6
                                         :width (max 120 (- width 170)) :height 28))
               (cl-fltk:resize-widget table :x 0 :y top :width left :height main-height)
               (if comparison-p
                   (progn
                     (cl-fltk:show before-caption)
                     (cl-fltk:show before-canvas)
                     (cl-fltk:resize-widget before-caption :x left :y top
                                            :width pane-width :height caption-height)
                     (cl-fltk:resize-widget before-canvas :x left :y viewer-y
                                            :width pane-width :height viewer-height)
                     (cl-fltk:resize-widget after-caption
                                            :x (+ left pane-width gutter) :y top
                                            :width (- center pane-width gutter)
                                            :height caption-height)
                     (cl-fltk:resize-widget after-canvas
                                            :x (+ left pane-width gutter) :y viewer-y
                                            :width (- center pane-width gutter)
                                            :height viewer-height))
                   (progn
                     (cl-fltk:hide before-caption)
                     (cl-fltk:hide before-canvas)
                     (cl-fltk:resize-widget after-caption :x left :y top
                                            :width center :height caption-height)
                     (cl-fltk:resize-widget after-canvas :x left :y viewer-y
                                            :width center :height viewer-height)))
               (cl-fltk:resize-widget inspector :x (+ left center) :y top
                                      :width right :height main-height)
               (cl-fltk:resize-widget tabs :x 4 :y 40
                                      :width (- right 8)
                                      :height (- main-height 80))
               (dolist (page (list basic-page optics-page effects-page))
                 (cl-fltk:resize-widget page :x 2 :y 24
                                        :width (- right 12)
                                        :height (- main-height 108)))
               (cl-fltk:resize-widget progress :x 0 :y (+ top main-height)
                                      :width 180 :height bottom)
               (cl-fltk:resize-widget status :x 180 :y (+ top main-height)
                                      :width (- width 180) :height bottom)
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
                              (:slider (max 80 (- basis-width 204)))
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
               (cl-fltk:redraw before-canvas)
               (cl-fltk:redraw after-canvas)))
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
        (cl-fltk:add-menu-item menu "File/Open Project" (lambda (&rest ignored)
                                                            (declare (ignore ignored))
                                                            (open-project)))
        (cl-fltk:add-menu-item menu "File/Save Project" (lambda (&rest ignored)
                                                            (declare (ignore ignored))
                                                            (save-project)))
        (cl-fltk:add-menu-item menu "File/Save Project As" (lambda (&rest ignored)
                                                               (declare (ignore ignored))
                                                               (save-project t)))
        (cl-fltk:add-menu-item menu "File/Export Selected" (lambda (&rest ignored)
                                                               (declare (ignore ignored))
                                                               (render-selected)))
        (cl-fltk:add-menu-item menu "File/Quit" (lambda (&rest ignored)
                                                    (declare (ignore ignored))
                                                    (cl-fltk:quit)))
        (cl-fltk:add-menu-item menu "Edit/Reset Photo"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (gui-model-reset-selected model)
                                 (sync-controls)
                                 (update-table)
                                 (schedule-edited-preview)))
        (cl-fltk:add-menu-item menu "Edit/Reset Defaults"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (setf (project-defaults project)
                                       (gui-default-processing-settings))
                                 (sync-controls)
                                 (update-table)
                                 (schedule-edited-preview)))
        (cl-fltk:add-menu-item menu "View/Before and After"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (toggle-comparison)))
        (cl-fltk:add-menu-item menu "View/Refresh Preview"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (schedule-initial-preview)))
        (cl-fltk:add-menu-item menu "Process/Export Selected"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (render-selected)))
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
                   button)))
          (rule 6 10 2 18 130 130 130)
          (rule 10 10 2 18 245 245 245)
          (toolbar-button 18 :open "Open RAW photographs" #'open-photo)
          (toolbar-button 48 :folder-open "Open project" #'open-project)
          (rule 82 7 1 24 150 150 150)
          (toolbar-button 90 :export "Export selected photograph" #'render-selected)
          (toolbar-button 120 :pipeline "Show or hide Before and After" #'toggle-comparison)
          (setf toolbar-bottom-rule (rule 0 38 1280 2 145 145 145)))
        (setf lens-name (cl-fltk:make-label :parent toolbar :x 160 :y 6
                                            :width 1094 :height 28
                                            :label "Lens: No photograph selected"))
        (cl-fltk:set-label-font lens-name 1)
        (setf table (cl-fltk:make-record-table
                     :parent window :x 0 :y 64 :width 240 :height 708
                     :columns '((:file "File" 230) (:settings "Settings" 90)
                                (:output "Output" 150))
                     :records (project-table-records project)
                     :callback (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (select-row))))
        (setf before-caption
              (cl-fltk:make-label :parent window :x 240 :y 64
                                  :width 357 :height 22
                                  :label "Before · neutral RAW")
              after-caption
              (cl-fltk:make-label :parent window :x 603 :y 64
                                  :width 357 :height 22
                                  :label "After · current adjustments"))
        (flet ((make-preview-canvas (role x)
                 (cl-fltk:make-canvas
                  :parent window :x x :y 86 :width 357 :height 686
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
                          (draw-preview-file widget path)
                          (progn
                            (cl-fltk:draw-color-rgb :red 205 :green 208 :blue 210)
                            (cl-fltk:draw-text "Developing RAW preview..."
                                               (+ (cl-fltk:widget-x widget) 20)
                                               (+ (cl-fltk:widget-y widget) 36)))))))))
          (setf before-canvas (make-preview-canvas :before 240)
                after-canvas (make-preview-canvas :after 603)))
        (setf inspector (cl-fltk:make-panel :parent window :x 960 :y 64
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
                                                  :label "Effects"))
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
        (setf lut-name
              (register-inspector
               (cl-fltk:make-output :parent effects-page :x 110 :y 12
                                    :width 190 :height 26
                                    :value "No LUT selected")
               110 12 :control 26 :page))
        (setf lut-menu
              (register-inspector
               (cl-fltk:make-menu-button :parent effects-page :x 8 :y 44
                                          :width 292 :height 26
                                          :label "Select LUT")
               8 44 :fill 26 :page))
        (dolist (path (gui-bundled-lut-paths))
          (let ((selected-path (namestring path))
                (label (substitute #\Space #\_ (pathname-name path))))
            (cl-fltk:add-menu-item
             lut-menu (format nil "Bundled/~:(~A~)" label)
             (lambda (&rest ignored)
               (declare (ignore ignored))
               (gui-model-set-setting model :lut-path selected-path)
               (sync-controls)
               (schedule-edited-preview)))))
        (cl-fltk:add-menu-item lut-menu "Browse..."
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (choose-lut)))
        (cl-fltk:add-menu-item lut-menu "None"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (clear-lut)))
        (make-number-field :lut-strength "LUT strength" 0 1 0.05 76
                           effects-page)
        (make-number-field :grain-amount "Grain amount" 0 1 0.05 108
                           effects-page)
        (make-number-field :grain-size "Grain size" 0.25 16 0.25 140
                           effects-page)
        (register-inspector
         (cl-fltk:make-button :parent inspector :x 12 :y 674
                              :width 140 :height 26 :label "Reset photo"
                              :callback (lambda (&rest ignored)
                                          (declare (ignore ignored))
                                          (gui-model-reset-selected model)
                                          (sync-controls)
                                          (update-table)
                                          (schedule-edited-preview)))
         12 :action-row :half-left 26)
        (register-inspector
         (cl-fltk:make-button :parent inspector :x 166 :y 674
                              :width 142 :height 26 :label "Export selected"
                              :callback (lambda (&rest ignored)
                                          (declare (ignore ignored))
                                          (render-selected)))
         166 :action-row :half-right 26)
        (setf progress (cl-fltk:make-progress :parent window :x 0 :y 772
                                              :width 180 :height 28 :value "0")
              status (cl-fltk:make-status-bar :parent window :x 180 :y 772
                                               :width 1100 :height 28
                                               :value "Ready"))
        (cl-fltk:on-resize window #'layout-ui)
        (sync-controls)
        (layout-ui)
        (setf poll-id (cl-fltk:add-timeout 0.08d0 #'poll :repeat t))
        (when (selected-job)
          (cl-fltk:table-select-row table 0)
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
