(in-package #:orfeus/gui)

(defparameter *preview-debounce-seconds* 0.25d0
  "Delay used to coalesce interactive control changes.")

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

(defun preview-pathname (preview-directory index job kind)
  (merge-pathnames
   (make-pathname :name (format nil "~(~A~)-~D-~A" kind index
                                (pathname-name (photo-job-input-path job)))
                  :type "jpg")
   preview-directory))

(defun preview-status-text (model kind)
  (let ((temperature (gui-model-setting model :white-balance-temperature))
        (lut (gui-model-setting model :lut-path))
        (strength (gui-model-setting model :lut-strength)))
    (format nil "~A preview  |  WB: ~A  |  LUT: ~A"
            (ecase kind (:embedded "Embedded") (:raw "RAW"))
            (if temperature "Custom" "As shot")
            (if (and lut (plusp strength))
                (format nil "~A (~D%%)" (file-namestring lut)
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
           (preview-directory (make-gui-preview-directory))
           window menu toolbar table canvas inspector status progress preview-file
           controls inspector-items lut-name lut-menu wb-choice target-choice
           debounce-id poll-id before-p
           (preview-generation 0))
      (labels
          ((set-status (text)
             (setf (cl-fltk:value status) text))
           (selected-job ()
             (gui-model-selected-job model))
           (clear-preview ()
             (when preview-file
               (forget-preview-file preview-file)
               (setf preview-file nil))
             (when canvas (cl-fltk:redraw canvas)))
           (publish-preview (path)
             (when (and preview-file (not (equal preview-file path)))
               (forget-preview-file preview-file))
             (forget-preview-file path)
             (setf preview-file path)
             (cl-fltk:redraw canvas))
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
                     (if path (file-namestring path) "No LUT selected"))))
           (replace-project (new-project &optional path)
             (incf preview-generation)
             (clear-preview)
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
             (let ((path (cl-fltk:choose-file
                          :title "Open RAW photograph"
                          :filter "RAW photographs\t*.{orf,ORF,dng,DNG}")))
               (when path (replace-project (gui-photo-project path)))))
           (open-project ()
             (let ((path (cl-fltk:choose-file
                          :title "Open Orfeus project"
                          :filter "Orfeus project\t*.sexp")))
               (when path (replace-project (project-read path) (pathname path)))))
           (save-project (&optional choose-p)
             (let ((path (or (and (not choose-p) (gui-model-project-path model))
                             (cl-fltk:choose-save-file
                              :title "Save Orfeus project"
                              :filter "Orfeus project\t*.sexp"
                              :preset-file "project.sexp"))))
               (when path
                 (project-write project path)
                 (setf (gui-model-project-path model) (pathname path))
                 (set-status "Project saved"))))
           (current-settings ()
             (if before-p
                 (make-processing-settings)
                 (processing-settings-with-overrides
                  (project-defaults project)
                  (photo-job-overrides (selected-job)))))
           (enqueue-preview (initial-p)
             (let* ((job (selected-job))
                    (index (gui-model-selected-index model))
                    (generation preview-generation)
                    (input (and job (photo-job-input-path job)))
                    (settings (and job (current-settings))))
               (when job
                 (enqueue-gui-task
                  queue :preview
                  (lambda ()
                    (when initial-p
                      (let ((embedded (preview-pathname preview-directory index job :embedded)))
                        (handler-case
                            (progn
                              (photo-extract-embedded-preview input embedded
                                                              :if-exists :supersede)
                              (queue-event queue
                                           (list :preview generation index job embedded
                                                 :embedded)))
                          (error () nil))))
                    (queue-event queue
                                 (list :status generation
                                       (format nil "Developing ~A..."
                                               (file-namestring input))))
                    (let ((raw (preview-pathname preview-directory index job :raw)))
                      (render-preview input raw settings :if-exists :supersede)
                      (queue-event queue
                                   (list :preview generation index job raw :raw))))
                  :replace-kind :preview))))
           (schedule-initial-preview ()
             (when debounce-id
               (ignore-errors (cl-fltk:remove-timeout debounce-id))
               (setf debounce-id nil))
             (incf preview-generation)
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
                 (clear-preview)
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
                          :title "Choose 3D LUT" :filter "Cube LUT\t*.{cube,CUBE}")))
               (when path
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
           (register-inspector (widget x y width-mode height)
             (push (list widget x y width-mode height) inspector-items)
             widget)
           (register-field (field y)
             (register-inspector (cl-fltk:field-label field) 12 y 112 26)
             (register-inspector (cl-fltk:field-control field) 132 y :control 26)
             field)
           (make-number-field (key label minimum maximum step y)
             (let* ((label-widget
                      (cl-fltk:make-label :parent inspector :x 12 :y y
                                          :width 105 :height 26 :label label))
                    (callback (lambda (widget event value)
                                (declare (ignore event value))
                                (setting-changed key widget)))
                    (spinner (cl-fltk:make-spinner
                              :parent inspector :x 270 :y y
                              :width 78 :height 26 :callback callback))
                    (slider (unless (member key '(:white-balance-temperature
                                                  :grain-size))
                              (cl-fltk:make-slider
                               :parent inspector :x 117 :y y
                               :width 140 :height 26 :callback callback))))
               (dolist (widget (remove nil (list slider spinner)))
                 (cl-fltk:set-range widget minimum maximum)
                 (cl-fltk:set-step widget step)
                 (push (list key widget) controls))
               (register-inspector label-widget 12 y 105 26)
               (if slider
                   (progn
                     (register-inspector slider 117 y :slider 26)
                     (register-inspector spinner 270 y :number 26))
                   (register-inspector spinner 117 y :control 26))
               spinner))
           (layout-ui (&optional ignored)
             (declare (ignore ignored))
             (let* ((width (cl-fltk:widget-width window))
                    (height (cl-fltk:widget-height window))
                    (top 64) (bottom 28)
                    (main-height (max 200 (- height top bottom)))
                    (left (min 260 (max 190 (floor width 5))))
                    (right (min 400 (max 360 (floor width 3))))
                    (center (max 300 (- width left right))))
               (cl-fltk:resize-widget menu :x 0 :y 0 :width width :height 24)
               (cl-fltk:resize-widget toolbar :x 0 :y 24 :width width :height 40)
               (cl-fltk:resize-widget table :x 0 :y top :width left :height main-height)
               (cl-fltk:resize-widget canvas :x left :y top :width center :height main-height)
               (cl-fltk:resize-widget inspector :x (+ left center) :y top
                                      :width right :height main-height)
               (cl-fltk:resize-widget progress :x 0 :y (+ top main-height)
                                      :width 180 :height bottom)
               (cl-fltk:resize-widget status :x 180 :y (+ top main-height)
                                      :width (- width 180) :height bottom)
               (dolist (item inspector-items)
                 (destructuring-bind (widget x y width-mode item-height) item
                   (let ((item-width
                           (case width-mode
                             (:control (max 100 (- right 144)))
                             (:slider (max 80 (- right 217)))
                             (:number 78)
                             (:fill (max 100 (- right 24)))
                             (:half-left (max 70 (floor (- right 30) 2)))
                             (:half-right (max 70 (floor (- right 30) 2)))
                             (otherwise width-mode)))
                         (item-x
                           (case width-mode
                             (:number (- right 90))
                             (:half-right (+ 18 (floor (- right 30) 2)))
                             (otherwise x))))
                     (cl-fltk:resize-widget
                      widget :x item-x :y y
                      :width item-width :height item-height))))
               (cl-fltk:redraw canvas)))
           (poll ()
             (dolist (event (drain-events queue))
               (case (first event)
                 (:status
                  (when (or (null (second event))
                            (= (second event) preview-generation))
                    (set-status (third event))))
                 (:preview
                  (when (gui-preview-event-current-p model event preview-generation)
                    (publish-preview (fifth event))
                    (set-status (preview-status-text model (sixth event)))))
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
                                 (setf before-p (not before-p))
                                 (schedule-edited-preview)))
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
        (setf toolbar (cl-fltk:make-group :parent window :x 0 :y 24
                                          :width 1280 :height 40))
        (flet ((toolbar-button (x width label icon action)
                 (let ((button (cl-fltk:make-button
                                :parent toolbar :x x :y 6 :width width :height 28
                                :label label
                                :callback (lambda (&rest ignored)
                                            (declare (ignore ignored))
                                            (funcall action)))))
                   (cl-fltk:set-stock-icon button icon)
                   button)))
          (toolbar-button 6 115 "Open photo" :open #'open-photo)
          (toolbar-button 127 125 "Open project" :folder-open #'open-project)
          (toolbar-button 258 105 "Export" :export #'render-selected)
          (toolbar-button 369 135 "Before / after" :reload
                          (lambda ()
                            (setf before-p (not before-p))
                            (schedule-edited-preview))))
        (setf table (cl-fltk:make-record-table
                     :parent window :x 0 :y 64 :width 240 :height 708
                     :columns '((:file "File" 230) (:settings "Settings" 90)
                                (:output "Output" 150))
                     :records (project-table-records project)
                     :callback (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (select-row))))
        (setf canvas
              (cl-fltk:make-canvas
               :parent window :x 240 :y 64 :width 720 :height 708
               :callback
               (lambda (widget event value)
                 (declare (ignore event value))
                 (cl-fltk:draw-color-rgb :red 30 :green 32 :blue 34)
                 (cl-fltk:draw-filled-rect
                  (cl-fltk:widget-x widget) (cl-fltk:widget-y widget)
                  (cl-fltk:widget-width widget) (cl-fltk:widget-height widget))
                 (if preview-file
                     (draw-preview-file widget preview-file)
                     (progn
                       (cl-fltk:draw-color-rgb :red 205 :green 208 :blue 210)
                       (cl-fltk:draw-text "Open a RAW photograph"
                                          (+ (cl-fltk:widget-x widget) 28)
                                          (+ (cl-fltk:widget-y widget) 42)))))))
        (setf inspector (cl-fltk:make-panel :parent window :x 960 :y 64
                                            :width 320 :height 708
                                            :label "Develop"))
        (flet ((section-label (text y)
                 (register-inspector
                  (cl-fltk:make-label :parent inspector :x 12 :y y
                                      :width 296 :height 22 :label text)
                  12 y :fill 22)))
          (section-label "BASIC" 14)
          (setf target-choice
                (cl-fltk:field-control
                 (register-field
                  (cl-fltk:make-labeled-choice
                   :parent inspector :x 12 :y 40 :width 296 :height 26
                   :label "Edit" :label-width 112
                   :items '("Photo" "Defaults")
                   :callback (lambda (widget event value)
                               (declare (ignore event value))
                               (setf (gui-model-edit-target model)
                                     (if (string-equal (cl-fltk:value widget)
                                                       "Defaults")
                                         :defaults :photo))
                               (sync-controls)))
                  40)))
          (setf wb-choice
                (cl-fltk:field-control
                 (register-field
                  (cl-fltk:make-labeled-choice
                   :parent inspector :x 12 :y 72 :width 296 :height 26
                   :label "White balance" :label-width 112
                   :items '("As shot" "Custom")
                   :callback (lambda (widget event value)
                               (declare (ignore event value))
                               (set-wb-mode widget)))
                  72)))
          (make-number-field :white-balance-temperature "Temperature (K)" 2000 15000 50 104)
          (make-number-field :white-balance-tint "Tint" -5 5 0.05 136)
          (make-number-field :exposure "Exposure EV" -10 10 0.1 168)
          (make-number-field :noise-reduction "Noise reduction" 0 1 0.05 200)
          (section-label "OPTICS" 238)
          (let ((lens (register-inspector
                       (cl-fltk:make-check-button
                        :parent inspector :x 12 :y 264 :width 296 :height 26
                        :label "Apply lens distortion correction"
                        :callback (lambda (widget event value)
                                    (declare (ignore event value))
                                    (gui-model-set-setting
                                     model :lens-correction-p
                                     (string/= "0" (cl-fltk:value widget)))
                                    (schedule-edited-preview)))
                       12 264 :fill 26)))
            (push (list :lens-correction-p lens) controls))
          (let ((tca (register-inspector
                      (cl-fltk:make-check-button
                       :parent inspector :x 12 :y 294 :width 296 :height 26
                       :label "Remove chromatic aberration"
                       :callback (lambda (widget event value)
                                   (declare (ignore event value))
                                   (gui-model-set-setting
                                    model :chromatic-aberration-correction-p
                                    (string/= "0" (cl-fltk:value widget)))
                                   (schedule-edited-preview)))
                      12 294 :fill 26)))
            (push (list :chromatic-aberration-correction-p tca) controls))
          (section-label "CREATIVE" 332)
          (register-inspector
           (cl-fltk:make-label :parent inspector :x 12 :y 358
                               :width 112 :height 26 :label "3D LUT")
           12 358 112 26)
          (setf lut-name
                (register-inspector
                 (cl-fltk:make-output :parent inspector :x 132 :y 358
                                      :width 176 :height 26
                                      :value "No LUT selected")
                 132 358 :control 26))
          (setf lut-menu
                (register-inspector
                 (cl-fltk:make-menu-button :parent inspector :x 12 :y 390
                                            :width 296 :height 26
                                            :label "Select LUT")
                 12 390 :fill 26))
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
          (make-number-field :lut-strength "LUT strength" 0 1 0.05 422)
          (make-number-field :grain-amount "Grain amount" 0 1 0.05 454)
          (make-number-field :grain-size "Grain size" 0.25 16 0.25 486)
          (register-inspector
           (cl-fltk:make-button :parent inspector :x 12 :y 530
                                :width 140 :height 28 :label "Reset photo"
                                :callback (lambda (&rest ignored)
                                            (declare (ignore ignored))
                                            (gui-model-reset-selected model)
                                            (sync-controls)
                                            (update-table)
                                            (schedule-edited-preview)))
           12 530 :half-left 28)
          (register-inspector
           (cl-fltk:make-button :parent inspector :x 166 :y 530
                                :width 142 :height 28 :label "Export selected"
                                :callback (lambda (&rest ignored)
                                            (declare (ignore ignored))
                                            (render-selected)))
           166 530 :half-right 28))
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
          (clear-preview-cache)
          (ignore-errors (delete-gui-preview-directory preview-directory))
          (when window (ignore-errors (cl-fltk:destroy window))))))))

(defun main (&optional pathname)
  "Launch Orfeus with optional PHOTO or PROJECT PATHNAME."
  (let ((argument (or pathname (first (uiop:command-line-arguments)))))
    (run-gui argument)))
