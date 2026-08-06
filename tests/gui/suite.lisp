(defpackage #:orfeus/gui-tests
  (:use #:cl)
  (:export #:run-tests))

(in-package #:orfeus/gui-tests)

(defun check (condition control &rest arguments)
  (unless condition (error (apply #'format nil control arguments))))

(defun test-model-settings ()
  (let* ((job (orfeus:make-photo-job :input-path #P"one.orf" :overrides '(:exposure 1.0)))
         (project (orfeus:make-project :output-directory #P"exports/" :photos (list job)))
         (model (orfeus/gui:make-gui-model :project project)))
    (check (= 1.0 (orfeus/gui:gui-model-setting model :exposure)) "Effective override missing")
    (orfeus/gui:gui-model-set-setting model :grain-amount 0.4)
    (check (= 0.4 (getf (orfeus:photo-job-overrides job) :grain-amount)) "Photo edit missing")
    (setf (orfeus/gui:gui-model-edit-target model) :defaults)
    (orfeus/gui:gui-model-set-setting model :noise-reduction 0.7)
    (check (= 0.7 (orfeus:processing-settings-noise-reduction (orfeus:project-defaults project))) "Default edit missing")
    (orfeus/gui:gui-model-set-setting model :lens-correction-strength 0.6)
    (check (= 0.6 (orfeus:processing-settings-lens-correction-strength
                   (orfeus:project-defaults project)))
           "Lens correction strength edit missing")
    (orfeus/gui:gui-model-reset-selected model)
    (check (null (orfeus:photo-job-overrides job)) "Reset did not clear overrides")))

(defun test-undo-history ()
  (let* ((job (orfeus:make-photo-job :input-path #P"one.orf"
                                     :overrides '(:exposure 1.0)))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos (list job)))
         (model (orfeus/gui:make-gui-model :project project))
         (exposure (lambda ()
                     (getf (orfeus:photo-job-overrides
                            (orfeus/gui:gui-model-selected-job model))
                           :exposure))))
    (check (not (orfeus/gui:gui-model-can-undo-p model))
           "A fresh model claimed something to undo")
    ;; One discrete edit steps back and forward again.
    (let ((orfeus/gui::*undo-coalesce-seconds* 0))
      (orfeus/gui:gui-model-set-setting model :exposure 2.0)
      (check (= 2.0 (funcall exposure)) "Edit did not take effect")
      (check (orfeus/gui:gui-model-can-undo-p model) "Edit was not recorded")
      (check (orfeus/gui:gui-model-undo model) "Undo refused a recorded edit")
      (check (= 1.0 (funcall exposure)) "Undo did not restore the value")
      (check (orfeus/gui:gui-model-can-redo-p model) "Undo offered no redo")
      (check (orfeus/gui:gui-model-redo model) "Redo refused an undone edit")
      (check (= 2.0 (funcall exposure)) "Redo did not reapply the value")
      ;; A fresh edit abandons the redo branch.
      (orfeus/gui:gui-model-undo model)
      (orfeus/gui:gui-model-set-setting model :exposure 3.0)
      (check (not (orfeus/gui:gui-model-can-redo-p model))
             "A new edit kept a stale redo step")
      ;; Distinct controls are distinct steps even back to back.
      (orfeus/gui:gui-model-set-setting model :grain-amount 0.5)
      (orfeus/gui:gui-model-undo model)
      (check (and (null (getf (orfeus:photo-job-overrides
                               (orfeus/gui:gui-model-selected-job model))
                              :grain-amount))
                  (= 3.0 (funcall exposure)))
             "Undo of one control disturbed another"))
    ;; Undo restores a copy, so the stack cannot be reached through the project.
    (let ((depth (length (orfeus/gui::gui-model-undo-stack model))))
      (setf (orfeus:photo-job-overrides
             (orfeus/gui:gui-model-selected-job model))
            '(:exposure 99.0))
      (orfeus/gui:gui-model-undo model)
      (check (/= 99.0 (funcall exposure))
             "A snapshot shared structure with the live project")
      (check (= (1- depth) (length (orfeus/gui::gui-model-undo-stack model)))
             "Undo did not consume exactly one step")))
  ;; A run of edits to one control is one undo, however many ticks it took.
  (let* ((job (orfeus:make-photo-job :input-path #P"one.orf"))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos (list job)))
         (model (orfeus/gui:make-gui-model :project project)))
    (dotimes (tick 20)
      (orfeus/gui:gui-model-set-setting model :exposure (/ tick 10.0)))
    (check (= 1 (length (orfeus/gui::gui-model-undo-stack model)))
           "A slider drag did not collapse into one undo step")
    (orfeus/gui:gui-model-undo model)
    (check (null (getf (orfeus:photo-job-overrides
                        (orfeus/gui:gui-model-selected-job model))
                       :exposure))
           "Undoing a drag did not return to before the drag"))
  ;; Adding a node and writing its parameters is one action, so it is one undo.
  (let* ((job (orfeus:make-photo-job :input-path #P"one.orf"
                                     :graph (orfeus:default-processing-graph)))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos (list job)))
         (model (orfeus/gui:make-gui-model :project project))
         (before (length (orfeus:processing-graph-nodes
                          (orfeus:photo-job-graph job)))))
    (orfeus/gui:gui-model-add-node model :exposure)
    (check (= 1 (length (orfeus/gui::gui-model-undo-stack model)))
           "Adding a node recorded more than one step")
    (let ((node (orfeus/gui:gui-model-selected-node model)))
      (check node "Adding a node did not select it")
      (orfeus/gui:gui-model-undo model)
      (check (= before (length (orfeus:processing-graph-nodes
                                (orfeus:photo-job-graph
                                 (orfeus/gui:gui-model-selected-job model)))))
             "Undo did not remove the added node")
      (orfeus/gui:gui-model-redo model)
      ;; The restored graph holds different objects, so the selection has to be
      ;; recovered by id rather than by identity.
      (let ((restored (orfeus/gui:gui-model-selected-node model)))
        (check restored "Redo lost the node selection")
        (check (= (orfeus:graph-node-id node) (orfeus:graph-node-id restored))
               "Redo selected a different node than the edit had"))))
  ;; History is bounded, and opening something else starts a new session.
  (let* ((project (orfeus:make-project
                   :output-directory #P"exports/"
                   :photos (list (orfeus:make-photo-job :input-path #P"one.orf"))))
         (model (orfeus/gui:make-gui-model :project project))
         (orfeus/gui::*undo-coalesce-seconds* 0)
         (orfeus/gui::*undo-depth* 4))
    (dotimes (step 10)
      (orfeus/gui:gui-model-set-setting model :exposure (float step)))
    (check (= 4 (length (orfeus/gui::gui-model-undo-stack model)))
           "History grew past its depth limit")
    (orfeus/gui::gui-model-replace-project model (orfeus/gui::gui-empty-project))
    (check (not (orfeus/gui:gui-model-can-undo-p model))
           "Opening a project left the previous one's history behind")))

(defun test-graph-node-placement ()
  "Editor positions must follow graph order, or reordering looks inert."
  (let* ((graph (orfeus:default-processing-graph)))
    (orfeus:graph-insert-node graph (orfeus:processing-graph-output graph) :film)
    (let* ((job (orfeus:make-photo-job :input-path #P"one.orf" :graph graph))
           (project (orfeus:make-project :output-directory #P"exports/"
                                         :photos (list job)))
           (model (orfeus/gui:make-gui-model :project project))
           (rows (lambda ()
                   (orfeus/gui::ensure-graph-node-positions
                    (orfeus:processing-graph-nodes graph))
                   (mapcar (lambda (node)
                             (list (orfeus:graph-node-kind node)
                                   (second (orfeus:graph-node-position node))))
                           (orfeus:processing-graph-nodes graph)))))
      (funcall rows)
      ;; A node inserted mid-chain must not land on top of the node below it.
      (let ((added (orfeus/gui::gui-model-add-node model :white-balance)))
        (check added "Adding a white balance node returned nothing")
        (let* ((placed (funcall rows))
               (ys (mapcar #'second placed)))
          (check (= (length ys) (length (remove-duplicates ys)))
                 "An inserted node shares a row with another node")
          (check (equal '(:optics :noise-reduction :white-balance :film)
                        (mapcar #'first placed))
                 "Insertion put the node in the wrong place in the chain"))
        ;; Reordering has to move the boxes, not only rewire them.
        (let ((before (funcall rows)))
          (check (orfeus/gui::gui-model-move-node model added :earlier)
                 "Moving a node earlier reported failure")
          (let ((after (funcall rows)))
            (check (equal '(:optics :white-balance :noise-reduction :film)
                          (mapcar #'first after))
                   "Moving earlier did not reorder the chain")
            (check (not (equal before after))
                   "Moving earlier left every box where it was")
            (check (equal (sort (mapcar #'second before) #'<)
                          (sort (mapcar #'second after) #'<))
                   "Moving earlier invented or lost a row")
            (check (= (second (second after)) (second (second before)))
                   "The row a moved node landed in is not its neighbour's")))
        (check (orfeus/gui::gui-model-move-node model added :later)
               "Moving a node later reported failure")
        (check (equal '(:optics :noise-reduction :white-balance :film)
                      (mapcar #'first (funcall rows)))
               "Moving later did not restore the original order")
        ;; Every kind the menu offers must actually insert.
        (dolist (kind (orfeus:graph-node-kinds))
          (check (orfeus/gui::gui-model-add-node model kind)
                 (format nil "Adding a ~S node returned nothing" kind)))))))

(defun test-thumbnail-context-menu ()
  "Right-clicking must target a row without changing what is being previewed."
  ;; The menu offers interning, and the action list and label list stay aligned.
  (let ((actions (mapcar #'car orfeus/gui::*thumbnail-context-actions*)))
    (check (member :intern actions) "The sidebar menu cannot intern")
    (check (= (length actions)
              (length (orfeus/gui::thumbnail-context-menu-items)))
           "Sidebar menu labels and actions are different lengths")
    (loop for action in actions
          for index from 0
          do (check (eq action (orfeus/gui::thumbnail-context-action-at index))
                    (format nil "Sidebar menu index ~D maps to the wrong action"
                            index))))
  (check (null (orfeus/gui::thumbnail-context-action-at nil))
         "A dismissed sidebar menu chose an action")
  ;; An already-selected row keeps the whole selection, so a menu action still
  ;; applies to every photograph the user had picked. An unselected row becomes
  ;; the only target.
  (check (equal '(1 2 3) (orfeus/gui::thumbnail-context-selection '(1 2 3) 2))
         "Right-clicking inside a selection discarded it")
  (check (equal '(5) (orfeus/gui::thumbnail-context-selection '(1 2 3) 5))
         "Right-clicking outside a selection did not retarget")
  ;; Retargeting must not move the preview anchor. GUI-MODEL-SET-SELECTED-INDICES
  ;; does move it, which is exactly why the context path restores it.
  (let* ((photos (loop for index below 4
                       collect (orfeus:make-photo-job
                                :input-path (make-pathname
                                             :name (format nil "p~D" index)
                                             :type "orf"))))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos photos))
         (model (orfeus/gui:make-gui-model :project project)))
    (orfeus/gui::gui-model-set-selected-indices model '(0))
    (check (= 0 (orfeus/gui::gui-model-selected-index model))
           "Selecting the first row did not anchor there")
    (let ((anchor (orfeus/gui::gui-model-selected-index model)))
      (orfeus/gui::gui-model-set-selected-indices model '(3))
      (check (= 3 (orfeus/gui::gui-model-selected-index model))
             "Setting indices no longer moves the anchor, so the context path
              no longer needs to restore it")
      (setf (orfeus/gui::gui-model-selected-index model) anchor)
      (check (eq (first photos) (orfeus/gui:gui-model-selected-job model))
             "Restoring the anchor did not keep the previewed photograph"))))

(defun test-export-destination-anchor ()
  "Exports go beside the photographs until a project says otherwise.

Before a project exists there is nowhere else to put them. Saving the project
says where the work lives, so exports follow it — and only when the project
moves, so a destination the user picked survives every later save."
  (let* ((photo #P"/shoots/incoming/_6040106.ORF")
         (project (orfeus/gui::gui-photos-project (list photo)))
         (model (orfeus/gui:make-gui-model :project project)))
    (check (equal #P"/shoots/incoming/orfeus-exports/"
                  (orfeus:project-output-directory project))
           "Exports did not start beside the photographs")
    (orfeus/gui::gui-model-anchor-export-directory
     model #P"/home/lukas/june/project.sexp")
    (check (equal #P"/home/lukas/june/orfeus-exports/"
                  (orfeus:project-output-directory project))
           "Saving a project did not move exports beside it")
    ;; Anchoring is only invoked when the project moves, so a chosen
    ;; destination is not clobbered by an ordinary save.
    (setf (orfeus:project-output-directory project) #P"/home/lukas/deliver/")
    (check (equal #P"/home/lukas/deliver/"
                  (orfeus:project-output-directory project))
           "A chosen destination did not stick"))
  ;; Importing into a named project must leave its exports where they are: the
  ;; point of making an empty project first is to settle that before importing.
  (let* ((project (orfeus/gui::gui-empty-project))
         (model (orfeus/gui:make-gui-model :project project)))
    (orfeus/gui::gui-model-replace-project model project
                                           #P"/home/lukas/june/project.sexp")
    (orfeus/gui::gui-model-anchor-export-directory
     model #P"/home/lukas/june/project.sexp")
    (orfeus/gui::gui-model-add-photos model (list #P"/shoots/incoming/a.orf"))
    (check (= 1 (length (orfeus:project-photos project)))
           "Importing into an empty named project added nothing")
    (check (equal #P"/home/lukas/june/orfeus-exports/"
                  (orfeus:project-output-directory project))
           "Importing dragged a named project's exports to the photographs"))
  ;; With no project, the photographs are the only anchor there is.
  (let* ((project (orfeus/gui::gui-empty-project))
         (model (orfeus/gui:make-gui-model :project project)))
    (orfeus/gui::gui-model-add-photos model (list #P"/shoots/incoming/a.orf"))
    (check (equal #P"/shoots/incoming/orfeus-exports/"
                  (orfeus:project-output-directory project))
           "Importing without a project did not anchor to the photographs")))

(defun test-curve-spline-shapes ()
  "The editor's spline matches the executor's, for any number of points.

The panel draws with this function and the render evaluates its own copy, so a
disagreement means the curve on screen is not the curve applied. Both are held
flat outside the control points, which is what makes dragging a white point
inward a gain rather than an extrapolation."
  (let ((endpoints orfeus:*identity-curve-points*))
    (check (= 2 (floor (length endpoints) 2))
           "A fresh channel did not start on its two endpoints")
    (dolist (x '(0.0 0.25 0.5 0.75 1.0))
      (check (< (abs (- x (orfeus/gui::curve-spline-value endpoints x))) 1.0d-6)
             "The two-point identity did not pass its input through")))
  ;; A white point pulled in to 0.6 gains everything below it and holds every
  ;; input above it at full output.
  (let ((gained '(0.0 0.0 0.6 1.0)))
    (check (< (abs (- 1.0 (orfeus/gui::curve-spline-value gained 0.6))) 1.0d-6)
           "The moved white point did not reach full output")
    (check (< (abs (- 1.0 (orfeus/gui::curve-spline-value gained 0.85))) 1.0d-6)
           "Input above the white point was not held flat")
    (check (> (orfeus/gui::curve-spline-value gained 0.3) 0.3)
           "Pulling the white point in did not lift the midtones"))
  ;; A black point pulled up lifts the shadows and holds below itself.
  (let ((lifted '(0.2 0.15 1.0 1.0)))
    (check (< (abs (- 0.15 (orfeus/gui::curve-spline-value lifted 0.0))) 1.0d-6)
           "Input below the black point was not held flat"))
  ;; Interior points stay monotone rather than overshooting between them.
  (let ((shaped '(0.0 0.0 0.3 0.5 0.62 0.55 1.0 1.0)))
    (loop for step below 40
          for x = (/ step 39.0)
          for y = (orfeus/gui::curve-spline-value shaped x)
          do (check (<= 0.0 y 1.0) "The spline left the unit range")))
  (let ((many (loop for index below 8
                    append (list (/ index 7.0) (/ index 7.0)))))
    (check (< (abs (- 0.5 (orfeus/gui::curve-spline-value many 0.5))) 1.0d-3)
           "An eight-point diagonal was not the identity")))

(defun test-viewport-render-bound ()
  "The viewport render is sized for the canvas, and grows only with zoom.

Rendering it at sensor resolution was the most expensive thing Orfeus did: an
80 MP frame was developed, denoised and JPEG-encoded at 10368 pixels wide so a
1400-pixel canvas could show it, which cost 3.4 seconds per interactive tick
against 45 milliseconds for a canvas-sized render."
  (check (= 1400 (orfeus/gui::gui-preview-bound 1400 1d0))
         "A fitted view did not render at the canvas size")
  ;; Zooming in is the one time the user is asking to see real detail, so any
  ;; zoom past fit gets real pixels rather than the fit proxy stretched. The
  ;; rungs are powers of two, so everything in (1, 2] shares one render and
  ;; scrolling within that range costs nothing.
  (check (= 2800 (orfeus/gui::gui-preview-bound 1400 1.4d0))
         "Zooming past fit reused the proxy instead of rendering real pixels")
  (check (= (orfeus/gui::gui-preview-bound 1400 1.4d0)
            (orfeus/gui::gui-preview-bound 1400 2d0))
         "Two zooms on the same rung did not share a render")
  (check (= 5600 (orfeus/gui::gui-preview-bound 1400 2.1d0))
         "Crossing a doubling did not climb to the next rung")
  ;; Zooming out is clamped at fit, and a tiny window still leaves room to
  ;; magnify rather than pinning the bound to a handful of pixels.
  (check (= 1400 (orfeus/gui::gui-preview-bound 1400 0.25d0))
         "Zooming out below fit shrank the render")
  (check (= orfeus/gui::*gui-preview-minimum-bound*
            (orfeus/gui::gui-preview-bound 200 1d0))
         "A small canvas ignored the minimum bound")
  (check (<= (orfeus/gui::gui-preview-bound 1400 10000d0)
             orfeus/gui::*gui-preview-maximum-bound*)
         "An extreme zoom escaped the ceiling"))

(defun test-preset-bulk-application ()
  (let* ((jobs (loop for name in '("one" "two" "three")
                     collect (orfeus:make-photo-job
                              :input-path (make-pathname :name name :type "orf"))))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos jobs))
         (model (orfeus/gui:make-gui-model :project project)))
    (orfeus/gui:gui-model-set-selected-indices model '(0 2))
    (orfeus/gui:gui-model-set-setting model :exposure 1.5)
    (orfeus/gui:gui-model-toggle-stage model :film)
    ;; The gallery is global, so saving builds a preset for the store rather
    ;; than putting it in the project.
    (let ((saved (orfeus/gui:gui-model-save-preset model "Bright")))
      (check (null (orfeus:project-presets project))
             "Saving a preset wrote it into the project")
      (check (string= "Bright" (orfeus:processing-preset-name saved))
             "Saving a preset lost its name")
      (check (equal '(:film)
                    (orfeus:processing-preset-disabled-stages saved))
             "Saving a preset lost its bypass state")
      (check (= 1.5 (orfeus:processing-settings-exposure
                     (orfeus:processing-preset-settings saved)))
             "Saving a preset lost its settings")
      ;; Applying by name still reads a project's own presets, which is how an
      ;; older project file keeps working.
      (setf (orfeus:project-presets project) (list saved)))
    (dolist (job jobs)
      (setf (orfeus:photo-job-disabled-stages job) '()))
    (check (= 2 (orfeus/gui:gui-model-apply-preset model "Bright"))
           "Preset did not report every selected photo")
    (check (= 1.5 (getf (orfeus:photo-job-overrides (first jobs)) :exposure))
           "Preset did not apply to the first selected photo")
    (check (null (orfeus:photo-job-overrides (second jobs)))
           "Preset changed an unselected photo")
    (check (= 1.5 (getf (orfeus:photo-job-overrides (third jobs)) :exposure))
           "Preset did not apply to the last selected photo")
    (check (and (equal '(:film)
                       (orfeus:photo-job-disabled-stages (first jobs)))
                (null (orfeus:photo-job-disabled-stages (second jobs)))
                (equal '(:film)
                       (orfeus:photo-job-disabled-stages (third jobs))))
           "Preset did not apply bypass state only to selected photos")
    (orfeus/gui:gui-model-reset-selected model)
    (check (and (null (orfeus:photo-job-overrides (first jobs)))
                (null (orfeus:photo-job-overrides (third jobs)))
                (null (orfeus:photo-job-disabled-stages (first jobs)))
                (null (orfeus:photo-job-disabled-stages (third jobs))))
           "Bulk reset did not clear all selected photos")))

(defun test-grade-workflow ()
  (let* ((jobs (loop for name in '("one" "two" "three")
                     collect (orfeus:make-photo-job
                              :input-path (make-pathname :name name :type "orf"))))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos jobs))
         (model (orfeus/gui:make-gui-model :project project)))
    (orfeus/gui:gui-model-set-setting model :exposure 1.25)
    (orfeus/gui:gui-model-set-setting model :grain-amount 0.5)
    (check (eq :bypassed (orfeus/gui:gui-model-toggle-stage model :film))
           "Toggling a fresh stage did not report a bypass")
    (check (orfeus/gui:gui-model-stage-bypassed-p model :film)
           "Bypassed stage is not reported as bypassed")
    (check (= 0.0 (orfeus:processing-settings-grain-amount
                   (orfeus/gui:gui-model-render-settings model)))
           "Bypassed film stage still renders grain")
    (check (= 0.5 (orfeus/gui:gui-model-setting model :grain-amount))
           "Bypass forgot the underlying grade value")
    (check (orfeus/gui:gui-model-stage-adjusted-p model :exposure)
           "Adjusted exposure stage is not flagged")
    (check (not (orfeus/gui:gui-model-stage-adjusted-p model :tone))
           "Untouched tone stage is flagged as adjusted")
    (multiple-value-bind (grade disabled)
        (orfeus/gui:gui-model-copy-grade model)
      (orfeus/gui:gui-model-set-selected-indices model '(1 2))
      (check (= 2 (orfeus/gui:gui-model-paste-grade model grade disabled))
             "Paste did not report every selected photo"))
    (check (= 1.25 (getf (orfeus:photo-job-overrides (second jobs)) :exposure))
           "Pasted grade lost the exposure value")
    (check (equal '(:film) (orfeus:photo-job-disabled-stages (third jobs)))
           "Pasted grade lost the bypass list")
    (orfeus/gui:gui-model-set-selected-indices model '(1))
    (let ((preset (orfeus/gui:gui-model-grab-still model)))
      (check (string= "Still 001 (two)" (orfeus:processing-preset-name preset))
             "Grabbed still name was ~S" (orfeus:processing-preset-name preset))
      (check (equal (orfeus:photo-job-input-path (second jobs))
                    (orfeus:processing-preset-source-photo preset))
             "Grabbed still forgot its source photograph")
      (check (= 0.0 (orfeus:processing-settings-grain-amount
                     (orfeus:processing-preset-settings preset)))
             "Grabbed still captured a bypassed stage as active"))
    (orfeus/gui:gui-model-reset-selected model)
    (check (null (orfeus:photo-job-disabled-stages (second jobs)))
           "Reset kept the stage bypass")))

(defun test-node-graph-model ()
  (let* ((jobs (list (orfeus:make-photo-job :input-path #P"one.orf"
                                            :overrides '(:exposure 1.0))
                     (orfeus:make-photo-job :input-path #P"two.orf")))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos jobs))
         (model (orfeus/gui:make-gui-model :project project)))
    (orfeus/gui:gui-model-ensure-graph model)
    (check (orfeus:photo-job-graph (first jobs))
           "Conversion did not store a graph")
    (check (null (orfeus:photo-job-overrides (first jobs)))
           "Conversion kept flat overrides")
    (check (= 1.0 (orfeus/gui:gui-model-setting model :exposure))
           "Converted graph lost the exposure value")
    ;; Editing a stage without a node creates one at the chain end.
    (orfeus/gui:gui-model-set-setting model :tone-shadows 0.4)
    (check (= 0.4 (orfeus/gui:gui-model-setting model :tone-shadows))
           "Tone edit did not round trip through its node")
    ;; Duplicate kinds: a second exposure node edited through selection.
    (let ((node (orfeus/gui:gui-model-add-node model :exposure)))
      (orfeus/gui:gui-model-set-node-params model node '(:exposure 2.0))
      (check (= 2.0 (orfeus/gui:gui-model-setting model :exposure))
             "Selected node did not win key routing")
      (check (= 2 (count :exposure
                         (mapcar #'orfeus:graph-node-kind
                                 (orfeus:processing-graph-nodes
                                  (orfeus:photo-job-graph (first jobs))))))
             "Stacked exposure node is missing"))
    ;; Paste the graph across the selection.
    (orfeus/gui:gui-model-set-selected-indices model '(0 1))
    (let ((graph (orfeus/gui:gui-model-copy-graph model)))
      (check (= 2 (orfeus/gui:gui-model-paste-graph model graph))
             "Graph paste did not cover the selection"))
    (check (orfeus:photo-job-graph (second jobs))
           "Graph paste skipped the second photo")
    ;; Stills carry graphs; smart apply bypasses chosen kinds.
    (orfeus/gui:gui-model-set-selected-indices model '(0))
    (let ((preset (orfeus/gui:gui-model-grab-still model)))
      (check (orfeus:processing-preset-graph preset)
             "Grabbed still lost its node graph")
      (orfeus/gui:gui-model-set-selected-indices model '(1))
      (check (= 1 (orfeus/gui:gui-model-apply-preset-graph
                   model preset :bypass-kinds (list :optics)))
             "Smart apply did not report the selection")
      (let* ((applied (orfeus:photo-job-graph (second jobs)))
             (optics (find :optics (orfeus:processing-graph-nodes applied)
                           :key #'orfeus:graph-node-kind)))
        (check (or (null optics) (orfeus:graph-node-bypassed-p optics))
               "Smart apply left optics active")))
    (orfeus/gui:gui-model-reset-selected model)
    (check (null (orfeus:photo-job-graph (second jobs)))
           "Reset kept the node graph")))

(defun test-output-path-semantics ()
  (let* ((relative (orfeus:make-photo-job :input-path #P"one.orf" :output-path #P"nested/one.jpg"))
         (automatic (orfeus:make-photo-job :input-path #P"two.orf"))
         (absolute (orfeus:make-photo-job :input-path #P"three.orf" :output-path #P"/var/tmp/three.jpg"))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos (list relative automatic absolute)))
         (model (orfeus/gui:make-gui-model :project project)))
    (check (equal #P"exports/nested/one.jpg" (orfeus/gui::gui-photo-output-path model relative))
           "Relative output path did not use project output directory")
    (check (equal #P"exports/two.jpg" (orfeus/gui::gui-photo-output-path model automatic))
           "Automatic output path differs from core batch semantics")
    (check (equal #P"/var/tmp/three.jpg" (orfeus/gui::gui-photo-output-path model absolute))
           "Absolute output path changed")
    (check (orfeus/gui::gui-preview-event-current-p
            model (list :preview 4 0 relative #P"preview.jpg") 4)
           "Matching preview completion was rejected")
    (check (not (orfeus/gui::gui-preview-event-current-p
                 model (list :preview 3 0 relative #P"preview.jpg") 4))
           "Stale preview generation was accepted")
    (setf (orfeus/gui:gui-model-selected-index model) 1)
    (check (not (orfeus/gui::gui-preview-event-current-p
                 model (list :preview 4 0 relative #P"preview.jpg") 4))
           "Preview for the wrong selection was accepted")))

(defun test-preview-job-identity ()
  (let* ((first (orfeus:make-photo-job :input-path #P"duplicate.orf"))
         (second (orfeus:make-photo-job :input-path #P"duplicate.orf"))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos (list first second)))
         (model (orfeus/gui:make-gui-model :project project :selected-index 1)))
    (check (orfeus/gui::gui-preview-event-current-p
            model (list :preview 7 1 second #P"preview.jpg") 7)
           "Selected duplicate-path job preview was rejected")
    (check (not (orfeus/gui::gui-preview-event-current-p
                 model (list :preview 7 0 first #P"preview.jpg") 7))
           "Preview for a different duplicate-path job was accepted")
    (check (not (orfeus/gui::gui-preview-event-current-p
                 model (list :preview 7 1 first #P"preview.jpg") 7))
           "Preview with a mismatched job identity was accepted")))

(defun test-preview-directory ()
  ;; The persistent cache keys previews by content: renames keep the
  ;; fingerprint, and eviction clears out only stale files.
  (let* ((directory (merge-pathnames
                     (format nil "orfeus-preview-cache-~D-~D/"
                             (get-universal-time)
                             (random most-positive-fixnum))
                     (uiop:temporary-directory)))
         (original (merge-pathnames "photo-a.bin" directory))
         (renamed (merge-pathnames "photo-b.bin" directory)))
    (ensure-directories-exist directory)
    (unwind-protect
         (progn
           (with-open-file (stream original :direction :output
                                            :element-type '(unsigned-byte 8))
             (dotimes (index 200000)
               (write-byte (mod index 251) stream)))
           (let ((key (orfeus/gui::photo-content-key original)))
             (rename-file original renamed)
             (check (string= key (orfeus/gui::photo-content-key renamed))
                    "Renaming a photo changed its content key")
             (check (= 32 (length key)) "Content key is not a 128-bit digest")
             ;; A copy of a photograph must key the same, or interning it off a
             ;; card would throw away every preview already rendered for it.
             (let ((copied (merge-pathnames "copied.orf" directory)))
               (uiop:copy-file renamed copied)
               (check (string= key (orfeus/gui::photo-content-key copied))
                      "A copy of a photograph keyed differently")
               (delete-file copied))
             ;; A middle-only edit defeated the former head/tail sampler.
             (with-open-file (stream renamed :direction :io
                                             :if-exists :overwrite
                                             :element-type '(unsigned-byte 8))
               (file-position stream 100000)
               (write-byte (logxor #xff (read-byte stream)) stream))
             (check (not (string= key (orfeus/gui::photo-content-key renamed)))
                    "Middle-byte replacement reused a stale source digest"))
           (let ((old-file (merge-pathnames "old.jpg" directory))
                 (new-file (merge-pathnames "new.jpg" directory)))
             (with-open-file (stream old-file :direction :output)
               (write-line "old" stream))
             (with-open-file (stream new-file :direction :output)
               (write-line "new" stream))
             (sb-posix:utime (namestring old-file) 0 0)
             (orfeus/gui::evict-stale-previews directory)
             (check (null (probe-file old-file))
                    "Eviction kept an ancient preview")
             (check (probe-file new-file)
                    "Eviction deleted a fresh preview")))
      (uiop:delete-directory-tree directory :validate t
                                            :if-does-not-exist :ignore))))

(defun test-preview-status-percent ()
  (let* ((settings (orfeus:make-processing-settings
                    :lut-path #P"look.cube" :lut-strength 1.0))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :defaults settings))
         (model (orfeus/gui:make-gui-model :project project))
         (text (orfeus/gui::preview-status-text model)))
    (check (search "look.cube (100%)" text)
           "Preview status did not use one literal percent sign")
    (check (null (search "100%%" text))
           "Preview status contains a C-style doubled percent sign")
    (check (search "NR: 35%" text)
           "Preview status does not report active noise reduction")))

(defun test-preview-priority-order ()
  (check (equal '(0 1 2 3 4)
                (orfeus/gui::preview-priority-indices 5 0))
         "Preview priority omitted photos at the project start")
  (check (equal '(2 1 3 0 4)
                (orfeus/gui::preview-priority-indices 5 2))
         "Preview priority is not selected-first and distance-ordered")
  (check (equal '(4 3 2 1 0)
                (orfeus/gui::preview-priority-indices 5 4))
         "Preview priority omitted photos at the project end")
  (check (null (orfeus/gui::preview-priority-indices 0 0))
         "Empty project produced preview work"))

(defun test-thumbnail-hit-testing ()
  (dolist (case '((0 0 104 0)
                  (103 0 104 0)
                  (104 0 104 1)
                  (207 0 104 1)
                  (0 104 104 1)))
    (destructuring-bind (event-y scroll row-height expected) case
      (check (= expected
                (orfeus/gui::thumbnail-row-at event-y scroll row-height))
             "Thumbnail hit test selected the wrong row for ~S" case))))

(defun test-thumbnail-multi-selection ()
  (check (>= orfeus/gui::*left-sidebar-min-width* 240)
         "Photo sidebar is too narrow for selection controls")
  (check (orfeus/gui::thumbnail-toggle-hit-p 220 240)
         "Visible photo checkbox was not clickable")
  (check (not (orfeus/gui::thumbnail-toggle-hit-p 180 240))
         "Photo row body was mistaken for its checkbox")
  (check (= 218 (orfeus/gui::thumbnail-checkbox-x 0 240))
         "Photo checkbox geometry did not remain numeric and right-aligned")
  (multiple-value-bind (selection anchor)
      (orfeus/gui::thumbnail-selection-after-click '(1 4) 2 1 0)
    (check (equal '(2) selection) "Plain thumbnail click did not replace selection")
    (check (= 2 anchor) "Plain thumbnail click did not move the anchor"))
  (multiple-value-bind (selection anchor)
      (orfeus/gui::thumbnail-selection-after-click
       '(1 4) 2 1 orfeus/gui::+thumbnail-control-mask+)
    (check (equal '(2 1 4) selection) "Control-click did not add a thumbnail")
    (check (= 2 anchor) "Control-click did not move the anchor"))
  (multiple-value-bind (selection anchor)
      (orfeus/gui::thumbnail-selection-after-click
       '(1 2 4) 2 1 orfeus/gui::+thumbnail-control-mask+)
    (check (equal '(1 4) selection) "Control-click did not remove a thumbnail")
    (check (= 2 anchor) "Control-click removal did not move the anchor"))
  (multiple-value-bind (selection anchor)
      (orfeus/gui::thumbnail-selection-after-click
       '(1 4) 5 2 orfeus/gui::+thumbnail-shift-mask+)
    (check (equal '(2 3 4 5) selection) "Shift-click did not select an inclusive range")
    (check (= 2 anchor) "Shift-click did not preserve the anchor"))
  (check (equal '(1 4) (orfeus/gui::thumbnail-context-selection '(1 4) 4))
         "Right-clicking a selected thumbnail did not preserve the selection")
  (check (equal '(3) (orfeus/gui::thumbnail-context-selection '(1 4) 3))
         "Right-clicking an unselected thumbnail did not select only that row")
  (check (equal '(:export :apply-still :reset-edits :copy-paths :divider
                  :select-all :intern :remove)
                (loop for index below
                      (length (orfeus/gui::thumbnail-context-menu-items))
                      collect (orfeus/gui::thumbnail-context-action-at index)))
         "Photo context menu actions do not match their visible order")
  (check (null (orfeus/gui::thumbnail-context-action-at nil))
         "Dismissing the photo context menu produced an action"))

(defun test-bundled-film-lut-menu ()
  (let ((names (mapcar #'file-namestring (orfeus/gui::gui-bundled-lut-paths))))
    (check (equal names
                  ;; Sorted case-insensitively, so the IWLTBAP stocks land
                  ;; between the Agfa and Kodak ones.
                  '("agfa_apx_100.cube"
                    "agfa_apx_25.cube"
                    "agfa_precisa_100.cube"
                    "agfa_ultra_color_100.cube"
                    "agfa_vista_200.cube"
                    "IWLTBAP K25.cube"
                    "IWLTBAP K64.cube"
                    "IWLTBAP K99.cube"
                    "kodak_kodachrome_200.cube"
                    "kodak_kodachrome_25.cube"
                    "kodak_kodachrome_64.cube"
                    "kodak_kodachrome_64_generic.cube"))
           "Bundled LUT menu does not contain the pinned film set")))

(defun test-file-filter-syntax ()
  (let ((filter (orfeus/gui::fltk-file-filter "RAW photographs" "*.orf")))
    (check (char= #\Tab (char filter (length "RAW photographs")))
           "FLTK filter does not contain a real tab separator")
    (check (null (search "\\t" filter))
           "FLTK filter contains a literal backslash-t")))

(defun test-direct-open-workflow ()
  (check (eq :photo (orfeus/gui::gui-open-kind #P"photo.DNG"))
         "DNG was not classified as a photograph")
  (check (eq :project (orfeus/gui::gui-open-kind #P"project.sexp"))
         "S-expression was not classified as a project")
  (check (null (orfeus/gui::gui-open-kind #P"notes.txt"))
         "Unsupported extension was accepted")
  (let* ((inputs (list #P"/photos/one.orf" #P"/other/two.dng"))
         (project (orfeus/gui::gui-photos-project inputs))
         (jobs (orfeus:project-photos project)))
    (check (equal #P"/photos/orfeus-exports/"
                  (orfeus:project-output-directory project))
           "Direct photo output directory is not beside the first input")
    (check (equal inputs (mapcar #'orfeus:photo-job-input-path jobs))
           "Multi-photo project changed input order")
    (check (search "agfa_precisa_100.cube"
                   (orfeus:processing-settings-lut-path
                    (orfeus:project-defaults project)))
           "Direct photo project did not select the bundled Agfa LUT")))

(defun test-project-photo-mutations ()
  (let* ((first (orfeus:make-photo-job :input-path #P"/photos/one.orf"))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos (list first)))
         (model (orfeus/gui:make-gui-model :project project)))
    (multiple-value-bind (count first-index)
        (orfeus/gui:gui-model-add-photos
         model (list #P"/photos/one.orf" #P"/photos/two.orf" #P"/photos/three.dng"))
      (check (= count 2) "Adding photos did not skip the existing path")
      (check (= first-index 1) "First added photo index was incorrect")
      (check (= (orfeus/gui:gui-model-selected-index model) 1)
             "First added photo was not selected")
      (check (equal '(#P"/photos/one.orf" #P"/photos/two.orf" #P"/photos/three.dng")
                    (mapcar #'orfeus:photo-job-input-path
                            (orfeus:project-photos project)))
             "Adding photos changed order"))
    (check (equal '(1) (orfeus/gui:gui-model-selected-indices model))
           "First added photo was not the sole selection")
    (orfeus/gui:gui-model-set-selected-indices model '(1))
    (let ((removed (orfeus/gui:gui-model-remove-selected model)))
      (check (and (= 1 (length removed))
                  (equal #P"/photos/two.orf"
                         (orfeus:photo-job-input-path (first removed))))
             "Removing selected photo removed the wrong job")
      (check (= (orfeus/gui:gui-model-selected-index model) 1)
             "Selection was not clamped to the following photo"))
    (orfeus/gui:gui-model-set-selected-indices model '(0 1))
    (check (= 2 (length (orfeus/gui:gui-model-remove-selected model)))
           "Bulk removal did not remove every selected photo")
    (check (null (orfeus:project-photos project))
           "Removing all photos left a project entry")
    (check (= (orfeus/gui:gui-model-selected-index model) 0)
           "Empty project selection was not reset")))

(defun test-checkbox-normalization ()
  (let* ((job (orfeus:make-photo-job :input-path #P"one.orf"))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos (list job)))
         (model (orfeus/gui:make-gui-model :project project)))
    (orfeus/gui:gui-model-set-setting model :lens-correction-p 0)
    (check (null (getf (orfeus:photo-job-overrides job) :lens-correction-p))
           "Numeric checkbox zero was not normalized to NIL")
    (orfeus/gui:gui-model-set-setting model :lens-correction-p 1)
    (check (eq t (getf (orfeus:photo-job-overrides job) :lens-correction-p))
           "Numeric checkbox one was not normalized to T")))

(defun test-render-queue ()
  (let ((queue (orfeus/gui::make-gui-queue))
        (lock (sb-thread:make-mutex))
        (values '()))
    (unwind-protect
         (progn
           (orfeus/gui::enqueue-gui-task
            queue :export
            (lambda ()
              (sb-thread:with-mutex (lock) (push :export values))))
           (orfeus/gui::enqueue-gui-task
            queue :preview
            (lambda ()
              (sb-thread:with-mutex (lock) (push :preview values)))
            :replace-kind :preview)
           (loop repeat 100
                 until (sb-thread:with-mutex (lock) (member :export values))
                 do (sleep 0.01))
           (check (sb-thread:with-mutex (lock) (member :export values))
                  "Queue did not run export task"))
      (orfeus/gui::stop-gui-queue queue))))

(defun test-parallel-render-queue ()
  (let ((queue (orfeus/gui::make-gui-queue :workers 2))
        (release (sb-thread:make-semaphore :count 0))
        (lock (sb-thread:make-mutex))
        (started 0))
    (unwind-protect
         (progn
           (loop repeat 2
                 do (orfeus/gui::enqueue-gui-task
                     queue :preview
                     (lambda ()
                       (sb-thread:with-mutex (lock) (incf started))
                       (sb-thread:wait-on-semaphore release))))
           (loop repeat 100
                 until (sb-thread:with-mutex (lock) (= started 2))
                 do (sleep 0.01))
           (check (sb-thread:with-mutex (lock) (= started 2))
                  "Render queue did not execute two tasks concurrently"))
      (sb-thread:signal-semaphore release)
      (sb-thread:signal-semaphore release)
      (orfeus/gui::stop-gui-queue queue))))

(defun test-preview-cache-key ()
  (let* ((first (orfeus:make-processing-settings :exposure 0.0))
         (same (orfeus:make-processing-settings :exposure 0.0))
         (changed (orfeus:make-processing-settings :exposure 1.0))
         (agfa (orfeus:make-processing-settings
                :lut-path "agfa_precisa_100.cube" :lut-strength 1.0))
         (portra (orfeus:make-processing-settings
                  :lut-path "Presetpro - Portra 160.cube" :lut-strength 1.0))
         (directory #P"/tmp/")
         (key "0123456789abcdef")
         (other "fedcba9876543210"))
    (check (equal (orfeus/gui::preview-pathname directory key :after first)
                  (orfeus/gui::preview-pathname directory key :after same))
           "Equivalent preview settings did not share a cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory key :after first)
                       (orfeus/gui::preview-pathname directory key :after changed)))
           "Edited preview settings reused a stale cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory key :after first)
                       (orfeus/gui::preview-pathname directory key :after agfa)))
           "Adding a LUT reused the no-LUT preview cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory key :after agfa)
                       (orfeus/gui::preview-pathname directory key :after portra)))
           "Distinct LUT paths reused one preview cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory key :before first)
                       (orfeus/gui::preview-pathname directory key :after first)))
           "Before and After previews shared one cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory key :after first)
                       (orfeus/gui::preview-pathname directory other :after first)))
           "Distinct photo content shared one preview cache path")
    (check (not (equal (orfeus/gui::preview-pathname directory key :after first
                                                       :max-width 1024)
                       (orfeus/gui::preview-pathname directory key :after first
                                                       :max-width 2048)))
           "Distinct preview dimensions shared one cache path")
    (check (= 64 (length (orfeus/gui::preview-settings-key first)))
           "Preview recipe key is not full SHA-256")))

(defun test-preview-cache-dependencies-and-permissions ()
  (let* ((directory (merge-pathnames
                     (format nil "orfeus-preview-security-~D/" (random most-positive-fixnum))
                     (uiop:temporary-directory)))
         (lut (merge-pathnames "look.cube" directory))
         (output (merge-pathnames "preview.jpg" directory))
         (coalesced (merge-pathnames "coalesced.jpg" directory))
         (raced (merge-pathnames "raced.jpg" directory))
         (session (merge-pathnames "session/" directory)))
    (ensure-directories-exist directory)
    (orfeus/gui::make-secure-preview-directory session)
    (unwind-protect
         (progn
           (orfeus/gui::protect-preview-cache-path directory #o700)
           (with-open-file (stream lut :direction :output :if-exists :supersede)
             (write-line "first LUT" stream))
           (let* ((settings (orfeus:make-processing-settings
                             :lut-path lut :lut-strength 1.0))
                  (first-key (orfeus/gui::preview-settings-key settings)))
             (with-open-file (stream lut :direction :output :if-exists :supersede)
               (write-line "replacement LUT" stream))
             (check (not (string= first-key
                                  (orfeus/gui::preview-settings-key settings)))
                    "Same-path LUT replacement reused a stale recipe key"))
           (let* ((node (orfeus:make-graph-node :id 1 :kind :exposure
                                                :params '(:exposure 1.0)
                                                :inputs '(0) :position '(10 20)))
                  (graph (orfeus:make-processing-graph :nodes (list node) :output 1))
                  (first-key (orfeus/gui::preview-settings-key graph)))
             (setf (orfeus:graph-node-position node) '(900 700)
                   (orfeus::graph-node-kind-states node)
                   '((:film :params (:lut-path "inactive.cube"
                                     :lut-strength 1.0))))
             (check (string= first-key (orfeus/gui::preview-settings-key graph))
                    "Editor-only graph state changed the render recipe key"))
           (orfeus/gui::call-with-preview-cache-fill
            output
            (lambda (temporary)
              (check (string-equal "jpg" (pathname-type temporary))
                     "Cache fill temporary lost the renderer output type")
              (with-open-file (stream temporary :direction :output
                                               :if-exists :supersede)
                (write-line "jpeg" stream))))
           (check (= #o700 (logand #o777 (sb-posix:stat-mode
                                          (sb-posix:stat (namestring directory)))))
                  "Preview cache directory is not mode 0700")
           (check (= #o600 (logand #o777 (sb-posix:stat-mode
                                          (sb-posix:stat (namestring output)))))
                  "Preview cache file is not mode 0600")
           (let* ((symbol 'orfeus/gui::protect-preview-cache-path)
                  (original (symbol-function symbol)))
             (unwind-protect
                  (progn
                    (setf (symbol-function symbol)
                          (lambda (pathname mode)
                            (declare (ignore pathname mode))
                            nil))
                    (check (not (orfeus/gui::preview-cache-hit-p output))
                           "A cache hit survived failed permission verification")
                    (check (not (probe-file output))
                           "An insecure cache hit was not removed"))
               (setf (symbol-function symbol) original)))
           (orfeus/gui::call-with-preview-cache-fill
            output
            (lambda (temporary)
              (with-open-file (stream temporary :direction :output)
                (write-line "jpeg" stream))))
           (let ((fills 0)
                 (lock (sb-thread:make-mutex)))
             (flet ((fill-cache ()
                      (orfeus/gui::call-with-preview-cache-fill
                       coalesced
                       (lambda (temporary)
                         (sb-thread:with-mutex (lock) (incf fills))
                         (sleep 0.05)
                         (with-open-file (stream temporary :direction :output)
                           (write-line "one fill" stream))))))
               (let ((first (sb-thread:make-thread #'fill-cache))
                     (second (sb-thread:make-thread #'fill-cache)))
                 (sb-thread:join-thread first)
                 (sb-thread:join-thread second)))
             (check (= 1 fills) "Concurrent preview fills were not coalesced"))
           (with-open-file (stream lut :direction :output :if-exists :supersede)
             (write-line "stable LUT" stream))
           (let* ((settings (orfeus:make-processing-settings
                             :lut-path lut :lut-strength 1.0))
                  (key (orfeus/gui::preview-settings-key settings))
                  (current-p (lambda ()
                               (string= key
                                        (orfeus/gui::preview-settings-key settings))))
                  (rejected-p nil))
             (handler-case
                 (orfeus/gui::call-with-preview-cache-fill
                  raced
                  (lambda (temporary)
                    (with-open-file (stream temporary :direction :output)
                      (write-line "rendered" stream))
                    (with-open-file (stream lut :direction :output
                                                :if-exists :supersede)
                      (write-line "changed during render" stream)))
                  :validation-function current-p)
               (error () (setf rejected-p t)))
             (check rejected-p "A LUT race published a mismatched cache entry")
             (check (not (probe-file raced))
                    "A cache entry survived failed post-render LUT validation")
             (with-open-file (stream lut :direction :output :if-exists :supersede)
               (write-line "stable again" stream))
             (setf key (orfeus/gui::preview-settings-key settings))
             (orfeus/gui::call-with-preview-cache-fill
              raced
              (lambda (temporary)
                (with-open-file (stream temporary :direction :output)
                  (write-line "rendered" stream))))
             (setf rejected-p nil)
             (handler-case
                 (orfeus/gui::materialize-preview-cache-hit
                  raced session
                  :validation-function
                  (lambda ()
                    (with-open-file (stream lut :direction :output
                                                :if-exists :supersede)
                      (write-line "changed during hit" stream))
                    (funcall current-p)))
               (error () (setf rejected-p t)))
             (check rejected-p "A LUT race materialized a mismatched cache hit")
             (check (null (uiop:directory-files session))
                    "A failed LUT hit validation retained a display copy"))
           (sb-posix:utime (namestring output) 0 0)
           (orfeus/gui::call-with-active-preview-cache-file
            output (lambda () (orfeus/gui::evict-stale-previews directory)))
           (check (probe-file output) "Eviction deleted an active cache hit"))
      (uiop:delete-directory-tree directory :validate t
                                           :if-does-not-exist :ignore))))

(defun test-preview-cache-recovery-and-generation-load ()
  (let* ((directory (merge-pathnames
                     (format nil "orfeus-preview-lock-~D/" (random most-positive-fixnum))
                     (uiop:temporary-directory)))
         (cache (merge-pathnames "hit.jpg" directory))
         (session (merge-pathnames "session/" directory))
         (lock (orfeus/gui::preview-lock-directory cache)))
    (ensure-directories-exist directory)
    (orfeus/gui::make-secure-preview-directory session)
    (unwind-protect
         (progn
           (sb-posix:mkdir (namestring lock) #o700)
           (with-open-file (stream (orfeus/gui::preview-lock-owner-pathname lock)
                                  :direction :output :if-exists :supersede)
             (write (list :pid 99999999
                          :created (get-universal-time)
                          :token "dead")
                    :stream stream))
           (let ((held (orfeus/gui::acquire-preview-lock cache :ignore-hit-p t
                                                               :timeout 1.0d0)))
             (check held "A fresh lock owned by a dead process was not reclaimed")
             (orfeus/gui::release-preview-lock held))
           (flet ((check-abandoned-lock (owner description)
                    (sb-posix:mkdir (namestring lock) #o700)
                    (when owner
                      (with-open-file
                          (stream (orfeus/gui::preview-lock-owner-pathname lock)
                                  :direction :output :if-exists :supersede)
                        (write owner :stream stream)))
                    (sb-posix:utime (namestring lock) 0 0)
                    (let* ((orfeus/gui::*preview-lock-owner-grace-seconds* 0)
                           (held (orfeus/gui::acquire-preview-lock
                                  cache :ignore-hit-p t :timeout 1.0d0)))
                      (check held "~A was not reclaimed after its grace" description)
                      (orfeus/gui::release-preview-lock held))))
             (check-abandoned-lock nil "An ownerless preview lock")
             (check-abandoned-lock '(:pid "bad" :created nil)
                                   "A malformed preview lock"))
           (with-open-file (stream cache :direction :output :if-exists :supersede)
             (write-line "jpeg" stream))
           (orfeus/gui::protect-preview-cache-path cache #o600)
           (let ((display (orfeus/gui::materialize-preview-cache-hit cache session)))
             (orfeus/gui::call-with-preview-key-lock
              cache (lambda () (delete-file cache)))
             (check (probe-file display)
                    "Materialized cache hit was lost during concurrent eviction")))
      (uiop:delete-directory-tree directory :validate t
                                            :if-does-not-exist :ignore)))
  (let ((queue (orfeus/gui::make-gui-queue))
        (release (sb-thread:make-semaphore)))
    (unwind-protect
         (progn
           (orfeus/gui::enqueue-gui-task queue :after
                                         (lambda () (sb-thread:wait-on-semaphore release))
                                         :generation 10)
           (orfeus/gui::enqueue-gui-task queue :after (lambda () nil)
                                         :generation 11)
           (sleep 0.02)
           (check (= 1 (orfeus/gui::gui-queue-load queue :generation 10))
                  "Generation progress omitted current foreground work")
           (check (= 1 (orfeus/gui::gui-queue-load queue :generation 11))
                  "Generation progress counted stale work"))
      (sb-thread:signal-semaphore release)
      (orfeus/gui::stop-gui-queue queue))))

(defun test-preview-progress-cancellation ()
  (multiple-value-bind (percent total generation)
      (orfeus/gui::preview-progress-state 2 10 8 7)
    (check (= 2 percent) "New preview generation inherited stale progress")
    (check (= 2 total) "New preview generation retained the old batch total")
    (check (= 8 generation) "New preview generation was not recorded"))
  (multiple-value-bind (percent total generation)
      (orfeus/gui::preview-progress-state 0 7 9 8)
    (declare (ignore generation))
    (check (and (zerop percent) (zerop total))
           "Cancelled preview generation did not reset progress")))

(defun test-preview-recipe-snapshot ()
  (let* ((points (list 0.0 0.0 0.33 0.25 0.66 0.75 1.0 1.0))
         (node (orfeus:make-graph-node :id 1 :kind :curves
                                      :params (list :red-points points)))
         (graph (orfeus:make-processing-graph :nodes (list node) :output 1))
         (snapshot (orfeus/gui::preview-recipe-snapshot graph))
         (snapshot-node (first (orfeus:processing-graph-nodes snapshot))))
    (setf (second points) 0.5
          (orfeus:graph-node-bypassed-p node) t)
    (check (= 0.0 (second (getf (orfeus:graph-node-params snapshot-node)
                                :red-points)))
           "Preview graph snapshot shared nested parameter storage")
    (check (not (orfeus:graph-node-bypassed-p snapshot-node))
           "Preview graph snapshot shared node state")
    (check (not (equal (orfeus/gui::preview-settings-key graph)
                       (orfeus/gui::preview-settings-key snapshot)))
           "Mutating the live graph also changed the preview snapshot"))
  (let* ((settings (orfeus:make-processing-settings :exposure 0.5))
         (snapshot (orfeus/gui::preview-recipe-snapshot settings)))
    (setf (orfeus:processing-settings-exposure settings) 1.5)
    (check (= 0.5 (orfeus:processing-settings-exposure snapshot))
           "Preview settings snapshot shared mutable state"))
  (check (orfeus/gui::crop-preview-current-p 7 7)
         "Current bypass preview generation was rejected")
  (check (not (orfeus/gui::crop-preview-current-p 8 7))
         "Stale cropped preview generation allowed crop interaction")
  (check (not (orfeus/gui::crop-preview-current-p 8 nil))
         "Missing bypass preview allowed crop interaction"))

(defun test-gallery-selection-provenance ()
  (let* ((project-preset (orfeus:make-processing-preset
                          :name "Shared" :settings
                          (orfeus:make-processing-settings :exposure 1.0)))
         (local-preset (orfeus:make-processing-preset
                        :name "Shared" :settings
                        (orfeus:make-processing-settings :exposure 2.0)))
         (project-still
           (orfeus/gui::make-project-gallery-still project-preset))
         (local-still (orfeus/gui::make-local-gallery-still local-preset))
         (stills (list project-still local-still))
         (local-key (orfeus/gui::gallery-selection-key stills 1)))
    (check (and (eq :local (first local-key))
                (equal (second local-key)
                       (second (orfeus/gui::gallery-still-key project-still))))
           "Gallery key lost same-name local provenance: ~S" local-key)
    (check (= 0 (orfeus/gui::gallery-selection-index
                 (list local-still project-still) local-key))
           "Gallery refresh did not re-find the selected local still")
    (check (null (orfeus/gui::gallery-selection-index
                  (list project-still) local-key))
           "Removed local still retargeted the same-name project still")
    (check (eq local-still
               (orfeus/gui::gallery-selected-still stills 1))
           "Visible gallery selection did not resolve to the local still")
    (let* ((job (orfeus:make-photo-job :input-path #P"target.orf"))
           (project (orfeus:make-project :output-directory #P"exports/"
                                         :photos (list job)
                                         :presets (list project-preset)))
           (model (orfeus/gui:make-gui-model :project project)))
      (orfeus/gui:gui-model-apply-preset-graph
       model (orfeus/gui::gallery-still-preset
              (orfeus/gui::gallery-selected-still stills 1)))
      (check (= 2.0 (orfeus/gui:gui-model-setting model :exposure))
             "Selected local still applied the same-name project preset"))
    (check (string= "local still"
                    (orfeus/gui::gallery-still-origin-description local-still))
           "Still status description lost provenance")))

(defun test-empty-graph-editor-geometry ()
  (check (>= orfeus/gui::*inspector-min-height* 378)
         "Minimum inspector height clips the Curves reset control")
  (let ((unplaced (orfeus:make-graph-node :id 1 :kind :optics))
        (placed (orfeus:make-graph-node :id 2 :kind :exposure
                                        :position '(31.0 97.0))))
    (orfeus/gui::ensure-graph-node-positions (list unplaced placed))
    (check (equal (list 18.0
                        (float (+ 6 orfeus/gui::*graph-row-pitch*) 1.0))
                  (orfeus:graph-node-position unplaced))
           "Default graph node position was not assigned before dragging")
    (check (equal '(31.0 97.0) (orfeus:graph-node-position placed))
           "Existing dragged graph node position was replaced"))
  (let* ((job (orfeus:make-photo-job :input-path #P"legacy-flat.orf"))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :photos (list job)))
         (model (orfeus/gui:make-gui-model :project project))
         (node (orfeus/gui::graph-node-for-edit model 0)))
    (check node "Flat photo did not materialize a graph node for dragging")
    (check (orfeus:graph-node-position node)
           "Materialized graph node still had no drag position")
    (check (member node
                   (orfeus:processing-graph-nodes
                    (orfeus:photo-job-graph job)))
           "Positioned drag node did not belong to the stored graph"))
  (multiple-value-bind (x y width height)
      (orfeus/gui::graph-output-box-position '())
    (check (equal (list 18
                        (+ 34 orfeus/gui::*graph-well-height*)
                        orfeus/gui::*graph-node-width*
                        orfeus/gui::*graph-well-height*)
                  (list x y width height))
           "Empty graph OUT geometry failed: ~S"
           (list x y width height)))
  (let ((node (orfeus:make-graph-node :id 1 :kind :exposure
                                      :position '(30.0 70.0))))
    (multiple-value-bind (x y width height)
        (orfeus/gui::graph-output-box-position (list node))
      (declare (ignore x width height))
      (check (= (+ 70 orfeus/gui::*graph-node-height* 28) y)
             "Populated graph OUT geometry was ~D" y))))

(defun test-discard-pending-tasks ()
  (let ((queue (orfeus/gui::make-gui-queue))
        (ran-p nil))
    (unwind-protect
         (progn
           (orfeus/gui::enqueue-gui-task queue :blocker (lambda () (sleep 0.1)))
           (orfeus/gui::enqueue-gui-task queue :background
                                         (lambda () (setf ran-p t)))
           (orfeus/gui::discard-gui-tasks queue :background)
           (sleep 0.2)
           (check (not ran-p) "Discarded background task still ran"))
      (orfeus/gui::stop-gui-queue queue))))

(defun test-graph-view-signature ()
  ;; The signature drives the poll repaint, so it must change whenever the
  ;; editor should show something different: the reported bug was an added
  ;; photograph leaving "Open a photograph to grade" on screen.
  (let* ((first-job (orfeus:make-photo-job :input-path #P"one.orf"))
         (second-job (orfeus:make-photo-job :input-path #P"two.orf"))
         (graph (orfeus:default-processing-graph))
         (node (first (orfeus:processing-graph-nodes graph)))
         (empty (orfeus/gui::graph-view-signature nil nil nil))
         (opened (orfeus/gui::graph-view-signature first-job nil nil)))
    (check (not (equal empty opened))
           "Opening a photograph did not change the graph view signature")
    (check (equal opened (orfeus/gui::graph-view-signature first-job nil nil))
           "An unchanged graph view produced a different signature")
    (check (not (equal opened
                       (orfeus/gui::graph-view-signature second-job nil nil)))
           "Switching photographs did not change the signature")
    (let ((flat (orfeus/gui::graph-view-signature first-job nil nil))
          (graded (orfeus/gui::graph-view-signature first-job graph nil)))
      (check (not (equal flat graded))
             "Converting a photograph to a graph did not change the signature")
      (check (not (equal graded
                         (orfeus/gui::graph-view-signature first-job graph
                                                           node)))
             "Selecting a node did not change the signature")
      (orfeus:graph-insert-node graph (orfeus:processing-graph-output graph)
                                :exposure :params '(:exposure 1.0))
      (check (not (equal graded
                         (orfeus/gui::graph-view-signature first-job graph
                                                           nil)))
             "Adding a node did not change the signature"))))

(defun test-lightfast-root-layout-and-export-validation ()
  (let* ((layout (orfeus/gui::make-root-layout
                  :menu :toolbar :main :progress :status))
         (placements
           (lightfast:compute-layout layout
                                     (lightfast:make-rect :width 1000
                                                          :height 800)))
         (rect (lambda (target)
                 (lightfast:layout-placement-rect
                  (find target placements
                        :key #'lightfast:layout-placement-target)))))
    (flet ((check-rect (target x y width height)
             (let ((held (funcall rect target)))
               (check (equal (list x y width height)
                             (list (lightfast:rect-x held)
                                   (lightfast:rect-y held)
                                   (lightfast:rect-width held)
                                   (lightfast:rect-height held)))
                      "Root layout placed ~S at ~S" target held))))
      (check-rect :menu 0 0 1000 24)
      (check-rect :toolbar 0 24 1000 40)
      (check-rect :main 0 64 1000 708)
      (check-rect :progress 0 772 180 28)
      (check-rect :status 180 772 820 28)))
  (check (= 92 (orfeus/gui::parse-export-integer-value "" "Quality" 92 1 100))
         "Blank export value did not use its fallback")
  (check (= 85 (orfeus/gui::parse-export-integer-value "85" "Quality" 92 1 100))
         "Valid export value did not parse")
  (dolist (text '("85px" "-1" "101"))
    (check (handler-case
               (progn
                 (orfeus/gui::parse-export-integer-value text "Quality" 92 1 100)
                 nil)
             (error () t))
           "Invalid export value ~S was accepted" text))
  (let* ((settings (orfeus:make-export-settings
                    :jpeg-quality 91
                    :max-width 2048
                    :max-height 1536
                    :preserve-metadata-p t
                    :timestamp-filenames-p nil))
         (project (orfeus:make-project :output-directory #P"old/"
                                       :export-settings settings)))
    (check (handler-case
               (progn
                 (orfeus/gui::update-project-export-settings
                  project "new" :tiff "85px" "4096" "3072" nil t)
                 nil)
             (error () t))
           "Invalid export settings update did not signal")
    (check (equal #P"old/" (orfeus:project-output-directory project))
           "Invalid export settings update changed the destination")
    (check (and (= 91 (orfeus:export-settings-jpeg-quality settings))
                (= 2048 (orfeus:export-settings-max-width settings))
                (= 1536 (orfeus:export-settings-max-height settings))
                (eq :jpeg (orfeus:export-settings-format settings))
                (orfeus:export-settings-preserve-metadata-p settings)
                (not (orfeus:export-settings-timestamp-filenames-p settings)))
           "Invalid export settings update partially mutated settings")
    (orfeus/gui::update-project-export-settings
     project "new" :tiff "85" "4096" "0" nil t)
    (check (equal #P"new/" (orfeus:project-output-directory project))
           "Valid export settings update did not change the destination")
    (check (and (= 85 (orfeus:export-settings-jpeg-quality settings))
                (= 4096 (orfeus:export-settings-max-width settings))
                (null (orfeus:export-settings-max-height settings))
                (eq :tiff (orfeus:export-settings-format settings))
                (not (orfeus:export-settings-preserve-metadata-p settings))
                (orfeus:export-settings-timestamp-filenames-p settings))
           "Valid export settings update did not commit every field")
    (check (string= "tif" (orfeus:export-format-extension :tiff))
           "TIFF export format named the wrong extension")
    (check (and (eq :tiff (orfeus/gui::export-format-from-caption "16-bit TIFF"))
                (eq :jpeg (orfeus/gui::export-format-from-caption "nonsense"))
                (string= "16-bit TIFF"
                         (orfeus/gui::export-format-caption :tiff)))
           "Export format captions did not round-trip"))
  ;; The tone bands are laid out by Lightfast's flex engine rather than by
  ;; hand-computed columns, so assert the geometry it produces: seven equal
  ;; columns, each with a fixed-width slider centred between caption and value.
  (let* ((bands (loop for index below 7
                      collect (orfeus/gui::tone-band-layout
                               (list :label index)
                               (list :slider index)
                               (list :input index))))
         (placements (lightfast:compute-layout
                      (orfeus/gui::tone-bands-layout bands)
                      (lightfast:make-rect :x 14 :y 92
                                           :width 306 :height 300)))
         (rect (lambda (role index)
                 (loop for placement in placements
                       when (equal (list role index)
                                   (lightfast:layout-placement-target placement))
                         return (lightfast:layout-placement-rect placement))))
         (labels* (loop for index below 7 collect (funcall rect :label index)))
         (sliders (loop for index below 7 collect (funcall rect :slider index)))
         (inputs (loop for index below 7 collect (funcall rect :input index))))
    (check (notany #'null (append labels* sliders inputs))
           "Tone band layout did not place every widget")
    ;; Leftover pixels are spread one per column rather than dropped, so equal
    ;; columns may differ by one.
    (let ((widths (mapcar #'lightfast:rect-width labels*)))
      (check (<= (- (reduce #'max widths) (reduce #'min widths)) 1)
             "Tone band columns came out unequal"))
    (check (every (lambda (slider)
                    (= orfeus/gui::*tone-slider-width*
                       (lightfast:rect-width slider)))
                  sliders)
           "Tone band slider stretched instead of keeping its width")
    (check (every (lambda (slider caption)
                    (= (- (lightfast:rect-x slider) (lightfast:rect-x caption))
                       (floor (- (lightfast:rect-width caption)
                                 orfeus/gui::*tone-slider-width*)
                              2)))
                  sliders labels*)
           "Tone band slider was not centred in its column")
    (check (every (lambda (caption slider value)
                    (and (<= (+ (lightfast:rect-y caption)
                                (lightfast:rect-height caption))
                             (lightfast:rect-y slider))
                         (<= (+ (lightfast:rect-y slider)
                                (lightfast:rect-height slider))
                             (lightfast:rect-y value))))
                  labels* sliders inputs)
           "Tone band rows overlapped one another")
    (check (loop for (first second) on labels*
                 always (or (null second)
                            (<= (+ (lightfast:rect-x first)
                                   (lightfast:rect-width first))
                                (lightfast:rect-x second))))
           "Tone band columns overlapped one another")
    ;; Every column together must still fit the rectangle it was given.
    (check (<= (+ (lightfast:rect-x (car (last labels*)))
                  (lightfast:rect-width (car (last labels*))))
               320)
           "Tone bands overflowed the panel they were laid out in"))
  (check (orfeus/gui::gallery-generation-event-current-p
          '(:still-error 4 "failed") 4)
         "Current still error was rejected")
  (check (not (orfeus/gui::gallery-generation-event-current-p
               '(:still-error 3 "stale") 4))
         "Stale still error was accepted"))

(defun run-tests ()
  (test-lightfast-root-layout-and-export-validation)
  (test-model-settings)
  (test-undo-history)
  (test-graph-node-placement)
  (test-thumbnail-context-menu)
  (test-viewport-render-bound)
  (test-curve-spline-shapes)
  (test-preset-bulk-application)
  (test-grade-workflow)
  (test-node-graph-model)
  (test-output-path-semantics)
  (test-preview-job-identity)
  (test-preview-directory)
  (test-preview-status-percent)
  (test-preview-priority-order)
  (test-thumbnail-hit-testing)
  (test-thumbnail-multi-selection)
  (test-graph-view-signature)
  (test-bundled-film-lut-menu)
  (test-file-filter-syntax)
  (test-direct-open-workflow)
  (test-project-photo-mutations)
  (test-checkbox-normalization)
  (test-render-queue)
  (test-parallel-render-queue)
  (test-preview-cache-key)
  (test-preview-cache-dependencies-and-permissions)
  (test-preview-cache-recovery-and-generation-load)
  (test-preview-progress-cancellation)
  (test-preview-recipe-snapshot)
  (test-gallery-selection-provenance)
  (test-empty-graph-editor-geometry)
  (test-discard-pending-tasks)
  t)
