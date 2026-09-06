(in-package #:orfeus/gui)

(defstruct gui-model
  "Frontend state that does not belong in an Orfeus project.

SELECTED-NODE holds the graph node object under edit; node structs survive
renumbering, so the selection stays valid across graph edits."
  project
  (selected-index 0 :type fixnum)
  (selected-indices '() :type list)
  ;; True when the selection was made on purpose — Space, control or shift,
  ;; Select All — and so is to be left alone while the cursor walks past it.
  (marked-p nil)
  (edit-target :photo :type (member :photo :defaults))
  (selected-node nil)
  project-path
  (undo-stack '() :type list)
  (redo-stack '() :type list)
  ;; Whether the project differs from what is on disk. Set by every edit,
  ;; cleared when the project is saved or another one takes its place; the
  ;; window title and the close question read it.
  (modified-p nil))

(defparameter *undo-depth* 64
  "Edits kept for undo. Each entry copies the project, so this bounds memory.")

(defparameter *undo-coalesce-seconds* 3/2
  "Window within which repeated edits of the same control share one undo entry.

Dragging a slider calls GUI-MODEL-SET-SETTING on every tick; without this each
tick would become its own undo step and one drag would take a hundred undos to
reverse.")

(defstruct (gui-snapshot (:constructor %make-gui-snapshot))
  "One reversible point in the editing session.

SELECTED-NODE is recorded by id rather than by identity: restoring rebuilds the
graph, so the node objects differ even though their ids do not."
  project
  (selected-index 0 :type fixnum)
  (selected-indices '() :type list)
  (edit-target :photo :type (member :photo :defaults))
  selected-node-id
  coalesce-key
  (time 0))

(defun gui-model-snapshot (model &optional coalesce-key)
  "Capture MODEL's reversible state."
  (%make-gui-snapshot
   :project (orfeus:copy-project-deep (gui-model-project model))
   :selected-index (gui-model-selected-index model)
   :selected-indices (copy-list (gui-model-selected-indices model))
   :edit-target (gui-model-edit-target model)
   :selected-node-id (let ((node (gui-model-selected-node model)))
                       (and node (orfeus:graph-node-id node)))
   :coalesce-key coalesce-key
   :time (get-internal-real-time)))

(defun gui-snapshot-supersedes-p (snapshot coalesce-key)
  "True when a new edit under COALESCE-KEY belongs to SNAPSHOT's edit already."
  (and coalesce-key
       snapshot
       (equal (gui-snapshot-coalesce-key snapshot) coalesce-key)
       (< (- (get-internal-real-time) (gui-snapshot-time snapshot))
          (* *undo-coalesce-seconds* internal-time-units-per-second))))

(defvar *inside-model-edit* nil
  "True while an edit that has already recorded its undo entry is running.

A few model mutators are built from others — setting a control on a graph-graded
photo adds a node and writes its parameters. Binding this around those inner
calls keeps one user action to one undo step, and keeps the recorded state the
one from before the action rather than from halfway through it.")

(defun gui-model-checkpoint (model &optional coalesce-key)
  "Record MODEL's current state so GUI-MODEL-UNDO can return to it.

Call this immediately before mutating. Consecutive edits sharing COALESCE-KEY
collapse into the entry already on the stack, so one slider drag is one undo.
Returns true when a new entry was pushed."
  (unless *inside-model-edit*
    (setf (gui-model-modified-p model) t))
  (cond
    (*inside-model-edit* nil)
    ((gui-snapshot-supersedes-p (first (gui-model-undo-stack model)) coalesce-key)
     ;; The stack already holds the state before this run of edits started;
     ;; keep it and let the run stay one step. Redo is still discarded: the
     ;; edit continues to diverge from whatever was undone.
     (setf (gui-model-redo-stack model) '())
     nil)
    (t
     (push (gui-model-snapshot model coalesce-key) (gui-model-undo-stack model))
     (let ((stack (gui-model-undo-stack model)))
       (when (> (length stack) *undo-depth*)
         (setf (gui-model-undo-stack model) (subseq stack 0 *undo-depth*))))
     (setf (gui-model-redo-stack model) '())
     t)))

(defun gui-model-clear-history (model)
  "Forget every undo and redo step, as when a different project is opened."
  (setf (gui-model-undo-stack model) '()
        (gui-model-redo-stack model) '())
  model)

(defun gui-model-can-undo-p (model)
  (and (gui-model-undo-stack model) t))

(defun gui-model-can-redo-p (model)
  (and (gui-model-redo-stack model) t))

(defun gui-model-restore (model snapshot)
  "Make SNAPSHOT current, leaving SNAPSHOT itself reusable."
  (setf (gui-model-project model)
        (orfeus:copy-project-deep (gui-snapshot-project snapshot))
        (gui-model-selected-index model) (gui-snapshot-selected-index snapshot)
        (gui-model-selected-indices model)
        (copy-list (gui-snapshot-selected-indices snapshot))
        (gui-model-edit-target model) (gui-snapshot-edit-target snapshot))
  (setf (gui-model-selected-node model)
        (let ((id (gui-snapshot-selected-node-id snapshot))
              (job (gui-model-selected-job model)))
          (when (and id job (photo-job-graph job))
            (orfeus:graph-find-node (photo-job-graph job) id))))
  model)

(defun gui-model-undo (model)
  "Step back one edit. Returns true when there was one to step back to."
  (let ((previous (pop (gui-model-undo-stack model))))
    (when previous
      (push (gui-model-snapshot model) (gui-model-redo-stack model))
      (gui-model-restore model previous)
      (setf (gui-model-modified-p model) t)
      t)))

(defun gui-model-redo (model)
  "Step forward one undone edit. Returns true when there was one."
  (let ((next (pop (gui-model-redo-stack model))))
    (when next
      (push (gui-model-snapshot model) (gui-model-undo-stack model))
      (gui-model-restore model next)
      (setf (gui-model-modified-p model) t)
      t)))

(defun gui-bundled-lut-paths ()
  "Return bundled CUBE LUT pathnames in stable display order."
  (sort (directory (merge-pathnames #P"*.cube"
                                    (asdf:system-relative-pathname
                                     "orfeus" #P"data/luts/")))
        #'string-lessp :key #'file-namestring))

(defun gui-default-lut-path ()
  "Return the bundled Agfa Precisa 100 LUT pathname, when installed."
  (find "agfa_precisa_100" (gui-bundled-lut-paths)
        :test #'string-equal :key #'pathname-name))

(defun gui-default-processing-settings ()
  (make-processing-settings
   :lut-path (let ((path (gui-default-lut-path)))
               (and path (namestring path)))))

(defun gui-empty-project ()
  "Return an empty in-memory project suitable for a newly opened GUI."
  (make-project :output-directory #P"exports/"
                :defaults (gui-default-processing-settings)
                :photos '()))

(defun anchored-export-directory (anchor)
  "Return the export directory Orfeus proposes beside ANCHOR."
  (merge-pathnames #P"orfeus-exports/"
                   (uiop:pathname-directory-pathname anchor)))

(defun gui-model-anchor-export-directory (model anchor)
  "Put the export directory beside ANCHOR, the project's file.

Called when a project moves to a new path. Before a project exists the only
place to put exports is beside the photographs; saving the project says where
the work actually lives, so exports follow it there. Where the user then puts
the project is the user's business."
  (setf (project-output-directory (gui-model-project model))
        (anchored-export-directory anchor)))

(defun gui-photos-project (pathnames)
  "Return one project containing PATHNAMES in selection order."
  (let* ((inputs (mapcar #'pathname pathnames))
         (first-input (or (first inputs)
                          (error "At least one photograph is required.")))
         (output-directory (anchored-export-directory first-input)))
    (make-project :output-directory output-directory
                  :defaults (gui-default-processing-settings)
                  :photos (mapcar (lambda (input)
                                    (make-photo-job :input-path input))
                                  inputs))))

(defun gui-photo-project (pathname)
  "Return a one-photo project for PATHNAME."
  (gui-photos-project (list pathname)))

(defun gui-open-kind (pathname)
  "Classify PATHNAME as :PROJECT, :PHOTO, or NIL for an unsupported extension."
  (let ((type (string-downcase (or (pathname-type (pathname pathname)) ""))))
    (cond ((member type '("sexp" "lisp") :test #'string=) :project)
          ((member type '("orf" "dng") :test #'string=) :photo))))

(defun gui-model-replace-project (model project &optional project-path)
  "Replace MODEL's project and reset its transient selection state."
  (setf (gui-model-project model) project
        (gui-model-project-path model) project-path
        (gui-model-selected-index model) 0
        (gui-model-selected-indices model)
        (if (project-photos project) '(0) '())
        (gui-model-marked-p model) nil
        (gui-model-edit-target model) :photo
        (gui-model-modified-p model) nil)
  (gui-model-clear-history model)
  model)

(defun gui-model-set-selected-indices (model indices)
  "Set MODEL's valid selected row INDICES and preview anchor."
  (let* ((count (length (project-photos (gui-model-project model))))
         (selected (sort (remove-duplicates
                          (remove-if-not (lambda (index)
                                           (and (integerp index)
                                                (<= 0 index)
                                                (< index count)))
                                         indices)
                          :test #'=)
                         #'<)))
    (setf (gui-model-selected-indices model) selected)
    (when selected
      (setf (gui-model-selected-index model) (first selected)))
    selected))

(defun gui-model-selected-jobs (model)
  "Return MODEL's selected photo jobs in project order."
  (let ((photos (project-photos (gui-model-project model))))
    (mapcar (lambda (index) (nth index photos))
            (gui-model-selected-indices model))))

(defun gui-model-add-photos (model pathnames)
  "Append unique PATHNAMES to MODEL's project and select the first addition."
  (gui-model-checkpoint model)
  (let* ((project (gui-model-project model))
         (photos (project-photos project))
         (existing (mapcar #'photo-job-input-path photos))
         (inputs (remove-duplicates (mapcar #'pathname pathnames) :test #'equal))
         (new-inputs (remove-if (lambda (path) (member path existing :test #'equal))
                                inputs))
         (first-index (length photos)))
    (when new-inputs
      ;; Only guess from the photographs when there is no project to anchor to.
      ;; A saved project already says where its exports go, and importing into it
      ;; must not drag them back to wherever the photographs happen to sit.
      (when (and (null photos) (null (gui-model-project-path model)))
        (setf (project-output-directory project)
              (anchored-export-directory (first new-inputs))))
      (setf (project-photos project)
            (append photos
                    (mapcar (lambda (input)
                              (make-photo-job :input-path input))
                            new-inputs))
            (gui-model-selected-index model) first-index
            (gui-model-selected-indices model) (list first-index)
            (gui-model-marked-p model) nil
            (gui-model-edit-target model) :photo))
    (values (length new-inputs) first-index)))

(defun gui-model-remove-selected (model)
  "Remove and return MODEL's selected photo jobs, safely clamping selection."
  (gui-model-checkpoint model)
  (let* ((project (gui-model-project model))
         (photos (project-photos project))
         (indices (or (gui-model-selected-indices model)
                      (list (gui-model-selected-index model))))
         (removed (loop for photo in photos
                        for index from 0
                        when (member index indices) collect photo))
         (remaining (loop for photo in photos
                          for index from 0
                          unless (member index indices) collect photo)))
    (when removed
      (let ((next-index (max 0 (min (first indices) (1- (length remaining))))))
        (setf (project-photos project) remaining
              (gui-model-selected-index model) next-index
              (gui-model-selected-indices model)
              (if remaining (list next-index) '())
              (gui-model-marked-p model) nil)))
    removed))

(defun gui-model-move-cursor (model index &optional (row-indices (list index)))
  "Put the preview on photograph INDEX without disturbing a selection made on
purpose.

A plain selection follows the cursor, as a list's does: moving to the next
photograph selects it and nothing else. One the photographer built — with
Space, control, shift or Select All — stays where it is while the cursor walks
past it, which is what lets a set of keepers be gathered with the arrow keys.
ROW-INDICES are the photographs the cursor's row stands for, several when a
burst is folded."
  (let ((count (length (project-photos (gui-model-project model)))))
    (when (< -1 index count)
      (unless (gui-model-marked-p model)
        (gui-model-set-selected-indices model row-indices))
      (setf (gui-model-selected-index model) index)
      index)))

(defun gui-model-toggle-mark (model &optional (row-indices
                                              (list (gui-model-selected-index
                                                     model))))
  "Add the cursor's photographs to the selection, or take them out again.

Returns :MARKED or :UNMARKED. The first mark turns whatever was selected into a
selection made on purpose, so the arrow keys leave it alone from then on; taking
the last photograph out returns the selection to following the cursor."
  (let* ((current (gui-model-selected-index model))
         (selected (gui-model-selected-indices model))
         (result
           (cond ((not (gui-model-marked-p model))
                  (gui-model-set-selected-indices model (union selected row-indices))
                  (setf (gui-model-marked-p model) t)
                  :marked)
                 ((subsetp row-indices selected)
                  (let ((remaining (set-difference selected row-indices)))
                    (if remaining
                        (gui-model-set-selected-indices model remaining)
                        (progn
                          (gui-model-set-selected-indices model row-indices)
                          (setf (gui-model-marked-p model) nil))))
                  :unmarked)
                 (t
                  (gui-model-set-selected-indices model (union selected row-indices))
                  :marked))))
    (setf (gui-model-selected-index model) current)
    result))

(defun gui-model-selected-job (model)
  "Return MODEL's selected photo job, or NIL for an empty project."
  (nth (gui-model-selected-index model)
       (project-photos (gui-model-project model))))

(defun gui-model-selected-settings (model)
  "Return effective processing settings for MODEL's selected photograph."
  (let* ((project (gui-model-project model))
         (job (gui-model-selected-job model)))
    (if job
        (processing-settings-with-overrides (project-defaults project)
                                            (photo-job-overrides job))
        (project-defaults project))))

(defun gui-model-save-preset (model name)
  "Return the selected effective settings as a preset named NAME.

No checkpoint is taken because nothing in the project changes: the gallery is
global, so the caller writes the result to the still store."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) name)))
    (unless (plusp (length trimmed))
      (error "Preset name cannot be empty."))
    ;; Returned rather than stored on the project: the gallery is global, so
    ;; the caller writes it to the still store. A project's own presets are
    ;; only read now, so older project files keep showing theirs.
    (let ((job (gui-model-selected-job model)))
      (make-processing-preset
       :name trimmed
       :settings (orfeus::copy-processing-settings
                  (gui-model-selected-settings model))
       :source-photo (and job (photo-job-input-path job))
       :disabled-stages (if job
                            (copy-list (photo-job-disabled-stages job))
                            '())))))

(defun gui-model-apply-preset (model name)
  "Apply named preset NAME to all selected photos and return the changed count.

Presets carrying a node graph apply it wholesale; flat presets keep the
override semantics."
  (let* ((project (gui-model-project model))
         (preset (find name (project-presets project)
                       :test #'string-equal :key #'processing-preset-name)))
    (unless preset
      (error "Unknown preset ~S." name))
    (gui-model-checkpoint model)
    (if (orfeus:processing-preset-graph preset)
        (let ((*inside-model-edit* t))
          (gui-model-apply-preset-graph model preset))
        (let ((overrides (orfeus::processing-settings->sexp
                          (processing-preset-settings preset)))
              (jobs (gui-model-acting-jobs model)))
          (dolist (job jobs)
            (setf (photo-job-overrides job) (copy-list overrides)
                  (photo-job-disabled-stages job)
                  (copy-list (processing-preset-disabled-stages preset))
                  (photo-job-graph job) nil))
          (length jobs)))))

(defun setting-reader (key)
  (ecase key
    (:exposure #'orfeus:processing-settings-exposure)
    (:white-balance-temperature #'orfeus:processing-settings-white-balance-temperature)
    (:white-balance-tint #'orfeus:processing-settings-white-balance-tint)
    (:noise-reduction #'orfeus:processing-settings-noise-reduction)
    (:neural-noise-reduction #'orfeus:processing-settings-neural-noise-reduction)
    (:sharpen-amount #'orfeus:processing-settings-sharpen-amount)
    (:sharpen-radius #'orfeus:processing-settings-sharpen-radius)
    (:sharpen-threshold #'orfeus:processing-settings-sharpen-threshold)
    (:tone-blacks #'orfeus:processing-settings-tone-blacks)
    (:tone-shadows #'orfeus:processing-settings-tone-shadows)
    (:tone-dark-mids #'orfeus:processing-settings-tone-dark-mids)
    (:tone-midtones #'orfeus:processing-settings-tone-midtones)
    (:tone-light-mids #'orfeus:processing-settings-tone-light-mids)
    (:tone-highlights #'orfeus:processing-settings-tone-highlights)
    (:tone-whites #'orfeus:processing-settings-tone-whites)
    (:lens-correction-p #'orfeus:processing-settings-lens-correction-p)
    (:lens-correction-strength
     #'orfeus:processing-settings-lens-correction-strength)
    (:chromatic-aberration-correction-p #'orfeus:processing-settings-chromatic-aberration-correction-p)
    (:chromatic-aberration-source
     #'orfeus:processing-settings-chromatic-aberration-source)
    (:lens-distortion #'orfeus:processing-settings-lens-distortion)
    (:lens-profile #'orfeus:processing-settings-lens-profile)
    (:lens-focal-length #'orfeus:processing-settings-lens-focal-length)
    (:lut-path #'orfeus:processing-settings-lut-path)
    (:lut-strength #'orfeus:processing-settings-lut-strength)
    (:grain-amount #'orfeus:processing-settings-grain-amount)
    (:grain-size #'orfeus:processing-settings-grain-size)))

(defun gui-model-setting (model key)
  "Read one effective setting from MODEL.

On a graph-graded photo the value comes from the node a control for KEY
addresses: the selected node when it covers the key, otherwise the most
downstream node of that stage, otherwise the stage's identity value."
  (let ((job (gui-model-selected-job model)))
    (if (and job (photo-job-graph job)
             (eq (gui-model-edit-target model) :photo))
        (let ((node (gui-model-node-for-key model key)))
          (if node
              (getf (orfeus:graph-node-params node) key
                    (getf orfeus::*stage-identity-plist* key))
              (getf orfeus::*stage-identity-plist* key)))
        (funcall (setting-reader key) (gui-model-selected-settings model)))))

(defun plist-put (plist key value)
  (let ((copy (copy-list plist)))
    (if (member key copy)
        (setf (getf copy key) value)
        (setf copy (list* key value copy)))
    copy))

(defun set-default-setting (settings key value)
  (ecase key
    (:exposure (setf (orfeus:processing-settings-exposure settings) value))
    (:white-balance-temperature (setf (orfeus:processing-settings-white-balance-temperature settings) value))
    (:white-balance-tint (setf (orfeus:processing-settings-white-balance-tint settings) value))
    (:noise-reduction (setf (orfeus:processing-settings-noise-reduction settings) value))
    (:neural-noise-reduction
     (setf (orfeus:processing-settings-neural-noise-reduction settings) value))
    (:sharpen-amount (setf (orfeus:processing-settings-sharpen-amount settings) value))
    (:sharpen-radius (setf (orfeus:processing-settings-sharpen-radius settings) value))
    (:sharpen-threshold
     (setf (orfeus:processing-settings-sharpen-threshold settings) value))
    (:tone-blacks (setf (orfeus:processing-settings-tone-blacks settings) value))
    (:tone-shadows (setf (orfeus:processing-settings-tone-shadows settings) value))
    (:tone-dark-mids (setf (orfeus:processing-settings-tone-dark-mids settings) value))
    (:tone-midtones (setf (orfeus:processing-settings-tone-midtones settings) value))
    (:tone-light-mids (setf (orfeus:processing-settings-tone-light-mids settings) value))
    (:tone-highlights (setf (orfeus:processing-settings-tone-highlights settings) value))
    (:tone-whites (setf (orfeus:processing-settings-tone-whites settings) value))
    (:lens-correction-p (setf (orfeus:processing-settings-lens-correction-p settings) value))
    (:lens-correction-strength
     (setf (orfeus:processing-settings-lens-correction-strength settings) value))
    (:chromatic-aberration-correction-p (setf (orfeus:processing-settings-chromatic-aberration-correction-p settings) value))
    (:chromatic-aberration-source
     (setf (orfeus:processing-settings-chromatic-aberration-source settings) value))
    (:lens-distortion (setf (orfeus:processing-settings-lens-distortion settings) value))
    (:lens-profile (setf (orfeus:processing-settings-lens-profile settings) value))
    (:lens-focal-length
     (setf (orfeus:processing-settings-lens-focal-length settings) value))
    (:lut-path (setf (orfeus:processing-settings-lut-path settings) value))
    (:lut-strength (setf (orfeus:processing-settings-lut-strength settings) value))
    (:grain-amount (setf (orfeus:processing-settings-grain-amount settings) value))
    (:grain-size (setf (orfeus:processing-settings-grain-size settings) value)))
  value)

(defun gui-boolean-value (value)
  "Normalize a frontend checkbox VALUE to a Common Lisp boolean."
  (not (or (null value)
           (eql value 0)
           (and (stringp value)
                (member value '("" "0" "false" "nil")
                        :test #'string-equal)))))

(defun stage-of-setting-key (key)
  (loop for (stage keys) in orfeus::*grade-stages*
        when (member key keys) return stage))

(defun gui-model-set-setting (model key value)
  "Set KEY to VALUE at MODEL's current :PHOTO or :DEFAULTS edit target.

Graph-graded photos route the edit to the node a control for KEY addresses;
when no node of that stage exists yet, one is created at the end of the
chain, exactly like adding a node before dragging its slider."
  (when (member key '(:lens-correction-p
                      :chromatic-aberration-correction-p))
    (setf value (gui-boolean-value value)))
  (gui-model-checkpoint model (list :setting key))
  (let ((*inside-model-edit* t))
   (cond
    ((eq (gui-model-edit-target model) :defaults)
     (set-default-setting (project-defaults (gui-model-project model))
                          key value))
    (t
     (let ((job (or (gui-model-selected-job model)
                    (error "Cannot edit a photo in an empty project."))))
       (if (photo-job-graph job)
           (let ((node (or (gui-model-node-for-key model key)
                           (gui-model-add-node model
                                               (stage-of-setting-key key)))))
             (gui-model-set-node-params model node (list key value)))
           (setf (photo-job-overrides job)
                 (plist-put (photo-job-overrides job) key value)))))))
  value)

(defun gui-model-acting-jobs (model)
  "Return the selected photo jobs, falling back to the current photograph."
  (or (gui-model-selected-jobs model)
      (let ((job (gui-model-selected-job model)))
        (and job (list job)))))

(defun gui-model-reset-selected (model)
  "Clear overrides, stage bypasses, and node graphs from selected photos."
  (gui-model-checkpoint model)
  (dolist (job (gui-model-acting-jobs model))
    (setf (photo-job-overrides job) '()
          (orfeus:photo-job-disabled-stages job) '()
          (photo-job-graph job) nil))
  (setf (gui-model-selected-node model) nil)
  model)

(defun gui-model-render-settings (model)
  "Return the selected photo's settings as rendered, honoring stage bypass."
  (let ((project (gui-model-project model))
        (job (gui-model-selected-job model)))
    (if job
        (orfeus:photo-render-settings project job)
        (project-defaults project))))

(defun gui-model-stage-bypassed-p (model stage)
  "Return true when STAGE is bypassed on the selected photograph."
  (let ((job (gui-model-selected-job model)))
    (and job
         (member stage (orfeus:photo-job-disabled-stages job))
         t)))

(defun gui-model-toggle-stage (model stage)
  "Toggle STAGE bypass across the selection; primary photo decides direction.
Returns :BYPASSED, :ENABLED, or :NONE without a photograph."
  (gui-model-checkpoint model)
  (let ((jobs (gui-model-acting-jobs model)))
    (if (null jobs)
        :none
        (let ((enable (and (member stage (orfeus:photo-job-disabled-stages
                                          (first jobs)))
                           t)))
          (dolist (job jobs)
            (setf (orfeus:photo-job-disabled-stages job)
                  (if enable
                      (remove stage (orfeus:photo-job-disabled-stages job))
                      (adjoin stage (orfeus:photo-job-disabled-stages job)))))
          (if enable :enabled :bypassed)))))

(defun stage-setting-inert-p (key grade)
  "True when KEY cannot affect rendering given the other GRADE values."
  (case key
    (:lut-strength (null (getf grade :lut-path)))
    (:grain-size (let ((amount (getf grade :grain-amount)))
                   (or (null amount) (not (plusp amount)))))
    (:lens-correction-strength (not (getf grade :lens-correction-p)))
    (t nil)))

(defun stage-setting-equal (first-value second-value)
  (cond ((and (null first-value) (null second-value)) t)
        ((and (realp first-value) (realp second-value))
         (= first-value second-value))
        ((and (stringp first-value) (stringp second-value))
         (string= first-value second-value))
        (t (eql first-value second-value))))

(defun gui-model-stage-adjusted-p (model stage)
  "True when STAGE departs from identity in the selected effective settings."
  (let ((grade (orfeus:settings-grade-plist
                (gui-model-selected-settings model) (list stage))))
    (loop for (key value) on grade by #'cddr
          thereis (and (not (stage-setting-inert-p key grade))
                       (not (stage-setting-equal
                             value
                             (getf orfeus::*stage-identity-plist* key)))))))

(defun gui-model-grab-still (model)
  "Capture the current grade as an auto-named still preset, or NIL.

The still stores the full node graph (deriving one for flat photos), so
applying it reproduces the grade node for node.

The preset is returned rather than added to the project: the gallery is global,
like a PowerGrade album, so a look grabbed while grading one shoot is there for
the next one. The caller writes it to the still store.

A still is meant to outlive the card it was shot from — it is a saved look, a
film stock, a reversal recipe, kept to apply to other photographs later. So the
source RAW is interned as part of grabbing it, and the still refers to that copy.
Returns a second value: the condition when interning failed, in which case the
still still exists but points at wherever the photograph currently is."
  (gui-model-checkpoint model)
  (let ((job (gui-model-selected-job model)))
    (when job
      (let* ((project (gui-model-project model))
             (source (photo-job-input-path job))
             (failure nil)
             (kept (handler-case (orfeus:intern-raw-file source)
                     (error (condition)
                       (setf failure condition)
                       source)))
             (preset (orfeus:make-processing-preset
                      :name (orfeus:next-still-preset-name project job)
                      :settings (orfeus:photo-render-settings project job)
                      :source-photo kept
                      :graph (gui-model-copy-graph model))))
        (values preset failure)))))

(defun gui-model-copy-grade (model)
  "Return the selected photo's grade plist and bypass list, or NIL."
  (let ((job (gui-model-selected-job model)))
    (when job
      (values (orfeus:settings-grade-plist (gui-model-selected-settings model))
              (copy-list (orfeus:photo-job-disabled-stages job))))))

(defun gui-model-paste-grade (model grade disabled-stages)
  "Apply GRADE and DISABLED-STAGES to every selected photo; return the count."
  (gui-model-checkpoint model)
  (let ((jobs (gui-model-acting-jobs model)))
    (dolist (job jobs)
      (orfeus:photo-job-apply-grade job grade disabled-stages))
    (length jobs)))

;;; Node graph editing.

(defun swap-graph-node-positions (one other)
  "Exchange two nodes' editor positions.

Reordering the chain rewires it and renumbers ids, but the boxes are drawn from
a position stored on each node. Without this the wires move and the nodes do
not, which reads as the command having done nothing at all."
  (let ((one-place (orfeus:graph-node-position one))
        (other-place (orfeus:graph-node-position other)))
    (setf (orfeus:graph-node-position one) other-place
          (orfeus:graph-node-position other) one-place)))

(defun reflow-graph-node-positions (graph from)
  "Drop stored positions from node FROM onwards so the column re-derives them.

A node inserted mid-chain arrives with no position and is placed by its index,
which is the slot the node after it already occupies — so the two would be drawn
on top of one another. Nodes above the insertion keep where they were put."
  (let* ((nodes (orfeus:processing-graph-nodes graph))
         (start (position from nodes)))
    (when start
      (dolist (node (nthcdr start nodes))
        (setf (orfeus:graph-node-position node) nil)))))

(defun gui-model-display-graph (model)
  "Return the graph to draw for the selected photo, without converting it.

Photographs still on flat settings get an equivalent throwaway graph."
  (let ((job (gui-model-selected-job model)))
    (when job
      (or (photo-job-graph job)
          (orfeus:settings->graph (gui-model-selected-settings model)
                                  (photo-job-disabled-stages job))))))

(defun gui-model-ensure-graph (model)
  "Convert the selected photo to graph grading when it is still flat."
  (let ((job (gui-model-selected-job model)))
    (when (and job (null (photo-job-graph job)))
      (gui-model-checkpoint model))
    (when job
      (or (photo-job-graph job)
          (let ((graph (orfeus:settings->graph
                        (gui-model-selected-settings model)
                        (photo-job-disabled-stages job))))
            (setf (photo-job-graph job) graph
                  (photo-job-overrides job) '()
                  (orfeus:photo-job-disabled-stages job) '())
            graph)))))

(defun gui-model-selected-graph-node (model)
  "Return the selected node when it still belongs to the photo's graph."
  (let ((graph (let ((job (gui-model-selected-job model)))
                 (and job (photo-job-graph job))))
        (node (gui-model-selected-node model)))
    (when (and graph node
               (member node (orfeus:processing-graph-nodes graph)))
      node)))

(defun gui-node-covers-key-p (node key)
  (and (orfeus:graph-node-filter-p node)
       (member key (orfeus:grade-stage-keys (orfeus:graph-node-kind node)))
       t))

(defun gui-model-node-for-key (model key)
  "Return the node a sidebar control for KEY reads and writes.

The selected node wins when its stage covers KEY; otherwise the most
downstream node of that stage."
  (let ((job (gui-model-selected-job model)))
    (when (and job (photo-job-graph job))
      (let ((selected (gui-model-selected-graph-node model)))
        (if (and selected (gui-node-covers-key-p selected key))
            selected
            (orfeus:graph-last-node-covering-key (photo-job-graph job) key))))))

(defparameter *default-crop-inset* 0.04
  "How far a fresh crop sits inside the frame, as a fraction of each edge.

Not zero, because a crop that starts on the frame edge puts its handles exactly
where the image ends and there is nothing to grab: the pointer is either on the
handle's outermost pixel or outside the picture. An inset costs a little of the
frame and makes the rectangle usable immediately, and typing 100 into the size
fields gets the whole frame back.")

(defun default-crop-params ()
  "The parameters a crop node starts with."
  (let ((inset *default-crop-inset*))
    (list :left (float inset 1.0) :top (float inset 1.0)
          :width (float (- 1.0 (* 2 inset)) 1.0)
          :height (float (- 1.0 (* 2 inset)) 1.0)
          :angle 0.0)))

(defun gui-model-add-node (model kind &key after params)
  "Insert a KIND node into the selected photo's graph and select it.

Without AFTER, film nodes append at the output and every other kind lands at
the end of the scene-linear chain. Signals INVALID-PROJECT-DATA when the
placement breaks the film-domain rules."
  (gui-model-checkpoint model)
  (let ((graph (let ((*inside-model-edit* t))
                 (gui-model-ensure-graph model))))
    (when graph
      (let* ((requested (or after
                            (if (eq kind :film)
                                (orfeus:processing-graph-output graph)
                                (orfeus:graph-tail-linear-node-id graph))))
             ;; Clamped to where the kind is legal: right-clicking the film node
             ;; and asking for a grade correction used to fail validation and
             ;; look like the menu entry did nothing.
             (after-id (orfeus:graph-insertion-point graph requested kind))
             (node (orfeus:graph-insert-node
                    graph after-id kind
                    :params (or params
                                (and (eq kind :crop) (default-crop-params))))))
        (reflow-graph-node-positions graph node)
        (setf (gui-model-selected-node model) node)
        node))))

(defun gui-model-delete-node (model node)
  "Delete NODE from the selected photo's graph."
  (gui-model-checkpoint model)
  (let ((job (gui-model-selected-job model)))
    (when (and job (photo-job-graph job))
      (orfeus:graph-delete-node (photo-job-graph job)
                                (orfeus:graph-node-id node))
      (when (eq node (gui-model-selected-node model))
        (setf (gui-model-selected-node model) nil))
      t)))

(defun gui-model-move-node (model node direction)
  "Move NODE one step :EARLIER or :LATER along its chain; true on success.

A move the graph will not accept is an answer, not a failure: swapping a crop
above an optics node is refused because optics cannot read a cropped branch, and
the caller wants to say so plainly rather than show the validator's report. The
swap rolls itself back before signalling, so the graph is untouched either way."
  (handler-case (gui-model-move-node-1 model node direction)
    (orfeus:invalid-project-data (condition)
      ;; The reason comes back as a second value so the caller can say why
      ;; rather than only that it did not happen. Guessing at the rule from a
      ;; refusal is not something a user should have to do.
      (values nil (orfeus:invalid-project-data-reason condition)))))

(defun gui-model-move-node-1 (model node direction)
  (gui-model-checkpoint model)
  (let ((job (gui-model-selected-job model)))
    (when (and job (photo-job-graph job))
      (let* ((graph (photo-job-graph job))
             ;; The partner has to be found before the swap, which renumbers.
             (partner
               (ecase direction
                 (:earlier
                  (let ((upstream (first (orfeus:graph-node-inputs node))))
                    (and upstream (orfeus:graph-find-node graph upstream))))
                 (:later
                  (let ((consumers (orfeus:graph-consumers
                                    graph (orfeus:graph-node-id node))))
                    (when (and (= 1 (length consumers))
                               (not (orfeus:graph-node-blend-p
                                     (first consumers))))
                      (first consumers)))))))
        (when partner
          (let ((moved (orfeus:graph-swap-with-upstream
                        graph
                        (orfeus:graph-node-id (if (eq direction :earlier)
                                                  node
                                                  partner)))))
            (when moved
              (swap-graph-node-positions node partner)
              t)))))))

(defun gui-model-set-node-kind (model node kind)
  "Assign a correction KIND to NODE on the selected photo's graph.

An untyped node is legal anywhere, including after the film tail, so the kind it
is given may not be. Rather than refuse — which looked like the Correction
dropdown doing nothing — the node moves upstream to the last position that kind
can occupy. Returns a second value: the node's new upstream neighbour when it
had to move."
  (gui-model-checkpoint model)
  (let* ((job (gui-model-selected-job model))
         (graph (and job (photo-job-graph job))))
    (when (and graph
               (member node (orfeus:processing-graph-nodes graph)))
      (let* ((id (orfeus:graph-node-id node))
             (upstream (first (orfeus:graph-node-inputs node)))
             (legal (orfeus:graph-insertion-point graph upstream kind))
             (moved (and (/= legal upstream)
                         (let ((*inside-model-edit* t))
                           (orfeus:graph-move-node-after graph id legal)
                           t))))
        (values (let ((*inside-model-edit* t))
                  (orfeus:graph-set-node-kind graph
                                              (orfeus:graph-node-id node)
                                              kind))
                (and moved legal))))))

(defun gui-model-set-primary-input (model node input-id)
  "Point NODE's primary input at INPUT-ID on the selected photo's graph."
  (gui-model-checkpoint model)
  (let* ((job (gui-model-selected-job model))
         (graph (and job (photo-job-graph job))))
    (when (and graph
               (member node (orfeus:processing-graph-nodes graph)))
      (orfeus:graph-set-primary-input graph (orfeus:graph-node-id node)
                                      input-id))))

(defun gui-model-set-output (model node)
  "Make NODE the selected photo's graph output."
  (gui-model-checkpoint model)
  (let* ((job (gui-model-selected-job model))
         (graph (and job (photo-job-graph job))))
    (when (and graph
               (member node (orfeus:processing-graph-nodes graph)))
      (orfeus:graph-set-output graph (orfeus:graph-node-id node)))))

(defun gui-model-rewire-node (model node after-id)
  "Splice NODE to read AFTER-ID on the selected photo's graph.

Returns true when the graph changed; invalid placements signal after the
core rolls the graph back."
  (gui-model-checkpoint model)
  (let* ((job (gui-model-selected-job model))
         (graph (and job (photo-job-graph job))))
    (when (and graph
               (member node (orfeus:processing-graph-nodes graph)))
      (orfeus:graph-move-node-after graph (orfeus:graph-node-id node)
                                    after-id))))

(defun gui-model-set-blend-input (model blend source-node)
  "Point BLEND's second branch at SOURCE-NODE (or NIL for the source)."
  (gui-model-checkpoint model)
  (let* ((job (gui-model-selected-job model))
         (graph (and job (photo-job-graph job))))
    (when (and graph
               (member blend (orfeus:processing-graph-nodes graph)))
      (orfeus:graph-set-blend-input
       graph (orfeus:graph-node-id blend)
       (if source-node
           (orfeus:graph-node-id source-node)
           orfeus:*graph-source-id*)))))

(defun gui-model-toggle-node (model node)
  "Toggle NODE's bypass; returns :BYPASSED or :ENABLED."
  (gui-model-checkpoint model)
  (setf (orfeus:graph-node-bypassed-p node)
        (not (orfeus:graph-node-bypassed-p node)))
  (if (orfeus:graph-node-bypassed-p node) :bypassed :enabled))

(defun gui-model-set-node-params (model node new-params)
  "Merge NEW-PARAMS into NODE's parameters, validating the graph."
  (gui-model-checkpoint model (list :node-params (orfeus:graph-node-id node)))
  (let* ((job (gui-model-selected-job model))
         (graph (and job (photo-job-graph job)))
         (previous (orfeus:graph-node-params node)))
    (setf (orfeus:graph-node-params node)
          (orfeus::plist-merge previous new-params))
    (when graph
      (handler-case (orfeus:graph-validate graph)
        (error (condition)
          (setf (orfeus:graph-node-params node) previous)
          (error condition))))
    node))

(defun gui-model-copy-graph (model)
  "Return an independent copy of the selected photo's grading graph, or NIL."
  (let ((graph (gui-model-display-graph model)))
    (when graph
      (orfeus:graph-copy graph))))

(defun graph-with-bypassed-kinds (graph kinds)
  (let ((copy (orfeus:graph-copy graph)))
    (dolist (node (orfeus:processing-graph-nodes copy))
      (when (member (orfeus:graph-node-kind node) kinds)
        (setf (orfeus:graph-node-bypassed-p node) t)))
    copy))

(defun gui-model-paste-graph (model graph &key bypass-kinds)
  "Set an independent copy of GRAPH on every selected photo.

BYPASS-KINDS lists node kinds switched off in the applied copies, so a grade
can be taken without, say, its optics or white balance. Returns the count."
  (gui-model-checkpoint model)
  (let ((jobs (gui-model-acting-jobs model))
        (template (if bypass-kinds
                      (graph-with-bypassed-kinds graph bypass-kinds)
                      graph)))
    (dolist (job jobs)
      (setf (photo-job-graph job) (orfeus:graph-copy template)
            (photo-job-overrides job) '()
            (orfeus:photo-job-disabled-stages job) '()))
    (length jobs)))

(defun gui-model-preset-graph (preset)
  "Return PRESET's graph, deriving one from flat settings when needed."
  (or (orfeus:processing-preset-graph preset)
      (orfeus:settings->graph (processing-preset-settings preset)
                              (processing-preset-disabled-stages preset))))

(defun gui-model-apply-preset-graph (model preset &key bypass-kinds)
  "Apply PRESET's node graph to the whole selection; returns the count."
  (gui-model-checkpoint model)
  (let ((*inside-model-edit* t))
    (gui-model-paste-graph model (gui-model-preset-graph preset)
                           :bypass-kinds bypass-kinds)))

(defun gui-photo-output-path (model job)
  "Return JOB's render output using the shared core project semantics."
  (photo-job-render-output (gui-model-project model) job))

(defun gui-preview-event-current-p (model event generation)
  "Return true when preview completion EVENT still matches MODEL and GENERATION."
  (let ((job (gui-model-selected-job model)))
    (and job
         (= (second event) generation)
         (= (third event) (gui-model-selected-index model))
         (eq (fourth event) job))))
