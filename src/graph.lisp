(in-package #:orfeus)

;;; Graph operations. The node and graph structures themselves live in
;;; graph-types.lisp, which is compiled before the files that serialize and
;;; copy them, so their accessors can be inlined at those call sites.

(defun graph-filter-kind-p (kind)
  (and (assoc kind *grade-stages*) t))

(defun graph-node-filter-p (node)
  (graph-filter-kind-p (graph-node-kind node)))

(defun graph-node-blend-p (node)
  (eq (graph-node-kind node) :blend))

(defun graph-node-kinds ()
  "Every node kind a graph may contain, in menu order."
  (append (grade-stages) *graph-only-node-kinds*))

(defun color-subtract-params-validate (params)
  (unless (and (listp params)
               (plist-known-keys-p params *color-subtract-keys*))
    (graph-invalid params "expected :red :green :blue color parameters"))
  (loop for (key value) on params by #'cddr
        unless (and (realp value) (<= 0 value 4))
          do (graph-invalid params "color component ~S must be within 0..4"
                            key))
  params)

(defun contrast-params-validate (params)
  (unless (and (listp params) (plist-known-keys-p params *contrast-keys*))
    (graph-invalid params "expected :contrast and :pivot parameters"))
  (let ((contrast (getf params :contrast 1.0))
        (pivot (getf params :pivot 0.435)))
    (unless (and (realp contrast) (<= 1/5 contrast 4))
      (graph-invalid params "contrast slope must be within 0.2..4"))
    (unless (and (realp pivot) (<= 1/20 pivot 19/20))
      (graph-invalid params "contrast pivot must be within 0.05..0.95")))
  params)

(defun sharpen-params-validate (params)
  (unless (and (listp params) (plist-known-keys-p params *sharpen-keys*))
    (graph-invalid params "expected :amount :radius :threshold parameters"))
  (let ((amount (getf params :amount 0.0))
        (radius (getf params :radius 1.0))
        (threshold (getf params :threshold 2.0)))
    (unless (and (realp amount) (<= 0 amount 3))
      (graph-invalid params "sharpen amount must be within 0..3"))
    (unless (and (realp radius) (<= 3/10 radius 5))
      (graph-invalid params "sharpen radius must be within 0.3..5 pixels"))
    (unless (and (realp threshold) (<= 0 threshold 8))
      (graph-invalid params "sharpen threshold must be within 0..8 deviations")))
  params)

(defun curve-points-validate (points key)
  (unless (and (listp points)
               (evenp (length points))
               (<= (* 2 *minimum-curve-points*)
                   (length points)
                   (* 2 *maximum-curve-points*))
               (every (lambda (value) (and (realp value) (<= 0 value 1)))
                      points))
    (graph-invalid points "~S must hold ~D to ~D (x y) points within 0..1"
                   key *minimum-curve-points* *maximum-curve-points*))
  (loop for (x nil next-x) on points by #'cddr
        while next-x
        unless (< (+ x 0.001) next-x)
          do (graph-invalid points "~S positions must ascend" key))
  points)

(defun curves-params-validate (params)
  (unless (and (listp params)
               (plist-known-keys-p params *curve-channel-keys*))
    (graph-invalid params
                   "expected :red-points :green-points :blue-points :master-points"))
  (dolist (key *curve-channel-keys*)
    (let ((points (getf params key)))
      (when points
        (curve-points-validate points key))))
  params)

(defun rotate-params-validate (params)
  (unless (and (listp params) (plist-known-keys-p params *rotate-keys*))
    (graph-invalid params "expected a :quarter-turns parameter"))
  (let ((turns (getf params :quarter-turns 0)))
    (unless (and (integerp turns) (<= 0 turns 3))
      (graph-invalid params "rotation must be 0, 1, 2, or 3 quarter turns")))
  params)

(defun crop-params-validate (params)
  (unless (and (listp params)
               (plist-known-keys-p params *crop-keys*))
    (graph-invalid params "expected :left :top :width :height parameters"))
  (let ((left (getf params :left 0.0))
        (top (getf params :top 0.0))
        (width (getf params :width 1.0))
        (height (getf params :height 1.0))
        (angle (getf params :angle 0.0)))
    (unless (and (realp left) (realp top) (realp width) (realp height)
                 (<= 0 left 1) (<= 0 top 1)
                 (<= 0.05 width 1) (<= 0.05 height 1)
                 (<= (+ left width) 1.0001)
                 (<= (+ top height) 1.0001))
      (graph-invalid params "crop rectangle must stay inside the frame"))
    (unless (and (realp angle) (<= -45 angle 45))
      (graph-invalid params "crop angle must be within -45..45 degrees")))
  params)

(defun graph-find-node (graph id)
  "Return the node of GRAPH with ID, or NIL for the source id."
  (find id (processing-graph-nodes graph) :key #'graph-node-id))

(defun graph-invalid (datum control &rest arguments)
  (error 'invalid-project-data
         :datum datum
         :reason (apply #'format nil control arguments)))

(defun graph-node-params-validate (node)
  (let ((params (graph-node-params node))
        (keys (grade-stage-keys (graph-node-kind node))))
    (processing-plist-validate params)
    (loop for (key nil) on params by #'cddr
          unless (member key keys)
            do (graph-invalid params "key ~S does not belong to stage ~S"
                              key (graph-node-kind node)))))

(defun graph-map-film-lut-paths (graph function)
  (when graph
    (dolist (node (processing-graph-nodes graph))
      (flet ((map-params (params)
               (let ((copy (copy-tree params)))
                 (when (getf copy :lut-path)
                   (setf (getf copy :lut-path)
                         (funcall function (getf copy :lut-path))))
                 copy)))
        (when (eq :film (graph-node-kind node))
          (setf (graph-node-params node)
                (map-params (graph-node-params node))))
        (let ((state (assoc :film (graph-node-kind-states node))))
          (when state
            (let ((params-tail (member :params (rest state))))
              (when params-tail
                (setf (second params-tail)
                      (map-params (second params-tail))))))))))
  graph)

(defun project-resolve-graph-lut-paths (graph base-directory)
  "Resolve active and dormant film LUT paths in GRAPH against BASE-DIRECTORY."
  (graph-map-film-lut-paths
   graph
   (lambda (lut-path)
     (namestring
      (project-resolve-pathname (pathname lut-path) base-directory)))))

(defun still-store-canonicalize-graph-lut-paths (graph)
  "Canonicalize active and dormant film LUT paths for durable local storage."
  (graph-map-film-lut-paths
   graph
   (lambda (lut-path)
     (namestring (still-store-canonical-pathname lut-path)))))

(defun graph-crop-geometry (upstream node)
  "Return NODE's canonical geometry history with NODE's own reframing appended.

Crops and rotations both belong here. Two branches of a blend must have been
reframed identically to be mixed, and an optics node cannot resample a branch
whose frame has already moved, whichever of the two moved it."
  (let ((params (graph-node-params node)))
    (if (eq :rotate (graph-node-kind node))
        (append upstream (list (list :rotate (getf params :quarter-turns 0))))
        (append upstream
                (list (list (getf params :left 0.0)
                            (getf params :top 0.0)
                            (getf params :width 1.0)
                            (getf params :height 1.0)
                            (getf params :angle 0.0)))))))

(defun graph-node-position-validate (position node)
  (unless (or (null position)
              (and (listp position)
                   (ignore-errors (= 2 (list-length position)))
                   (every (lambda (value)
                            (and (realp value) (<= -100000 value 100000)))
                          position)))
    (graph-invalid node
                   "node :position must be two coordinates within -100000..100000"))
  position)

(defun graph-kind-params-validate (kind params datum)
  (cond
    ((member kind '(:blend :node))
     (when params
       (graph-invalid datum "node kind ~S cannot carry parameters" kind)))
    ((eq kind :color-subtract) (color-subtract-params-validate params))
    ((eq kind :crop) (crop-params-validate params))
    ((eq kind :rotate) (rotate-params-validate params))
    ((eq kind :curves) (curves-params-validate params))
    ((eq kind :contrast) (contrast-params-validate params))
    ((eq kind :sharpen) (sharpen-params-validate params))
    ((graph-filter-kind-p kind)
     (graph-node-params-validate
      (make-graph-node :kind kind :params params)))
    (t (graph-invalid datum "unknown saved node kind ~S" kind))))

(defun graph-kind-states-validate (states node)
  (unless (ignore-errors (list-length states))
    (graph-invalid node "node kind history must be a proper list"))
  (let ((seen '()))
    (dolist (entry states)
      (unless (and (consp entry)
                   (keywordp (first entry))
                   (ignore-errors (evenp (length (rest entry))))
                   (plist-known-keys-p (rest entry)
                                          '(:params :opacity :second-input)))
        (graph-invalid node "malformed saved node kind state ~S" entry))
      (let* ((kind (first entry))
             (state (rest entry))
             (opacity (getf state :opacity 1.0))
             (second-input (getf state :second-input *graph-source-id*)))
        (when (member kind seen)
          (graph-invalid node "duplicate saved node kind ~S" kind))
        (push kind seen)
        (graph-kind-params-validate kind (getf state :params '()) entry)
        (if (eq kind :blend)
            (progn
              (unless (and (realp opacity) (<= 0 opacity 1))
                (graph-invalid entry "saved blend opacity must be within 0..1"))
              (unless (and (integerp second-input) (not (minusp second-input)))
                (graph-invalid entry "saved blend input must be a node id")))
            (when (or (member :opacity state) (member :second-input state))
              (graph-invalid entry
                             "only saved blend state may carry blend fields"))))))
  states)

(defun graph-validate (graph)
  "Signal INVALID-PROJECT-DATA unless GRAPH is well formed; return GRAPH.

Checks topological id order, input arity and reference validity, per-stage
parameter keys, blend opacities, compatible branch crop geometry, optics before
crop, and the display-domain rule that only film nodes may consume film output
while blends stay scene-linear."
  (let ((seen (list *graph-source-id*))
        (display '())
        (geometry (make-hash-table)))
    (setf (gethash *graph-source-id* geometry) '())
    (dolist (node (processing-graph-nodes graph))
      (let ((id (graph-node-id node))
            (kind (graph-node-kind node))
            (inputs (graph-node-inputs node)))
        (graph-node-position-validate (graph-node-position node) node)
        (graph-kind-states-validate (graph-node-kind-states node) node)
        (when (member id seen)
          (graph-invalid node "node id ~S is not strictly increasing" id))
        (unless (> id (first seen))
          (graph-invalid node "node id ~S is not strictly increasing" id))
        (dolist (input inputs)
          (unless (member input seen)
            (graph-invalid node "node ~S input ~S is not upstream" id input)))
        (cond
          ((graph-node-blend-p node)
           (unless (= 2 (length inputs))
             (graph-invalid node "blend node ~S needs exactly two inputs" id))
           (let ((opacity (graph-node-opacity node)))
             (unless (and (realp opacity) (<= 0 opacity 1))
               (graph-invalid node "blend opacity ~S must be within 0..1"
                              opacity)))
           (dolist (input inputs)
             (when (member input display)
               (graph-invalid node
                              "blend node ~S cannot consume film output" id))))
          ((eq kind :color-subtract)
           (unless (= 1 (length inputs))
             (graph-invalid node "filter node ~S needs exactly one input" id))
           (color-subtract-params-validate (graph-node-params node))
           (when (member (first inputs) display)
             (graph-invalid node "node ~S cannot process film output" id)))
          ((eq kind :crop)
           (unless (= 1 (length inputs))
             (graph-invalid node "filter node ~S needs exactly one input" id))
           (crop-params-validate (graph-node-params node))
           ;; Crops keep their branch's domain.
           (when (member (first inputs) display)
             (push id display)))
          ((eq kind :rotate)
           (unless (= 1 (length inputs))
             (graph-invalid node "filter node ~S needs exactly one input" id))
           (rotate-params-validate (graph-node-params node))
           ;; A rotation is indifferent to which domain it turns, like a crop.
           (when (member (first inputs) display)
             (push id display)))
          ((eq kind :curves)
           (unless (= 1 (length inputs))
             (graph-invalid node "filter node ~S needs exactly one input" id))
           (curves-params-validate (graph-node-params node))
           (when (member (first inputs) display)
             (graph-invalid node "node ~S cannot process film output" id)))
          ((member kind '(:contrast :sharpen))
           (unless (= 1 (length inputs))
             (graph-invalid node "filter node ~S needs exactly one input" id))
           (if (eq kind :contrast)
               (contrast-params-validate (graph-node-params node))
               (sharpen-params-validate (graph-node-params node)))
           (when (member (first inputs) display)
             (graph-invalid node "node ~S cannot process film output" id)))
          ((eq kind :node)
           ;; An untyped container: passes its branch through unchanged,
           ;; in either domain, until a correction type is assigned.
           (unless (= 1 (length inputs))
             (graph-invalid node "filter node ~S needs exactly one input" id))
           (when (graph-node-params node)
             (graph-invalid node "untyped node ~S cannot carry parameters"
                            id))
           (when (member (first inputs) display)
             (push id display)))
          ((graph-filter-kind-p kind)
           (unless (= 1 (length inputs))
             (graph-invalid node "filter node ~S needs exactly one input" id))
           (graph-node-params-validate node)
           (when (and (eq kind :optics)
                      (not (graph-node-bypassed-p node))
                      (gethash (first inputs) geometry))
             (graph-invalid node
                            "optics node ~S cannot process cropped output" id))
           (if (eq kind :film)
               (pushnew id display)
               (when (member (first inputs) display)
                 (graph-invalid node
                                "node ~S cannot process film output" id))))
          (t (graph-invalid node "unknown node kind ~S" kind)))
        (setf (gethash id geometry)
              (cond
                ;; Match GRAPH-EFFECTIVE-NODES exactly: every bypassed node,
                ;; including blends, resolves to its first input.
                ((graph-node-bypassed-p node)
                 (copy-tree (gethash (first inputs) geometry)))
                ((graph-node-blend-p node)
                 (let ((first-geometry (gethash (first inputs) geometry))
                       (second-geometry (gethash (second inputs) geometry)))
                   (unless (equalp first-geometry second-geometry)
                     (graph-invalid node
                                    "blend node ~S inputs have incompatible crop geometry"
                                    id))
                   (copy-tree first-geometry)))
                ((member kind '(:crop :rotate))
                 (graph-crop-geometry
                  (copy-tree (gethash (first inputs) geometry)) node))
                (t
                 (copy-tree (gethash (first inputs) geometry)))))
        (push id seen)))
    (let ((output (processing-graph-output graph)))
      (unless (member output seen)
        (graph-invalid graph "graph output ~S is not a node" output)))
    graph))

(defun graph-node->sexp (node &key render-only-p)
  (append (list :id (graph-node-id node)
                :kind (graph-node-kind node)
                :inputs (copy-list (graph-node-inputs node)))
          (when (graph-node-params node)
            (list :params (copy-tree (graph-node-params node))))
          (when (graph-node-blend-p node)
            (list :opacity (graph-node-opacity node)))
          (when (graph-node-bypassed-p node)
            (list :bypassed-p t))
          (unless render-only-p
            (append
             (when (graph-node-position node)
               (list :position (copy-list (graph-node-position node))))
             (when (graph-node-kind-states node)
               (list :kind-states
                     (copy-tree (graph-node-kind-states node))))))))

(defun graph->sexp (graph)
  "Convert validated GRAPH to its portable project S-expression."
  (graph-validate graph)
  (list :nodes (mapcar #'graph-node->sexp (processing-graph-nodes graph))
        :output (processing-graph-output graph)))

(defun graph->render-sexp (graph)
  "Return GRAPH's render identity, excluding editor positions and kind history."
  (graph-validate graph)
  (list :nodes (mapcar (lambda (node)
                         (graph-node->sexp node :render-only-p t))
                       (processing-graph-nodes graph))
        :output (processing-graph-output graph)))

(defun sexp->graph-node (sexp)
  (unless (and (listp sexp)
               (plist-known-keys-p
                sexp '(:id :kind :inputs :params :opacity :bypassed-p
                       :position :kind-states)))
    (graph-invalid sexp "expected a graph node property list"))
  (let ((position (getf sexp :position))
        (id (getf sexp :id)))
    (graph-node-position-validate position sexp)
    ;; Checked here rather than after construction: the slot is typed
    ;; (INTEGER 1), so a bad id from a project file would signal a raw type
    ;; error from MAKE-GRAPH-NODE instead of naming the malformed data.
    (unless (and (integerp id) (plusp id))
      (graph-invalid sexp "node id ~S must be a positive integer" id))
    (make-graph-node :id id
                     :kind (getf sexp :kind)
                     :params (getf sexp :params '())
                     :opacity (getf sexp :opacity 1.0)
                     :inputs (getf sexp :inputs '())
                     :bypassed-p (and (getf sexp :bypassed-p) t)
                     :position (when position (copy-list position))
                     :kind-states (copy-tree (getf sexp :kind-states '())))))

(defun sexp->graph (sexp)
  "Validate and convert a graph S-expression into a PROCESSING-GRAPH."
  (unless (and (listp sexp)
               (plist-known-keys-p sexp '(:nodes :output)))
    (graph-invalid sexp "expected a graph property list"))
  (graph-validate
   (make-processing-graph
    :nodes (mapcar #'sexp->graph-node (getf sexp :nodes '()))
    :output (getf sexp :output *graph-source-id*))))

(defparameter *flat-pipeline-stage-order*
  '(:white-balance :exposure :optics :noise-reduction :tone :film)
  "The order the flat pipeline actually executes its stages in.
Graph conversion must chain nodes this way — notably optics before noise
reduction — so a converted photograph renders identically.")

(defun settings->graph (settings &optional disabled-stages)
  "Chain the non-identity stages of SETTINGS as a linear graph.

Stages listed in DISABLED-STAGES become bypassed nodes so their grades stay
editable. Nodes follow the flat pipeline's execution order, so the result
renders identically to the flat pipeline."
  (disabled-stages-validate disabled-stages)
  (let ((nodes '())
        (next-id 1)
        (previous *graph-source-id*))
    (dolist (stage *flat-pipeline-stage-order*)
      (let* ((params (settings-grade-plist settings (list stage)))
             (bypassed (and (member stage disabled-stages) t))
             (active (loop for (key value) on params by #'cddr
                             thereis (and (not (grade-key-inert-p key params))
                                          (not (equal value
                                                      (getf
                                                       *stage-identity-plist*
                                                       key)))))))
        (when (or active bypassed)
          (push (make-graph-node :id next-id
                                 :kind stage
                                 :params params
                                 :inputs (list previous)
                                 :bypassed-p bypassed)
                nodes)
          (setf previous next-id)
          (incf next-id))))
    (make-processing-graph :nodes (nreverse nodes) :output previous)))

(defun graph-effective-nodes (graph)
  "Return GRAPH's reachable, non-bypassed nodes with inputs resolved.

Bypassed filters pass their input through; bypassed blends pass their first
input. Nodes that do not contribute to the output are dropped. The result is
a fresh topologically ordered node list whose references remain valid."
  (graph-validate graph)
  (let ((forward (make-hash-table)))
    (setf (gethash *graph-source-id* forward) *graph-source-id*)
    (let ((kept '()))
      (dolist (node (processing-graph-nodes graph))
        (let ((resolved (mapcar (lambda (input) (gethash input forward))
                                (graph-node-inputs node))))
          (if (or (graph-node-bypassed-p node)
                  ;; Untyped containers render as passthrough.
                  (eq :node (graph-node-kind node)))
              (setf (gethash (graph-node-id node) forward) (first resolved))
              (progn
                (setf (gethash (graph-node-id node) forward)
                      (graph-node-id node))
                (push (let ((copy (copy-graph-node node)))
                        (setf (graph-node-inputs copy) resolved)
                        copy)
                      kept)))))
      (let* ((nodes (nreverse kept))
             (output (gethash (processing-graph-output graph) forward))
             (needed (list output)))
        (dolist (node (reverse nodes))
          (when (member (graph-node-id node) needed)
            (dolist (input (graph-node-inputs node))
              (pushnew input needed))))
        (values (remove-if-not (lambda (node)
                                 (member (graph-node-id node) needed))
                               nodes)
                output)))))

(defun graph-remap-kind-state-inputs (node mapping)
  (dolist (entry (graph-node-kind-states node))
    (when (eq :blend (first entry))
      (let ((tail (member :second-input (rest entry))))
        (when tail
          (multiple-value-bind (mapped present-p)
              (gethash (second tail) mapping)
            (setf (second tail) (if present-p mapped *graph-source-id*)))))))
  node)

(defun graph-normalize (graph)
  "Topologically order GRAPH's nodes and renumber them 1..N.

Relative order between independent nodes follows their current listing, so
edits stay visually stable. Signals INVALID-PROJECT-DATA on cycles."
  (let ((remaining (copy-list (processing-graph-nodes graph)))
        (placed (list *graph-source-id*))
        (ordered '()))
    (loop while remaining
          do (let ((ready (find-if (lambda (node)
                                     (every (lambda (input)
                                              (member input placed))
                                            (graph-node-inputs node)))
                                   remaining)))
               (unless ready
                 (graph-invalid graph "graph contains a cycle"))
               (push (graph-node-id ready) placed)
               (push ready ordered)
               (setf remaining (remove ready remaining))))
    (setf (processing-graph-nodes graph) (nreverse ordered)))
  (let ((mapping (make-hash-table))
        (next-id 1))
    (setf (gethash *graph-source-id* mapping) *graph-source-id*)
    (dolist (node (processing-graph-nodes graph))
      (setf (gethash (graph-node-id node) mapping) next-id)
      (incf next-id))
    (dolist (node (processing-graph-nodes graph))
      (setf (graph-node-id node) (gethash (graph-node-id node) mapping)
            (graph-node-inputs node)
            (mapcar (lambda (input) (gethash input mapping))
                    (graph-node-inputs node)))
      (graph-remap-kind-state-inputs node mapping))
    (setf (processing-graph-output graph)
          (gethash (processing-graph-output graph) mapping))
    (graph-validate graph)))

(defun graph-consumers (graph id)
  "Return the nodes of GRAPH that read node ID directly."
  (remove-if-not (lambda (node) (member id (graph-node-inputs node)))
                 (processing-graph-nodes graph)))

(defun graph-insert-node (graph after-id kind &key params (opacity 0.5)
                                                second-input)
  "Insert a new node reading AFTER-ID; every consumer of AFTER-ID moves to it.

For KIND :blend, SECOND-INPUT names the other branch (default: the source).
Returns the new node after normalizing GRAPH."
  (unless (or (eql after-id *graph-source-id*) (graph-find-node graph after-id))
    (graph-invalid graph "cannot insert after unknown node ~S" after-id))
  (let* ((blend (eq kind :blend))
         (node (make-graph-node
                :id (1+ (reduce #'max (processing-graph-nodes graph)
                                :key #'graph-node-id
                                :initial-value *graph-source-id*))
                :kind kind
                :params (and (not blend) (copy-tree params))
                :opacity (if blend opacity 1.0)
                :inputs (if blend
                            (list after-id (or second-input *graph-source-id*))
                            (list after-id)))))
    (call-with-graph-rollback
     graph
     (lambda ()
       (dolist (consumer (graph-consumers graph after-id))
         (setf (graph-node-inputs consumer)
               (substitute (graph-node-id node) after-id
                           (graph-node-inputs consumer))))
       (when (eql (processing-graph-output graph) after-id)
         (setf (processing-graph-output graph) (graph-node-id node)))
       (setf (processing-graph-nodes graph)
             (append (processing-graph-nodes graph) (list node)))
       (graph-normalize graph)
       node))))

(defun graph-replace-kind-state-input (node old-id replacement)
  (dolist (entry (graph-node-kind-states node))
    (when (eq :blend (first entry))
      (let ((tail (member :second-input (rest entry))))
        (when (and tail (eql (second tail) old-id))
          (setf (second tail) replacement)))))
  node)

(defun graph-delete-node (graph id)
  "Remove node ID; its consumers and dormant blend states read its first input.

An invalid result rolls GRAPH back and re-signals."
  (let ((node (or (graph-find-node graph id)
                  (graph-invalid graph "cannot delete unknown node ~S" id))))
    (call-with-graph-rollback
     graph
     (lambda ()
       (let ((replacement (first (graph-node-inputs node))))
         (dolist (consumer (graph-consumers graph id))
           (setf (graph-node-inputs consumer)
                 (substitute replacement id (graph-node-inputs consumer))))
         (dolist (other (processing-graph-nodes graph))
           (unless (eq other node)
             (graph-replace-kind-state-input other id replacement)))
         (when (eql (processing-graph-output graph) id)
           (setf (processing-graph-output graph) replacement))
         (setf (processing-graph-nodes graph)
               (remove node (processing-graph-nodes graph)))
         (graph-normalize graph))))))

(defun graph-swap-with-upstream (graph id)
  "Swap filter node ID with its single upstream filter neighbor, when both
sides of the pair are plain single-input filters. Returns true on success."
  (let* ((node (graph-find-node graph id))
         (upstream-id (and node (first (graph-node-inputs node))))
         (upstream (and upstream-id (graph-find-node graph upstream-id))))
    (when (and node upstream
               (not (graph-node-blend-p node))
               (not (graph-node-blend-p upstream))
               (not (eq :film (graph-node-kind node)))
               (not (eq :film (graph-node-kind upstream)))
               (= 1 (length (graph-consumers graph upstream-id))))
      (call-with-graph-rollback
       graph
       (lambda ()
         (let ((below (first (graph-node-inputs upstream))))
           (dolist (consumer (graph-consumers graph id))
             (setf (graph-node-inputs consumer)
                   (substitute upstream-id id (graph-node-inputs consumer))))
           (when (eql (processing-graph-output graph) id)
             (setf (processing-graph-output graph) upstream-id))
           (setf (graph-node-inputs node) (list below)
                 (graph-node-inputs upstream) (list id))
           (setf (processing-graph-nodes graph)
                 (let ((nodes (remove node (processing-graph-nodes graph))))
                   (let ((position (position upstream nodes)))
                     (append (subseq nodes 0 position)
                             (list node)
                             (subseq nodes position)))))
           (graph-normalize graph)
           t))))))

(defun call-with-graph-rollback (graph thunk)
  "Run THUNK; restore GRAPH's node state, order, and output on any error."
  (let ((entries (mapcar (lambda (node)
                           (list node (graph-node-id node)
                                 (copy-list (graph-node-inputs node))
                                 (graph-node-kind node)
                                 (copy-tree (graph-node-params node))
                                 (graph-node-opacity node)
                                 (copy-tree (graph-node-kind-states node))))
                         (processing-graph-nodes graph)))
        (nodes (copy-list (processing-graph-nodes graph)))
        (output (processing-graph-output graph)))
    (handler-case (funcall thunk)
      (error (condition)
        (dolist (entry entries)
          (destructuring-bind (node id inputs kind params opacity kind-states)
              entry
            (setf (graph-node-id node) id
                  (graph-node-inputs node) inputs
                  (graph-node-kind node) kind
                  (graph-node-params node) params
                  (graph-node-opacity node) opacity
                  (graph-node-kind-states node) kind-states)))
        (setf (processing-graph-nodes graph) nodes
              (processing-graph-output graph) output)
        (error condition)))))

(defun graph-save-node-kind-state (node)
  (let* ((kind (graph-node-kind node))
         (state (append
                 (when (graph-node-params node)
                   (list :params (copy-tree (graph-node-params node))))
                 (when (graph-node-blend-p node)
                   (list :opacity (graph-node-opacity node)
                         :second-input (second (graph-node-inputs node))))))
         (entry (cons kind state)))
    (setf (graph-node-kind-states node)
          (cons entry
                (remove kind (graph-node-kind-states node)
                        :key #'first)))
    entry))

(defun graph-restorable-second-input (graph node state)
  (let ((input (getf state :second-input *graph-source-id*)))
    (if (and (integerp input)
             (< input (graph-node-id node))
             (or (eql input *graph-source-id*) (graph-find-node graph input)))
        input
        *graph-source-id*)))

(defun graph-set-node-kind (graph id kind)
  "Change node ID's correction KIND without discarding prior kind settings.

Each kind's parameters, and a blend's opacity and second input, survive later
kind switches and portable project round trips. Converting away keeps the
primary input. Returns true when the kind changed; invalid results roll back."
  (let ((node (or (graph-find-node graph id)
                  (graph-invalid graph "cannot retype unknown node ~S" id))))
    (unless (or (eq kind :node) (member kind (graph-node-kinds)))
      (graph-invalid graph "unknown node kind ~S" kind))
    (if (eq kind (graph-node-kind node))
        nil
        (call-with-graph-rollback
         graph
         (lambda ()
           (graph-save-node-kind-state node)
           (let ((state (rest (assoc kind (graph-node-kind-states node)))))
             (setf (graph-node-kind node) kind
                   (graph-node-params node)
                   (copy-tree (getf state :params '())))
             (if (eq kind :blend)
                 (setf (graph-node-inputs node)
                       (list (first (graph-node-inputs node))
                             (graph-restorable-second-input graph node state))
                       (graph-node-opacity node) (getf state :opacity 0.5))
                 (setf (graph-node-inputs node)
                       (list (first (graph-node-inputs node)))
                       (graph-node-opacity node) 1.0)))
           (graph-normalize graph)
           t)))))

(defun graph-set-primary-input (graph id input-id)
  "Point node ID's primary input at INPUT-ID (or the source).

Returns true when the wiring changed; invalid wiring (cycles included)
rolls the graph back and re-signals."
  (let ((node (or (graph-find-node graph id)
                  (graph-invalid graph "cannot rewire unknown node ~S" id))))
    (unless (or (eql input-id *graph-source-id*)
                (graph-find-node graph input-id))
      (graph-invalid graph "cannot read unknown node ~S" input-id))
    (cond
      ((eql input-id id)
       (graph-invalid graph "a node cannot read itself"))
      ((eql input-id (first (graph-node-inputs node))) nil)
      (t
       (call-with-graph-rollback
        graph
        (lambda ()
          (setf (graph-node-inputs node)
                (cons input-id (rest (graph-node-inputs node))))
          (graph-normalize graph)
          t))))))

(defun graph-set-output (graph id)
  "Make node ID the graph output; invalid choices roll back and re-signal."
  (unless (graph-find-node graph id)
    (graph-invalid graph "cannot output unknown node ~S" id))
  (if (eql (processing-graph-output graph) id)
      nil
      (call-with-graph-rollback
       graph
       (lambda ()
         (setf (processing-graph-output graph) id)
         (graph-normalize graph)
         t))))

(defun graph-move-node-after (graph id after-id)
  "Splice node ID out of its chain and re-insert it reading AFTER-ID.

Consumers of ID re-read its first input, then consumers of AFTER-ID move to
ID, exactly like inserting a fresh node there. A blend keeps its second
branch. Returns true when the graph changed; an invalid placement rolls the
graph back and re-signals."
  (let ((node (or (graph-find-node graph id)
                  (graph-invalid graph "cannot move unknown node ~S" id))))
    (unless (or (eql after-id *graph-source-id*)
                (graph-find-node graph after-id))
      (graph-invalid graph "cannot move after unknown node ~S" after-id))
    (cond
      ((eql id after-id) nil)
      ((eql (first (graph-node-inputs node)) after-id) nil)
      (t
       (call-with-graph-rollback
        graph
        (lambda ()
          (let ((replacement (first (graph-node-inputs node))))
            (dolist (consumer (graph-consumers graph id))
              (setf (graph-node-inputs consumer)
                    (substitute replacement id
                                (graph-node-inputs consumer))))
            (when (eql (processing-graph-output graph) id)
              (setf (processing-graph-output graph) replacement))
            (dolist (consumer (graph-consumers graph after-id))
              (unless (eq consumer node)
                (setf (graph-node-inputs consumer)
                      (substitute id after-id
                                  (graph-node-inputs consumer)))))
            (setf (graph-node-inputs node)
                  (cons after-id (rest (graph-node-inputs node))))
            (when (eql (processing-graph-output graph) after-id)
              (setf (processing-graph-output graph) id)))
          (graph-normalize graph)
          t))))))

(defun graph-set-blend-input (graph id source-id)
  "Point blend node ID's second branch at SOURCE-ID (or the source).

Returns true when the wiring changed; an invalid wiring (cycles included)
rolls the graph back and re-signals."
  (let ((node (or (graph-find-node graph id)
                  (graph-invalid graph "cannot rewire unknown node ~S" id))))
    (unless (graph-node-blend-p node)
      (graph-invalid graph "node ~S is not a blend" id))
    (unless (or (eql source-id *graph-source-id*)
                (graph-find-node graph source-id))
      (graph-invalid graph "cannot read unknown node ~S" source-id))
    (cond
      ((eql source-id id)
       (graph-invalid graph "a blend cannot read itself"))
      ((eql source-id (second (graph-node-inputs node))) nil)
      (t
       (call-with-graph-rollback
        graph
        (lambda ()
          (setf (graph-node-inputs node)
                (list (first (graph-node-inputs node)) source-id))
          (graph-normalize graph)
          t))))))

(defun graph-kind-accepts-display-p (kind)
  "True when KIND may read a branch that has already passed through film."
  (and (member kind '(:film :crop :node)) t))

(defun graph-display-domain-p (graph id)
  "True when the branch ending at ID has passed through a film node.

Mirrors the rule GRAPH-VALIDATE enforces: a film node converts its branch to
display space, a crop or an untyped node keeps whatever domain it was handed,
and nothing else may consume display output at all. The source is scene-linear."
  (let ((node (graph-find-node graph id)))
    (and node
         (case (graph-node-kind node)
           (:film t)
           ((:crop :node)
            (graph-display-domain-p graph (first (graph-node-inputs node))))
           (t nil)))))

(defun graph-insertion-point (graph after-id kind)
  "The furthest downstream position at or above AFTER-ID that KIND may occupy.

A scene-linear correction cannot read a film node's output, so asking for one
after the film tail used to fail outright: the node did not appear and the
reason went to the status bar, which read as the button doing nothing. Walking
upstream to the last position where the kind is legal puts it where the user
can only have meant, since a grade node placed after the film transform would
have nothing sensible to do there anyway."
  (cond ((graph-kind-accepts-display-p kind) after-id)
        ((not (graph-display-domain-p graph after-id)) after-id)
        (t (let ((node (graph-find-node graph after-id)))
             (if node
                 (graph-insertion-point graph (first (graph-node-inputs node))
                                        kind)
                 after-id)))))

(defun graph-can-insert-p (graph after-id kind)
  "True when a KIND node inserted directly after AFTER-ID would be well formed.

Decided by trying it on a copy rather than by restating the rules, so this
cannot drift from GRAPH-VALIDATE as the rules grow. It already answers for more
than the film domain: an optics node may not read a cropped branch either, and
a blend may not consume film output.

Copying a graph per candidate kind is only worth it because the caller is a
context menu being opened by hand."
  (handler-case
      (progn (graph-insert-node (graph-copy graph) after-id kind) t)
    (invalid-project-data () nil)))

(defun graph-insertable-kinds (graph after-id)
  "The node kinds that may be inserted directly after AFTER-ID, in menu order."
  (remove-if-not (lambda (kind) (graph-can-insert-p graph after-id kind))
                 (graph-node-kinds)))

(defun graph-tail-linear-node-id (graph)
  "Return the last node id on the output path before any film tail."
  (let ((id (processing-graph-output graph)))
    (loop for node = (graph-find-node graph id)
          while (and node (eq :film (graph-node-kind node)))
          do (setf id (first (graph-node-inputs node))))
    id))

(defun graph-last-node-covering-key (graph key)
  "Return the most downstream node whose kind's stage covers setting KEY."
  (let ((stage (loop for (name keys) in *grade-stages*
                     when (member key keys) return name)))
    (when stage
      (find stage (reverse (processing-graph-nodes graph))
            :key #'graph-node-kind))))

(defun graph-copy (graph)
  "Return a structurally independent copy of GRAPH."
  (make-processing-graph
   :nodes (mapcar (lambda (node)
                    (let ((copy (copy-graph-node node)))
                      (setf (graph-node-params copy)
                            (copy-tree (graph-node-params node))
                            (graph-node-inputs copy)
                            (copy-list (graph-node-inputs node))
                            (graph-node-position copy)
                            (copy-list (graph-node-position node))
                            (graph-node-kind-states copy)
                            (copy-tree (graph-node-kind-states node)))
                      copy))
                  (processing-graph-nodes graph))
   :output (processing-graph-output graph)))

(defun default-processing-graph ()
  "Return the default graph: lens corrections piped into noise reduction."
  (make-processing-graph
   :nodes (list (make-graph-node
                 :id 1
                 :kind :optics
                 :params (list :lens-correction-p t
                               :lens-correction-strength 1.0
                               :chromatic-aberration-correction-p t)
                 :inputs (list *graph-source-id*))
                (make-graph-node
                 :id 2
                 :kind :noise-reduction
                 :params (list :noise-reduction 0.35
                               :neural-noise-reduction 0.0)
                 :inputs (list 1)))
   :output 2))
