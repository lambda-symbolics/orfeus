(in-package #:orfeus)

;;; Processing graphs: an ordered DAG of piped filter nodes and blend nodes.
;;;
;;; Node id 0 always denotes the decoded RAW source. Filter nodes carry the
;;; parameters of exactly one pipeline stage and one input; blend nodes mix
;;; two upstream results by opacity in scene-linear space. Film nodes work in
;;; display space, so they may only sit on the tail of the graph: nothing but
;;; further film nodes may consume them, and no blend may. Photographs without
;;; a graph keep the flat settings pipeline unchanged.

(defparameter *graph-source-id* 0
  "The reserved node id of the decoded RAW source image.")

(defparameter *graph-only-node-kinds* '(:blend :color-subtract :crop :curves)
  "Node kinds that exist only in graphs, beyond the flat pipeline stages.

:COLOR-SUBTRACT computes picked-color minus pixel per channel in scene-linear
space, the film-negative inversion primitive. :CROP keeps a normalized
rectangle given in display (oriented) coordinates, so one graph fits both
previews and full-resolution exports. :CURVES applies a monotone spline per
channel on the encoded signal, the per-stock decompression for inverted
negatives.")

(defparameter *color-subtract-keys* '(:red :green :blue))

(defparameter *crop-keys* '(:left :top :width :height :angle))

(defparameter *curve-channel-keys* '(:red-points :green-points :blue-points))

(defparameter *identity-curve-points*
  '(0.0 0.0 0.33 0.33 0.67 0.67 1.0 1.0)
  "Four (x y) control points along the diagonal: the do-nothing curve.")

(defstruct graph-node
  "One processing node: a stage filter, or a blend of two branches."
  (id 1 :type (integer 1))
  (kind :exposure)
  (params '())
  (opacity 1.0)
  (inputs (list 0))
  (bypassed-p nil))

(defstruct processing-graph
  "A topologically ordered processing DAG ending at OUTPUT."
  (nodes '())
  (output 0))

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

(defun curve-points-validate (points key)
  (unless (and (listp points) (= 8 (length points))
               (every (lambda (value) (and (realp value) (<= 0 value 1)))
                      points))
    (graph-invalid points "~S must hold four (x y) points within 0..1" key))
  (loop for (x nil next-x) on points by #'cddr
        while next-x
        unless (< (+ x 0.001) next-x)
          do (graph-invalid points "~S positions must ascend" key))
  points)

(defun curves-params-validate (params)
  (unless (and (listp params)
               (plist-known-keys-p params *curve-channel-keys*))
    (graph-invalid params
                   "expected :red-points :green-points :blue-points"))
  (dolist (key *curve-channel-keys*)
    (let ((points (getf params key)))
      (when points
        (curve-points-validate points key))))
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

(defun graph-validate (graph)
  "Signal INVALID-PROJECT-DATA unless GRAPH is well formed; return GRAPH.

Checks topological id order, input arity and reference validity, per-stage
parameter keys, blend opacities, and the display-domain rule that only film
nodes may consume film output while blends stay scene-linear."
  (let ((seen (list *graph-source-id*))
        (display '()))
    (dolist (node (processing-graph-nodes graph))
      (let ((id (graph-node-id node))
            (kind (graph-node-kind node))
            (inputs (graph-node-inputs node)))
        (unless (and (integerp id) (plusp id))
          (graph-invalid node "node id ~S must be a positive integer" id))
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
          ((eq kind :curves)
           (unless (= 1 (length inputs))
             (graph-invalid node "filter node ~S needs exactly one input" id))
           (curves-params-validate (graph-node-params node))
           (when (member (first inputs) display)
             (graph-invalid node "node ~S cannot process film output" id)))
          ((graph-filter-kind-p kind)
           (unless (= 1 (length inputs))
             (graph-invalid node "filter node ~S needs exactly one input" id))
           (graph-node-params-validate node)
           (if (eq kind :film)
               (pushnew id display)
               (when (member (first inputs) display)
                 (graph-invalid node
                                "node ~S cannot process film output" id))))
          (t (graph-invalid node "unknown node kind ~S" kind)))
        (push id seen)))
    (let ((output (processing-graph-output graph)))
      (unless (member output seen)
        (graph-invalid graph "graph output ~S is not a node" output)))
    graph))

(defun graph-node->sexp (node)
  (append (list :id (graph-node-id node)
                :kind (graph-node-kind node)
                :inputs (copy-list (graph-node-inputs node)))
          (when (graph-node-params node)
            (list :params (copy-list (graph-node-params node))))
          (when (graph-node-blend-p node)
            (list :opacity (graph-node-opacity node)))
          (when (graph-node-bypassed-p node)
            (list :bypassed-p t))))

(defun graph->sexp (graph)
  "Convert GRAPH to its portable S-expression representation."
  (list :nodes (mapcar #'graph-node->sexp (processing-graph-nodes graph))
        :output (processing-graph-output graph)))

(defun sexp->graph-node (sexp)
  (unless (and (listp sexp)
               (plist-known-keys-p
                sexp '(:id :kind :inputs :params :opacity :bypassed-p)))
    (graph-invalid sexp "expected a graph node property list"))
  (make-graph-node :id (getf sexp :id 0)
                   :kind (getf sexp :kind)
                   :params (getf sexp :params '())
                   :opacity (getf sexp :opacity 1.0)
                   :inputs (getf sexp :inputs '())
                   :bypassed-p (and (getf sexp :bypassed-p) t)))

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
          (if (graph-node-bypassed-p node)
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
                    (graph-node-inputs node))))
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
                :params (and (not blend) (copy-list params))
                :opacity (if blend opacity 1.0)
                :inputs (if blend
                            (list after-id (or second-input *graph-source-id*))
                            (list after-id)))))
    (dolist (consumer (graph-consumers graph after-id))
      (setf (graph-node-inputs consumer)
            (substitute (graph-node-id node) after-id
                        (graph-node-inputs consumer))))
    (when (eql (processing-graph-output graph) after-id)
      (setf (processing-graph-output graph) (graph-node-id node)))
    (setf (processing-graph-nodes graph)
          (append (processing-graph-nodes graph) (list node)))
    (graph-normalize graph)
    node))

(defun graph-delete-node (graph id)
  "Remove node ID; its consumers re-read the node's first input."
  (let ((node (or (graph-find-node graph id)
                  (graph-invalid graph "cannot delete unknown node ~S" id))))
    (let ((replacement (first (graph-node-inputs node))))
      (dolist (consumer (graph-consumers graph id))
        (setf (graph-node-inputs consumer)
              (substitute replacement id (graph-node-inputs consumer))))
      (when (eql (processing-graph-output graph) id)
        (setf (processing-graph-output graph) replacement))
      (setf (processing-graph-nodes graph)
            (remove node (processing-graph-nodes graph)))
      (graph-normalize graph))))

(defun graph-swap-with-upstream (graph id)
  "Swap filter node ID with its single upstream filter neighbor, when both
sides of the pair are plain single-input filters. Returns true on success."
  (let* ((node (graph-find-node graph id))
         (upstream-id (and node (first (graph-node-inputs node))))
         (upstream (and upstream-id (graph-find-node graph upstream-id))))
    (when (and node upstream
               (not (graph-node-blend-p node))
               (not (graph-node-blend-p upstream))
               (= 1 (length (graph-consumers graph upstream-id))))
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
        t))))

(defun call-with-graph-rollback (graph thunk)
  "Run THUNK; restore GRAPH's ids, inputs, order, and output on any error."
  (let ((entries (mapcar (lambda (node)
                           (list node (graph-node-id node)
                                 (copy-list (graph-node-inputs node))))
                         (processing-graph-nodes graph)))
        (nodes (copy-list (processing-graph-nodes graph)))
        (output (processing-graph-output graph)))
    (handler-case (funcall thunk)
      (error (condition)
        (dolist (entry entries)
          (destructuring-bind (node id inputs) entry
            (setf (graph-node-id node) id
                  (graph-node-inputs node) inputs)))
        (setf (processing-graph-nodes graph) nodes
              (processing-graph-output graph) output)
        (error condition)))))

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
                            (copy-list (graph-node-params node))
                            (graph-node-inputs copy)
                            (copy-list (graph-node-inputs node)))
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
