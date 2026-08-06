(in-package #:orfeus)

;;; Core data definitions, kept ahead of everything that reads them.
;;;
;;; The pipeline stage tables, the settings structure, and the graph structures
;;; live here rather than beside the code that operates on them, because
;;; native.lisp serializes graphs and settings and is compiled first. Defining
;;; them later left SBCL unable to inline the graph accessors and warning about
;;; an undefined stage table.

(defparameter *processing-setting-keys*
  '(:exposure :white-balance-temperature :white-balance-tint
    :noise-reduction :neural-noise-reduction
    :tone-blacks :tone-shadows :tone-dark-mids
    :tone-midtones :tone-light-mids :tone-highlights :tone-whites
    :lens-correction-p :lens-correction-strength
    :chromatic-aberration-correction-p :lut-path :lut-strength
    :grain-amount :grain-size)
  "Keys accepted in processing setting S-expressions.")

(defstruct processing-settings
  "Frontend-independent controls for one rendering operation."
  (exposure 0.0)
  (white-balance-temperature nil)
  (white-balance-tint 0.0)
  (noise-reduction 0.35)
  (neural-noise-reduction 0.0)
  (tone-blacks 0.0)
  (tone-shadows 0.0)
  (tone-dark-mids 0.0)
  (tone-midtones 0.0)
  (tone-light-mids 0.0)
  (tone-highlights 0.0)
  (tone-whites 0.0)
  (lens-correction-p t)
  (lens-correction-strength 1.0)
  (chromatic-aberration-correction-p t)
  (lut-path nil)
  (lut-strength 1.0)
  (grain-amount 0.0)
  (grain-size 1.0))

(defparameter *grade-stages*
  '((:white-balance (:white-balance-temperature :white-balance-tint))
    (:exposure (:exposure))
    (:noise-reduction (:noise-reduction :neural-noise-reduction))
    (:tone (:tone-blacks :tone-shadows :tone-dark-mids :tone-midtones
            :tone-light-mids :tone-highlights :tone-whites))
    (:optics (:lens-correction-p :lens-correction-strength
              :chromatic-aberration-correction-p))
    (:film (:lut-path :lut-strength :grain-amount :grain-size)))
  "The fixed processing pipeline as named stages over setting keys.
Together the stages partition *PROCESSING-SETTING-KEYS*; frontends present
them as a copyable node chain.")

(defparameter *stage-identity-plist*
  '(:white-balance-temperature nil :white-balance-tint 0.0
    :exposure 0.0
    :noise-reduction 0.0 :neural-noise-reduction 0.0
    :tone-blacks 0.0 :tone-shadows 0.0 :tone-dark-mids 0.0 :tone-midtones 0.0
    :tone-light-mids 0.0 :tone-highlights 0.0 :tone-whites 0.0
    :lens-correction-p nil :lens-correction-strength 1.0
    :chromatic-aberration-correction-p nil
    :lut-path nil :lut-strength 0.0 :grain-amount 0.0 :grain-size 1.0)
  "Setting values under which every stage passes pixels through unchanged.")

(defun grade-stage-keys (stage)
  "Return the setting keys belonging to pipeline STAGE."
  (or (second (assoc stage *grade-stages*))
      (error "Unknown grade stage ~S." stage)))

(defun grade-stages ()
  "Return the pipeline stage names in processing order."
  (mapcar #'first *grade-stages*))

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

(defparameter *curve-channel-keys*
  '(:red-points :green-points :blue-points :master-points)
  "Curve channels in wire order: the three channels, then luma over all.")

(defparameter *identity-curve-points*
  '(0.0 0.0 0.33 0.33 0.67 0.67 1.0 1.0)
  "Four (x y) control points along the diagonal: the do-nothing curve.")

(defstruct graph-node
  "One processing node: a stage filter, or a blend of two branches.

KIND :NODE is an untyped container fresh from \"New Node\": it passes its
branch through unchanged until the user assigns a correction type.
POSITION, when set, is the node's (x y) spot on the graph editor canvas."
  (id 1 :type (integer 1))
  (kind :node)
  (params '())
  (opacity 1.0)
  (inputs (list 0))
  (bypassed-p nil)
  (position nil)
  ;; Previous correction configurations survive kind switching and project
  ;; round trips.  Entries are (KIND . STATE-PLIST).
  (kind-states '()))

(defstruct processing-graph
  "A topologically ordered processing DAG ending at OUTPUT."
  (nodes '())
  (output 0))
