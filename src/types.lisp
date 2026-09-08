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
    :sharpen-amount :sharpen-radius :sharpen-threshold
    :tone-blacks :tone-shadows :tone-dark-mids
    :tone-midtones :tone-light-mids :tone-highlights :tone-whites
    :lens-correction-p :lens-correction-strength
    :chromatic-aberration-correction-p :chromatic-aberration-source
    :lens-distortion :lens-profile :lens-focal-length :demosaic
    :lut-path :lut-strength :grain-amount :grain-size)
  "Keys accepted in processing setting S-expressions.")

(defparameter *demosaic-methods* '(:rcd :ppg)
  "How the sensor's colour mosaic may be interpolated: Ratio Corrected
Demosaicing, or Pattern Pixel Grouping. The renderer's choice, so it lives on
the optics node, the front of the pipeline; the source itself has no panel.")

(defstruct processing-settings
  "Frontend-independent controls for one rendering operation."
  (exposure 0.0)
  (white-balance-temperature nil)
  (white-balance-tint 0.0)
  ;; Set together with the sharpening below, against the camera's own JPEG on
  ;; one flat patch of sky at the camera's preview size: at 0.35 the sharpened
  ;; default matched the camera's noise there to within two percent, at 0.45 it
  ;; sits about thirty percent under it in the finest band while the textured
  ;; parts of the frame keep all but a quarter of a percent of their detail.
  (noise-reduction 0.45)
  (neural-noise-reduction 0.0)
  ;; Sharpening by default, by as much as the camera does. Calibrated against
  ;; the OM-1's own JPEG (Sharpness at Soft, which is how these frames were
  ;; shot): the spectral gain the camera adds over an unsharpened render, read
  ;; on textured patches, is matched to within a few percent by an unsharp
  ;; mask of this amount and radius; see `sharpen-compare` in the session notes.
  ;; The radius is in pixels of the full frame and scales with a preview.
  (sharpen-amount 0.5)
  (sharpen-radius 1.5)
  (sharpen-threshold 2.0)
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
  ;; Where the colour fringing correction comes from: :MEASURED reads how far
  ;; red and blue sit from green in the photograph itself, :PROFILE trusts the
  ;; lens database. Measured is the default because the database describes
  ;; somebody else's copy of the lens, and on the one frame this was checked
  ;; against it prescribed fringing the frame did not have.
  (chromatic-aberration-source :measured)
  ;; Hand-set barrel or pincushion correction, for the lenses no database
  ;; describes. Positive straightens barrel, negative straightens pincushion.
  (lens-distortion 0.0)
  ;; A lens profile chosen by hand, by its database name, for a lens the
  ;; photograph's metadata does not identify; and the focal length to read it
  ;; at when the file records none. NIL leaves both to the metadata.
  (lens-profile nil)
  (lens-focal-length nil)
  ;; RCD reads fine texture without the maze PPG draws near the sensor's
  ;; limit, for two to three times PPG's small share of a render.
  (demosaic :rcd)
  (lut-path nil)
  (lut-strength 1.0)
  (grain-amount 0.0)
  (grain-size 1.0))

(defparameter *grade-stages*
  '((:white-balance (:white-balance-temperature :white-balance-tint))
    (:exposure (:exposure))
    (:noise-reduction (:noise-reduction :neural-noise-reduction))
    (:sharpen (:sharpen-amount :sharpen-radius :sharpen-threshold))
    (:tone (:tone-blacks :tone-shadows :tone-dark-mids :tone-midtones
            :tone-light-mids :tone-highlights :tone-whites))
    (:optics (:lens-correction-p :lens-correction-strength
              :chromatic-aberration-correction-p :chromatic-aberration-source
              :lens-distortion :lens-profile :lens-focal-length :demosaic))
    (:film (:lut-path :lut-strength :grain-amount :grain-size)))
  "The fixed processing pipeline as named stages over setting keys.
Together the stages partition *PROCESSING-SETTING-KEYS*; frontends present
them as a copyable node chain.")

(defparameter *stage-identity-plist*
  '(:white-balance-temperature nil :white-balance-tint 0.0
    :exposure 0.0
    :noise-reduction 0.0 :neural-noise-reduction 0.0
    :sharpen-amount 0.0 :sharpen-radius 1.5 :sharpen-threshold 2.0
    :tone-blacks 0.0 :tone-shadows 0.0 :tone-dark-mids 0.0 :tone-midtones 0.0
    :tone-light-mids 0.0 :tone-highlights 0.0 :tone-whites 0.0
    :lens-correction-p nil :lens-correction-strength 1.0
    :chromatic-aberration-correction-p nil :chromatic-aberration-source :measured
    :lens-distortion 0.0 :lens-profile nil :lens-focal-length nil
    :demosaic :rcd
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

(defparameter *rotate-keys* '(:quarter-turns))

(defparameter *quarter-turn-labels*
  '((0 . "None") (1 . "90 clockwise") (2 . "180") (3 . "270 clockwise"))
  "Rotation amounts a rotate node offers, as quarter turns clockwise.")

(defparameter *graph-only-node-kinds*
  '(:blend :color-subtract :negative :contrast :hdr :dust :vignette :clarity :dehaze :crop
    :rotate :flip :curves)
  "Node kinds that exist only in graphs, beyond the flat pipeline stages.

:COLOR-SUBTRACT computes picked-color minus pixel per channel in scene-linear
space, the linear inversion a colourist builds from a layer mixer. :NEGATIVE
inverts by density instead, the way a print does: the film base is divided out
channel by channel, which removes the orange mask exactly, and ten to the power
of the remaining dye density times a paper gamma restores the scene, white
anchored at the frame's brightest tones. :CROP keeps a normalized
rectangle given in display (oriented) coordinates, so one graph fits both
previews and full-resolution exports. :ROTATE turns the frame by whole quarter
turns, which is the part of orientation a crop's -45..45 degree angle cannot
reach. :CURVES applies a monotone spline per channel on the encoded signal, the
per-stock decompression for inverted negatives. :CONTRAST is a straight slope in
the logarithm of the signal about a fixed tone, which is the operator DaVinci's
contrast control applies. :SHARPEN is an unsharp mask on brightness alone, with
the frame's own noise floor kept out of what it amplifies. :FLIP mirrors the
frame across either axis, which is the part of orientation a rotation cannot
reach — a negative laid on the light table emulsion side up comes out mirrored,
and no amount of turning fixes a mirror. :HDR compresses the tonal range the
way the camera's HDR modes do: a slope in the logarithm of luminance about a
displayed pivot, the shadow lift eased into a cap, colour kept. :DUST fills the
specks dust leaves on a scan — compact patches darker or lighter than their
surroundings by a stated contrast, no wider than a stated size — from the sound
pixels around them. :VIGNETTE darkens or lightens the frame away from its
centre in scene-linear light, as a lens does, so a highlight in a darkened
corner stays a highlight. :CLARITY is Lightroom's clarity: the contrast
between each pixel and its wide surroundings, raised or lowered in the
midtones, colour kept. :DEHAZE takes the veiling light of haze out by the
dark channel prior, measured from the whole frame; negative amounts lay it
on.")

(defparameter *flip-keys* '(:horizontal :vertical)
  "Parameters of a flip node: which axes to mirror across.")

(defparameter *contrast-keys* '(:contrast :pivot)
  "Parameters of a contrast node: the slope, and the tone it turns about.")

(defparameter *hdr-keys* '(:lift :strength :pivot :shadows)
  "Parameters of an HDR node: a plain exposure lift in stops for a frame held
back at capture; the strength of the compression, where 0 leaves the tones
alone and 1 would flatten them entirely; the displayed brightness that stays
put; and the most the shadows may be lifted, in stops.")

(defparameter *hdr-presets*
  '((:hdr1 . (:lift 0.0 :strength 0.4 :pivot 0.47 :shadows 0.7))
    (:hdr2 . (:lift 0.0 :strength 0.8 :pivot 0.63 :shadows 3.4)))
  "The camera's two HDR modes as an HDR node, measured off an OM-1.

Fitted against the camera's own HDR JPEGs, read through the inverse of its
normal rendering — measured from normal frames of the same day — against
Orfeus's neutral development of the RAW it kept. HDR1 came out as a slope of
0.6 in the log about displayed middle grey with two thirds of a stop of shadow
lift, within a tenth of a stop across the range; HDR2 as a slope of 0.2 about
a brighter pivot with three and a half stops of lift. The RAW itself is
recorded half a stop under the setting in both modes, and the JPEG's midtones
sit where that RAW's do, so neither preset lifts the exposure: the camera keeps
the frame dark and opens the shadows.")

(defun hdr-preset-params (mode)
  "The HDR node parameters for MODE, :HDR1 or :HDR2; NIL for anything else."
  (copy-list (rest (assoc mode *hdr-presets*))))

(defparameter *dust-keys* '(:size :contrast :specks)
  "Parameters of a dust node: the widest speck it fills, in pixels of the
photograph; how much darker or lighter than its surroundings a speck must be,
in stops; and which specks it looks for, :DARK, :LIGHT or :BOTH. Dust blocks
light, so on a negative it is dark before the inversion and light after, and on
a positive it is dark.")

(defparameter *dust-speck-kinds* '(:dark :light :both)
  "What a dust node may be told to look for.")

(defun dust-default-params (&key (specks :dark))
  "A dust node's parameters as the panel first shows them: twelve pixels, a
third of a stop, SPECKS. Twelve pixels is a speck on a 20 megapixel frame and
a large one on a scan at that size. A third of a stop is what the specks on a
scanned negative measured against the film about them — small and softened,
they are fainter than they look — and six times the grain of its sky."
  (list :size 12.0 :contrast 0.3 :specks specks))

(defparameter *vignette-keys* '(:amount :midpoint :feather :roundness)
  "Parameters of a vignette node: the gain change at the frame's corner, -1
black to +1 doubled; how far out toward the corner the change is half done; how
gradually it comes on; and the shape of its contours, 0 an ellipse following
the frame, +1 a circle, -1 a rounded rectangle.")

(defun vignette-default-params ()
  "A vignette node's parameters as the panel first shows them: about a third
of a stop off the corners, half way out, half feathered, following the frame."
  (list :amount -0.25 :midpoint 0.5 :feather 0.5 :roundness 0.0))

(defparameter *clarity-keys* '(:amount :radius)
  "Parameters of a clarity node: how much local contrast to add, -1 to 1, and
how wide the surroundings it is measured against are, in pixels of the
photograph.")

(defun clarity-default-params ()
  "A clarity node's parameters as the panel first shows them: a quarter of the
range, measured over a hundred and fifty pixels of the photograph, which on a
20 megapixel frame is the scale of a face rather than of its texture."
  (list :amount 0.25 :radius 150.0))

(defparameter *dehaze-keys* '(:amount)
  "Parameter of a dehaze node: how much of the haze to take out, -1 to 1,
negative laying veiling light over the frame instead.")

(defun dehaze-default-params ()
  "A dehaze node's parameters as the panel first shows them."
  (list :amount 0.3))

(defparameter *color-subtract-keys* '(:red :green :blue))

(defparameter *negative-keys* '(:red :green :blue :gamma :balance)
  "Parameters of a negative node: the film base per channel, zero for measured
from the frame; the paper gamma the dye density is printed through; and how far
the channels' densities at the brightest tones are made to agree.")

(defparameter *negative-default-gamma* 2.2
  "Paper contrast a fresh negative node prints through. A colour negative holds
the scene at about 0.6; 2.2 gives back a little more than the scene, as a print
does.")

(defparameter *negative-default-balance* 1.0
  "A fresh negative node neutralises the brightest tones across channels.")

(defparameter *crop-keys* '(:left :top :width :height :angle))

(defparameter *curve-channel-keys*
  '(:red-points :green-points :blue-points :master-points)
  "Curve channels in wire order: the three channels, then luma over all.")

(defparameter *identity-curve-points*
  '(0.0 0.0 1.0 1.0)
  "The do-nothing curve: a black point at the origin and a white point at full.

Two points, not four along the diagonal. A channel starts with exactly the
handles Resolve's custom curves start with, and pulling the white point left is
the per-channel gain that reverses a negative. Interior points are added by
clicking the curve, so a stock's shape costs points only where it needs them.")

(defparameter *minimum-curve-points* 2
  "A curve always keeps its two endpoints.")

(defparameter *maximum-curve-points* 16
  "Point ceiling per channel, matching MAX_CURVE_POINTS in the native executor.")

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
