(in-package #:orfeus/gui)

(defparameter *preview-debounce-seconds* 0.15d0
  "Delay used to coalesce interactive control changes.")

(defparameter *gui-preview-minimum-bound* 1024
  "Floor on the viewport render bound, so a tiny window still zooms a little.")

(defparameter *gui-preview-maximum-bound* 16384
  "Ceiling on the viewport render bound; above the sensor it has no effect.")

(defun gui-preview-bound (canvas-pixels zoom)
  "Bounding size for a viewport render of a canvas CANVAS-PIXELS wide at ZOOM.

Rendering the viewport at sensor resolution is the single most expensive thing
Orfeus used to do. An 80 MP frame shown in a 1400-pixel canvas was developed,
lens-corrected, denoised, tone-mapped, JPEG-encoded and written to disk at
10400 pixels wide so that 1400 of them could be displayed: fifty-five times the
necessary work on every selection and every settled edit.

What the display needs is one source pixel per screen pixel: at zoom Z the
image is drawn `C * Z` wide, so `P = C * Z` is exactly sharp. The bound is
rounded up to a power of two times the canvas width, so zooming inside one
doubling reuses the render already on screen.

This also keeps the zoom ceiling honest without anyone having to know the
sensor's size. The adapter magnifies a preview pixel at most twice, so
PREVIEW-ZOOM-LIMIT reads `2P/C` off whatever file is loaded: at fit that is 2,
one step in the bound doubles, the limit doubles with it, and the ladder climbs
until a render comes back smaller than its bound because the source ran out.
From then on P is fixed at the sensor and the limit settles at `2 * sensor / C`,
which is where it always was."
  (let ((needed (max *gui-preview-minimum-bound*
                     (ceiling (* canvas-pixels (max 1d0 zoom))))))
    (min *gui-preview-maximum-bound*
         (loop for bound = (max *gui-preview-minimum-bound* canvas-pixels)
                 then (* 2 bound)
               when (>= bound needed) return bound))))

(defparameter *viewport-margin* 0.3d0
  "How far past the visible rectangle a windowed render develops, per side.

As a fraction of the visible extent. This is what makes panning nearly free: a
drag inside the margin moves the view across pixels that are already developed,
and only a drag approaching its edge asks for more.

The number is a straight trade and there is no setting that wins both ways. The
margin costs render time by area — at three tenths, two and a half times the
visible rectangle — and buys pan travel by length. Three tenths was chosen for
the denoiser people actually use: with the edge-aware one a windowed re-render
measures 320 milliseconds and a pan 190, so being a little eager to re-render
costs nothing noticeable. The neural denoiser is slow at any window size (six
seconds against twenty-two for the whole frame), and no margin makes that
interactive.")

(defparameter *viewport-margin-spent* 0.25d0
  "How much of the margin may be left before more frame is asked for.

Asking while a quarter of the margin is still ahead of the view means the answer
usually arrives before the view reaches the edge of what is developed, so a pan
slides rather than stalling against it.")

(defparameter *viewport-whole-frame-fraction* 0.45d0
  "How much of the frame may be on screen before a window stops being worth it.

Past this the window plus its margin is most of the frame anyway, and asking for
part of it would only cost a cache entry that the fitted view cannot share.")

(defparameter *gui-draft-preview-size* 2048
  "Bounding size of the fast draft preview rendered before the full one.

Not a free choice: it is the size below which the bridge develops by binning
whole sensor quads instead of interpolating every photosite, which is the entire
reason a draft is fast. Raising it past that would make the draft cost what the
full render costs.")

(defun recipe-geometry-key (recipe)
  "What in RECIPE decides the *shape* of the developed frame.

A viewport is a rectangle of one particular frame, so it means nothing once the
frame changes shape — and several ordinary edits change it. Selecting a crop
node bypasses that crop, which makes the frame bigger; changing the crop rect,
turning the frame, or switching lens correction all do the same.

A window computed against the old shape and applied to the new one develops an
unrelated part of the photograph, and the interface then draws it believing it
is the part it asked for. That is exactly what happened: zooming into a crop and
then selecting the crop node to adjust it jumped the view somewhere else
entirely and drew the crop handles across it."
  (typecase recipe
    (orfeus:processing-graph
     (loop for node in (orfeus:processing-graph-nodes recipe)
           for kind = (orfeus:graph-node-kind node)
           when (member kind '(:crop :rotate :optics))
             collect (list kind
                           (and (orfeus:graph-node-bypassed-p node) t)
                           (let ((params (orfeus:graph-node-params node)))
                             (loop for key in '(:left :top :width :height :angle
                                                :quarter-turns
                                                :lens-correction-p
                                                :lens-correction-strength)
                                   collect (getf params key))))))
    (orfeus:processing-settings
     (list :lens (and (orfeus:processing-settings-lens-correction-p recipe) t)
           :strength (orfeus:processing-settings-lens-correction-strength
                      recipe)))
    (t nil)))

(defun draft-preview-fits-viewport-p (canvas-pixels zoom draft-bound)
  "Whether a draft bounded at DRAFT-BOUND can stand in for the current view.

The adapter magnifies a preview at most twice, so a draft covers a viewport up
to twice its own bound and no further. Past that it is drawn *smaller* than the
render it replaces, and changing an expensive node while zoomed in made the
photograph visibly zoom out and then back in as the full render landed. The
frame a photographer is looking at should not move because a node was retimed."
  (<= (* canvas-pixels zoom) (* 2 draft-bound)))

(defparameter *gui-live-preview-size* 1600
  "Bounding size of live drag previews rendered straight into the canvas
buffer, with no JPEG or file on the path.")

(defparameter *crop-aspect-choices*
  '(("Free" . nil)
    ("Original" . :original)
    ("1:1" . (1 . 1))
    ("5:4" . (5 . 4))
    ("4:5" . (4 . 5))
    ("4:3" . (4 . 3))
    ("3:4" . (3 . 4))
    ("3:2" . (3 . 2))
    ("2:3" . (2 . 3))
    ("16:9" . (16 . 9))
    ("9:16" . (9 . 16))
    ("2:1" . (2 . 1))
    ("1:2" . (1 . 2)))
  "Crop aspect ratios offered in the Node panel, as width to height.

Both orientations of each ratio are listed rather than offering a swap: a
portrait crop of a landscape frame is a normal thing to want, and picking 2:3
from a list says exactly what you get. :ORIGINAL follows the photograph's own
proportions, whatever they are.")

(defun rect-turned-clockwise (rect)
  "Return RECT as it lands after its frame turns one quarter clockwise."
  (destructuring-bind (left top width height) rect
    (list (- 1d0 (+ top height)) left height width)))

(defun rect-mirrored (rect horizontal vertical)
  "Return RECT as it lands after its frame is mirrored across either axis."
  (destructuring-bind (left top width height) rect
    (list (if horizontal (- 1d0 (+ left width)) left)
          (if vertical (- 1d0 (+ top height)) top)
          width height)))

(defun graph-forward-chain (graph id)
  "Return the nodes from ID's consumer down to the output, in order.

NIL when the path is not a simple chain, which is the honest answer: a branch
that rejoins through a blend has no single geometry to speak of."
  (let ((nodes (orfeus:processing-graph-nodes graph))
        (output (orfeus:processing-graph-output graph))
        (chain '())
        (current id))
    (loop
      (when (eql current output) (return (nreverse chain)))
      (let ((consumers (remove-if-not
                        (lambda (node)
                          (member current (orfeus:graph-node-inputs node)))
                        nodes)))
        (unless (= 1 (length consumers)) (return nil))
        (push (first consumers) chain)
        (setf current (orfeus:graph-node-id (first consumers)))))))

(defun downstream-geometry (graph node)
  "Return the turns and mirrors applied to NODE's output before it is shown.

A crop rectangle means what it means in the frame the crop node itself sees,
which is not the frame on screen when anything downstream turns or mirrors it.
Without this the overlay drew a rectangle in one place and the render took it
from another — with a half turn below the crop, from the diagonally opposite
corner, which is as wrong as it can get."
  (loop for downstream in (graph-forward-chain
                           graph (orfeus:graph-node-id node))
        for params = (orfeus:graph-node-params downstream)
        unless (orfeus:graph-node-bypassed-p downstream)
          append (case (orfeus:graph-node-kind downstream)
                   (:rotate (list (list :turn (getf params :quarter-turns 0))))
                   (:flip (list (list :mirror (getf params :horizontal)
                                      (getf params :vertical))))
                   (t '()))))

(defun rect-through-geometry (rect geometry &optional backwards)
  "Carry RECT through GEOMETRY, or back against it when BACKWARDS."
  (let ((steps (if backwards (reverse geometry) geometry)))
    (dolist (step steps rect)
      (setf rect
            (ecase (first step)
              (:turn
               (let ((turns (mod (if backwards (- (second step)) (second step))
                                 4)))
                 (loop repeat turns
                       do (setf rect (rect-turned-clockwise rect)))
                 rect))
              ;; A mirror undoes itself, so it needs no separate inverse.
              (:mirror (rect-mirrored rect (second step) (third step))))))))

(defparameter *flip-choices*
  '(("None" nil nil)
    ("Left to right" t nil)
    ("Top to bottom" nil t)
    ("Both" t t))
  "Mirror states offered by a flip node, as label then the two axes.")

(defun flip-choice-label (horizontal vertical)
  "Return the menu label for a mirror across HORIZONTAL and VERTICAL."
  (or (first (find-if (lambda (entry)
                        (and (eq (not (second entry)) (not horizontal))
                             (eq (not (third entry)) (not vertical))))
                      *flip-choices*))
      "None"))

(defun quarter-turn-label (turns)
  (or (rest (assoc turns orfeus:*quarter-turn-labels*)) "None"))

(defun quarter-turns-for-label (label)
  (car (rassoc label orfeus:*quarter-turn-labels* :test #'string=)))

(defun crop-aspect-label (aspect)
  (car (rassoc aspect *crop-aspect-choices* :test #'equal)))

(defun crop-aspect-for-label (label)
  (cdr (assoc label *crop-aspect-choices* :test #'string=)))

(defun crop-aspect-ratio (aspect frame-width frame-height)
  "The width-to-height ratio ASPECT asks for, in FRAME pixels, or NIL for free.

The frame is the whole uncropped photograph as drawn, so its pixels are
proportional to the sensor's: a crop whose on-screen spans are in ratio R has
photosites in ratio R too, and no conversion through the image's dimensions is
needed anywhere."
  (cond ((null aspect) nil)
        ((eq aspect :original)
         (and (plusp frame-height) (/ frame-width (float frame-height 1d0))))
        (t (/ (car aspect) (float (cdr aspect) 1d0)))))

(defun graph-node-body-style (node display-domain-p)
  "The body colour NODE is drawn with, which encodes where it may sit.

Three tiers, in pipeline order:

  :OPTICS  lens correction, which resamples against the full frame's geometry
           and so may not read a branch that has already been cropped. It has to
           come before every crop, which makes it the one correction with an
           ordering constraint of its own.
  :LINEAR  the scene-linear grade, indifferent to cropping, anywhere above film.
  :DISPLAY the film transform and everything after it.

:BYPASSED and :BLEND win over the tier, because a node that is switched off or
that merges two branches is a more important thing to notice about it. A blend
is always scene-linear anyway, and its blue belongs to the same cool family."
  (cond ((orfeus:graph-node-bypassed-p node) :bypassed)
        ((orfeus:graph-node-blend-p node) :blend)
        ((eq :optics (orfeus:graph-node-kind node)) :optics)
        (display-domain-p :display)
        (t :linear)))

(defun crop-rect-for-ratio (left top width height frame-width frame-height
                            ratio)
  "Reshape the normalized rectangle to RATIO about its own centre.

RATIO is a width-to-height proportion in frame pixels. The result shrinks to fit
inside the rectangle it started from rather than growing to fill the frame, so
choosing a ratio never pulls back in a part of the picture that had been cropped
away: a crop only tightens until the user widens it again. Returns left, top,
width and height, clamped into the frame."
  (let* ((span-x (min (* width frame-width) (* height frame-height ratio)))
         (span-y (/ span-x ratio))
         (new-width (/ span-x frame-width))
         (new-height (/ span-y frame-height))
         (new-left (- (+ left (/ width 2)) (/ new-width 2)))
         (new-top (- (+ top (/ height 2)) (/ new-height 2))))
    (values (min (- 1 new-width) (max 0 new-left))
            (min (- 1 new-height) (max 0 new-top))
            new-width new-height)))

(defparameter *thumbnail-preview-size* 320
  "Maximum width and height for orientation-correct thumbnail renders.")

(defparameter *background-preview-workers* 1
  "Workers rendering speculative thumbnails and neighbouring previews.

One, not one per CPU. Every render is already rayon-parallel across the whole
machine, so a worker per core meant clicking a photograph started as many
concurrent full renders of *other* photographs as there are cores, each spawning
its own parallel pass: hundreds of threads over twenty cores, all competing for
the memory bandwidth this pipeline is actually limited by, while the user waited
on the one photograph they had asked for. Measured elsewhere in this codebase,
rendering two photographs at once is slower than rendering them in sequence, for
the same reason.")

(defconstant +thumbnail-shift-mask+ #x00010000
  "FLTK's FL_SHIFT bit in the raw event state the bridge forwards.")
(defconstant +thumbnail-control-mask+ #x00040000
  "FLTK's FL_CTRL bit in the raw event state the bridge forwards.")

(defparameter *left-sidebar-min-width* 240
  "Minimum width of the photo and stills sidebar.")

(defparameter *left-sidebar-initial-width* 260
  "Preferred initial width of the photo and stills sidebar.")

(defparameter *right-sidebar-min-width* 320
  "Minimum width of the Node panel and graph sidebar.

Wide enough for a caption, a slider and a spinner side by side with room to
spare; the controls that used to hang out of a narrower column now follow its
width, but a spinner cannot shrink below its digits.")

(defparameter *thumbnail-selection-gutter* 28
  "Width reserved for the visible multi-selection checkbox in each photo row.")

(defconstant +menu-shift+ #x00010000
  "FLTK FL_SHIFT modifier for menu shortcuts.")
(defconstant +menu-ctrl+ #x00040000
  "FLTK FL_CTRL modifier for menu shortcuts.")
(defconstant +key-f5+ #xffc2
  "FLTK key code for the F5 function key.")
(defconstant +key-page-up+ #xff55 "FLTK key code for Page Up.")
(defconstant +key-page-down+ #xff56 "FLTK key code for Page Down.")
(defconstant +key-home+ #xff50 "FLTK key code for Home.")
(defconstant +key-end+ #xff57 "FLTK key code for End.")
(defconstant +key-left+ #xff51 "FLTK key code for the left arrow.")
(defconstant +key-up+ #xff52 "FLTK key code for the up arrow.")
(defconstant +key-right+ #xff53 "FLTK key code for the right arrow.")
(defconstant +key-down+ #xff54 "FLTK key code for the down arrow.")
(defconstant +key-space+ 32 "FLTK key code for the space bar.")
(defconstant +key-delete+ #xffff "FLTK key code for Delete.")

(defparameter *fringing-source-choices*
  '(("Measured" . :measured) ("Lens profile" . :profile))
  "How the optics panel names the sources of a colour fringing correction.")

(defparameter *node-kind-labels*
  '((:white-balance . "WB")
    (:exposure . "Expo")
    (:noise-reduction . "NR")
    (:tone . "Tone")
    (:optics . "Optics")
    (:film . "Film")
    (:blend . "Blend")
    (:color-subtract . "Subtr")
    (:negative . "Negative")
    (:contrast . "Contr")
    (:sharpen . "Sharp")
    (:crop . "Crop")
    (:rotate . "Rotate")
    (:flip . "Flip")
    (:curves . "Curves")
    (:node . "Node"))
  "Short node captions for the graph panel, in menu order.")

(defparameter *node-kind-choices*
  '(("None" . :node)
    ("White Balance" . :white-balance)
    ("Exposure" . :exposure)
    ("Noise Reduction" . :noise-reduction)
    ("Tone" . :tone)
    ("Optics" . :optics)
    ("Film" . :film)
    ("Blend" . :blend)
    ("Color Subtract" . :color-subtract)
    ("Negative" . :negative)
    ("Contrast" . :contrast)
    ("Sharpen" . :sharpen)
    ("Crop" . :crop)
    ("Rotate" . :rotate)
    ("Flip" . :flip)
    ("Curves" . :curves))
  "Correction picker entries for the Node panel, in menu order.")

(defun node-kind-full-name (kind)
  "The name a menu shows for KIND: the picker's full one, not the node box's
abbreviation, which is sized for a box and reads as a typo in a menu."
  (or (car (rassoc kind *node-kind-choices*))
      (node-kind-label kind)))

(defparameter *graph-node-width* 132
  "Width of a node box on the graph editor canvas.")

(defparameter *graph-node-height* 40
  "Height of a node box on the graph editor canvas.")

(defparameter *graph-well-height* 26
  "Height of the RAW and OUT terminal wells on the graph editor canvas.")

(defparameter *graph-row-pitch* 58
  "Vertical distance between auto-laid-out nodes on the graph canvas.")

(defparameter *level-confidence-floor* 2.0
  "How far the picture's own level reading has to stand out to be believed.

The native estimate compares the strongest line at the winning angle with the
run of angles'; texture alone scores about one everywhere, and a wall or a
window frame scores two and more (2.0 to 3.2 on the frames this was tried
on; trees, a bicycle and a blurred wall scored 1.7 and under). Under this the
picture had no straight edge to offer, and the photographer is told so rather
than turned by noise.")

(defparameter *inspector-min-height* 378
  "Minimum inspector height that keeps the complete Curves panel accessible.")

(defun make-root-layout (menu toolbar main-tile progress status)
  "Build the automatic Lightfast layout for Orfeus's top-level window regions."
  (lightfast:make-layout-column
   :children
   (list
    (lightfast:make-layout-item menu :basis 24 :shrink 0)
    (lightfast:make-layout-item toolbar :basis 40 :shrink 0)
    (lightfast:make-layout-item main-tile :basis 0 :grow 1 :min-height 200)
    (lightfast:make-layout-row
     :basis 28 :shrink 0
     :children
     (list
      (lightfast:make-layout-item progress :basis 180 :shrink 0)
      (lightfast:make-layout-item status :basis 0 :grow 1))))))

(defun parse-export-integer-value (text label fallback minimum maximum)
  "Parse one export dialog integer completely and enforce its inclusive range."
  (let* ((text (string-trim " " text))
         (value (if (zerop (length text))
                    fallback
                    (handler-case
                        (parse-integer text)
                      (error ()
                        (error "~A must be a whole number" label))))))
    (unless (<= minimum value maximum)
      (error "~A must be between ~D and ~D" label minimum maximum))
    value))

(defparameter *export-format-captions*
  '(("JPEG" . :jpeg) ("16-bit TIFF" . :tiff))
  "Export encoders offered in the dialog, in menu order.")

(defun export-format-from-caption (caption)
  "Return the export format CAPTION names, defaulting to JPEG."
  (or (cdr (assoc caption *export-format-captions* :test #'string-equal))
      :jpeg))

(defun export-format-caption (format)
  "Return the dialog caption naming FORMAT."
  (or (car (rassoc format *export-format-captions*))
      "JPEG"))

(defun update-project-export-settings
    (project destination format quality-text width-text height-text
     metadata-p timestamp-p)
  "Validate and atomically store all export dialog values on PROJECT."
  (let ((destination (string-trim " " destination)))
    (when (zerop (length destination))
      (error "Choose a destination folder first"))
    (let ((destination-path
            (pathname
             (concatenate 'string (string-right-trim "/" destination) "/")))
          (width (parse-export-integer-value
                  width-text "Maximum width" 0 0 100000))
          (height (parse-export-integer-value
                   height-text "Maximum height" 0 0 100000))
          (quality (parse-export-integer-value
                    quality-text "JPEG quality" 92 1 100))
          (settings (project-export-settings project)))
      (setf (project-output-directory project) destination-path
            (export-settings-format settings) format
            (export-settings-jpeg-quality settings) quality
            (export-settings-max-width settings) (when (plusp width) width)
            (export-settings-max-height settings) (when (plusp height) height)
            (export-settings-preserve-metadata-p settings) metadata-p
            (export-settings-timestamp-filenames-p settings) timestamp-p)
      settings)))

(defparameter *tone-slider-width* 20
  "Width of one tone band's vertical slider, which does not stretch.")

(defun tone-band-layout (label slider input)
  "Return one tone band as a flex column: caption, slider, value.

The slider takes whatever height the panel has left over, and the bands grow
equally across the row, so no part of this knows the panel's size."
  (lightfast:make-layout-column
   :basis 0 :grow 1 :gap 2
   :children
   (list (lightfast:make-layout-item label :basis 22 :shrink 0)
         (lightfast:make-layout-item slider :basis 0 :grow 1
                                            :align-self :center
                                            :preferred-width *tone-slider-width*)
         (lightfast:make-layout-item input :basis 24 :shrink 0))))

(defun labeled-field-layout (label control label-width)
  "Return a caption-and-control row whose control takes the leftover width."
  (lightfast:make-layout-row
   :gap 8
   :children
   (list (lightfast:make-layout-item label :basis label-width :shrink 0)
         (lightfast:make-layout-item control :basis 0 :grow 1 :min-width 100))))

(defparameter *number-field-label-width* 88
  "Width of a numeric control's caption, so every row's slider starts alike.")

(defparameter *number-field-value-width* 78
  "Width of a numeric control's spinner, which keeps its size as rows widen.")

(defun number-field-layout (label slider value)
  "Return one numeric control row: caption, slider, spinner.

The slider absorbs the row's slack, so widening the inspector widens the
sliders and nothing else has to be recomputed."
  (lightfast:make-layout-row
   :gap 8
   :children
   (list (lightfast:make-layout-item label
                                     :basis *number-field-label-width*
                                     :shrink 0)
         (lightfast:make-layout-item slider :basis 0 :grow 1 :min-width 64)
         (lightfast:make-layout-item value
                                     :basis *number-field-value-width*
                                     :shrink 0))))

(defun tone-bands-layout (bands)
  "Return the row holding every tone BANDS column, in display order."
  (lightfast:make-layout-row :gap 4 :children bands))

(defun gallery-generation-event-current-p (event generation)
  "Return true when EVENT belongs to the current still-gallery GENERATION."
  (= (second event) generation))

(defparameter *node-kind-glyphs*
  ;; Twelve-by-twelve line drawings, one per node kind, as :LINE x1 y1 x2 y2 and
  ;; :RECT x y width height in the glyph's own coordinates. A node graph read at
  ;; a glance is read by shape long before the four-letter caption is spelled
  ;; out, and a chain of eight boxes that differ only in their text is a chain
  ;; nobody scans.
  '((:white-balance (:rect 4 1 4 2) (:rect 4 9 4 2) (:rect 1 4 2 4)
                    (:rect 9 4 2 4) (:rect 4 4 4 4))
    (:exposure (:rect 4 4 4 4) (:line 6 0 6 2) (:line 6 10 6 12)
               (:line 0 6 2 6) (:line 10 6 12 6)
               (:line 1 1 3 3) (:line 9 9 11 11)
               (:line 11 1 9 3) (:line 3 9 1 11))
    (:noise-reduction (:rect 1 2 2 2) (:rect 6 1 2 2) (:rect 9 5 2 2)
                      (:rect 3 6 2 2) (:rect 7 8 2 2) (:rect 1 9 2 2))
    (:tone (:rect 1 8 2 4) (:rect 5 5 2 7) (:rect 9 2 2 10))
    (:optics (:line 3 1 3 11) (:line 9 1 9 11) (:line 3 1 9 4)
             (:line 3 11 9 8) (:line 6 5 6 7))
    (:film (:rect 2 1 8 10) (:rect 0 2 2 2) (:rect 0 7 2 2)
           (:rect 10 2 2 2) (:rect 10 7 2 2))
    (:blend (:rect 1 1 7 7) (:rect 4 4 7 7))
    (:color-subtract (:rect 1 1 10 10) (:rect 3 5 6 2))
    (:negative (:rect 1 1 10 1) (:rect 1 10 10 1) (:rect 1 1 1 10)
               (:rect 10 1 1 10) (:rect 4 4 4 4))
    (:contrast (:rect 1 1 10 10) (:rect 6 2 4 8))
    (:sharpen (:line 6 0 1 11) (:line 6 0 11 11) (:line 1 11 11 11))
    (:crop (:line 3 0 3 9) (:line 0 3 9 3) (:line 8 2 8 11) (:line 2 8 11 8))
    (:rotate (:line 1 10 1 2) (:line 1 2 9 2) (:line 6 0 9 2) (:line 6 4 9 2))
    (:flip (:line 6 0 6 12) (:line 0 3 4 3) (:line 0 9 4 9) (:line 0 3 0 9)
           (:rect 8 3 4 7))
    (:curves (:line 0 11 4 8) (:line 4 8 7 4) (:line 7 4 11 1))
    (:node (:rect 2 2 8 8)))
  "Per-kind glyphs for the node graph, drawn beside each node's caption.")

(defparameter *node-kind-icons*
  '((:white-balance . :white-balance)
    (:exposure . :exposure)
    (:noise-reduction . :noise)
    (:tone . :tone)
    (:optics . :optics)
    (:film . :film)
    (:blend . :blend)
    (:color-subtract . :color-subtract)
    (:negative . :negative)
    (:contrast . :contrast)
    (:sharpen . :sharpen)
    (:crop . :crop)
    (:rotate . :rotate)
    (:flip . :flip)
    (:curves . :curves))
  "The stock icon each node kind wears in the graph, the same sixteen-colour
pictures the buttons carry, so a node and the button that acts on it read as
one thing.")

(defun draw-node-icon (kind bypassed x y)
  "Draw KIND's icon with its corner at X, Y; a kind without one gets its glyph,
dimmed when BYPASSED, since a bypassed node is still there and still wants
identifying."
  (let ((icon (rest (assoc kind *node-kind-icons*))))
    (cond (icon (lightfast:draw-stock-icon icon x y))
          (t (if bypassed
                 (lightfast:draw-color-rgb :red 128 :green 128 :blue 132)
                 (lightfast:draw-color-rgb :red 58 :green 74 :blue 104))
             (draw-node-glyph kind (+ x 1) (+ y 2))))))

(defun draw-node-glyph (kind x y)
  "Draw KIND's glyph with its top-left corner at X, Y."
  (dolist (part (rest (assoc kind *node-kind-glyphs*)))
    (ecase (first part)
      (:line (destructuring-bind (x1 y1 x2 y2) (rest part)
               (lightfast:draw-line (+ x x1) (+ y y1) (+ x x2) (+ y y2))))
      (:rect (destructuring-bind (left top width height) (rest part)
               (lightfast:draw-filled-rect (+ x left) (+ y top)
                                           width height))))))

(defun node-kind-label (kind)
  (or (rest (assoc kind *node-kind-labels*)) (string kind)))

(defun curve-spline-value (points x)
  "Evaluate the monotone cubic through POINTS (x y pairs) at X.

Mirrors the native executor's Fritsch-Carlson spline, held flat outside the
control points, so the editor draws exactly the curve the render applies. A
channel carries anywhere from its two endpoints to a full film-stock shape."
  (let* ((count (floor (length points) 2))
         (xs (make-array count))
         (ys (make-array count))
         (last (1- count)))
    (dotimes (index count)
      (setf (aref xs index) (float (nth (* index 2) points) 1d0)
            (aref ys index) (float (nth (1+ (* index 2)) points) 1d0)))
    (cond
      ((<= x (aref xs 0)) (aref ys 0))
      ((>= x (aref xs last)) (aref ys last))
      (t
       (let ((slopes (make-array last))
             (tangents (make-array count)))
         (dotimes (segment last)
           (setf (aref slopes segment)
                 (/ (- (aref ys (1+ segment)) (aref ys segment))
                    (max 1.0d-4 (- (aref xs (1+ segment))
                                   (aref xs segment))))))
         (setf (aref tangents 0) (aref slopes 0)
               (aref tangents last) (aref slopes (1- last)))
         (loop for point from 1 below last
               do (setf (aref tangents point)
                        (let ((before (aref slopes (1- point)))
                              (after (aref slopes point)))
                          (if (<= (* before after) 0)
                              0d0
                              (let ((left (- (aref xs point)
                                             (aref xs (1- point))))
                                    (right (- (aref xs (1+ point))
                                              (aref xs point))))
                                (/ (* 3 (+ left right))
                                   (+ (/ (+ (* 2 right) left) before)
                                      (/ (+ right (* 2 left)) after))))))))
         (let* ((segment (or (loop for index from (1- last) downto 0
                                   when (>= x (aref xs index)) return index)
                             0))
                (width (max 1.0d-4 (- (aref xs (1+ segment))
                                      (aref xs segment))))
                (v (/ (- x (aref xs segment)) width))
                (v2 (* v v))
                (v3 (* v2 v)))
           (max 0d0
                (min 1d0
                     (+ (* (+ (* 2 v3) (* -3 v2) 1) (aref ys segment))
                        (* (+ v3 (* -2 v2) v) width
                           (aref tangents segment))
                        (* (+ (* -2 v3) (* 3 v2)) (aref ys (1+ segment)))
                        (* (- v3 v2) width
                           (aref tangents (1+ segment))))))))))))

(defun srgb-encode-component (value)
  "Encode one scene-linear component for a display color dialog."
  (let ((value (max 0.0 (min 1.0 value))))
    (if (<= value 0.0031308)
        (* 12.92 value)
        (- (* 1.055 (expt value (/ 1.0 2.4))) 0.055))))

(defun srgb-decode-component (value)
  "Decode one display component back to scene-linear."
  (if (<= value 0.04045)
      (/ value 12.92)
      (expt (/ (+ value 0.055) 1.055) 2.4)))

(defun graph-node-active-p (node)
  "True when NODE visibly changes the image, for the panel's indicator."
  (case (orfeus:graph-node-kind node)
    (:blend t)
    ((:color-subtract :negative) t)
    (:contrast (/= 1.0 (getf (orfeus:graph-node-params node) :contrast 1.0)))
    (:flip (let ((params (orfeus:graph-node-params node)))
             (or (getf params :horizontal) (getf params :vertical))))
    (:sharpen (plusp (getf (orfeus:graph-node-params node) :sharpen-amount 0.0)))
    (:crop (let ((params (orfeus:graph-node-params node)))
             (or (plusp (getf params :left 0.0))
                 (plusp (getf params :top 0.0))
                 (< (getf params :width 1.0) 0.999)
                 (< (getf params :height 1.0) 0.999))))
    (otherwise
     (loop for (key value) on (orfeus:graph-node-params node) by #'cddr
             thereis (not (equal value
                                 (getf orfeus::*stage-identity-plist*
                                       key)))))))

(defparameter *gallery-cell-width* 96
  "Width of one still cell in the gallery grid.")

(defparameter *gallery-cell-height* 92
  "Height of one still cell in the gallery grid.")

(defstruct gallery-still
  "One gallery row with an explicit project or local origin.

UNAVAILABLE-P records that neither the source photograph nor a persisted
thumbnail could be found, so the cell will never fill in and says so rather
than looking like a render still in flight."
  origin
  identity
  preset
  (unavailable-p nil))

(defun gallery-still-key (still)
  "Return the cache/task key for STILL, including its provenance."
  (list (gallery-still-origin still) (gallery-still-identity still)))

(defun gallery-selection-key (stills selected-index)
  "Return the stable key selected by SELECTED-INDEX, or NIL."
  (let ((still (and selected-index (nth selected-index stills))))
    (and still (gallery-still-key still))))

(defun gallery-selection-index (stills selected-key)
  "Find SELECTED-KEY in STILLS without retargeting a stale numeric index."
  (and selected-key
       (position selected-key stills :key #'gallery-still-key :test #'equal)))

(defun gallery-selected-still (stills selected-index)
  "Return the selected gallery still, or NIL for an invalid stale index."
  (and selected-index (nth selected-index stills)))

(defun gallery-still-origin-description (still)
  "Return a short user-visible provenance description for STILL."
  (format nil "~(~A~) still" (gallery-still-origin still)))

(defun ensure-graph-node-positions (nodes)
  "Assign default editor positions to unplaced NODES and return NODES.

Existing positions are preserved so dragged nodes remain where the user put them."
  (loop for node in nodes
        for index from 1
        unless (orfeus:graph-node-position node)
          do (setf (orfeus:graph-node-position node)
                   (list 18.0
                         (float (+ 6 (* index *graph-row-pitch*)) 1.0))))
  nodes)

(defun graph-node-for-edit (model index)
  "Return the positioned node at INDEX after materializing MODEL's graph."
  (let ((graph (gui-model-ensure-graph model)))
    (when graph
      (nth index
           (ensure-graph-node-positions
            (orfeus:processing-graph-nodes graph))))))

(defun graph-output-box-position (nodes)
  "Return the graph-space OUT box for NODES, including an empty graph."
  ;; The well hangs below the lowest placed node, and never rises above where
  ;; it would sit in an empty graph.
  (let ((lowest 0)
        (empty-graph-bottom (+ 6 *graph-well-height*)))
    (dolist (node nodes)
      (let ((place (orfeus:graph-node-position node)))
        (when place
          (setf lowest (max lowest (+ (round (second place))
                                      *graph-node-height*))))))
    (values 18
            (+ (max lowest empty-graph-bottom) 28)
            *graph-node-width*
            *graph-well-height*)))

(defun make-project-gallery-still (preset)
  (make-gallery-still :origin :project
                      :identity (orfeus:still-store-identity
                                 (processing-preset-name preset))
                      :preset preset))

(defun make-local-gallery-still (preset)
  (make-gallery-still :origin :local
                      :identity (orfeus:still-store-identity
                                 (processing-preset-name preset))
                      :preset preset))

(defun thumbnail-row-at (event-y scroll row-height)
  "Return the zero-based thumbnail row at local EVENT-Y."
  (floor (+ event-y scroll) row-height))

(defun thumbnail-toggle-hit-p (event-x canvas-width)
  "Return true when EVENT-X hits a photo row's selection checkbox."
  (and event-x canvas-width
       (>= event-x (- canvas-width *thumbnail-selection-gutter*))))

(defun thumbnail-checkbox-x (canvas-x canvas-width)
  "Return the left edge of a photo row's selection checkbox."
  (- (+ canvas-x canvas-width) 22))

(defun preview-priority-indices (count selected)
  "Return every photo index ordered from SELECTED outward."
  (when (and (plusp count) (<= 0 selected) (< selected count))
    (stable-sort (loop for index below count collect index)
                 #'< :key (lambda (index) (abs (- index selected))))))

(defun thumbnail-selection-after-click (selected row anchor state)
  "Return the selection and anchor produced by clicking ROW with modifier STATE.

Plain clicks select one photo, control toggles, shift selects the range
from the anchor, and control-shift adds that range to the selection."
  (cond
    ((logtest +thumbnail-shift-mask+ state)
     (let ((range (loop for index from (min anchor row) to (max anchor row)
                        collect index)))
       (values (if (logtest +thumbnail-control-mask+ state)
                   (remove-duplicates (append selected range) :from-end t)
                   range)
               anchor)))
    ((logtest +thumbnail-control-mask+ state)
     (values (if (member row selected)
                 (remove row selected)
                 (cons row selected))
             row))
    (t (values (list row) row))))

(defparameter *thumbnail-context-actions*
  '((:export . "Export selected photographs...")
    (:apply-still . "Apply selected still to selection")
    (:reset-edits . "Remove edits from selection")
    (:copy-paths . "Copy source paths")
    (:divider . "-")
    (:select-all . "Select all photographs")
    (:intern . "Intern selection (copy off the card)")
    (:remove . "Remove selection from project..."))
  "Actions and labels shown by the photo sidebar's context menu.")

(defun thumbnail-context-menu-items ()
  "Return the labels for the photo sidebar's context menu."
  (mapcar #'rest *thumbnail-context-actions*))

(defun thumbnail-context-action-at (index)
  "Return the photo sidebar action selected at zero-based INDEX, or NIL."
  (and index (first (nth index *thumbnail-context-actions*))))

(defun thumbnail-context-selection (selected row)
  "Preserve SELECTED when ROW is already selected, otherwise select only ROW."
  (if (member row selected) selected (list row)))

(defun draw-star (x y filled)
  "Draw one five-pointed star at X, Y, solid when FILLED."
  (let ((points '((8 . 1) (10 . 6) (15 . 6) (11 . 9) (13 . 14)
                  (8 . 11) (3 . 14) (5 . 9) (1 . 6) (6 . 6))))
    (if filled
        (lightfast:draw-color-rgb :red 240 :green 200 :blue 90)
        (lightfast:draw-color-rgb :red 96 :green 98 :blue 102))
    ;; Drawn as its outline: the toolkit has no polygon fill, and at this size
    ;; an outline in the same colour reads as a solid star anyway.
    (loop for (from . rest) on points
          for to = (or (first rest) (first points))
          do (lightfast:draw-line (+ x (car from)) (+ y (cdr from))
                                  (+ x (car to)) (+ y (cdr to))))
    (when filled
      (lightfast:draw-line (+ x 5) (+ y 8) (+ x 11) (+ y 8))
      (lightfast:draw-line (+ x 6) (+ y 10) (+ x 10) (+ y 10)))))

(defun draw-star-rating (rating x y)
  "Draw RATING out of five stars, or nothing at all when unrated.

An unrated frame shows no stars rather than five empty ones: a filmstrip of
hollow stars beside every photograph says nothing and reads as clutter."
  (when rating
    (dotimes (index 5)
      (draw-star (+ x (* index 15)) y (< index rating)))))

(defun draw-interned-badge (x y)
  "Draw the mark that says a photograph no longer needs its card.

A filled disc with a downward arrow, in the corner of the thumbnail: the RAW
has been pulled in. Small and quiet, because the interesting state is the
absence of it."
  (lightfast:draw-color-rgb :red 24 :green 26 :blue 28)
  (lightfast:draw-filled-circle (+ x 8) (+ y 8) 9)
  (lightfast:draw-color-rgb :red 120 :green 210 :blue 140)
  (lightfast:draw-filled-circle (+ x 8) (+ y 8) 7)
  (lightfast:draw-color-rgb :red 18 :green 40 :blue 24)
  ;; A stubby arrow into a floor line: pulled down and kept.
  (lightfast:draw-line (+ x 8) (+ y 3) (+ x 8) (+ y 9))
  (lightfast:draw-line (+ x 5) (+ y 6) (+ x 8) (+ y 9))
  (lightfast:draw-line (+ x 11) (+ y 6) (+ x 8) (+ y 9))
  (lightfast:draw-line (+ x 4) (+ y 12) (+ x 12) (+ y 12)))

(defun graph-view-signature (job graph selected-node)
  "Return a cheap identity of what the node graph editor should display.

Comparing this between poll ticks repaints the editor after any path that
changes the graph without invalidating the canvas itself, so an added,
removed, or replaced photograph can never leave a stale graph on screen."
  (list job graph
        (and graph (length (orfeus:processing-graph-nodes graph)))
        (and graph (orfeus:processing-graph-output graph))
        selected-node))

(defun crop-preview-current-p (preview-generation published-generation)
  "True when the displayed preview uses the current crop-edit generation."
  (and published-generation
       (= preview-generation published-generation)))

(defun fltk-file-filter (label pattern)
  "Build FLTK's LABEL<TAB>PATTERN native file chooser syntax."
  (format nil "~A~C~A" label #\Tab pattern))

(defun alias-body-crop-factor (pathname model focal-length)
  "The crop factor an adapted-lens mapping for PATHNAME's body should carry.

The database's figure for the body, read back through the same lookup a render
makes with MODEL chosen by hand; two when the database does not know the body,
which is the crop of every body Orfeus reads. Never the profile's own
calibration crop: that is the sensor the lens was measured on, and for a
full-frame lens on a Four Thirds body the two differ by exactly the factor the
correction has to allow for — written down as the body's, it had the lens's
corner correction land on the sensor's corner at four times its strength."
  (let ((match (orfeus:photo-lens-profile-status pathname :profile model
                                                          :focal-length focal-length)))
    (or (and match (getf match :crop-factor)) 2.0)))

(defun place-dialog-over (dialog owner)
  "Put DIALOG over the middle of OWNER, the window that asked for it.

Left where the toolkit put it, a dialog opened at the screen's corner, a screen
away from the button that called it. Before the dialog is shown, so that it
appears there rather than jumps there."
  (lightfast:refresh-geometry owner)
  (lightfast:refresh-geometry dialog)
  (lightfast:resize-widget
   dialog
   :x (max 0 (+ (lightfast:widget-x owner)
                (floor (- (lightfast:widget-width owner)
                          (lightfast:widget-width dialog))
                       2)))
   :y (max 0 (+ (lightfast:widget-y owner)
                (floor (- (lightfast:widget-height owner)
                          (lightfast:widget-height dialog))
                       2)))))

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

(defconstant +preview-cache-schema-version+ 2)

(defvar *active-preview-cache-files* (make-hash-table :test #'equal))
(defvar *active-preview-cache-lock*
  (sb-thread:make-mutex :name "Orfeus active preview cache files"))
(defparameter *preview-lock-owner-grace-seconds* 2)

(defun protect-preview-cache-path (pathname mode)
  "Enforce MODE on PATHNAME, returning PATHNAME only when verification succeeds."
  (handler-case
      (progn
        (sb-posix:chmod (namestring pathname) mode)
        (when (= mode (logand #o777
                              (sb-posix:stat-mode
                               (sb-posix:stat (namestring pathname)))))
          pathname))
    (error () nil)))

(defun make-secure-preview-directory (directory)
  (handler-case
      (progn
        (ensure-directories-exist directory)
        (or (protect-preview-cache-path directory #o700)
            (progn
              (ignore-errors
                (uiop:delete-directory-tree directory :validate t
                                                      :if-does-not-exist :ignore))
              nil)))
    (error () nil)))

(defun gui-preview-cache-directory ()
  "Return a private persistent cache, or a secure session cache on failure."
  (or (make-secure-preview-directory (uiop:xdg-cache-home "orfeus/previews/"))
      (make-secure-preview-directory
       (merge-pathnames
        (format nil "orfeus-preview-session-~D-~D/"
                (sb-posix:getpid) (random most-positive-fixnum))
        (uiop:temporary-directory)))
      (error "Cannot create a secure preview cache directory.")))

(defun gui-focus-cache-directory ()
  "Return where focus measurements are kept between runs, or NIL.

Beside the previews, and for the same reason: measuring focus means developing
the frame, so a card's worth of photographs is minutes of work that must not be
repeated on every launch. Keyed by content, so interning a RAW off a card keeps
what was already measured for it."
  (make-secure-preview-directory (uiop:xdg-cache-home "orfeus/focus/")))

(defun photo-content-key (pathname)
  "Return the shared content key for PATHNAME. See ORFEUS:FILE-CONTENT-KEY."
  (orfeus:file-content-key pathname))

(defun preview-cache-hit-p (pathname)
  (when (probe-file pathname)
    (if (protect-preview-cache-path pathname #o600)
        t
        (progn
          (ignore-errors (delete-file pathname))
          nil))))

(defun preview-cache-file-active-p (pathname)
  (sb-thread:with-mutex (*active-preview-cache-lock*)
    (plusp (gethash (namestring pathname) *active-preview-cache-files* 0))))

(defun call-with-active-preview-cache-file (pathname function)
  (let ((key (namestring pathname)))
    (sb-thread:with-mutex (*active-preview-cache-lock*)
      (incf (gethash key *active-preview-cache-files* 0)))
    (unwind-protect (funcall function)
      (sb-thread:with-mutex (*active-preview-cache-lock*)
        (if (= 1 (gethash key *active-preview-cache-files*))
            (remhash key *active-preview-cache-files*)
            (decf (gethash key *active-preview-cache-files*)))))))

(defun evict-stale-previews (directory &key (max-age-days 45)
                                            (max-bytes (* 4 1024 1024 1024)))
  "Delete cached previews older than MAX-AGE-DAYS, then trim to MAX-BYTES.

Trimming removes the oldest files first. Errors are swallowed: eviction is
best effort housekeeping."
  (let* ((files (ignore-errors (uiop:directory-files directory)))
         (cutoff (- (get-universal-time) (* max-age-days 24 60 60)))
         (entries '()))
    (dolist (file files)
      (let ((date (or (ignore-errors (file-write-date file)) 0))
            (size (or (ignore-errors
                        (with-open-file (stream file :element-type
                                                '(unsigned-byte 8))
                          (file-length stream)))
                      0)))
        (if (and (< date cutoff)
                 (not (preview-cache-file-active-p file)))
            (unless (ignore-errors
                      (call-with-preview-key-lock
                       file (lambda () (when (probe-file file) (delete-file file)))
                       :timeout 0.0d0)
                      t)
              (push (list file date size) entries))
            (push (list file date size) entries))))
    (let ((total (reduce #'+ entries :key #'third :initial-value 0)))
      (dolist (entry (sort entries #'< :key #'second))
        (when (<= total max-bytes)
          (return))
        (unless (preview-cache-file-active-p (first entry))
          (when (ignore-errors
                  (call-with-preview-key-lock
                   (first entry)
                   (lambda ()
                     (when (probe-file (first entry))
                       (delete-file (first entry))))
                   :timeout 0.0d0)
                  t)
            (decf total (third entry))))))))

(defun discard-cached-previews (directory)
  "Delete every cached preview in DIRECTORY and return how many went.

A file another render is holding is left alone, the same way eviction leaves
one: that render is about to publish it, and taking it out from under the
publish is how half a preview ends up on screen. Anything skipped is simply
rendered again on demand, so the cache still ends up rebuilt."
  (let ((removed 0))
    (dolist (file (ignore-errors (uiop:directory-files directory)) removed)
      (unless (preview-cache-file-active-p file)
        (ignore-errors
          (call-with-preview-key-lock
           file
           (lambda () (when (probe-file file) (delete-file file)))
           :timeout 0.0d0))
        (unless (probe-file file)
          (incf removed))))))

(defun preview-recipe-snapshot (recipe)
  "Return an immutable deep copy of a settings or graph render RECIPE."
  (etypecase recipe
    (orfeus:processing-graph
     (orfeus:sexp->graph (orfeus:graph->sexp recipe)))
    (orfeus:processing-settings
     (orfeus::sexp->processing-settings
      (orfeus::processing-settings->sexp recipe)))))

(defun active-preview-lut-paths (recipe)
  (remove-duplicates
   (etypecase recipe
     (orfeus:processing-settings
      (let ((path (orfeus:processing-settings-lut-path recipe)))
        (when (and path (plusp (orfeus:processing-settings-lut-strength recipe)))
          (list path))))
     (orfeus:processing-graph
      (loop for node in (orfeus:processing-graph-nodes recipe)
            for params = (orfeus:graph-node-params node)
            for path = (getf params :lut-path)
            when (and (eq :film (orfeus:graph-node-kind node))
                      (not (orfeus:graph-node-bypassed-p node))
                      path
                      (plusp (getf params :lut-strength 1.0)))
              collect path)))
   :test #'equal))

(defun preview-settings-key (recipe &key (max-width 0) (max-height 0)
                                         (jpeg-quality 88))
  "Return a full digest of the render recipe, output shape, and active LUTs."
  (let* ((dependencies
           (loop for path in (active-preview-lut-paths recipe)
                 collect (list (namestring (pathname path))
                               (handler-case (photo-content-key path)
                                 (error (condition)
                                   (list :unreadable (princ-to-string condition)))))))
         (identity
           (list :preview-cache-schema +preview-cache-schema-version+
                 :max-width max-width :max-height max-height
                 :jpeg-quality jpeg-quality
                 :recipe (etypecase recipe
                           (orfeus:processing-graph
                            (orfeus:graph->render-sexp recipe))
                           (orfeus:processing-settings
                            (orfeus::processing-settings->sexp recipe)))
                 :dependencies dependencies))
         (*print-readably* t)
         (digest (ironclad:digest-sequence
                  :sha256
                  (sb-ext:string-to-octets (prin1-to-string identity)
                                           :external-format :utf-8))))
    (ironclad:byte-array-to-hex-string digest)))

(defun preview-pathname (preview-directory content-key role settings
                         &key (max-width 0) (max-height 0) (jpeg-quality 88)
                              settings-key)
  (merge-pathnames
   (make-pathname :name (format nil "~(~A~)-~A-~A"
                                role content-key
                                (or settings-key
                                    (preview-settings-key
                                     settings :max-width max-width
                                     :max-height max-height
                                     :jpeg-quality jpeg-quality)))
                  :type "jpg")
   preview-directory))

(defun thumbnail-pathname (preview-directory content-key)
  (preview-pathname preview-directory content-key :thumbnail
                    (make-processing-settings :noise-reduction 0.0
                                              :lens-correction-p nil
                                              :chromatic-aberration-correction-p nil
                                              :lut-path nil :grain-amount 0.0)
                    :max-width *thumbnail-preview-size*
                    :max-height *thumbnail-preview-size*
                    :jpeg-quality 82))

(defun camera-thumbnail-pathname (preview-directory content-key)
  "Where the camera's own preview of a photograph is kept as a thumbnail.

Keyed by the file's content like the developed thumbnail, and by the size it
was bounded to; a different name so that both can sit in the cache and the
developed one is not mistaken for the camera's."
  (merge-pathnames
   (make-pathname :name (format nil "camera-~A-~D" content-key
                                *thumbnail-preview-size*)
                  :type "jpg")
   preview-directory))

(defun preview-lock-directory (pathname)
  (pathname (format nil "~A.lock/" (namestring pathname))))

(defun preview-lock-owner-pathname (lock)
  (merge-pathnames "owner.sexp" lock))

(defun process-alive-p (pid)
  (handler-case
      (progn (sb-posix:kill pid 0) t)
    (sb-posix:syscall-error (condition)
      ;; EPERM also proves that the process exists.
      (= (sb-posix:syscall-errno condition) sb-posix:eperm))))

(defun write-preview-lock-owner (lock token)
  (let ((owner (preview-lock-owner-pathname lock)))
    (with-open-file (stream owner :direction :output :if-exists :error
                                  :if-does-not-exist :create)
      (let ((*print-readably* t))
        (write (list :pid (sb-posix:getpid) :created (get-universal-time)
                     :token token)
               :stream stream)))
    (or (protect-preview-cache-path owner #o600)
        (error "Cannot secure preview lock owner ~A" owner))))

(defun read-preview-lock-owner (lock)
  (handler-case
      (with-open-file (stream (preview-lock-owner-pathname lock))
        (let ((*read-eval* nil)) (read stream nil nil)))
    (error () nil)))

(defun preview-lock-old-enough-p (lock seconds)
  (let ((written (ignore-errors (file-write-date lock))))
    (and (integerp written)
         (> (- (get-universal-time) written) seconds))))

(defun stale-preview-lock-p (lock)
  (let* ((owner (read-preview-lock-owner lock))
         (pid (and (listp owner) (getf owner :pid)))
         (created (and (listp owner) (getf owner :created)))
         (token (and (listp owner) (getf owner :token)))
         (recorded-pid-p (and (integerp pid) (plusp pid)))
         (well-formed-p (and recorded-pid-p (integerp created) (stringp token))))
    (cond
      ((and recorded-pid-p (not (process-alive-p pid))) t)
      (well-formed-p nil)
      (t (preview-lock-old-enough-p lock *preview-lock-owner-grace-seconds*)))))

(defun reclaim-stale-preview-lock (lock)
  (when (stale-preview-lock-p lock)
    (let ((tombstone
            (pathname (format nil "~A.reclaimed-~D-~D/"
                              (string-right-trim "/" (namestring lock))
                              (sb-posix:getpid) (random most-positive-fixnum)))))
      ;; Rename establishes exclusive ownership of exactly the stale instance.
      (when (ignore-errors (rename-file lock tombstone) t)
        (ignore-errors
          (uiop:delete-directory-tree tombstone :validate t
                                                :if-does-not-exist :ignore))
        t))))

(defun acquire-preview-lock (pathname &key (timeout 120.0d0) ignore-hit-p)
  "Acquire a crash-recoverable cross-process per-key lock.

Return NIL only when IGNORE-HIT-P is false and another process filled PATHNAME."
  (let ((lock (preview-lock-directory pathname))
        (deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop
      (when (and (not ignore-hit-p) (preview-cache-hit-p pathname)) (return nil))
      (when (handler-case (progn (sb-posix:mkdir (namestring lock) #o700) t)
              (sb-posix:syscall-error () nil))
        (unless (protect-preview-cache-path lock #o700)
          (ignore-errors
            (uiop:delete-directory-tree lock :validate t
                                             :if-does-not-exist :ignore))
          (error "Cannot secure preview lock ~A" lock))
        (let ((token (format nil "~D-~D" (sb-posix:getpid)
                             (random most-positive-fixnum))))
          (write-preview-lock-owner lock token)
          (return (cons lock token))))
      (reclaim-stale-preview-lock lock)
      (when (> (get-internal-real-time) deadline)
        (error "Timed out waiting for preview cache lock ~A" pathname))
      (sleep 0.02d0))))

(defun release-preview-lock (lock-and-token)
  (when lock-and-token
    (destructuring-bind (lock . token) lock-and-token
      (let ((owner (read-preview-lock-owner lock)))
        (when (equal token (getf owner :token))
          (ignore-errors
            (uiop:delete-directory-tree lock :validate t
                                             :if-does-not-exist :ignore)))))))

(defun call-with-preview-key-lock (pathname function &key (timeout 120.0d0))
  (let ((lock (acquire-preview-lock pathname :timeout timeout :ignore-hit-p t)))
    (unwind-protect (funcall function)
      (release-preview-lock lock))))

(defun call-with-preview-cache-fill (pathname function &key validation-function)
  "Coalesce cache fills across processes; FUNCTION writes a secure temp file.

When supplied, VALIDATION-FUNCTION must return true after the render before the
new cache entry is published."
  (call-with-active-preview-cache-file
   pathname
   (lambda ()
     (if (preview-cache-hit-p pathname)
         pathname
         (let ((lock (acquire-preview-lock pathname)))
           (if (null lock)
               pathname
               (let ((temporary
                       (merge-pathnames
                        (make-pathname
                         :name (format nil "~A.~D-~D.tmp"
                                       (pathname-name pathname)
                                       (sb-posix:getpid)
                                       (random most-positive-fixnum))
                         :type (pathname-type pathname))
                        (uiop:pathname-directory-pathname pathname))))
                 (unwind-protect
                      (progn
                        (funcall function temporary)
                        (when (and validation-function
                                   (not (funcall validation-function)))
                          (error "Preview dependencies changed during render"))
                        (unless (protect-preview-cache-path temporary #o600)
                          (error "Cannot secure preview cache file ~A" temporary))
                        (rename-file temporary pathname)
                        (unless (protect-preview-cache-path pathname #o600)
                          (ignore-errors (delete-file pathname))
                          (error "Cannot secure preview cache file ~A" pathname))
                        pathname)
                   (ignore-errors (delete-file temporary))
                   (release-preview-lock lock)))))))))

(defun materialize-preview-cache-hit (pathname session-directory
                                      &key validation-function)
  "Copy and validate a cache hit under its per-key lock to a secure path."
  (let ((target (merge-pathnames
                 (format nil "display-~D-~D.jpg" (sb-posix:getpid)
                         (random most-positive-fixnum))
                 session-directory)))
    (handler-case
        (progn
          (call-with-preview-key-lock
           pathname
           (lambda ()
             (unless (preview-cache-hit-p pathname)
               (return-from materialize-preview-cache-hit nil))
             (uiop:copy-file pathname target)
             (unless (protect-preview-cache-path target #o600)
               (error "Cannot secure materialized preview ~A" target))))
          (when (and validation-function
                     (not (funcall validation-function)))
            (error "Preview dependencies changed while loading cache hit"))
          target)
      (error (condition)
        (ignore-errors (delete-file target))
        (error condition)))))

(defun preview-progress-state (load total generation tracked-generation)
  "Return percent, updated high-water TOTAL, and tracked GENERATION."
  (when (/= generation tracked-generation)
    (setf total 0
          tracked-generation generation))
  (if (zerop load)
      (values 0 0 tracked-generation)
      (let ((total (max total load)))
        (values (max 2 (min 99 (round (* 100 (- total load)) total)))
                total tracked-generation))))

(defun preview-status-text (model)
  (let ((job (gui-model-selected-job model)))
    (if (and job (orfeus:photo-job-graph job))
        (let* ((graph (orfeus:photo-job-graph job))
               (nodes (orfeus:processing-graph-nodes graph))
               (bypassed (count-if #'orfeus:graph-node-bypassed-p nodes)))
          (format nil "RAW preview  |  Graph: ~D node~:P~@[, ~D bypassed~]"
                  (length nodes)
                  (when (plusp bypassed) bypassed)))
        (let ((temperature (gui-model-setting model :white-balance-temperature))
              (noise-reduction (gui-model-setting model :noise-reduction))
              (neural (gui-model-setting model :neural-noise-reduction))
              (lut (gui-model-setting model :lut-path))
              (strength (gui-model-setting model :lut-strength)))
          ;; One denoiser runs, not both, so name the one that did rather
          ;; than listing a strength that was never applied.
          (format nil "RAW preview  |  WB: ~A  |  NR: ~A  |  LUT: ~A"
                  (if temperature "Custom" "As shot")
                  (cond ((plusp neural)
                         (format nil "neural ~D%" (round (* 100 neural))))
                        ((plusp noise-reduction)
                         (format nil "edge-aware ~D%"
                                 (round (* 100 noise-reduction))))
                        (t "off"))
                  (if (and lut (plusp strength))
                      (format nil "~A (~D%)" (file-namestring lut)
                              (round (* 100 strength)))
                      "Off"))))))

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
    ;; The optics panel states what the database said about each lens; the
    ;; warning that said it again on the terminal, once per photograph, is
    ;; switched off for the whole session, workers included.
    (setf orfeus:*report-missing-lens-profiles* nil)
    ;; Load both CFFI bridges before workers can race to initialize them.
    (orfeus:native-bridge-version)
    (load-gui-preview-library)
    ;; Vulkan initialization costs enough to show on the first frames, so spend
    ;; it while the window is still being built.
    (orfeus:native-gpu-warm-up)
    (let* ((project initial-project)
           (model (make-gui-model :project project
                                  :project-path initial-path))
           (queue (make-gui-queue :name "Orfeus foreground render worker"))
           (background-queue
             (make-gui-queue :workers *background-preview-workers*
                             :name "Orfeus background render worker"))
           (histogram-queue
             (make-gui-queue :name "Orfeus histogram worker"))
           (preview-directory (gui-preview-cache-directory))
           (preview-session-directory
             (or (make-secure-preview-directory
                  (merge-pathnames
                   (format nil "orfeus-display-~D-~D/" (sb-posix:getpid)
                           (random most-positive-fixnum))
                   (uiop:temporary-directory)))
                 (error "Cannot create secure preview display directory.")))
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
           ;; Photograph -> :camera or :developed, saying which kind of
           ;; thumbnail THUMBNAIL-FILES holds for it.
           (thumbnail-grades (make-hash-table :test #'eq))
           ;; Photograph -> (burst number, place in it, how many): filled in
           ;; from a background pass, because reading capture times is a
           ;; subprocess per photograph and the filmstrip has to keep drawing.
           ;; Which part of the frame each drawn preview covers, as four
           ;; fractions of the oriented frame, or NIL for all of it. A zoomed
           ;; preview develops only what is on screen, so the file on disk is a
           ;; window and the drawing has to know which one.
           before-preview-region after-preview-region requested-viewport-region
           ;; The frame shape the drawn preview was developed with. A viewport
           ;; is only asked for while the recipe still has that shape.
           after-preview-geometry
           ;; (KEY WIDGET DEFAULT) for controls that edit a node's own
           ;; parameters rather than a pipeline setting.
           (node-param-controls '())
           ;; What the renderer last said it was doing, and how far along. Set
           ;; from a background render thread through the queue, so the status
           ;; line can name the stage rather than the queue depth.
           render-stage
           (render-stage-fraction 0.0)
           (photo-groups (make-hash-table :test #'eq))
           ;; Which bursts the photographer has folded. Stored the way round
           ;; that makes open the default without a pass to open them: absence
           ;; is open, so a burst that has only just been discovered shows every
           ;; frame when it first draws, and culling with the arrows walks
           ;; through the frames instead of over them.
           (collapsed-bursts (make-hash-table :test #'eq))
           ;; Photograph -> focus report, from a background pass of its own.
           ;; Measuring means developing the frame, so this arrives slowly and
           ;; the filmstrip draws whatever has arrived.
           (photo-focus-reports (make-hash-table :test #'eq))
           (focus-pass-running-p nil)
           (lut-paths (make-hash-table :test #'equal))
           window menu toolbar toolbar-bottom-rule main-tile root-layout left-column
           filmstrip-pane gallery-pane center-pane
           thumbnail-canvas thumbnail-scrollbar photo-selection-label
           before-canvas after-canvas before-caption after-caption
           still-button copy-grade-button paste-grade-button
           right-column graph-pane graph-canvas graph-scrollbar graph-title
           (graph-scroll-x 0)
           (graph-scroll-y 0)
           pick-color-node crop-drag node-drag
           ;; A sort waiting for the next pass over the photographs' metadata:
           ;; :AUTOMATIC when the photographs have just arrived and take the
           ;; default order, :CHOSEN when the photographer asked from the menu.
           (pending-sort (and (null initial-path)
                              (project-photos initial-project)
                              :automatic))
           (node-click (cons 0 nil))
           grade-clipboard
           inspector tabs node-page export-page
           kind-choice
           (node-panel-groups '())
           blend-opacity-input crop-angle-input rotate-turn-input flip-axis-input
           (crop-angle-controls '())
           base-red-input base-green-input base-blue-input base-swatch
           negative-red-input negative-green-input negative-blue-input
           negative-swatch
           crop-aspect-input crop-width-input crop-height-input
           ;; NIL is a free crop; otherwise an entry from
           ;; *CROP-ASPECT-CHOICES*. An editing mode, not a render parameter:
           ;; the rectangle itself is what the graph stores and what the render
           ;; needs, and the lock only governs how a drag reshapes it.
           ;; The proportions a crop keeps while it is dragged. The frame's own
           ;; by default: most crops trim a picture rather than reshape it.
           (crop-aspect :original)
           curve-canvas curve-drag scope-canvas
           (curve-channel :master-points)
           (curve-channel-buttons '())
           curve-histogram waveform-buffer
           (interned-photos (make-hash-table :test #'equal))
           status progress before-preview-file after-preview-file
           after-preview-generation
           lens-name controls inspector-items inspector-rows
           tone-items inspector-widgets
           lut-choice wb-choice target-choice
           export-quality export-max-width export-max-height export-metadata
           export-timestamp
           export-dialog export-dialog-destination export-dialog-filename
           export-dialog-scope
           export-dialog-format export-dialog-quality
           export-dialog-width export-dialog-height
           export-dialog-metadata export-dialog-timestamp
           lens-profile-label fringing-source-choice lens-picker lens-picker-caption
           lens-picker-search lens-picker-list lens-picker-focal
           lens-picker-remember lens-picker-records
           gallery-canvas gallery-title preset-name-input
           preset-apply-button preset-save-button
           (gallery-scroll 0)
           (gallery-selected nil)
           (gallery-stills '())
           (gallery-generation 0)
           (gallery-thumbs (make-hash-table :test #'equal))
           (gallery-click (cons 0 -1))
           debounce-id poll-id comparison-p layout-initialized-p
           ;; The divider is remembered as a share of the right column, not as
           ;; an absolute height: a window that grows must hand the graph its
           ;; proportion of the new space instead of freezing it at the height
           ;; computed for the window's creation size.
           (graph-height-fraction 62/100)
           applying-layout-p
           (progress-total 0)
           (graph-view-state :unset)
           (last-render-ms 10000)
           (live-render-ms 10000)
           after-live-front after-live-back after-live-capacity
           (after-live-width 0)
           (after-live-height 0)
           (after-live-generation 0)
           after-live-p live-settle-id
           (progress-generation -1)
           (thumbnail-scroll 0)
           (thumbnail-anchor 0)
           (preview-generation 0)
           (preview-zoom 1d0)
           (preview-center-x .5d0)
           (preview-center-y .5d0)
           preview-drag-p preview-drag-x preview-drag-y
           preview-drag-center-x preview-drag-center-y
           ;; Set only by "1:1 pixels": every other view renders a proxy sized
           ;; for the canvas rather than for the sensor.
           preview-native-p preview-one-to-one-pending-p)
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
           (selected-as-shot-kelvin ()
             ;; The temperature the camera balanced for. The renderer treats
             ;; this value as the one that changes nothing, so a control that
             ;; starts here starts where the photograph already is.
             (let ((job (selected-job)))
               (when job
                 (ignore-errors
                   (photo-as-shot-kelvin (photo-job-input-path job))))))
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
             (when before-canvas (lightfast:redraw before-canvas))
             (when after-canvas (lightfast:redraw after-canvas)))
           (reset-preview-view ()
             (let ((was-native preview-native-p))
               (setf preview-zoom 1d0
                     preview-center-x .5d0
                     preview-center-y .5d0
                     preview-native-p nil)
               ;; Coming back from 1:1 drops to a canvas-sized render, which is
               ;; a cache hit if this photo has been fitted before.
               (when was-native
                 (discard-gui-tasks queue :after)
                 (enqueue-preview nil)))
             (redraw-previews)
             (set-status "Preview fitted to window"))
           (viewport-render-bound ()
             ;; One number for both axes: the render bounds a box, and the
             ;; canvas can be taller than it is wide. Zero means the sensor,
             ;; which only "1:1 pixels" asks for: it is the one view whose
             ;; whole purpose is to show real photosites, so it is worth the
             ;; wait that the fit view is not.
             (if preview-native-p
                 0
                 (let ((canvas (or after-canvas before-canvas)))
                   (gui-preview-bound
                    (if canvas
                        (max (lightfast:widget-width canvas)
                             (lightfast:widget-height canvas))
                        *gui-preview-minimum-bound*)
                    preview-zoom))))
           (live-view-p (&optional canvas)
             ;; The live buffer is bounded, so its zoom ceiling is lower
             ;; than the full-resolution preview's. Showing it only at fit
             ;; keeps framing identical across the swap; zoomed in, the
             ;; file preview stays on screen and stays sharp.
             (and after-live-p after-live-front (plusp after-live-width)
                  (<= preview-zoom 1.0001d0)
                  (or (null canvas) (eq canvas after-canvas))))
           (draw-current-preview (widget path)
             ;; The adapter fits, zooms and pans a file about its own centre, so
             ;; a file that is only part of the frame is drawn by restating the
             ;; frame's placement in the file's own terms: the same screen
             ;; pixels per render pixel, and the frame point that belongs at the
             ;; canvas centre expressed as a fraction of the window instead of
             ;; of the frame.
             (let ((region (preview-region-for widget)))
               (multiple-value-bind (frame-width frame-height)
                   (preview-frame-size widget path)
                 (multiple-value-bind (file-width file-height)
                     (preview-file-size path)
                   (if (and region frame-width file-width
                            (plusp file-width) (plusp file-height))
                       (destructuring-bind (left top width height) region
                         (let* ((canvas-width (lightfast:widget-width widget))
                                (canvas-height (lightfast:widget-height widget))
                                (frame-fit
                                  (min (/ canvas-width (float frame-width 1d0))
                                       (/ canvas-height (float frame-height 1d0))))
                                (scale (min (max frame-fit 2d0)
                                            (* frame-fit preview-zoom)))
                                (file-fit
                                  (min (/ canvas-width (float file-width 1d0))
                                       (/ canvas-height (float file-height 1d0)))))
                           (draw-preview-file
                            widget path
                            :zoom (max 1d0 (/ scale file-fit))
                            :center-x (/ (- preview-center-x left)
                                         (max 1d-6 width))
                            :center-y (/ (- preview-center-y top)
                                         (max 1d-6 height)))))
                       (draw-preview-file widget path
                                          :zoom preview-zoom
                                          :center-x preview-center-x
                                          :center-y preview-center-y))))))
           (preview-region-for (canvas)
             (if (eq canvas before-canvas)
                 before-preview-region
                 after-preview-region))
           (preview-frame-size (canvas path)
             ;; The whole frame's size in render pixels, which is the file's own
             ;; size only when the file is the whole frame. A windowed preview
             ;; covers a fraction of it, and dividing the file's size by that
             ;; fraction recovers what the frame would have measured — which is
             ;; what every fit, zoom limit and overlay is stated against.
             (multiple-value-bind (source-width source-height)
                 (cond ((live-view-p canvas)
                        (values after-live-width after-live-height))
                       ;; Asked before anything has been developed, which is
                       ;; where the viewport machinery first looks: no file, no
                       ;; frame, and no window to ask for.
                       ((null path) (values nil nil))
                       (t (preview-file-size path)))
               (let ((region (and (not (live-view-p canvas))
                                  (preview-region-for canvas))))
                 (if (and source-width region)
                     (values (/ source-width (max 1d-6 (third region)))
                             (/ source-height (max 1d-6 (fourth region))))
                     (values source-width source-height)))))
           (preview-scaled-size (canvas path zoom)
             (multiple-value-bind (source-width source-height)
                 (preview-frame-size canvas path)
               (when source-width
                 (let ((fit (min (/ (lightfast:widget-width canvas)
                                    (float source-width 1d0))
                                 (/ (lightfast:widget-height canvas)
                                    (float source-height 1d0)))))
                   (let ((scale (min (max fit 2d0) (* fit zoom))))
                     (values (* source-width scale)
                             (* source-height scale)
                             fit))))))
           (maybe-refresh-viewport ()
             ;; Called after the view moves. A windowed preview covers the
             ;; visible rectangle and a margin around it, so most movement is
             ;; free; this is what asks for more once the margin is nearly
             ;; spent, while the developed window is still on screen to look at.
             (let ((job (selected-job)))
               (when (and job (not (live-view-p)))
                 (let ((needed (viewport-request))
                       (current (or requested-viewport-region after-preview-region)))
                   (when (and needed current (not (region-covers-view-p current)))
                     (setf requested-viewport-region needed)
                     (discard-gui-tasks queue :after)
                     (enqueue-render queue :after job
                                     (gui-model-selected-index model)
                                     (current-settings) preview-generation t
                                     :front-p t :cache-p t :viewport needed))))))
           (visible-frame-rect (canvas)
             ;; The part of the frame on screen, as fractions of it. Clamped the
             ;; same way the drawing clamps its pan, so the answer describes
             ;; what is actually visible rather than what was asked for.
             (let ((canvas (or canvas after-canvas before-canvas)))
               (when canvas
                 (multiple-value-bind (scaled-width scaled-height)
                     (preview-scaled-size canvas
                                          (preview-path-for-canvas canvas)
                                          preview-zoom)
                   (when (and scaled-width (plusp scaled-width))
                     (let* ((width (min 1d0 (/ (lightfast:widget-width canvas)
                                               scaled-width)))
                            (height (min 1d0 (/ (lightfast:widget-height canvas)
                                                scaled-height)))
                            (center-x (min (- 1d0 (/ width 2))
                                           (max (/ width 2) preview-center-x)))
                            (center-y (min (- 1d0 (/ height 2))
                                           (max (/ height 2) preview-center-y))))
                       (list (- center-x (/ width 2)) (- center-y (/ height 2))
                             width height)))))))
           (viewport-request ()
             ;; What to ask the renderer to develop: the visible rectangle grown
             ;; by a margin, or NIL for the whole frame when most of the frame is
             ;; on screen anyway.
             ;;
             ;; The margin is what makes panning free. Without it every drag
             ;; would leave the developed region immediately and the photograph
             ;; would have to be re-developed to move it a pixel; with it the
             ;; view slides inside what is already there, and a new render is
             ;; only wanted when it approaches the edge.
             (let ((visible (and (equal (recipe-geometry-key (current-settings))
                                        after-preview-geometry)
                                 (visible-frame-rect nil))))
               (when visible
                 (destructuring-bind (left top width height) visible
                   (when (< (* width height) *viewport-whole-frame-fraction*)
                     (let ((grow-x (* width *viewport-margin*))
                           (grow-y (* height *viewport-margin*)))
                       (let ((new-left (max 0d0 (- left grow-x)))
                             (new-top (max 0d0 (- top grow-y)))
                             (new-right (min 1d0 (+ left width grow-x)))
                             (new-bottom (min 1d0 (+ top height grow-y))))
                         (list new-left new-top
                               (- new-right new-left)
                               (- new-bottom new-top)))))))))
           (region-covers-view-p (region)
             ;; Whether what is developed still comfortably covers what is on
             ;; screen. Comfortably: a new render is asked for while the view is
             ;; still inside the developed region, so the answer usually lands
             ;; before the edge is reached and the pan never stalls.
             (let ((visible (visible-frame-rect nil)))
               (cond ((null visible) t)
                     ((null region) t)
                     (t (destructuring-bind (left top width height) visible
                          (destructuring-bind (rl rt rw rh) region
                            (let ((slack-x (* width *viewport-margin*
                                              *viewport-margin-spent*))
                                  (slack-y (* height *viewport-margin*
                                              *viewport-margin-spent*)))
                              (and (or (<= rl 0d0)
                                       (>= (- left slack-x) rl))
                                   (or (<= rt 0d0)
                                       (>= (- top slack-y) rt))
                                   (or (>= (+ rl rw) 1d0)
                                       (<= (+ left width slack-x) (+ rl rw)))
                                   (or (>= (+ rt rh) 1d0)
                                       (<= (+ top height slack-y) (+ rt rh)))))))))))
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
                     (let* ((canvas-center-x (/ (lightfast:widget-width canvas) 2d0))
                            (canvas-center-y (/ (lightfast:widget-height canvas) 2d0))
                            (source-x (+ preview-center-x
                                         (/ (- pointer-x canvas-center-x) old-width)))
                            (source-y (+ preview-center-y
                                         (/ (- pointer-y canvas-center-y) old-height)))
                            (ratio (/ old-zoom new-zoom)))
                       (setf preview-center-x
                             (- source-x (* (- source-x preview-center-x) ratio))
                             preview-center-y
                             (- source-y (* (- source-y preview-center-y) ratio)))))))
               (let ((previous-bound (viewport-render-bound)))
                 (setf preview-zoom new-zoom)
                 ;; Leaving fit hands the canvas back to the file preview, so
                 ;; a pending live edit must be settled now instead of after
                 ;; the drag timer, or the sharp image would lag behind. A
                 ;; zoom that crosses a doubling of the bound needs the same
                 ;; render for a different reason: the proxy on screen has
                 ;; fewer pixels than the canvas is about to ask of it.
                 (when (or (and after-live-p (> new-zoom 1.0001d0))
                           (/= previous-bound (viewport-render-bound)))
                   (when live-settle-id
                     (ignore-errors (lightfast:remove-timeout live-settle-id))
                     (setf live-settle-id nil))
                   (discard-gui-tasks queue :after)
                   (enqueue-preview nil))
                 ;; Zooming out inside one rung asks for no new render, but it
                 ;; does widen what is on screen — possibly past the window that
                 ;; was developed for the tighter view.
                 (maybe-refresh-viewport))
               (redraw-previews)
               ;; ~,0F leaves the decimal point behind: "125.%".
               (set-status (format nil "Preview zoom: ~D%"
                                   (round (* 100 new-zoom))))))
           (fit-preview-to-source-pixels ()
             ;; One photosite per screen pixel, given whatever preview is
             ;; loaded. Only meaningful once the sensor-resolution render has
             ;; landed, so APPLY-ONE-TO-ONE calls this again from the event.
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
                     t)))))
           (preview-one-to-one ()
             ;; The fit view is a canvas-sized proxy, so its own pixels are not
             ;; the sensor's: 1:1 has to ask for the unbounded render and set
             ;; the zoom again when it arrives.
             (cond (preview-native-p
                    (fit-preview-to-source-pixels)
                    (set-status "Preview at 1:1 pixels"))
                   (t
                    (setf preview-native-p t
                          preview-one-to-one-pending-p t)
                    (discard-gui-tasks queue :after)
                    (enqueue-preview nil)
                    (set-status "Developing 1:1 preview"))))
           (crop-node-geometry (node)
             (let ((graph (gui-model-display-graph model)))
               (and graph (downstream-geometry graph node))))
           (crop-node-rect (node)
             ;; In the coordinates of the picture on screen, which is not where
             ;; the rectangle is stored: it is stored in the frame the crop node
             ;; itself sees, and anything below it that turns or mirrors the
             ;; frame moves it out from under the overlay.
             (let ((params (orfeus:graph-node-params node)))
               (destructuring-bind (left top width height)
                   (rect-through-geometry
                    (list (float (getf params :left 0.0) 1d0)
                          (float (getf params :top 0.0) 1d0)
                          (float (getf params :width 1.0) 1d0)
                          (float (getf params :height 1.0) 1d0))
                    (crop-node-geometry node))
                 (values left top width height
                         (float (getf params :angle 0.0) 1d0)))))
           (preview-image-frame (canvas path)
             ;; Widget-relative placement of the drawn preview, mirroring
             ;; the native adapter's pan clamping so overlays land on the
             ;; same pixels the image occupies.
             (multiple-value-bind (scaled-width scaled-height)
                 (preview-scaled-size canvas path preview-zoom)
               (when scaled-width
                 (let* ((width (lightfast:widget-width canvas))
                        (height (lightfast:widget-height canvas))
                        (visible-x (min 1d0 (/ width scaled-width)))
                        (visible-y (min 1d0 (/ height scaled-height)))
                        (center-x (min (- 1d0 (/ visible-x 2))
                                       (max (/ visible-x 2)
                                            preview-center-x)))
                        (center-y (min (- 1d0 (/ visible-y 2))
                                       (max (/ visible-y 2)
                                            preview-center-y))))
                   (values (- (/ width 2d0) (* center-x scaled-width))
                           (- (/ height 2d0) (* center-y scaled-height))
                           scaled-width scaled-height)))))
           (crop-overlay-geometry (canvas)
             ;; The crop footprint on the displayed preview in widget
             ;; pixels: rect center, half extents, rotation basis, and the
             ;; image frame. Only the after canvas hosts the overlay.
             (let ((node (crop-editing-node))
                   (path (preview-path-for-canvas canvas)))
               (when (and node path (eq canvas after-canvas))
                 (multiple-value-bind (frame-x frame-y frame-width
                                       frame-height)
                     (preview-image-frame canvas path)
                   (when frame-x
                     (multiple-value-bind (left top width height angle)
                         (crop-node-rect node)
                       (declare (ignore angle))
                       ;; Upright, always. The preview beneath it is already
                       ;; straightened by the same angle, so a tilted rectangle
                       ;; here would tilt the crop twice over.
                       (let ((radians 0d0))
                         (values (+ frame-x (* (+ left (/ width 2))
                                               frame-width))
                                 (+ frame-y (* (+ top (/ height 2))
                                               frame-height))
                                 (/ (* width frame-width) 2)
                                 (/ (* height frame-height) 2)
                                 (cos radians) (sin radians)
                                 frame-x frame-y
                                 frame-width frame-height))))))))
           (crop-corner-offsets (half-width half-height)
             (list (list (- half-width) (- half-height))
                   (list half-width (- half-height))
                   (list half-width half-height)
                   (list (- half-width) half-height)))
           (crop-corner-point (center-x center-y cosine sine dx dy)
             ;; The source-space footprint of a crop corner: the executor
             ;; samples output offsets at (cos*dx+sin*dy, cos*dy-sin*dx).
             (values (+ center-x (* cosine dx) (* sine dy))
                     (+ center-y (- (* cosine dy) (* sine dx)))))
           (crop-corner-at (canvas x y)
             (multiple-value-bind (center-x center-y half-width half-height
                                   cosine sine)
                 (crop-overlay-geometry canvas)
               (when center-x
                 (loop for index from 0
                       for (dx dy) in (crop-corner-offsets half-width
                                                           half-height)
                       do (multiple-value-bind (px py)
                              (crop-corner-point center-x center-y
                                                 cosine sine dx dy)
                            (when (and (<= (abs (- x px)) 9)
                                       (<= (abs (- y py)) 9))
                              (return index)))))))
           (crop-rect-contains-p (canvas x y)
             (multiple-value-bind (center-x center-y half-width half-height
                                   cosine sine)
                 (crop-overlay-geometry canvas)
               (when center-x
                 (let* ((dx (- x center-x))
                        (dy (- y center-y))
                        (local-x (- (* cosine dx) (* sine dy)))
                        (local-y (+ (* sine dx) (* cosine dy))))
                   (and (<= (abs local-x) half-width)
                        (<= (abs local-y) half-height))))))
           (apply-crop-rect (node left top width height)
             (let* ((width (min 1.0 (max 0.05 width)))
                    (height (min 1.0 (max 0.05 height)))
                    (left (min (- 1.0 width) (max 0.0 left)))
                    (top (min (- 1.0 height) (max 0.0 top))))
               (handler-case
                   (destructuring-bind (stored-left stored-top
                                        stored-width stored-height)
                       ;; Back out of display coordinates into the frame the
                       ;; crop node reads, which is what CROP-NODE-RECT undoes.
                       (rect-through-geometry
                        (list (float left 1d0) (float top 1d0)
                              (float width 1d0) (float height 1d0))
                        (crop-node-geometry node) t)
                     (gui-model-set-node-params
                      model node
                      (list :left (float stored-left 1.0)
                            :top (float stored-top 1.0)
                            :width (float stored-width 1.0)
                            :height (float stored-height 1.0)))
                     (when after-canvas (lightfast:redraw after-canvas))
                     (set-status
                      (format nil "Crop ~,2F ~,2F ~,2Fx~,2F"
                              left top width height)))
                 (error (condition)
                   (set-status (princ-to-string condition))))))
           (crop-frame-size ()
             ;; The whole uncropped photograph as drawn. The crop stage is
             ;; bypassed while its node is selected, so this really is the full
             ;; frame and its proportions are the sensor's.
             (let ((path (and after-canvas
                              (preview-path-for-canvas after-canvas))))
               (when path
                 (multiple-value-bind (frame-x frame-y frame-width frame-height)
                     (preview-image-frame after-canvas path)
                   (declare (ignore frame-x frame-y))
                   (when (and frame-width (plusp frame-width)
                              (plusp frame-height))
                     (values frame-width frame-height))))))
           (crop-frame-and-ratio ()
             ;; The frame and the proportions locked against it. Both callers
             ;; need the frame as well as the ratio — a ratio is meaningless
             ;; without the frame it is a ratio of — so this returns the three
             ;; together rather than each fetching the frame again.
             (multiple-value-bind (frame-width frame-height) (crop-frame-size)
               (values frame-width frame-height
                       (and frame-width
                            (crop-aspect-ratio crop-aspect frame-width
                                               frame-height)))))
           (reshape-crop-to-aspect (node)
             ;; Shrink the current rectangle to the locked proportions about its
             ;; own centre. Shrinking rather than growing, so choosing a ratio
             ;; never pulls back in a part of the frame that had been cropped
             ;; away — the crop only ever tightens until the user widens it.
             (multiple-value-bind (frame-width frame-height ratio)
                 (crop-frame-and-ratio)
               (when ratio
                 (multiple-value-bind (left top width height)
                     (crop-node-rect node)
                   (multiple-value-call #'apply-crop-rect node
                     (crop-rect-for-ratio left top width height
                                          frame-width frame-height
                                          ratio))))))
           (set-crop-size-fraction (node axis fraction)
             ;; Size one axis as a share of the frame, keeping the centre. Under
             ;; a locked ratio the other axis follows, so the two fields stay
             ;; consistent with the lock instead of fighting it.
             (multiple-value-bind (frame-width frame-height ratio)
                 (crop-frame-and-ratio)
               (multiple-value-bind (left top width height)
                   (crop-node-rect node)
                 (let* ((fraction (min 1.0d0 (max 0.05d0 fraction)))
                        (new-width (if (eq axis :width) fraction width))
                        (new-height (if (eq axis :height) fraction height)))
                   (when ratio
                     (if (eq axis :width)
                         (setf new-height (/ (* new-width frame-width)
                                             ratio frame-height))
                         (setf new-width (/ (* new-height frame-height ratio)
                                            frame-width))))
                   (let ((center-u (+ left (/ width 2)))
                         (center-v (+ top (/ height 2))))
                     (apply-crop-rect node
                                      (- center-u (/ new-width 2))
                                      (- center-v (/ new-height 2))
                                      new-width new-height))))))
           (set-crop-node-angle (node angle)
             ;; The preview shows the straightening, so an angle change is a
             ;; change to the picture and not merely to the overlay.
             (handler-case
                 (progn
                   (gui-model-set-node-params model node (list :angle angle))
                   (when after-canvas (lightfast:redraw after-canvas))
                   (schedule-edited-preview)
                   (set-status (format nil "Crop angle ~,1F deg" angle)))
               (error (condition)
                 (set-status (princ-to-string condition)))))
           (begin-crop-drag (canvas x y)
             ;; A left press while a crop node is selected: corners resize,
             ;; the interior moves the rectangle. Returns true when the
             ;; gesture is claimed so panning is untouched elsewhere.
             (multiple-value-bind (center-x center-y half-width half-height
                                   cosine sine frame-x frame-y
                                   frame-width frame-height)
                 (crop-overlay-geometry canvas)
               (when (and center-x
                          (crop-preview-current-p preview-generation
                                                  after-preview-generation))
                 (let ((corner (crop-corner-at canvas x y)))
                   (cond
                     (corner
                      (destructuring-bind (dx dy)
                          (nth corner (crop-corner-offsets half-width
                                                           half-height))
                        (multiple-value-bind (anchor-x anchor-y)
                            (crop-corner-point center-x center-y cosine sine
                                               (- dx) (- dy))
                          (setf crop-drag
                                (list :resize
                                      :sign-x (if (minusp dx) -1 1)
                                      :sign-y (if (minusp dy) -1 1)
                                      :anchor-x anchor-x :anchor-y anchor-y
                                      :cosine cosine :sine sine
                                      :frame (list frame-x frame-y
                                                   frame-width
                                                   frame-height)))))
                      t)
                     ((crop-rect-contains-p canvas x y)
                      (multiple-value-bind (left top)
                          (crop-node-rect (crop-editing-node))
                        (setf crop-drag
                              (list :move
                                    :offset-u (- (/ (- x frame-x)
                                                    frame-width)
                                                 left)
                                    :offset-v (- (/ (- y frame-y)
                                                    frame-height)
                                                 top)
                                    :frame (list frame-x frame-y
                                                 frame-width
                                                 frame-height))))
                      t))))))
           (update-crop-drag (x y)
             (let ((node (crop-editing-node))
                   (plist (rest crop-drag)))
               (when (and node crop-drag)
                 (destructuring-bind (frame-x frame-y frame-width
                                      frame-height)
                     (getf plist :frame)
                   (ecase (first crop-drag)
                     (:resize
                      (let* ((sign-x (getf plist :sign-x))
                             (sign-y (getf plist :sign-y))
                             (anchor-x (getf plist :anchor-x))
                             (anchor-y (getf plist :anchor-y))
                             (cosine (getf plist :cosine))
                             (sine (getf plist :sine))
                             (dx (- x anchor-x))
                             (dy (- y anchor-y))
                             ;; Pointer in the rect's rotated frame,
                             ;; measured from the fixed opposite corner.
                             (local-x (- (* cosine dx) (* sine dy)))
                             (local-y (+ (* sine dx) (* cosine dy)))
                             (raw-span-x (min frame-width
                                              (max (* frame-width 0.05)
                                                   (* sign-x local-x))))
                             (raw-span-y (min frame-height
                                              (max (* frame-height 0.05)
                                                   (* sign-y local-y))))
                             (ratio (crop-aspect-ratio crop-aspect
                                                       frame-width
                                                       frame-height))
                             ;; Constrained here rather than in
                             ;; APPLY-CROP-RECT because this is where the
                             ;; opposite corner is known: deriving one span
                             ;; from the other keeps that corner pinned while
                             ;; the handle follows the pointer. The larger of
                             ;; the two candidate sizes wins, so the rectangle
                             ;; grows towards the pointer on whichever axis it
                             ;; moved furthest.
                             (span-x (if ratio
                                         (max raw-span-x (* raw-span-y ratio))
                                         raw-span-x))
                             (span-y (if ratio
                                         (/ (max raw-span-x (* raw-span-y ratio))
                                            ratio)
                                         raw-span-y))
                             (overflow (max 1.0d0
                                            (/ span-x frame-width)
                                            (/ span-y frame-height)))
                             (span-x (/ span-x overflow))
                             (span-y (/ span-y overflow))
                             (center-dx (* sign-x span-x 0.5))
                             (center-dy (* sign-y span-y 0.5))
                             (center-x (+ anchor-x
                                          (* cosine center-dx)
                                          (* sine center-dy)))
                             (center-y (+ anchor-y
                                          (- (* cosine center-dy)
                                             (* sine center-dx))))
                             (width (/ span-x frame-width))
                             (height (/ span-y frame-height)))
                        (apply-crop-rect
                         node
                         (- (/ (- center-x frame-x) frame-width)
                            (/ width 2))
                         (- (/ (- center-y frame-y) frame-height)
                            (/ height 2))
                         width height)))
                     (:move
                      (multiple-value-bind (left top width height)
                          (crop-node-rect node)
                        (declare (ignore left top))
                        (apply-crop-rect
                         node
                         (- (/ (- x frame-x) frame-width)
                            (getf plist :offset-u))
                         (- (/ (- y frame-y) frame-height)
                            (getf plist :offset-v))
                         width height))))))))
           (draw-crop-overlay (widget)
             (multiple-value-bind (center-x center-y half-width half-height
                                   cosine sine)
                 (crop-overlay-geometry widget)
               (when center-x
                 (let* ((x (lightfast:widget-x widget))
                        (y (lightfast:widget-y widget))
                        (corners
                          (loop for (dx dy) in (crop-corner-offsets
                                                half-width half-height)
                                collect
                                (multiple-value-bind (px py)
                                    (crop-corner-point center-x center-y
                                                       cosine sine dx dy)
                                  (list (round (+ x px))
                                        (round (+ y py)))))))
                   (lightfast:draw-push-clip x y
                                           (lightfast:widget-width widget)
                                           (lightfast:widget-height widget))
                   (loop for (from to) in '((0 1) (1 2) (2 3) (3 0))
                         do (destructuring-bind (x1 y1) (nth from corners)
                              (destructuring-bind (x2 y2) (nth to corners)
                                (lightfast:draw-color-rgb :red 20 :green 22
                                                        :blue 26)
                                (lightfast:draw-line (1+ x1) (1+ y1)
                                                   (1+ x2) (1+ y2))
                                (lightfast:draw-color-rgb :red 235 :green 235
                                                        :blue 240)
                                (lightfast:draw-line x1 y1 x2 y2))))
                   (loop for (px py) in corners
                         do (lightfast:draw-color-rgb :red 20 :green 22
                                                    :blue 26)
                            (lightfast:draw-filled-circle px py 5)
                            (lightfast:draw-color-rgb :red 235 :green 235
                                                    :blue 240)
                            (lightfast:draw-filled-circle px py 4))
                   (lightfast:draw-pop-clip)))))
           (curves-editing-node ()
             ;; The editor drives the selected curves node, else the most
             ;; downstream curves node on the photo's real graph.
             (let* ((job (selected-job))
                    (graph (and job (orfeus:photo-job-graph job)))
                    (selected (gui-model-selected-graph-node model)))
               (cond
                 ((and selected (eq :curves (orfeus:graph-node-kind selected)))
                  selected)
                 (graph
                  (find :curves
                        (reverse (orfeus:processing-graph-nodes graph))
                        :key #'orfeus:graph-node-kind)))))
           (curve-node-points (node key)
             (or (getf (orfeus:graph-node-params node) key)
                 orfeus:*identity-curve-points*))
           (curve-channel-color (key active)
             (ecase key
               (:red-points (if active
                                (values 235 90 90)
                                (values 120 55 55)))
               (:green-points (if active
                                  (values 95 220 95)
                                  (values 55 110 55)))
               (:blue-points (if active
                                 (values 110 150 255)
                                 (values 60 75 125)))
               (:master-points (if active
                                   (values 240 240 245)
                                   (values 120 120 128)))))
           (curve-channel-label (key)
             (ecase key
               (:red-points "Red")
               (:green-points "Green")
               (:blue-points "Blue")
               (:master-points "Luma")))
           (curve-channel-button-label (key)
             (ecase key
               (:red-points "R")
               (:green-points "G")
               (:blue-points "B")
               (:master-points "Y")))
           (sync-curve-channel-buttons ()
             ;; The selected channel's button carries that channel's colour,
             ;; the rest stay the panel's grey.
             (dolist (entry curve-channel-buttons)
               (if (eq (first entry) curve-channel)
                   (multiple-value-bind (red green blue)
                       (curve-channel-color (first entry) t)
                     (lightfast:set-color-rgb (rest entry) :red red
                                                           :green green
                                                           :blue blue))
                   (lightfast:set-color-rgb (rest entry) :red 58 :green 58
                                                         :blue 64))))
           (release-waveform ()
             (when waveform-buffer
               (cffi:foreign-free waveform-buffer)
               (setf waveform-buffer nil)))
           (schedule-curve-histogram (path generation)
             ;; Scope decoding is bounded to one pending background task; draw
             ;; callbacks only consume the last completed result. The worker
             ;; owns its waveform image until the event hands it over, so no
             ;; buffer is ever read while a decode still writes to it.
             (discard-gui-tasks histogram-queue :histogram)
             (setf curve-histogram nil)
             (enqueue-gui-task
              histogram-queue :histogram
              (lambda ()
                (let ((image (allocate-waveform-buffer)))
                  (multiple-value-bind (red green blue)
                      (ignore-errors (preview-scopes path image))
                    (if red
                        (queue-event queue
                                     (list :histogram generation path
                                           (list red green blue) image))
                        (cffi:foreign-free image)))))))
           (curve-chart-geometry ()
             ;; The chart rectangle, widget-relative.
             (values 12 8
                     (- (lightfast:widget-width curve-canvas) 24)
                     (- (lightfast:widget-height curve-canvas) 16)))
           (channel-clip-fraction (plane edge)
             ;; The share of samples piled into the darkest or brightest bin.
             (let ((total (reduce #'+ plane)))
               (if (plusp total)
                   (/ (aref plane (ecase edge
                                    (:low 0)
                                    (:high (1- (length plane)))))
                      (float total))
                   0.0)))
           (draw-scope (widget)
             ;; The waveform: one column per band of image columns, levels
             ;; rising up the frame, so shadows sit at the bottom and
             ;; highlights at the top. Traces that reach an edge are clipping,
             ;; and the corner letters name the channels that got there.
             (let* ((x (lightfast:widget-x widget))
                    (y (lightfast:widget-y widget))
                    (width (lightfast:widget-width widget))
                    (height (lightfast:widget-height widget))
                    (left (1+ x))
                    (top (1+ y))
                    (inner-width (- width 2))
                    (inner-height (- height 2)))
               (lightfast:draw-color-rgb :red 8 :green 8 :blue 10)
               (lightfast:draw-filled-rect x y width height)
               (cond
                 ((and waveform-buffer (plusp inner-width) (plusp inner-height))
                  (draw-waveform widget waveform-buffer left top
                                 inner-width inner-height)
                  (lightfast:draw-color-rgb :red 46 :green 46 :blue 54)
                  (dotimes (line 3)
                    (let ((grid-y (+ top (floor (* (1+ line) inner-height) 4))))
                      (lightfast:draw-line left grid-y
                                         (+ left inner-width -1) grid-y)))
                  (when curve-histogram
                    (lightfast:draw-font :size 10)
                    (loop for plane in curve-histogram
                          for label in '("R" "G" "B")
                          for column from 0
                          do (loop for edge in '(:high :low)
                                   for label-y = (if (eq edge :high)
                                                     (+ top 11)
                                                     (+ top inner-height -3))
                                   do (multiple-value-bind (red green blue)
                                          (if (> (channel-clip-fraction plane
                                                                        edge)
                                                 0.002)
                                              (curve-channel-color
                                               (nth column
                                                    orfeus:*curve-channel-keys*)
                                               t)
                                              (values 62 62 68))
                                        (lightfast:draw-color-rgb :red red
                                                                :green green
                                                                :blue blue)
                                        (lightfast:draw-text
                                         label
                                         (+ left inner-width -34
                                            (* column 11))
                                         label-y))))
                    (lightfast:draw-font :size 12))
                  ;; The trace is decoded from the settled preview, so during a
                  ;; drag it still shows the grade before the current one. Say
                  ;; so rather than let it be read as the live result.
                  (when after-live-p
                    (lightfast:draw-color-rgb :red 150 :green 130 :blue 60)
                    (lightfast:draw-font :size 10)
                    (lightfast:draw-text "settling" (+ left 4) (+ top 11))
                    (lightfast:draw-font :size 12)))
                 (t
                  (lightfast:draw-color-rgb :red 128 :green 128 :blue 134)
                  (lightfast:draw-font :size 11)
                  (lightfast:draw-text "Waveform appears with the preview"
                                     (+ left 12) (+ top (floor inner-height 2)))
                  (lightfast:draw-font :size 12)))
               (lightfast:draw-color-rgb :red 72 :green 72 :blue 80)
               (lightfast:draw-rect x y width height)))
           (curve-point-position (points index left top chart-w chart-h)
             ;; Where one control point lands on screen, in absolute pixels.
             (values (+ left (round (* (nth (* index 2) points)
                                       (1- chart-w))))
                     (+ top (round (* (- 1 (nth (1+ (* index 2)) points))
                                      (1- chart-h))))))
           (curve-handle-at (node x y)
             ;; The control point under the pointer, in the widget-relative
             ;; coordinates the event carries. The active channel is searched
             ;; first, so the stack of untouched white points at the top right
             ;; hands over the one the user is already editing.
             (multiple-value-bind (chart-x chart-y chart-w chart-h)
                 (curve-chart-geometry)
               (dolist (key (cons curve-channel
                                  (remove curve-channel
                                          orfeus:*curve-channel-keys*)))
                 (let ((points (curve-node-points node key)))
                   (loop for index below (floor (length points) 2)
                         do (multiple-value-bind (point-x point-y)
                                (curve-point-position points index
                                                      chart-x chart-y
                                                      chart-w chart-h)
                              (when (and (<= (abs (- x point-x)) 8)
                                         (<= (abs (- y point-y)) 8))
                                (return-from curve-handle-at
                                  (values key index)))))))))
           (curve-chart-position (x y)
             ;; Pointer position as curve coordinates, or NIL outside the chart.
             (multiple-value-bind (chart-x chart-y chart-w chart-h)
                 (curve-chart-geometry)
               (let ((fx (/ (- x chart-x) (float (1- chart-w))))
                     (fy (- 1 (/ (- y chart-y) (float (1- chart-h))))))
                 (when (and (<= -0.05 fx 1.05) (<= -0.05 fy 1.05))
                   (values (max 0.0 (min 1.0 fx)) (max 0.0 (min 1.0 fy)))))))
           (insert-curve-point (node at-x at-y)
             ;; Clicking empty chart adds a point to the active curve, the way
             ;; the reference panel does, so a film stock's shape costs points
             ;; only where it actually bends.
             (let* ((points (curve-node-points node curve-channel))
                    (count (floor (length points) 2)))
               (cond
                 ((>= count orfeus:*maximum-curve-points*)
                  (set-status (format nil "A curve holds at most ~D points"
                                      orfeus:*maximum-curve-points*)))
                 ;; A new point has to fall strictly between two existing ones,
                 ;; with room for the ascending-position rule on both sides.
                 ((let ((before (loop for index below count
                                      when (< (nth (* index 2) points) at-x)
                                        maximize index)))
                    (and before
                         (< before (1- count))
                         (> at-x (+ (nth (* before 2) points) 0.02))
                         (< at-x (- (nth (* (1+ before) 2) points) 0.02))
                         (let ((updated (append (subseq points 0
                                                        (* 2 (1+ before)))
                                                (list (float at-x 1.0)
                                                      (float at-y 1.0))
                                                (subseq points
                                                        (* 2 (1+ before))))))
                           (handler-case
                               (progn
                                 (gui-model-set-node-params
                                  model node (list curve-channel updated))
                                 (setf curve-drag (1+ before))
                                 (lightfast:redraw curve-canvas)
                                 (schedule-edited-preview)
                                 (set-status
                                  (format nil "~A point added"
                                          (curve-channel-label curve-channel)))
                                 t)
                             (error (condition)
                               (set-status (princ-to-string condition))
                               t))))))
                 (t (set-status "No room for a point there")))))
           (delete-curve-point (node index)
             ;; The endpoints are the channel's black and white points, so they
             ;; stay: without them the curve would have nothing to hold flat.
             (let* ((points (curve-node-points node curve-channel))
                    (count (floor (length points) 2)))
               (cond
                 ((or (zerop index) (= index (1- count)))
                  (set-status "The black and white points cannot be removed"))
                 ((<= count orfeus:*minimum-curve-points*)
                  (set-status "A curve keeps its two endpoints"))
                 (t
                  (handler-case
                      (progn
                        (gui-model-set-node-params
                         model node
                         (list curve-channel
                               (append (subseq points 0 (* 2 index))
                                       (subseq points (* 2 (1+ index))))))
                        (setf curve-drag nil)
                        (lightfast:redraw curve-canvas)
                        (schedule-edited-preview)
                        (set-status
                         (format nil "~A point removed"
                                 (curve-channel-label curve-channel))))
                    (error (condition)
                      (set-status (princ-to-string condition))))))))
           (draw-curve-editor (widget)
             (let* ((x (lightfast:widget-x widget))
                    (y (lightfast:widget-y widget))
                    (width (lightfast:widget-width widget))
                    (height (lightfast:widget-height widget))
                    (node (curves-editing-node)))
               (lightfast:draw-color-rgb :red 44 :green 44 :blue 48)
               (lightfast:draw-filled-rect x y width height)
               (multiple-value-bind (chart-x chart-y chart-w chart-h)
                   (curve-chart-geometry)
                 (let ((left (+ x chart-x))
                       (top (+ y chart-y)))
                   (lightfast:draw-color-rgb :red 16 :green 16 :blue 18)
                   (lightfast:draw-filled-rect left top chart-w chart-h)
                   ;; Channel occupancy: dim cached RGB histogram bars behind
                   ;; the grid. Decoding runs on the background queue.
                   (when curve-histogram
                     (loop for plane in curve-histogram
                           for (red green blue) in '((120 50 50)
                                                     (50 105 50)
                                                     (60 75 140))
                           do (let* ((bins (length plane))
                                     (peak (max 1 (loop for index from 1
                                                          below (1- bins)
                                                        maximize
                                                        (aref plane index)))))
                                (lightfast:draw-color-rgb :red red :green green
                                                        :blue blue)
                                (loop for index from 0 below bins
                                      for value = (min 1.0
                                                       (/ (aref plane index)
                                                          (float peak)))
                                      for bar = (round (* value
                                                          (- chart-h 2)))
                                      for bar-x = (+ left
                                                     (floor (* index chart-w)
                                                            bins))
                                      for bar-w = (max 1
                                                       (- (floor (* (1+ index)
                                                                    chart-w)
                                                                 bins)
                                                          (floor (* index
                                                                    chart-w)
                                                                 bins)))
                                      do (when (plusp bar)
                                           (lightfast:draw-filled-rect
                                            bar-x (+ top chart-h (- bar))
                                            bar-w bar))))))
                   (lightfast:draw-color-rgb :red 58 :green 58 :blue 64)
                   (dotimes (line 2)
                     (let ((grid-x (+ left (floor (* (1+ line) chart-w) 3)))
                           (grid-y (+ top (floor (* (1+ line) chart-h) 3))))
                       (lightfast:draw-line grid-x top grid-x (+ top chart-h))
                       (lightfast:draw-line left grid-y (+ left chart-w)
                                          grid-y)))
                   (lightfast:draw-color-rgb :red 84 :green 84 :blue 92)
                   (lightfast:draw-line left (+ top chart-h) (+ left chart-w)
                                      top)
                   (if (null node)
                       (progn
                         (lightfast:draw-color-rgb :red 205 :green 205
                                                 :blue 210)
                         (lightfast:draw-font :size 11)
                         (lightfast:draw-text "Right-click the node strip"
                                            (+ left 14) (+ top 26))
                         (lightfast:draw-text "and add a Curves node"
                                            (+ left 14) (+ top 42))
                         (lightfast:draw-font :size 12))
                       (progn
                         ;; Every channel is drawn, inactive ones first so the
                         ;; active curve sits on top, and every channel keeps
                         ;; its own handles: the reference panel lets you grab
                         ;; any of the four white points directly rather than
                         ;; selecting a channel first.
                         (dolist (key (append (remove curve-channel
                                                      orfeus:*curve-channel-keys*)
                                              (list curve-channel)))
                           (let ((points (curve-node-points node key))
                                 (active (eq key curve-channel)))
                             (multiple-value-bind (red green blue)
                                 (curve-channel-color key active)
                               (lightfast:draw-color-rgb :red red :green green
                                                       :blue blue))
                             (loop with steps = 96
                                   for step from 0 below steps
                                   for x0 = (/ step (float steps))
                                   for x1 = (/ (1+ step) (float steps))
                                   for y0 = (curve-spline-value points x0)
                                   for y1 = (curve-spline-value points x1)
                                   do (lightfast:draw-line
                                       (+ left (round (* x0 (1- chart-w))))
                                       (+ top (round (* (- 1 y0)
                                                        (1- chart-h))))
                                       (+ left (round (* x1 (1- chart-w))))
                                       (+ top (round (* (- 1 y1)
                                                        (1- chart-h))))))
                             (loop for index below (floor (length points) 2)
                                   do (multiple-value-bind (point-x point-y)
                                          (curve-point-position points index
                                                                left top
                                                                chart-w chart-h)
                                        (lightfast:draw-color-rgb
                                         :red 16 :green 16 :blue 20)
                                        (lightfast:draw-filled-circle
                                         point-x point-y (if active 5 4))
                                        (multiple-value-bind (red green blue)
                                            (curve-channel-color key active)
                                          (lightfast:draw-color-rgb
                                           :red red :green green :blue blue))
                                        (lightfast:draw-filled-circle
                                         point-x point-y (if active 4 3))))))))))))
           (update-curve-drag (node x y)
             (multiple-value-bind (chart-x chart-y chart-w chart-h)
                 (curve-chart-geometry)
               (let* ((points (copy-list (curve-node-points node
                                                            curve-channel)))
                      (index curve-drag)
                      (last (1- (floor (length points) 2)))
                      (raw-x (/ (- x chart-x) (float (1- chart-w))))
                      (raw-y (- 1 (/ (- y chart-y) (float (1- chart-h)))))
                      (new-y (max 0.0 (min 1.0 raw-y)))
                      ;; Every point moves in x, the ends included: pulling the
                      ;; white point left is how a channel gains without
                      ;; disturbing the rest of the curve.
                      (lower (if (zerop index)
                                 0.0
                                 (+ (nth (* 2 (1- index)) points) 0.02)))
                      (upper (if (= index last)
                                 1.0
                                 (- (nth (* 2 (1+ index)) points) 0.02)))
                      (new-x (max lower (min upper raw-x))))
                 (setf (nth (* index 2) points) (float new-x 1.0)
                       (nth (1+ (* index 2)) points) (float new-y 1.0))
                 (handler-case
                     (progn
                       (gui-model-set-node-params
                        model node (list curve-channel points))
                       (lightfast:redraw curve-canvas)
                       (schedule-edited-preview)
                       (set-status
                        (format nil "~A point ~D: ~,2F ~,2F"
                                (curve-channel-label curve-channel)
                                (1+ index) new-x new-y)))
                   (error (condition)
                     (set-status (princ-to-string condition)))))))
           (handle-curve-mouse (widget event value)
             (declare (ignore widget))
             (multiple-value-bind (x y button) (parse-preview-event value)
               (when x
                 (let ((node (curves-editing-node)))
                   (when node
                     (case event
                       (#.lightfast:+event-push+
                        (setf curve-drag nil)
                        (multiple-value-bind (key index)
                            (curve-handle-at node x y)
                          (cond
                            ;; Grabbing a handle selects its channel, so the
                            ;; four white points are all directly reachable
                            ;; without visiting the selector first.
                            (key
                             (unless (eq key curve-channel)
                               (setf curve-channel key)
                               (sync-curve-channel-buttons))
                             (if (= button 3)
                                 (delete-curve-point node index)
                                 (setf curve-drag index)))
                            ((= button 3) nil)
                            (t
                             (multiple-value-bind (at-x at-y)
                                 (curve-chart-position x y)
                               (when at-x
                                 (insert-curve-point node at-x at-y)))))))
                       (#.lightfast:+event-drag+
                        (when curve-drag
                          (update-curve-drag node x y)))
                       (#.lightfast:+event-release+
                        (when curve-drag
                          (setf curve-drag nil)
                          (set-status
                           (format nil "~A curve updated"
                                   (curve-channel-label
                                    curve-channel)))))))))))
           (reset-curve-channel ()
             (let ((node (curves-editing-node)))
               (if node
                   (handler-case
                       (progn
                         (gui-model-set-node-params
                          model node
                          (list curve-channel
                                (copy-list orfeus:*identity-curve-points*)))
                         (after-graph-edit
                          (format nil "~A curve reset"
                                  (curve-channel-label curve-channel))))
                     (error (condition)
                       (set-status (princ-to-string condition))))
                   (set-status "Add a Curves node first"))))
           (handle-preview-mouse (canvas event value)
             (multiple-value-bind (x y button dx dy state)
                 (parse-preview-event value)
               (declare (ignore dx state))
               (when x
                 (case event
                   (#.lightfast:+event-push+
                    (cond
                      (pick-color-node
                       (sample-base-at canvas x y))
                      ((and (= button 1) (begin-crop-drag canvas x y)))
                      ((or (= button 1) (= button 2))
                       (setf preview-drag-p t
                             preview-drag-x x
                             preview-drag-y y
                             preview-drag-center-x preview-center-x
                             preview-drag-center-y preview-center-y))))
                   (#.lightfast:+event-drag+
                    (cond
                      (crop-drag (update-crop-drag x y))
                      (preview-drag-p
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
                               (maybe-refresh-viewport)
                               (redraw-previews))))))))
                   (#.lightfast:+event-release+
                    (setf crop-drag nil
                          preview-drag-p nil))
                   (#.lightfast:+event-wheel+
                    (zoom-preview (if (minusp dy) 1.25d0 .8d0)
                                  canvas x y))))))
           (show-render-progress ()
             (setf (lightfast:value progress)
                   (format nil "~D"
                           (max 2 (min 99 (round (* 100 render-stage-fraction)))))))
           (render-summary-text ()
             ;; After a render, what it cost and at what size — the two facts
             ;; that explain the wait that just happened and predict the next.
             (format nil "~A~@[  |  ~A~]" (preview-status-text model)
                     (when (plusp last-render-ms)
                       (let ((bound (viewport-render-bound)))
                         (format nil "Last render: ~D ms at ~D px~:[~; (window)~]"
                                 (round last-render-ms)
                                 bound
                                 (and after-preview-region t))))))
           (render-activity-text ()
             ;; What the renderer is doing, in its own words, with the size it
             ;; is doing it at. The size matters: the same photograph takes a
             ;; fifth as long developed for a window as for the whole frame, and
             ;; a photographer wondering why one edit was instant and the next
             ;; was not deserves to be told which happened.
             (let ((bound (viewport-render-bound))
                   (window (and after-preview-region t)))
               (format nil "Developing~@[ ~A~]~@[ · ~A~]~@[ · ~D px~]~:[~; · window~]"
                       (and render-stage (string-downcase render-stage))
                       (let ((job (selected-job)))
                         (and job (orfeus:photo-display-name
                                   (photo-job-input-path job)
                                   :interned-p (photo-interned-cached-p job))))
                       (and (plusp bound) bound)
                       window)))
           (set-status (text)
             (setf (lightfast:value status) text))
           (selected-job ()
             (gui-model-selected-job model))
           (thumbnail-preview-for (job)
             ;; The rendered sidebar thumbnail for JOB, when one exists. Used as
             ;; the stand-in while the full preview develops.
             (and job (gethash job thumbnail-files)))
           (photo-content-key-for (job generation &optional refresh-p)
             ;; PHOTO-CONTENT-KEY memoizes on the file's stat, so this no longer
             ;; keeps a per-generation table of its own: keying that table by
             ;; the generation counter, which advances on every click, meant a
             ;; full digest of the RAW on every click and never a reuse.
             (declare (ignore generation))
             (let ((path (photo-job-input-path job)))
               (when refresh-p
                 (orfeus:forget-content-key path))
               (photo-content-key path)))
           (thumbnail-row-height () 104)
           (photo-group-of (job) (gethash job photo-groups))
           (draw-photo-rating (job x y)
             (draw-star-rating
              (ignore-errors (orfeus:photo-rating (photo-job-input-path job)))
              x y))
           (draw-burst-bar (job x row-y row-height)
             ;; A bar down the left edge of every frame of a burst, closed at
             ;; the ends, so a run of them reads as one block rather than as
             ;; unrelated rows that happen to look alike.
             (let* ((place (photo-group-of job))
                    (position (second place))
                    (size (third place)))
               (when (and size (> size 1))
                 (lightfast:draw-color-rgb :red 120 :green 170 :blue 230)
                 (lightfast:draw-filled-rect
                  (+ x 1) (if (zerop position) (+ row-y 6) row-y)
                  3
                  (- row-height (if (zerop position) 6 0)
                     (if (= position (1- size)) 6 0))))))
           (draw-burst-position (job x y)
             (let* ((place (photo-group-of job))
                    (position (second place))
                    (size (third place)))
               (when (and size (> size 1))
                 (lightfast:draw-color-rgb :red 150 :green 190 :blue 240)
                 (lightfast:draw-text (format nil "burst ~D of ~D"
                                              (1+ position) size)
                                      x y))))
           (burst-leader-p (job)
             (let ((place (photo-group-of job)))
               (and place (eql 0 (second place)) (> (third place) 1))))
           (burst-collapsed-p (job)
             (and (burst-leader-p job) (gethash job collapsed-bursts)))
           (thumbnail-display-rows ()
             ;; What the filmstrip actually shows: one entry per drawn row,
             ;; (INDICES . LEADER), where INDICES are places in the project and
             ;; LEADER is the leading photograph when the row stands for a
             ;; folded burst. A burst arrives as a run of consecutive
             ;; photographs, which is what lets a folded one be one row without
             ;; the project's own order being disturbed.
             ;;
             ;; Recomputed on every draw rather than kept and invalidated. A
             ;; card's worth of photographs is a few hundred conses, which is
             ;; nothing beside the drawing, and a cache here would have to be
             ;; invalidated from every place that adds a photograph, removes
             ;; one, regroups, or folds — four chances to show the wrong rows.
             (let ((rows '())
                   (photos (project-photos project))
                   (skip 0))
               (loop for job in photos
                     for index from 0
                     do (cond
                          ((plusp skip) (decf skip))
                          ((burst-collapsed-p job)
                           (let* ((size (third (photo-group-of job)))
                                  (end (min (+ index size) (length photos))))
                             (setf skip (- end index 1))
                             (push (cons (loop for place from index below end
                                               collect place)
                                         job)
                                   rows)))
                          (t (push (cons (list index) nil) rows))))
               (nreverse rows)))
           (toggle-burst (leader)
             (if (gethash leader collapsed-bursts)
                 (remhash leader collapsed-bursts)
                 (setf (gethash leader collapsed-bursts) t))
             (redraw-thumbnails))
           (set-bursts-expanded (expanded-p)
             (clrhash collapsed-bursts)
             (unless expanded-p
               (dolist (job (project-photos project))
                 (when (burst-leader-p job)
                   (setf (gethash job collapsed-bursts) t))))
             (redraw-thumbnails)
             (set-status (if expanded-p "Bursts expanded" "Bursts collapsed")))
           (photo-focus-of (job) (gethash job photo-focus-reports))
           (photo-focus-verdict (job)
             ;; A frame that could not be measured at all is remembered as
             ;; such, so the pass does not try it again every time it runs; it
             ;; is not a report, and it is not evidence of anything either.
             (let ((report (photo-focus-of job)))
               (if (orfeus:photo-focus-report-p report)
                   (orfeus:focus-verdict report)
                   :unknown)))
           (draw-burst-chevron (x row-y collapsed-p count)
             ;; The one thing on a row that is not a selection: folding a burst
             ;; open and shut. Drawn over the thumbnail's own corner on a dark
             ;; square, because the row has no spare width and a control that
             ;; sits on the picture is at least unambiguous about which picture
             ;; it belongs to.
             (lightfast:draw-color-rgb :red 26 :green 28 :blue 30)
             (lightfast:draw-filled-rect (+ x 6) (+ row-y 6) 18 18)
             (lightfast:draw-color-rgb :red 150 :green 190 :blue 240)
             (if collapsed-p
                 (loop for offset below 5
                       do (lightfast:draw-line (+ x 12 offset) (+ row-y 10 offset)
                                               (+ x 12 offset) (+ row-y 20 (- offset))))
                 (loop for offset below 5
                       do (lightfast:draw-line (+ x 10 offset) (+ row-y 12 offset)
                                               (+ x 20 (- offset)) (+ row-y 12 offset))))
             (when collapsed-p
               (lightfast:draw-color-rgb :red 150 :green 190 :blue 240)
               (lightfast:draw-text (format nil "~D frames" count)
                                    (+ x 102) (+ row-y 78))))
           (burst-chevron-hit-p (event-x event-y row-y)
             (and event-x event-y
                  (< event-x 26)
                  (>= event-y (+ row-y 4))
                  (< event-y (+ row-y 26))))
           (draw-focus-mark (job x y)
             ;; Only ever drawn for a frame that came back soft. A row saying
             ;; `sharp' beside every photograph is a row of noise: the useful
             ;; information is the exception.
             (when (eq :soft (photo-focus-verdict job))
               (lightfast:draw-stock-icon :focus x (- y 13))
               (lightfast:draw-color-rgb :red 240 :green 176 :blue 84)
               (lightfast:draw-text
                (or (and (orfeus:photo-focus-report-p (photo-focus-of job))
                         (orfeus:focus-description (photo-focus-of job)))
                    "soft")
                (+ x 20) y)))
           (thumbnail-scroll-limit ()
             (if thumbnail-canvas
                 (max 0 (- (* (length (thumbnail-display-rows))
                              (thumbnail-row-height))
                           (lightfast:widget-height thumbnail-canvas)))
                 0))
           (clamp-thumbnail-scroll ()
             (setf thumbnail-scroll
                   (min (thumbnail-scroll-limit) (max 0 thumbnail-scroll))))
           (redraw-thumbnails ()
             (when thumbnail-canvas
               (clamp-thumbnail-scroll)
               (when thumbnail-scrollbar
                 (lightfast:set-range thumbnail-scrollbar
                                    0 (thumbnail-scroll-limit))
                 (setf (lightfast:value thumbnail-scrollbar)
                       (format nil "~D" thumbnail-scroll)))
               (lightfast:redraw thumbnail-canvas)))
           (select-thumbnail-row (row state)
             ;; ROW is a drawn row, which is not a photograph when a burst is
             ;; folded: it stands for all of them. The click arithmetic — plain,
             ;; control, shift — therefore happens in drawn rows, and only the
             ;; answer is translated back into places in the project, which is
             ;; the only space the rest of the interface knows about. Doing it
             ;; the other way round would make shift-clicking across a folded
             ;; burst select its first frame and nothing else.
             (let* ((rows (thumbnail-display-rows))
                    (entry (nth row rows)))
               (when entry
                 (let ((chosen (loop for (indices . nil) in rows
                                     for place from 0
                                     when (subsetp indices
                                                   (gui-model-selected-indices model))
                                       collect place)))
                   (multiple-value-bind (drawn anchor)
                       (thumbnail-selection-after-click
                        chosen row thumbnail-anchor state)
                     (setf (gui-model-marked-p model)
                           (logtest (logior +thumbnail-shift-mask+
                                            +thumbnail-control-mask+)
                                    state))
                     (apply-thumbnail-selection
                      (sort (loop for place in drawn
                                  append (copy-list (car (nth place rows))))
                            #'<)
                      (first (car entry))
                      anchor))))))
           (apply-thumbnail-cursor (focus-index anchor)
             ;; Put the preview on FOCUS-INDEX and leave the selection as the
             ;; photographer built it: everything APPLY-THUMBNAIL-SELECTION
             ;; does for the view, and nothing it does to the set.
             (setf thumbnail-anchor anchor)
             (gui-model-move-cursor model focus-index)
             (setf (gui-model-selected-node model) nil
                   pick-color-node nil)
             (set-preview-cursor :default)
             (clear-previews)
             (sync-controls)
             (sync-node-tools)
             (when graph-canvas (lightfast:redraw graph-canvas))
             (schedule-initial-preview))
           (apply-thumbnail-selection (selection focus-index anchor)
             (setf thumbnail-anchor anchor)
             (gui-model-set-selected-indices model selection)
             (when (member focus-index selection)
               (setf (gui-model-selected-index model) focus-index))
             (setf (gui-model-selected-node model) nil
                   pick-color-node nil)
             (set-preview-cursor :default)
             (clear-previews)
             (sync-controls)
             (sync-node-tools)
             (when graph-canvas (lightfast:redraw graph-canvas))
             (if selection
                 (schedule-initial-preview)
                 (progn
                   (incf preview-generation)
                   (when debounce-id
                     (ignore-errors (lightfast:remove-timeout debounce-id))
                     (setf debounce-id nil))
                   (discard-gui-tasks queue :before)
                   (discard-gui-tasks queue :after)
                   (discard-gui-tasks background-queue :before)
                   (discard-gui-tasks background-queue :after)
                   (discard-gui-tasks histogram-queue :histogram)
                   (release-waveform)
                   (setf after-preview-generation nil
                         curve-histogram nil)))
             (redraw-thumbnails))
           (current-photo-row ()
             ;; The drawn row holding the photograph on view; a folded burst is
             ;; one row for several photographs.
             (let ((index (gui-model-selected-index model)))
               (or (position-if (lambda (entry) (member index (car entry)))
                                (thumbnail-display-rows))
                   0)))
           (move-to-photo-row (row)
             ;; Keyboard navigation lands on a drawn row and brings it into
             ;; view, the way a list does when its cursor is moved. A plain
             ;; selection follows the cursor; one the photographer marked out
             ;; with Space, control or shift stays put while the cursor walks
             ;; past it, so that keepers can be gathered with the arrow keys.
             (let* ((rows (thumbnail-display-rows))
                    (count (length rows)))
               (when (plusp count)
                 (let* ((row (max 0 (min (1- count) row)))
                        (entry (nth row rows))
                        (height (thumbnail-row-height))
                        (visible (if thumbnail-canvas
                                     (lightfast:widget-height thumbnail-canvas)
                                     0)))
                   (if (gui-model-marked-p model)
                       (apply-thumbnail-cursor (first (car entry)) row)
                       (apply-thumbnail-selection (copy-list (car entry))
                                                  (first (car entry)) row))
                   (cond ((< (* row height) thumbnail-scroll)
                          (setf thumbnail-scroll (* row height)))
                         ((and (plusp visible)
                               (> (* (1+ row) height)
                                  (+ thumbnail-scroll visible)))
                          (setf thumbnail-scroll
                                (- (* (1+ row) height) visible))))
                   (redraw-thumbnails)
                   (set-status (format nil "Photo ~D of ~D: ~A"
                                       (1+ row) count
                                       (file-namestring
                                        (photo-job-input-path
                                         (nth (first (car entry))
                                              (project-photos project))))))))))
           (step-photo (delta)
             (move-to-photo-row (+ (current-photo-row) delta)))
           (toggle-photo-mark ()
             ;; Space: the photograph on view joins the selection or leaves
             ;; it again, and the arrow keys then walk on without disturbing
             ;; the set. Culling is arrows and Space, nothing else.
             (let* ((rows (thumbnail-display-rows))
                    (row (current-photo-row))
                    (entry (nth row rows)))
               (when entry
                 (let ((result (gui-model-toggle-mark model
                                                     (copy-list (car entry)))))
                   (setf thumbnail-anchor row)
                   (sync-preset-action-label)
                   (redraw-thumbnails)
                   (set-status
                    (format nil "~:[Taken out of~;Added to~] the selection · ~D selected"
                            (eq result :marked)
                            (length (gui-model-selected-indices model))))))))
           (project-title ()
             (let ((path (gui-model-project-path model)))
               (if path (file-namestring path) "Untitled")))
           (sync-window-title ()
             ;; The document's name and whether it has unsaved edits, where
             ;; every desktop application of the era put them.
             (when window
               (setf (lightfast:label window)
                     (format nil "~A~:[~;*~] - Orfeus"
                             (project-title)
                             (gui-model-modified-p model)))))
           (confirm-discard (verb)
             ;; True when it is fine to go ahead: nothing is unsaved, or the
             ;; user chose to save it or to let it go.
             (or (not (gui-model-modified-p model))
                 (case (lightfast:choice-box
                        (format nil "Save changes to ~A before ~A?"
                                (project-title) verb)
                        :button0 "Cancel" :button1 "Don't Save" :button2 "Save")
                   (1 t)
                   (2 (and (save-project) t))
                   (t nil))))
           (quit-application ()
             (when (confirm-discard "quitting")
               (lightfast:quit)))
           (select-all-thumbnails ()
             (let ((indices (loop for index below (length (project-photos project))
                                  collect index))
                   (current (gui-model-selected-index model)))
               (gui-model-set-selected-indices model indices)
               (setf (gui-model-marked-p model) (and indices t))
               (when indices
                 (setf (gui-model-selected-index model)
                       (min current (1- (length indices)))
                       thumbnail-anchor (gui-model-selected-index model)))
               (sync-controls)
               (redraw-thumbnails)
               (set-status (format nil "Selected ~D photograph~:P"
                                   (length indices)))))
           (copy-selected-photo-paths ()
             (let ((jobs (gui-model-selected-jobs model)))
               (if jobs
                   (progn
                     (lightfast:copy-text
                      (format nil "~{~A~^~%~}"
                              (mapcar (lambda (job)
                                        (namestring (photo-job-input-path job)))
                                      jobs)))
                     (set-status (format nil "Copied ~D source path~:P"
                                         (length jobs))))
                   (set-status "No photographs selected"))))
           (step-history (direction)
             ;; Undo and redo restore the whole project, so every view that
             ;; reads it has to be told, not just the one the edit came from.
             (if (funcall (ecase direction
                            (:undo #'gui-model-undo)
                            (:redo #'gui-model-redo))
                          model)
                 (progn
                   (sync-controls)
                   (sync-node-tools)
                   (sync-export-controls)
                   (when graph-canvas (lightfast:redraw graph-canvas))
                   (refresh-gallery)
                   (redraw-thumbnails)
                   (schedule-edited-preview)
                   (set-status (ecase direction
                                 (:undo "Undid one edit")
                                 (:redo "Redid one edit"))))
                 (set-status (ecase direction
                               (:undo "Nothing left to undo")
                               (:redo "Nothing left to redo")))))
           (reset-selected-photo-edits ()
             (let ((count (length (gui-model-acting-jobs model))))
               (if (plusp count)
                   (progn
                     (gui-model-reset-selected model)
                     (sync-controls)
                     (sync-node-tools)
                     (when graph-canvas (lightfast:redraw graph-canvas))
                     (redraw-thumbnails)
                     (schedule-edited-preview)
                     (set-status (format nil "Removed edits from ~D photograph~:P"
                                         count)))
                   (set-status "No photographs selected"))))
           (remove-selected-photo-with-confirmation ()
             (let ((count (length (gui-model-selected-jobs model))))
               (when (and (plusp count)
                          (= 1 (lightfast:choice-box
                                (format nil
                                        "Remove ~D selected photograph~:P from this project?"
                                        count)
                                :button0 "Cancel" :button1 "Remove")))
                 (remove-selected-photo))))
           (photo-interned-cached-p (job)
             ;; PHOTO-INTERNED-P consults the filesystem, and the filmstrip asks
             ;; for every visible row on every repaint. Interning is the only
             ;; thing that changes the answer, and it clears this.
             (let* ((path (photo-job-input-path job))
                    (known (gethash path interned-photos :unknown)))
               (if (eq known :unknown)
                   (setf (gethash path interned-photos)
                         (and (ignore-errors (orfeus:photo-interned-p path)) t))
                   known)))
           (intern-selected-photos ()
             ;; Copy each selected RAW into the private store and repoint the
             ;; project at the copy, so the card is only needed once.
             (let ((jobs (remove-if #'photo-interned-cached-p
                                    (gui-model-acting-jobs model))))
               (cond
                 ((null jobs)
                  (set-status "Every selected photograph is already interned"))
                 (t
                  (gui-model-checkpoint model)
                  (let ((interned 0) (failed 0) (last-error nil))
                    (dolist (job jobs)
                      (handler-case
                          (let ((was (photo-job-input-path job)))
                            (orfeus:intern-photo-job job)
                            (remhash was interned-photos)
                            ;; Nothing else is invalidated: the copy holds the
                            ;; same bytes, so it has the same content key, and
                            ;; every preview and thumbnail already rendered for
                            ;; this photograph stays valid. Discarding them here
                            ;; made interning redevelop both.
                            (incf interned))
                        (error (condition)
                          (incf failed)
                          (setf last-error condition)
                          (format *error-output*
                                  "~&orfeus: interning ~A failed: ~A~%"
                                  (photo-job-input-path job) condition))))
                    (clrhash interned-photos)
                    (redraw-thumbnails)
                    (set-status
                     (if (plusp failed)
                         (format nil "Interned ~D; ~D failed: ~A"
                                 interned failed last-error)
                         (format nil "Interned ~D photograph~:P into ~A"
                                 interned
                                 (namestring (orfeus:interned-raw-directory))))))))))
           (select-thumbnail-context-row (indices)
             ;; Right-clicking targets a photograph without opening it: the menu
             ;; acts on the row under the cursor while the preview keeps showing
             ;; whatever it already had. Opening one is a left-click.
             ;;
             ;; A folded burst is one row standing for several photographs, so
             ;; the menu acts on all of them — which is the point of folding it.
             (let ((selection
                     (if (subsetp indices (gui-model-selected-indices model))
                         (gui-model-selected-indices model)
                         indices)))
               (unless (equal selection (gui-model-selected-indices model))
                 (let ((anchor (gui-model-selected-index model)))
                   (gui-model-set-selected-indices model selection)
                   ;; SET-SELECTED-INDICES moves the anchor to the first row,
                   ;; which is what would switch the view.
                   (setf (gui-model-selected-index model) anchor))
                 (sync-preset-action-label)
                 (redraw-thumbnails))))
           (show-thumbnail-context-menu (row)
             (let ((entry (nth row (thumbnail-display-rows))))
               (when entry
                 (select-thumbnail-context-row (car entry))
               (case (thumbnail-context-action-at
                      (lightfast:popup-menu (thumbnail-context-menu-items)))
                 (:export (open-export-dialog "Selected photographs"))
                 (:apply-still (apply-current-preset))
                 (:reset-edits (reset-selected-photo-edits))
                 (:copy-paths (copy-selected-photo-paths))
                 (:select-all (select-all-thumbnails))
                 (:intern (intern-selected-photos))
                 (:remove (remove-selected-photo-with-confirmation))))))
           (handle-thumbnail-mouse (canvas event value)
             (multiple-value-bind (x y button dx dy state)
                 (parse-preview-event value)
               (declare (ignore dx))
               (when y
                 (case event
                   (#.lightfast:+event-push+
                    (let* ((row-height (thumbnail-row-height))
                           (row (thumbnail-row-at y thumbnail-scroll row-height))
                           (row-y (- (* row row-height) thumbnail-scroll))
                           (entry (nth row (thumbnail-display-rows)))
                           (photos (project-photos project))
                           (job (and entry (nth (first (car entry)) photos)))
                           (leader (and job
                                        (or (cdr entry)
                                            (and (burst-leader-p job) job)))))
                      (cond
                        ((= button 3) (show-thumbnail-context-menu row))
                        ((and leader (burst-chevron-hit-p x y row-y))
                         (toggle-burst leader))
                        (t
                         (select-thumbnail-row
                          row
                          (if (thumbnail-toggle-hit-p
                               x (lightfast:widget-width canvas))
                              (logior state +thumbnail-control-mask+)
                              state))))))
                   (#.lightfast:+event-wheel+
                    (incf thumbnail-scroll (* dy 36))
                    (redraw-thumbnails))))))
           (discard-previews ()
             ;; Everything earlier renders left on disk, then everything held
             ;; in memory, then a fresh render of what is on screen. Wanted
             ;; whenever a render changes without the settings changing — a new
             ;; build of Orfeus, most often — because a cached preview is keyed
             ;; by the photograph and the grade, neither of which moved.
             (let ((removed (discard-cached-previews preview-directory)))
               (clear-previews)
               (clear-preview-cache)
               (clrhash thumbnail-files)
               (clrhash thumbnail-grades)
               (clrhash gallery-thumbs)
               (clrhash photo-groups)
               (refresh-gallery)
               (schedule-initial-preview)
               (redraw-thumbnails)
               (set-status
                (format nil "Discarded ~D cached preview~:P; rebuilding" removed))))
           (clear-previews ()
             (setf preview-zoom 1d0
                   preview-center-x .5d0
                   preview-center-y .5d0
                   preview-drag-p nil
                   after-live-p nil
                   ;; A new photograph starts fitted, so it starts on a proxy.
                   preview-native-p nil
                   preview-one-to-one-pending-p nil)
             (dolist (path (list before-preview-file after-preview-file))
               (when path (forget-preview-file path)))
             (setf before-preview-file nil
                   after-preview-file nil
                   before-preview-region nil
                   after-preview-region nil
                   requested-viewport-region nil)
             (when before-canvas (lightfast:redraw before-canvas))
             (when after-canvas (lightfast:redraw after-canvas)))
           (published-region (path region bound)
             ;; Trust the file over the request. A renderer that quietly ignored
             ;; the viewport would hand back a whole frame while the interface
             ;; believed it held a window, and the frame would then be drawn at
             ;; the scale the window would have needed — which is how a zoomed
             ;; view was seen shrinking by two and a half times. A file as wide
             ;; as the whole bound is not a window, whatever was asked for.
             (multiple-value-bind (file-width file-height) (preview-file-size path)
               (cond ((null region) nil)
                     ;; A render with no bound — the one-to-one view asks the
                     ;; bridge for sensor resolution — leaves nothing to check
                     ;; the file against.
                     ((or (null bound) (zerop bound)) region)
                     ((null file-width) nil)
                     ((< (max file-width file-height) (* 0.98 bound)) region)
                     (t nil))))
           (publish-preview (role path generation region geometry)
             (let ((old-path (ecase role
                               (:before before-preview-file)
                               (:after after-preview-file))))
               (when (and old-path (not (equal old-path path)))
                 (forget-preview-file old-path))
               (forget-preview-file path)
               (ecase role
                 (:before (setf before-preview-region region)
                          (setf before-preview-file path)
                          (lightfast:redraw before-canvas))
                 (:after (setf after-preview-region region
                               after-preview-geometry geometry
                               after-preview-file path
                               after-preview-generation generation
                               after-live-p nil
                               (gethash (selected-job) thumbnail-files) path)
                         (when preview-one-to-one-pending-p
                           (setf preview-one-to-one-pending-p nil)
                           (fit-preview-to-source-pixels))
                         (schedule-curve-histogram path generation)
                         (lightfast:redraw after-canvas)
                         (when curve-canvas
                           (lightfast:redraw curve-canvas))
                         (redraw-thumbnails)))))
           (sync-export-controls ()
             (when export-quality
               (let ((settings (project-export-settings project)))
                 (setf (lightfast:value export-quality)
                       (format nil "~D" (export-settings-jpeg-quality settings))
                       (lightfast:value export-max-width)
                       (format nil "~D" (or (export-settings-max-width settings) 0))
                       (lightfast:value export-max-height)
                       (format nil "~D" (or (export-settings-max-height settings) 0))
                       (lightfast:value export-metadata)
                       (if (export-settings-preserve-metadata-p settings) "1" "0")
                       (lightfast:value export-timestamp)
                       (if (export-settings-timestamp-filenames-p settings)
                           "1" "0")))))
           (export-setting-changed (key widget)
             (handler-case
                 (let ((settings (project-export-settings project)))
                   (ecase key
                     (:jpeg-quality
                      (setf (export-settings-jpeg-quality settings)
                            (parse-export-integer-value
                             (lightfast:value widget) "JPEG quality" 92 1 100)))
                     (:max-width
                      (let ((value (parse-export-integer-value
                                    (lightfast:value widget)
                                    "Maximum width" 0 0 100000)))
                        (setf (export-settings-max-width settings)
                              (unless (zerop value) value))))
                     (:max-height
                      (let ((value (parse-export-integer-value
                                    (lightfast:value widget)
                                    "Maximum height" 0 0 100000)))
                        (setf (export-settings-max-height settings)
                              (unless (zerop value) value))))
                     (:preserve-metadata-p
                      (setf (export-settings-preserve-metadata-p settings)
                            (gui-boolean-value (lightfast:value widget))))
                     (:timestamp-filenames-p
                      (setf (export-settings-timestamp-filenames-p settings)
                            (gui-boolean-value (lightfast:value widget)))))
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
                     (when lut-choice (lightfast:add-item lut-choice name))
                     name))))
           (selected-photo-count ()
             (length (or (gui-model-selected-indices model)
                         (and (selected-job) '(0)))))
           (editor-nodes ()
             (let ((graph (gui-model-display-graph model)))
               (if graph (orfeus:processing-graph-nodes graph) '())))
           (node-in-display-domain-p (node)
             ;; Where the node sits, not merely what kind it is: a crop is
             ;; scene-linear above the film transform and display-space below
             ;; it, and showing the position is what makes the boundary legible.
             (let ((graph (gui-model-display-graph model)))
               (and graph
                    (orfeus:graph-display-domain-p
                     graph (orfeus:graph-node-id node)))))
           (crop-editing-node ()
             (let ((node (gui-model-selected-graph-node model)))
               (and node (eq :crop (orfeus:graph-node-kind node)) node)))
           (sync-node-tools ()
             ;; The Node panel follows the selection: the correction picker
             ;; plus only the active kind's controls, DaVinci style.
             (let* ((node (gui-model-selected-graph-node model))
                    (kind (and node (orfeus:graph-node-kind node))))
               (when kind-choice
                 (setf (lightfast:value kind-choice)
                       (or (first (find kind *node-kind-choices*
                                        :key #'rest))
                           "None")))
               (dolist (entry node-panel-groups)
                 (let ((show (case (first entry)
                               (:picker (and node t))
                               (:none (null node))
                               (otherwise (eq (first entry) kind)))))
                   (dolist (widget (rest entry))
                     (if show
                         (lightfast:show widget)
                         (lightfast:hide widget)))))
               (when (and node blend-opacity-input
                          (orfeus:graph-node-blend-p node))
                 (setf (lightfast:value blend-opacity-input)
                       (format nil "~,2F"
                               (orfeus:graph-node-opacity node))))
               (when (and node (eq kind :crop))
                 (let ((angle (getf (orfeus:graph-node-params node) :angle 0.0)))
                   (dolist (entry crop-angle-controls)
                     (setf (lightfast:value (second entry))
                           (format nil "~,1F" angle))))
                 (multiple-value-bind (left top width height)
                     (crop-node-rect node)
                   (declare (ignore left top))
                   (when crop-width-input
                     (setf (lightfast:value crop-width-input)
                           (format nil "~,1F" (* 100 width))))
                   (when crop-height-input
                     (setf (lightfast:value crop-height-input)
                           (format nil "~,1F" (* 100 height)))))
                 (when crop-aspect-input
                   (setf (lightfast:value crop-aspect-input)
                         (or (crop-aspect-label crop-aspect) "Free"))))
               (when (and node (member kind '(:contrast :negative)))
                 (let ((params (orfeus:graph-node-params node)))
                   (dolist (entry node-param-controls)
                     (destructuring-bind (key widget default) entry
                       (setf (lightfast:value widget)
                             (format nil "~,3F" (getf params key default)))))))
               (when (and node (eq kind :flip) flip-axis-input)
                 (let ((params (orfeus:graph-node-params node)))
                   (setf (lightfast:value flip-axis-input)
                         (flip-choice-label (getf params :horizontal)
                                            (getf params :vertical)))))
               (when (and node (eq kind :rotate) rotate-turn-input)
                 (setf (lightfast:value rotate-turn-input)
                       (quarter-turn-label
                        (getf (orfeus:graph-node-params node)
                              :quarter-turns 0))))
               (when (and node (member kind '(:color-subtract :negative)))
                 (multiple-value-bind (inputs swatch) (base-controls kind)
                   (let ((params (orfeus:graph-node-params node)))
                     (loop for (key input) in inputs
                           do (when input
                                (setf (lightfast:value input)
                                      (format nil "~,1F"
                                              (* 100 (srgb-encode-component
                                                      (getf params key
                                                            (base-default kind)))))))))
                   (sync-base-swatch node swatch)))))
           (graph-node-box (node)
             (let ((place (orfeus:graph-node-position node)))
               (values (round (first place)) (round (second place))
                       *graph-node-width* *graph-node-height*)))
           (graph-output-box (nodes)
             (graph-output-box-position nodes))
           (graph-editor-hit (gx gy)
             ;; What lies at graph coordinates: a node body, its output
             ;; port, a blend's branch port, the source, or the output box.
             (let ((nodes (ensure-graph-node-positions (editor-nodes))))
               (loop for node in nodes
                     for index from 0
                     do (multiple-value-bind (x y w h) (graph-node-box node)
                          (let ((port-x (+ x (floor w 2)))
                                (port-y (+ y h)))
                            (when (and (<= (abs (- gx port-x)) 9)
                                       (<= (abs (- gy port-y)) 7))
                              (return-from graph-editor-hit
                                (values :output-port index))))
                          (when (orfeus:graph-node-blend-p node)
                            (let ((port-x (+ x w))
                                  (port-y (+ y (floor h 2))))
                              (when (and (<= (abs (- gx port-x)) 8)
                                         (<= (abs (- gy port-y)) 9))
                                (return-from graph-editor-hit
                                  (values :branch-port index)))))
                          (when (and (<= x gx (+ x w)) (<= y gy (+ y h)))
                            (return-from graph-editor-hit
                              (values :node index)))))
               (when (and (<= 18 gx (+ 18 *graph-node-width*))
                          (<= 6 gy (+ 6 *graph-well-height*)))
                 (return-from graph-editor-hit
                   (values (if (>= gy (+ 2 *graph-well-height*))
                               :source-port
                               :source)
                           nil)))
               (multiple-value-bind (x y w h) (graph-output-box nodes)
                 (when (and (<= x gx (+ x w)) (<= y gy (+ y h)))
                   (return-from graph-editor-hit (values :output nil))))
               (values nil nil)))
           (draw-editor-box (x y w h label style selected &optional (indent 0))
             ;; Body colour carries the pipeline order the graph enforces, in
             ;; three tiers: green optics, which must precede every crop; cool
             ;; scene-linear corrections; and warm display space, from the film
             ;; transform down. GRAPH-NODE-BODY-STYLE explains why each tier
             ;; exists. Showing them puts the ordering rules on screen instead
             ;; of leaving them to be discovered by a refused edit.
             ;;
             ;; :terminal is the darker RAW and OUT wells; :blend keeps its own
             ;; stronger blue, a saturated member of the same cool family, since
             ;; a blend is always scene-linear and already carries A and B marks.
             (ecase style
               (:terminal (lightfast:draw-color-rgb :red 78 :green 80 :blue 86))
               (:bypassed (lightfast:draw-color-rgb :red 148 :green 148
                                                  :blue 148))
               (:blend (lightfast:draw-color-rgb :red 176 :green 190 :blue 224))
               (:optics (lightfast:draw-color-rgb :red 182 :green 212
                                                :blue 188))
               (:display (lightfast:draw-color-rgb :red 226 :green 205
                                                 :blue 168))
               (:linear (lightfast:draw-color-rgb :red 196 :green 205
                                                :blue 216)))
             (lightfast:draw-filled-rect x y w h)
             (lightfast:draw-color-rgb :red 240 :green 240 :blue 244)
             (lightfast:draw-filled-rect x y w 1)
             (lightfast:draw-filled-rect x y 1 h)
             (lightfast:draw-color-rgb :red 70 :green 70 :blue 74)
             (lightfast:draw-filled-rect x (+ y h -1) w 1)
             (lightfast:draw-filled-rect (+ x w -1) y 1 h)
             (when selected
               (lightfast:draw-color-rgb :red 40 :green 110 :blue 235)
               (lightfast:draw-rect (1- x) (1- y) (+ w 2) (+ h 2))
               (lightfast:draw-rect x y w h)
               (lightfast:draw-rect (1+ x) (1+ y) (- w 2) (- h 2)))
             (if (eq style :terminal)
                 (lightfast:draw-color-rgb :red 225 :green 225 :blue 230)
                 (lightfast:draw-color-rgb :red 10 :green 10 :blue 12))
             (lightfast:draw-font :size 12)
             ;; Two below the arithmetic centre: the caption's capitals stand
             ;; on the baseline and reach nine up, so centring the baseline's
             ;; offset left them sitting a pixel and a half above the icon.
             (lightfast:draw-text label (+ x 10 indent) (+ y (floor h 2) 7))
             (lightfast:draw-font :size 12))
           (draw-graph-wire (fx fy tx ty red green blue)
             ;; Wires leave the bottom of a node and enter the top of the
             ;; next, routed as two verticals joined by a horizontal so
             ;; parallel branches never overlap, with an arrowhead at the
             ;; input. Each segment is drawn twice for a two-pixel wire.
             (let ((mid (if (< fy ty)
                            (floor (+ fy ty) 2)
                            (+ fy 16))))
               (lightfast:draw-color-rgb :red red :green green :blue blue)
               (dotimes (pass 2)
                 (lightfast:draw-line (+ fx pass) fy (+ fx pass) mid)
                 (lightfast:draw-line fx (+ mid pass) tx (+ mid pass))
                 (lightfast:draw-line (+ tx pass) mid (+ tx pass) ty))
               (dotimes (pass 2)
                 (lightfast:draw-line (- tx 4) (- ty 5 pass) tx (- ty pass))
                 (lightfast:draw-line (+ tx 4) (- ty 5 pass) tx (- ty pass)))))
           (draw-graph-editor (widget)
             (let* ((wx (lightfast:widget-x widget))
                    (wy (lightfast:widget-y widget))
                    (width (lightfast:widget-width widget))
                    (height (lightfast:widget-height widget))
                    (nodes (ensure-graph-node-positions (editor-nodes)))
                    (selected (gui-model-selected-graph-node model))
                    (ox (- wx graph-scroll-x))
                    (oy (- wy (synced-graph-scroll-y height))))
               (lightfast:draw-push-clip wx wy width height)
               (lightfast:draw-color-rgb :red 52 :green 54 :blue 58)
               (lightfast:draw-filled-rect wx wy width height)
               ;; A quiet dot grid anchored to graph space.
               (lightfast:draw-color-rgb :red 66 :green 68 :blue 72)
               (loop for gy from (* 24 (ceiling graph-scroll-y 24))
                       below (+ graph-scroll-y height) by 24
                     do (loop for gx from (* 24 (ceiling graph-scroll-x 24))
                                below (+ graph-scroll-x width) by 24
                              do (lightfast:draw-filled-rect (+ ox gx)
                                                           (+ oy gy) 1 1)))
               (when (null nodes)
                 (lightfast:draw-color-rgb :red 168 :green 170 :blue 174)
                 (lightfast:draw-font :size 11)
                 (lightfast:draw-text
                  (if (selected-job)
                      "Right-click for a New Node"
                      "Open a photograph to grade")
                  (+ wx 14) (+ wy 52))
                 (lightfast:draw-font :size 12))
               (flet ((node-center-top (node)
                        (multiple-value-bind (x y w h) (graph-node-box node)
                          (declare (ignore h))
                          (values (+ ox x (floor w 2)) (+ oy y))))
                      (node-center-bottom (node)
                        (multiple-value-bind (x y w h) (graph-node-box node)
                          (values (+ ox x (floor w 2)) (+ oy y h))))
                      (node-right-middle (node)
                        (multiple-value-bind (x y w h) (graph-node-box node)
                          (values (+ ox x w) (+ oy y (floor h 2)))))
                      (source-bottom ()
                        (values (+ ox 18 (floor *graph-node-width* 2))
                                (+ oy 6 *graph-well-height*))))
                 ;; Wires first, boxes on top.
                 (dolist (node nodes)
                   (let ((inputs (orfeus:graph-node-inputs node)))
                     (multiple-value-bind (tx ty) (node-center-top node)
                       (multiple-value-bind (fx fy)
                           (let ((from (first inputs)))
                             (if (eql from 0)
                                 (source-bottom)
                                 (node-center-bottom
                                  (find from nodes
                                        :key #'orfeus:graph-node-id))))
                         (when fx
                           (draw-graph-wire fx fy tx ty 150 152 158))))
                     ;; A blend forks: its B branch runs down its own column
                     ;; and turns in through the right-hand port.
                     (when (orfeus:graph-node-blend-p node)
                       (multiple-value-bind (tx ty) (node-right-middle node)
                         (multiple-value-bind (fx fy)
                             (let ((from (second inputs)))
                               (if (eql from 0)
                                   (source-bottom)
                                   (node-center-bottom
                                    (find from nodes
                                          :key #'orfeus:graph-node-id))))
                           (when fx
                             (let ((turn (+ tx 24))
                                   (drop (+ fy 14)))
                               (lightfast:draw-color-rgb :red 96 :green 148
                                                       :blue 235)
                               (dotimes (pass 2)
                                 (lightfast:draw-line (+ fx pass) fy
                                                    (+ fx pass) drop)
                                 (lightfast:draw-line fx (+ drop pass)
                                                    turn (+ drop pass))
                                 (lightfast:draw-line (+ turn pass) drop
                                                    (+ turn pass) ty)
                                 (lightfast:draw-line turn (+ ty pass)
                                                    tx (+ ty pass)))
                               (dotimes (pass 2)
                                 (lightfast:draw-line (+ tx 5) (- ty 4 pass)
                                                    (+ tx pass) ty)
                                 (lightfast:draw-line (+ tx 5) (+ ty 4 pass)
                                                    (+ tx pass) ty)))))))))
                 ;; Output wire down to the OUT well.
                 (multiple-value-bind (out-x out-y out-w out-h)
                     (graph-output-box nodes)
                   (declare (ignore out-h))
                   (let* ((graph (gui-model-display-graph model))
                          (output-id (and graph
                                          (orfeus:processing-graph-output
                                           graph)))
                          (output-node
                            (and output-id
                                 (find output-id nodes
                                       :key #'orfeus:graph-node-id))))
                     (multiple-value-bind (fx fy)
                         (if output-node
                             (node-center-bottom output-node)
                             (and graph (source-bottom)))
                       (when fx
                         (draw-graph-wire fx fy
                                          (+ ox out-x (floor out-w 2))
                                          (+ oy out-y)
                                          150 152 158))))
                   ;; Terminal wells.
                   (when (selected-job)
                     (draw-editor-box (+ ox 18) (+ oy 6)
                                      *graph-node-width* *graph-well-height*
                                      "RAW" :terminal nil 16)
                     (lightfast:draw-stock-icon
                      :photo (+ ox 24)
                      (+ oy 6 (floor (- *graph-well-height* 16) 2)))
                     (draw-editor-box (+ ox out-x) (+ oy out-y)
                                      out-w *graph-well-height*
                                      "OUT" :terminal nil 16)
                     (lightfast:draw-stock-icon
                      :export (+ ox out-x 6)
                      (+ oy out-y (floor (- *graph-well-height* 16) 2)))))
                 ;; Node boxes.
                 (dolist (node nodes)
                   (multiple-value-bind (x y w h) (graph-node-box node)
                     (let* ((bx (+ ox x)) (by (+ oy y))
                            (bypassed (orfeus:graph-node-bypassed-p node))
                            (blend-p (orfeus:graph-node-blend-p node)))
                       ;; The caption steps aside for the glyph rather than
                       ;; sharing its pixels with it.
                       (draw-editor-box bx by w h
                                        (node-kind-label
                                         (orfeus:graph-node-kind node))
                                        (graph-node-body-style
                                         node
                                         (node-in-display-domain-p node))
                                        (eq node selected)
                                        16)
                       (if (graph-node-active-p node)
                           (lightfast:draw-color-rgb :red 40 :green 150
                                                   :blue 40)
                           (lightfast:draw-color-rgb :red 150 :green 150
                                                   :blue 150))
                       (lightfast:draw-filled-rect (+ bx w -13) (+ by 5) 8 8)
                       ;; The kind's own shape, in the corner opposite the
                       ;; activity light. Dimmed when the node is bypassed,
                       ;; because a bypassed node is still there and still
                       ;; wants identifying.
                       (draw-node-icon (orfeus:graph-node-kind node) bypassed
                                       (+ bx 6) (+ by (floor (- h 16) 2)))
                       ;; The input and output port nubs.
                       (lightfast:draw-color-rgb :red 235 :green 235 :blue 240)
                       (lightfast:draw-filled-rect (+ bx (floor w 2) -4)
                                                 (+ by h -3) 9 6)
                       (lightfast:draw-filled-rect (+ bx (floor w 2) -4)
                                                 (- by 3) 9 6)
                       (when blend-p
                         ;; The fork made visible: an A branch arriving from
                         ;; above and a B branch from the right, merging into
                         ;; the single output below.
                         (lightfast:draw-color-rgb :red 96 :green 148 :blue 235)
                         (lightfast:draw-filled-rect (+ bx w -4)
                                                   (+ by (floor h 2) -4)
                                                   7 9)
                         (lightfast:draw-font :size 9)
                         (lightfast:draw-color-rgb :red 30 :green 60 :blue 130)
                         (lightfast:draw-text "A" (+ bx (floor w 2) 7)
                                            (+ by 10))
                         (lightfast:draw-text "B" (+ bx w -14)
                                            (+ by (floor h 2) 12))
                         (lightfast:draw-font :size 12)
                         (let ((cx (+ bx (floor w 2)))
                               (cy (+ by (floor h 2))))
                           (lightfast:draw-line cx by cx cy)
                           (lightfast:draw-line (+ bx w -6) cy cx cy)
                           (lightfast:draw-line cx cy cx (+ by h))))
                       (when bypassed
                         (lightfast:draw-color-rgb :red 165 :green 40 :blue 40)
                         (lightfast:draw-line bx (+ by h -1)
                                            (+ bx w -1) by)))))
                 ;; Source port nub.
                 (when (selected-job)
                   (lightfast:draw-color-rgb :red 235 :green 235 :blue 240)
                   (multiple-value-bind (sx sy) (source-bottom)
                     (lightfast:draw-filled-rect (- sx 3) (- sy 3) 7 6)))
                 ;; Wire rubber band.
                 (when (and node-drag (eq (first node-drag) :wire)
                            (getf (rest node-drag) :moved-p))
                   (let ((from (getf (rest node-drag) :from)))
                     (multiple-value-bind (fx fy)
                         (if (eq from :source)
                             (source-bottom)
                             (let ((node (nth from nodes)))
                               (and node (node-center-bottom node))))
                       (when fx
                         (lightfast:draw-color-rgb :red 0 :green 0 :blue 128)
                         (lightfast:draw-line
                          fx fy
                          (+ ox (getf (rest node-drag) :x))
                          (+ oy (getf (rest node-drag) :y))))))))
               (lightfast:draw-pop-clip)))
           (after-graph-edit (message)
             (sync-controls)
             (sync-node-tools)
             (when graph-canvas (lightfast:redraw graph-canvas))
             (when curve-canvas (lightfast:redraw curve-canvas))
             (redraw-previews)
             (schedule-edited-preview)
             (when message (set-status message)))
           (select-graph-node (index)
             ;; Selecting a node upgrades flat photos to graph grading so the
             ;; selection can drive the Node panel and later edits. Entering
             ;; or leaving crop editing changes the preview recipe.
             (when (selected-job)
               (let* ((node (graph-node-for-edit model index))
                      (was-cropping (crop-editing-node)))
                 (when node
                   (setf (gui-model-selected-node model) node)
                   (unless (eq was-cropping (crop-editing-node))
                     (schedule-edited-preview))
                   (sync-controls)
                   (sync-node-tools)
                   (when graph-canvas (lightfast:redraw graph-canvas))
                   (when curve-canvas (lightfast:redraw curve-canvas))
                   (set-status (format nil "Node ~D: ~A~@[ (bypassed)~]"
                                       (orfeus:graph-node-id node)
                                       (node-kind-label
                                        (orfeus:graph-node-kind node))
                                       (orfeus:graph-node-bypassed-p node)))
                   node))))
           (deselect-graph-node ()
             (when (gui-model-selected-graph-node model)
               (let ((was-cropping (crop-editing-node)))
                 (setf (gui-model-selected-node model) nil)
                 (sync-controls)
                 (sync-node-tools)
                 (when graph-canvas (lightfast:redraw graph-canvas))
                 (when was-cropping
                   (schedule-edited-preview))
                 (set-status "Node deselected"))))
           (graph-node-menu-actions (node)
             (let ((kind (orfeus:graph-node-kind node)))
               (append
                (list (cons (if (orfeus:graph-node-bypassed-p node)
                                "Enable Node"
                                "Bypass Node")
                            (lambda ()
                              (let ((state (gui-model-toggle-node model node)))
                                (after-graph-edit
                                 (format nil "~A ~A"
                                         (if (eq state :bypassed)
                                             "Bypassed"
                                             "Enabled")
                                         (node-kind-label kind))))))
                      (cons "Move Earlier" (move-node-action node :earlier))
                      (cons "Move Later" (move-node-action node :later)))
                (when (eq kind :crop)
                  (list (cons "-" nil)
                        (cons "Level From Camera"
                              (lambda () (level-crop-from-camera node)))
                        (cons "Level From Photo"
                              (lambda () (level-crop-from-photo node)))
                        (cons "Autocrop Negative"
                              (lambda () (autocrop-negative node)))
                        (cons "Reset Crop"
                              (lambda ()
                                (gui-model-set-node-params
                                 model node (default-crop-params))
                                (setf crop-aspect :original)
                                (after-graph-edit "Crop reset")))))
                (when (member kind '(:color-subtract :negative))
                  (list (cons "-" nil)
                        (cons "Sample Base From Photo"
                              (lambda ()
                                (setf pick-color-node node)
                                (set-preview-cursor :cross)
                                (set-status
                                 "Click the preview to sample the film base")))
                        (cons "Auto Base From Border"
                              (lambda () (auto-base-from-border node)))))
                (list (cons "-" nil)
                      (cons "Delete Node"
                            (lambda ()
                              (gui-model-delete-node model node)
                              (after-graph-edit "Node deleted")))))))
           (move-node-action (node direction)
             (lambda ()
               (multiple-value-bind (moved reason)
                   (gui-model-move-node model node direction)
                 (if moved
                     (after-graph-edit (format nil "Node moved ~(~A~)"
                                               direction))
                     ;; The validator's reason when there is one: a crop that
                     ;; will not move up is being stopped by an optics node,
                     ;; which cannot resample a branch that has been cropped.
                     (set-status
                      (if reason
                          (format nil "Cannot move ~(~A~): ~A" direction reason)
                          (format nil "This node cannot move ~(~A~)"
                                  direction)))))))
           (insertable-kinds-after (after-node)
             ;; Only the kinds that would actually be well formed in that spot.
             ;; Below a film node that is Crop and Film alone, and below a crop
             ;; it excludes Optics, which may not read a cropped branch. Offering
             ;; the rest there and quietly relocating them would make the menu
             ;; lie about where a click puts things.
             (let ((graph (gui-model-display-graph model)))
               (if (null graph)
                   (orfeus:graph-node-kinds)
                   (orfeus:graph-insertable-kinds
                    graph
                    (if after-node
                        (orfeus:graph-node-id after-node)
                        (orfeus:graph-tail-linear-node-id graph))))))
           (add-node-menu-actions (after-node)
             ;; The Resolve flow first: New Node makes an untyped container
             ;; the Node panel then assigns a correction to. The direct
             ;; per-kind entries remain as shortcuts.
             (let ((kinds (insertable-kinds-after after-node)))
               (append
                (list (cons "New Node"
                            (lambda ()
                              (handler-case
                                  (progn
                                    (gui-model-add-node
                                     model :node
                                     :after (and after-node
                                                 (orfeus:graph-node-id
                                                  after-node)))
                                    (after-graph-edit
                                     "New node: pick a correction type"))
                                (error (condition)
                                  (set-status
                                   (princ-to-string condition))))))
                      (cons "-" nil))
                (mapcar
                 (lambda (kind)
                   (cons (format nil "Add ~A" (node-kind-full-name kind))
                         (lambda ()
                           (handler-case
                               (let* ((node (gui-model-add-node
                                             model kind
                                             :after (and after-node
                                                         (orfeus:graph-node-id
                                                          after-node))))
                                      (angle (getf (orfeus:graph-node-params node)
                                                   :angle 0.0)))
                                 (after-graph-edit
                                  (if (and (eq kind :crop) (/= angle 0.0))
                                      ;; A crop that started level says so:
                                      ;; the picture just turned by itself.
                                      (format nil "Added Crop node, levelled from the camera: ~,1F deg"
                                              angle)
                                      (format nil "Added ~A node"
                                              (node-kind-label kind)))))
                             (error (condition)
                               (set-status (princ-to-string condition)))))))
                 kinds)
                ;; Say why the list is short rather than leaving the missing
                ;; entries to be puzzled over. The label carries no action, and
                ;; SHOW-NODE-MENU ignores entries that have none.
                (when (< (length kinds) (length (orfeus:graph-node-kinds)))
                  (list (cons "-" nil)
                        (cons (omitted-kinds-reason after-node) nil))))))
           (omitted-kinds-reason (after-node)
             ;; Two rules narrow the list, and they read very differently: below
             ;; a film node almost nothing scene-linear is legal, while below a
             ;; crop it is only optics and blends that drop out.
             (let* ((graph (gui-model-display-graph model))
                    (id (and after-node (orfeus:graph-node-id after-node))))
               (if (and graph id (orfeus:graph-display-domain-p graph id))
                   "grades go above the film transform"
                   "optics and blends go above a crop")))
           (show-node-menu (actions)
             ;; Every entry is guarded here rather than one at a time. A graph
             ;; edit that the validator refuses signals, and an unhandled signal
             ;; out of an FLTK callback takes the whole application down — moving
             ;; a crop above an optics node did exactly that, because optics may
             ;; not read a cropped branch. Guarding the one place that invokes
             ;; these means an entry added later cannot reintroduce it.
             (let ((chosen (lightfast:popup-menu (mapcar #'first actions))))
               (when chosen
                 (let ((action (rest (nth chosen actions))))
                   (when action
                     (handler-case (funcall action)
                       (error (condition)
                         (sync-node-tools)
                         (when graph-canvas (lightfast:redraw graph-canvas))
                         (set-status (princ-to-string condition)))))))))
           (synced-graph-scroll-y (height)
             ;; The vertical scroll clamped to what a canvas HEIGHT tall
             ;; cannot show, and the bar beside the canvas told the same;
             ;; every draw passes through here, so the bar is never stale.
             (multiple-value-bind (right bottom) (graph-content-extent)
               (declare (ignore right))
               (let ((limit (max 0 (- (+ bottom 12) height))))
                 (setf graph-scroll-y (max 0 (min limit graph-scroll-y)))
                 (when graph-scrollbar
                   (lightfast:set-range graph-scrollbar 0 limit)
                   (setf (lightfast:value graph-scrollbar)
                         (format nil "~D" graph-scroll-y)))
                 graph-scroll-y)))
           (graph-content-extent ()
             ;; (values right bottom) of everything drawn, in graph space.
             (let ((nodes (ensure-graph-node-positions (editor-nodes))))
               (multiple-value-bind (out-x out-y out-w out-h)
                   (graph-output-box nodes)
                 (values (max (+ out-x out-w)
                              (loop for node in nodes
                                    maximize
                                    (multiple-value-bind (x y w)
                                        (graph-node-box node)
                                      (declare (ignore y))
                                      (+ x w))
                                      into right
                                    finally (return (or right 0))))
                         (+ out-y out-h)))))
           (drop-graph-wire (drag gx gy)
             ;; Releasing a wire: onto a node feeds its primary input, onto
             ;; a blend's right port feeds the second branch, onto OUT makes
             ;; the source node the graph output.
             (when (selected-job)
               (gui-model-ensure-graph model)
               (let* ((nodes (editor-nodes))
                      (from (getf (rest drag) :from))
                      (source-node (unless (eq from :source)
                                     (nth from nodes)))
                      (source-id (if source-node
                                     (orfeus:graph-node-id source-node)
                                     orfeus:*graph-source-id*)))
                 (handler-case
                     (multiple-value-bind (part index)
                         (graph-editor-hit gx gy)
                       (case part
                         ((:node :output-port)
                          (let ((target (nth index nodes)))
                            (if (or (null target) (eq target source-node))
                                (lightfast:redraw graph-canvas)
                                (if (gui-model-set-primary-input
                                     model target source-id)
                                    (after-graph-edit
                                     (format nil "~A reads ~A"
                                             (node-kind-label
                                              (orfeus:graph-node-kind
                                               target))
                                             (if source-node
                                                 (node-kind-label
                                                  (orfeus:graph-node-kind
                                                   source-node))
                                                 "the source")))
                                    (lightfast:redraw graph-canvas)))))
                         (:branch-port
                          (let ((target (nth index nodes)))
                            (if (and target
                                     (orfeus:graph-node-blend-p target)
                                     (not (eq target source-node)))
                                (if (gui-model-set-blend-input
                                     model target source-node)
                                    (after-graph-edit
                                     (format nil "Blend branch reads ~A"
                                             (if source-node
                                                 (node-kind-label
                                                  (orfeus:graph-node-kind
                                                   source-node))
                                                 "the source")))
                                    (lightfast:redraw graph-canvas))
                                (lightfast:redraw graph-canvas))))
                         (:output
                          (if source-node
                              (if (gui-model-set-output model source-node)
                                  (after-graph-edit
                                   (format nil "Output is now ~A"
                                           (node-kind-label
                                            (orfeus:graph-node-kind
                                             source-node))))
                                  (lightfast:redraw graph-canvas))
                              (lightfast:redraw graph-canvas)))
                         (t (lightfast:redraw graph-canvas))))
                   (error (condition)
                     (lightfast:redraw graph-canvas)
                     (set-status (princ-to-string condition)))))))
           (handle-graph-mouse (widget event value)
             (declare (ignore widget))
             (multiple-value-bind (x y button dx dy) (parse-preview-event
                                                      value)
               (when (and x (selected-job))
                 (let ((gx (+ x graph-scroll-x))
                       (gy (+ y graph-scroll-y)))
                   (case event
                     (#.lightfast:+event-push+
                      (multiple-value-bind (part index)
                          (graph-editor-hit gx gy)
                        (cond
                          ((= button 3)
                           (if (eq part :node)
                               (let ((node (select-graph-node index)))
                                 (when node
                                   (show-node-menu
                                    (append (graph-node-menu-actions node)
                                            (list (cons "-" nil))
                                            (add-node-menu-actions node)))))
                               (progn
                                 (gui-model-ensure-graph model)
                                 (show-node-menu
                                  (add-node-menu-actions
                                   (gui-model-selected-graph-node
                                    model))))))
                          ((= button 1)
                           (case part
                             ((:output-port :source-port)
                              (setf node-drag
                                    (list :wire
                                          :from (if (eq part :source-port)
                                                    :source
                                                    index)
                                          :x gx :y gy :moved-p nil)))
                             (:node
                              (let ((node (select-graph-node index))
                                    (now (get-internal-real-time)))
                                (when node
                                  (when (and (eq node (cdr node-click))
                                             (< (- now (car node-click))
                                                (* 0.4
                                                   internal-time-units-per-second)))
                                    (let ((state (gui-model-toggle-node
                                                  model node)))
                                      (after-graph-edit
                                       (format nil "~A ~A"
                                               (if (eq state :bypassed)
                                                   "Bypassed"
                                                   "Enabled")
                                               (node-kind-label
                                                (orfeus:graph-node-kind
                                                 node))))))
                                  (setf node-click (cons now node))
                                  (multiple-value-bind (bx by)
                                      (graph-node-box node)
                                    (setf node-drag
                                          (list :move :node node
                                                :dx (- gx bx)
                                                :dy (- gy by)
                                                :moved-p nil))))))
                             ((:source :output))
                             (t
                              (setf node-drag
                                    (list :pan :x x :y y
                                          :sx graph-scroll-x
                                          :sy graph-scroll-y
                                          :moved-p nil))))))))
                     (#.lightfast:+event-drag+
                      (when node-drag
                        (let ((plist (rest node-drag)))
                          (case (first node-drag)
                            (:move
                             (let ((node (getf plist :node)))
                               (setf (getf plist :moved-p) t
                                     (orfeus:graph-node-position node)
                                     (list (float (max 0 (- gx
                                                            (getf plist
                                                                  :dx)))
                                                  1.0)
                                           (float (max 0 (- gy
                                                            (getf plist
                                                                  :dy)))
                                                  1.0)))
                               (lightfast:redraw graph-canvas)))
                            (:wire
                             (setf (getf plist :x) gx
                                   (getf plist :y) gy
                                   (getf plist :moved-p) t)
                             (lightfast:redraw graph-canvas))
                            (:pan
                             (setf (getf plist :moved-p) t
                                   graph-scroll-x
                                   (max 0 (- (getf plist :sx)
                                             (- x (getf plist :x))))
                                   graph-scroll-y
                                   (max 0 (- (getf plist :sy)
                                             (- y (getf plist :y)))))
                             (lightfast:redraw graph-canvas))))))
                     (#.lightfast:+event-release+
                      (let ((drag node-drag))
                        (setf node-drag nil)
                        (when drag
                          (case (first drag)
                            (:wire
                             (if (getf (rest drag) :moved-p)
                                 (drop-graph-wire drag gx gy)
                                 (lightfast:redraw graph-canvas)))
                            (:move (lightfast:redraw graph-canvas))
                            (:pan
                             (unless (getf (rest drag) :moved-p)
                               (deselect-graph-node)))))))
                     (#.lightfast:+event-wheel+
                      ;; Clamped when drawn, against the canvas's own height.
                      (setf graph-scroll-y (max 0 (+ graph-scroll-y (* dy 32)))
                            graph-scroll-x (max 0 (+ graph-scroll-x (* dx 32))))
                      (lightfast:redraw graph-canvas)))))))
           (autocrop-negative (node)
             (let ((job (selected-job)))
               (when job
                 (handler-case
                     (multiple-value-bind (rect base angle)
                         (orfeus:analyze-negative-frame
                          (photo-job-input-path job) :cache-p t)
                       (declare (ignore base))
                       (gui-model-set-node-params
                        model node
                        (list :left (first rect) :top (second rect)
                              :width (third rect) :height (fourth rect)
                              :angle (or angle 0.0)))
                       (after-graph-edit
                        (format nil "Autocrop: ~,2F ~,2F ~,2Fx~,2F tilt ~,1F"
                                (first rect) (second rect)
                                (third rect) (fourth rect)
                                (or angle 0.0))))
                   (error (condition)
                     (set-status (princ-to-string condition)))))))
           (auto-base-from-border (node)
             (let ((job (selected-job)))
               (when job
                 (handler-case
                     (multiple-value-bind (rect base)
                         (orfeus:analyze-negative-frame
                          (photo-job-input-path job) :cache-p t)
                       (declare (ignore rect))
                       (gui-model-set-node-params
                        model node
                        (list :red (min 4.0 (max 0.0 (first base)))
                              :green (min 4.0 (max 0.0 (second base)))
                              :blue (min 4.0 (max 0.0 (third base)))))
                       (after-graph-edit
                        (format nil "Film base ~,3F ~,3F ~,3F"
                                (first base) (second base) (third base))))
                   (error (condition)
                     (set-status (princ-to-string condition)))))))
           (level-crop (node angle source)
             ;; Turn the crop by ANGLE and shrink its rectangle to whatever
             ;; the turned frame still covers, so no corner prints black.
             (let ((params (orfeus:graph-node-params node)))
               (multiple-value-bind (left top width height)
                   (crop-rect-within-turned-frame
                    (getf params :left 0.0) (getf params :top 0.0)
                    (getf params :width 1.0) (getf params :height 1.0)
                    angle)
                 (gui-model-set-node-params
                  model node
                  (list :left left :top top :width width :height height
                        :angle angle))
                 (sync-node-tools)
                 (after-graph-edit
                  (format nil "Levelled from the ~A: ~,1F deg" source angle)))))
           (level-crop-from-camera (node)
             ;; The maker note's RollAngle is the camera's clockwise roll at
             ;; exposure, which turned the scene the other way; turning the
             ;; picture clockwise by the same amount puts it back. Checked
             ;; against drainpipes and window frames of OM-1 frames rolled
             ;; -6.7 to +6.3 degrees in every orientation the body writes.
             (let* ((job (selected-job))
                    (roll (and job
                               (ignore-errors
                                 (orfeus:photo-roll-angle
                                  (photo-job-input-path job))))))
               (cond ((null roll)
                      (set-status
                       "The camera recorded no level for this photograph"))
                     ((> (abs roll) 45)
                      (set-status
                       (format nil "The camera rolled ~,1F deg, more than a crop can level"
                               roll)))
                     (t (level-crop node (float roll 1.0) "camera")))))
           (level-crop-from-photo (node)
             ;; For a body that writes no level, or a photographer who trusts
             ;; the walls over the gauge: the strongest straight edges near
             ;; vertical or horizontal say where level is.
             (let ((job (selected-job)))
               (when job
                 (handler-case
                     (multiple-value-bind (angle confidence)
                         (orfeus:analyze-level (photo-job-input-path job)
                                               :cache-p t)
                       (if (< confidence *level-confidence-floor*)
                           (set-status
                            "No straight edge to level by was found in the photograph")
                           (level-crop node (float angle 1.0) "photograph")))
                   (error (condition)
                     (set-status (princ-to-string condition)))))))
           (set-preview-cursor (shape)
             ;; A crosshair while a click on the preview will be read as a
             ;; colour rather than acted on as a pan or a crop drag.
             (when after-canvas
               (ignore-errors (lightfast:set-cursor after-canvas shape))))
           (base-default (kind)
             ;; What an unset base channel stands for: full scale to a
             ;; subtraction, "measure it from the frame" to a negative.
             (if (eq kind :negative) 0.0 1.0))
           (base-controls (kind)
             ;; The three channel fields and the swatch of KIND's panel, as
             ;; (values ((key input)...) swatch).
             (if (eq kind :negative)
                 (values (list (list :red negative-red-input)
                               (list :green negative-green-input)
                               (list :blue negative-blue-input))
                         negative-swatch)
                 (values (list (list :red base-red-input)
                               (list :green base-green-input)
                               (list :blue base-blue-input))
                         base-swatch)))
           (set-base-channel (node key percent)
             ;; The fields read in encoded percent, which is how the colour
             ;; looks; the node keeps it scene-linear, which is where the
             ;; inversion happens.
             (let* ((params (orfeus:graph-node-params node))
                    (default (base-default (orfeus:graph-node-kind node)))
                    (value (srgb-decode-component
                            (max 0.0 (min 1.0 (/ percent 100.0))))))
               (handler-case
                   (progn
                     (gui-model-set-node-params
                      model node
                      (list :red (getf params :red default)
                            :green (getf params :green default)
                            :blue (getf params :blue default)
                            key value))
                     (sync-node-tools)
                     (schedule-edited-preview))
                 (error (condition) (set-status (princ-to-string condition))))))
           (sync-base-swatch (node swatch)
             (when swatch
               (let ((params (orfeus:graph-node-params node))
                     (default (base-default (orfeus:graph-node-kind node))))
                 (flet ((channel (key)
                          (round (* 255 (srgb-encode-component
                                         (getf params key default))))))
                   (lightfast:set-color-rgb swatch
                                            :red (channel :red)
                                            :green (channel :green)
                                            :blue (channel :blue))
                   (lightfast:redraw swatch)))))
           (sample-base-at (canvas x y)
             ;; The eyedropper: map the click through the current fit and pan
             ;; to normalized image coordinates, then sample linear color.
             (let ((node pick-color-node)
                   (job (selected-job))
                   (path (preview-path-for-canvas canvas)))
               (setf pick-color-node nil)
               (set-preview-cursor :default)
               (when (and node job path)
                 (multiple-value-bind (scaled-width scaled-height)
                     (preview-scaled-size canvas path preview-zoom)
                   (when scaled-width
                     (let* ((normalized-x
                              (+ preview-center-x
                                 (/ (- x (/ (lightfast:widget-width canvas) 2d0))
                                    scaled-width)))
                            (normalized-y
                              (+ preview-center-y
                                 (/ (- y (/ (lightfast:widget-height canvas)
                                            2d0))
                                    scaled-height))))
                       (handler-case
                           (let ((color (orfeus:sample-photo-linear-color
                                         (photo-job-input-path job)
                                         (max 0.0 (min 1.0 normalized-x))
                                         (max 0.0 (min 1.0 normalized-y))
                                         :radius 0.008 :cache-p t)))
                             (gui-model-set-node-params
                              model node
                              (list :red (min 4.0 (max 0.0 (first color)))
                                    :green (min 4.0 (max 0.0 (second color)))
                                    :blue (min 4.0 (max 0.0 (third color)))))
                             (after-graph-edit
                              (format nil "Sampled base ~,3F ~,3F ~,3F"
                                      (first color) (second color)
                                      (third color))))
                         (error (condition)
                           (set-status (princ-to-string condition))))))))))
           (copy-grade ()
             (let ((graph (gui-model-copy-graph model)))
               (if graph
                   (progn
                     (setf grade-clipboard graph)
                     (set-status "Node graph copied"))
                   (set-status "No photograph selected"))))
           (paste-grade ()
             (cond
               ((null grade-clipboard)
                (set-status "Copy a node graph first"))
               (t
                (let ((count (gui-model-paste-graph model grade-clipboard)))
                  (if (plusp count)
                      (after-graph-edit
                       (format nil "Node graph pasted to ~D photo~:P" count))
                      (set-status "No photograph selected"))))))
           (reload-gallery-stills ()
             ;; Local stills are view-only gallery entries, never project data.
             ;; Preserve provenance, never a numeric index that can retarget.
             (let ((selected-key
                     (gallery-selection-key gallery-stills gallery-selected)))
               (incf gallery-generation)
               (discard-gui-tasks background-queue :still)
               (clrhash gallery-thumbs)
               (setf gallery-stills
                     ;; The gallery is global, like a PowerGrade album: stills
                     ;; live in the per-user store and are the same in every
                     ;; project. Presets carried by an older project file are
                     ;; still shown, after the global ones and only when the
                     ;; store has nothing of that name, so nothing grabbed
                     ;; before the gallery went global disappears.
                     (let* ((global (handler-case
                                        (mapcar #'make-local-gallery-still
                                                (orfeus:still-store-list))
                                      (error (condition)
                                        (set-status (princ-to-string condition))
                                        '())))
                            (names (mapcar #'gallery-still-identity global)))
                       (append
                        global
                        (remove-if
                         (lambda (still)
                           (member (gallery-still-identity still) names
                                   :test #'equal))
                         (mapcar #'make-project-gallery-still
                                 (project-presets project)))))
                     gallery-selected
                     (gallery-selection-index gallery-stills selected-key))))
           (persist-still (preset)
             ;; The local gallery copy survives removable source media and
             ;; unsaved projects, but never replaces an unrelated exact name.
             (handler-case
                 (values (orfeus:still-store-write preset :if-exists :error) nil)
               (error (condition)
                 (values nil condition))))
           (grab-still ()
             (multiple-value-bind (preset intern-failure)
                 (gui-model-grab-still model)
               (if preset
                   (multiple-value-bind (stored condition)
                       (persist-still preset)
                     ;; The still now owns a copy of its RAW, so the sidebar
                     ;; badge may have just become true for this photograph.
                     (clrhash interned-photos)
                     (refresh-gallery)
                     (redraw-thumbnails)
                     (setf (lightfast:value preset-name-input)
                           (processing-preset-name preset))
                     (sync-preset-action-label)
                     (set-status
                      (cond
                        ((not stored)
                         (format nil "Grabbed ~A in project only: ~A"
                                 (processing-preset-name preset) condition))
                        (intern-failure
                         (format nil "Grabbed ~A, but its RAW was not copied: ~A"
                                 (processing-preset-name preset)
                                 intern-failure))
                        (t (format nil "Grabbed ~A"
                                   (processing-preset-name preset))))))
                   (set-status "No photograph selected"))))
           (still-recipe (preset)
             (or (orfeus:processing-preset-graph preset)
                 (orfeus::settings-apply-stage-bypass
                  (processing-preset-settings preset)
                  (processing-preset-disabled-stages preset))))
           (still-thumbnail-pathname (still)
             (let* ((preset (gallery-still-preset still))
                    (source (processing-preset-source-photo preset))
                    (source-identity
                      (orfeus:still-store-identity
                       (if source
                           (namestring
                            (or (ignore-errors (truename source)) source))
                           "none"))))
               ;; Three whole digests overflowed the 255-byte filename limit
               ;; once the atomic write added its dot prefix and temporary
               ;; suffix, so every still preview failed to write. Sixty-four
               ;; bits per component is plenty to key a cache file.
               (flet ((short (text)
                        (let ((text (string text)))
                          (subseq text 0 (min 16 (length text))))))
                 (merge-pathnames
                  (make-pathname
                   :name (format nil "still-~(~A~)-~A-~A-~A"
                                 (gallery-still-origin still)
                                 (short (gallery-still-identity still))
                                 (short source-identity)
                                 (short (preview-settings-key
                                         (still-recipe preset))))
                   :type "jpg")
                  preview-directory))))
           (request-still-thumbnail (still)
             (let* ((preset (gallery-still-preset still))
                    (key (gallery-still-key still))
                    (generation gallery-generation)
                    (name (processing-preset-name preset))
                    (source (processing-preset-source-photo preset))
                    ;; Both origins keep the local thumbnail warm, so the
                    ;; fallback copy stays usable once the project row is the
                    ;; only one shown.
                    (stored (ignore-errors
                              (orfeus:still-store-thumbnail-pathname name))))
               (when (null (gethash key gallery-thumbs))
                 (cond
                   ((and source (probe-file source))
                    (let* ((output (still-thumbnail-pathname still))
                           (recipe (still-recipe preset))
                           (graph-p (typep recipe
                                           'orfeus:processing-graph)))
                      (if (preview-cache-hit-p output)
                          (progn
                            (when (and stored (not (probe-file stored)))
                              (ignore-errors
                                (orfeus:still-store-write-thumbnail name output)))
                            (setf (gethash key gallery-thumbs) output))
                          (enqueue-gui-task
                           background-queue :still
                           (lambda ()
                             (handler-case
                                 (progn
                                   (render-preview
                                    source output
                                    (if graph-p nil recipe)
                                    :graph (when graph-p recipe)
                                    :max-width *thumbnail-preview-size*
                                    :max-height *thumbnail-preview-size*
                                    :jpeg-quality 82
                                    :if-exists :supersede)
                                   (queue-event queue
                                                (list :still-thumb generation
                                                      key output stored)))
                               ;; A failed still render used to vanish, leaving
                               ;; a permanently blank gallery cell with no clue
                               ;; why. The status bar clips long reasons, so
                               ;; the whole condition also goes to the log.
                               (error (condition)
                                 (format *error-output*
                                         "~&orfeus: still preview failed for ~A: ~A~%"
                                         name condition)
                                 (queue-event
                                  queue
                                  (list :still-error generation
                                        (format nil "Still preview failed: ~A"
                                                condition))))))))))
                   ;; Source gone (card ejected, file moved): fall back to
                   ;; the persisted local copy of the thumbnail.
                   ((and stored (probe-file stored))
                    (setf (gethash key gallery-thumbs) stored))
                   (t (setf (gallery-still-unavailable-p still) t))))))
           (refresh-gallery ()
             (reload-gallery-stills)
             (when gallery-canvas
               (dolist (still gallery-stills)
                 (request-still-thumbnail still))
               (lightfast:redraw gallery-canvas)))
           (gallery-columns ()
             (max 1 (floor (- (lightfast:widget-width gallery-canvas) 8)
                           *gallery-cell-width*)))
           (gallery-index-at (x y)
             (let* ((columns (gallery-columns))
                    (column (floor (- x 4) *gallery-cell-width*))
                    (row (floor (+ (- y 4) gallery-scroll)
                                *gallery-cell-height*))
                    (index (+ (* row columns) (min column (1- columns)))))
               (when (and (>= column 0) (< column columns) (>= row 0)
                          (< index (length gallery-stills)))
                 index)))
           (gallery-scroll-limit ()
             (let ((columns (gallery-columns)))
               (max 0 (- (* (ceiling (length gallery-stills)
                                     columns)
                            *gallery-cell-height*)
                         (- (lightfast:widget-height gallery-canvas) 8)))))
           (draw-gallery (widget)
             (let ((x (lightfast:widget-x widget))
                   (y (lightfast:widget-y widget))
                   (width (lightfast:widget-width widget))
                   (height (lightfast:widget-height widget))
                   (columns (gallery-columns)))
               (lightfast:draw-color-rgb :red 255 :green 255 :blue 255)
               (lightfast:draw-filled-rect x y width height)
               (loop for still in gallery-stills
                     for preset = (gallery-still-preset still)
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
                              (lightfast:draw-color-rgb :red 0 :green 0 :blue 128)
                              (lightfast:draw-filled-rect
                               cell-x cell-y (- *gallery-cell-width* 6)
                               (- *gallery-cell-height* 6)))
                            (let ((thumb (gethash (gallery-still-key still)
                                                  gallery-thumbs))
                                  (thumb-width (- *gallery-cell-width* 12)))
                              (cond
                                (thumb
                                 (draw-thumbnail-file
                                  widget thumb (+ cell-x 3) (+ cell-y 3)
                                  thumb-width 62))
                                ((gallery-still-unavailable-p still)
                                 ;; Lost source and no kept thumbnail: an empty
                                 ;; grey box was indistinguishable from one
                                 ;; still rendering, so say which it is.
                                 (lightfast:draw-color-rgb
                                  :red 232 :green 216 :blue 216)
                                 (lightfast:draw-filled-rect
                                  (+ cell-x 3) (+ cell-y 3) thumb-width 62)
                                 (lightfast:draw-color-rgb
                                  :red 150 :green 90 :blue 90)
                                 (lightfast:draw-font :size 10)
                                 (lightfast:draw-text
                                  "source missing" (+ cell-x 6) (+ cell-y 38)))
                                (t
                                 (lightfast:draw-color-rgb
                                  :red 205 :green 205 :blue 205)
                                 (lightfast:draw-filled-rect
                                  (+ cell-x 3) (+ cell-y 3) thumb-width 62))))
                            (if selected
                                (lightfast:draw-color-rgb :red 255 :green 255
                                                        :blue 255)
                                (lightfast:draw-color-rgb :red 0 :green 0
                                                        :blue 0))
                            (lightfast:draw-font :size 10)
                            (lightfast:draw-text
                             (if (> (length name) 14)
                                 (subseq name 0 14)
                                 name)
                             (+ cell-x 3) (+ cell-y 80))
                            (lightfast:draw-font :size 12)))))
           (gallery-select (index)
             (setf gallery-selected index)
             (let* ((still (nth index gallery-stills))
                    (preset (and still (gallery-still-preset still))))
               (when preset
                 (setf (lightfast:value preset-name-input)
                       (processing-preset-name preset))))
             (lightfast:redraw gallery-canvas))
           (still-lens-note (preset)
             ;; Optics parameters travel, but the lens profile is re-matched
             ;; from each photo's own metadata; note when they differ.
             (let ((source (processing-preset-source-photo preset)))
               (when source
                 (let ((source-lens (ignore-errors
                                      (photo-lens-description source))))
                   (when (and source-lens
                              (loop for job in (gui-model-acting-jobs model)
                                      thereis
                                      (let ((target-lens
                                              (ignore-errors
                                                (photo-lens-description
                                                 (photo-job-input-path job)))))
                                        (and target-lens
                                             (string/= source-lens
                                                       target-lens)))))
                     " (lens differs; optics re-match per photo)")))))
           (apply-still (still &key bypass-kinds description)
             (let ((preset (gallery-still-preset still)))
               (handler-case
                   (let ((count (gui-model-apply-preset-graph
                                 model preset :bypass-kinds bypass-kinds)))
                     (after-graph-edit
                      (format nil "Applied ~A (~A)~@[ ~A~] to ~D photo~:P~@[~A~]"
                              (processing-preset-name preset)
                              (gallery-still-origin-description still)
                              description count (still-lens-note preset)))
                     (redraw-thumbnails))
                 (error (condition)
                   (set-status (princ-to-string condition))))))
           (delete-still (still)
             (let ((preset (gallery-still-preset still)))
               (ecase (gallery-still-origin still)
                 (:project
                  (setf (project-presets project)
                        (remove preset (project-presets project) :test #'eq)))
                 (:local
                  (orfeus:still-store-delete (processing-preset-name preset))))
               (setf gallery-selected nil)
               (refresh-gallery)
               (set-status (format nil "Deleted ~A ~A"
                                   (string-downcase
                                    (symbol-name (gallery-still-origin still)))
                                   (processing-preset-name preset)))))
           (rename-still (still)
             (let* ((preset (gallery-still-preset still))
                    (old-name (processing-preset-name preset))
                    (answer (lightfast:input-dialog "Still name"
                                                  :initial old-name)))
               (when answer
                 (let ((new-name (string-trim '(#\Space #\Tab) answer)))
                   (unless (or (zerop (length new-name))
                               (string= new-name old-name))
                     (let* ((local-p (eq :local (gallery-still-origin still)))
                            (siblings
                              (remove preset
                                      (if local-p
                                          (mapcar #'gallery-still-preset
                                                  (remove-if-not
                                                   (lambda (entry)
                                                     (eq :local
                                                         (gallery-still-origin
                                                          entry)))
                                                   gallery-stills))
                                          (project-presets project))
                                      :test #'eq)))
                       (if (find new-name siblings :key #'processing-preset-name
                                               :test #'string=)
                           (set-status (format nil "A still named ~A already exists"
                                               new-name))
                           (handler-case
                               (progn
                                 (if local-p
                                     (let ((candidate
                                             (orfeus::sexp->processing-preset
                                              (orfeus::processing-preset->sexp
                                               preset))))
                                       (setf (processing-preset-name candidate)
                                             new-name)
                                       (orfeus:still-store-rename candidate old-name))
                                     (setf (processing-preset-name preset)
                                           new-name))
                                 (setf (lightfast:value preset-name-input) new-name)
                                 (refresh-gallery)
                                 (set-status (format nil "Renamed ~A to ~A"
                                                     old-name new-name)))
                             (error (condition)
                               (set-status (princ-to-string condition)))))))))))
           (intern-still-source (still)
             ;; Stills grabbed before interning existed still point at the card
             ;; they were shot from. A still is a look to reuse later, so it
             ;; should own its RAW; this repairs the older ones.
             (let* ((preset (gallery-still-preset still))
                    (source (orfeus:processing-preset-source-photo preset)))
               (cond
                 ((null source)
                  (set-status "This still has no source photograph"))
                 ((orfeus:photo-interned-p source)
                  (set-status "This still already owns its source RAW"))
                 ((not (probe-file source))
                  (set-status
                   (format nil "The source is not reachable: ~A"
                           (namestring source))))
                 (t
                  (handler-case
                      (let ((kept (orfeus:intern-raw-file source)))
                        (setf (orfeus:processing-preset-source-photo preset) kept)
                        ;; Deliberately replacing this still's stored copy;
                        ;; PERSIST-STILL refuses an existing name because it is
                        ;; for grabbing new ones.
                        (orfeus:still-store-write preset :if-exists :supersede)
                        (clrhash interned-photos)
                        (refresh-gallery)
                        (redraw-thumbnails)
                        (set-status
                         (format nil "~A now keeps its own copy of ~A"
                                 (processing-preset-name preset)
                                 (file-namestring source))))
                    (error (condition)
                      (set-status (format nil "Could not keep the source: ~A"
                                          condition))))))))
           (gallery-context-menu (still)
             (let ((actions
                     (list (cons (format nil "Apply Graph to ~D Photo~:P"
                                         (selected-photo-count))
                                 (lambda () (apply-still still)))
                           (cons "Apply Without Optics"
                                 (lambda ()
                                   (apply-still still
                                                :bypass-kinds '(:optics)
                                                :description
                                                "without optics")))
                           (cons "Apply Without White Balance"
                                 (lambda ()
                                   (apply-still still
                                                :bypass-kinds
                                                '(:white-balance)
                                                :description
                                                "without white balance")))
                           (cons "-" nil)
                           (cons "Rename Still..."
                                 (lambda () (rename-still still)))
                           (cons "Keep Source RAW"
                                 (lambda () (intern-still-source still)))
                           (cons "Delete Still"
                                 (lambda () (delete-still still))))))
               (let ((chosen (lightfast:popup-menu (mapcar #'first actions))))
                 (when chosen
                   (let ((action (rest (nth chosen actions))))
                     (when action (funcall action)))))))
           (handle-gallery-mouse (widget event value)
             (declare (ignore widget))
             (multiple-value-bind (x y button dx dy state)
                 (parse-preview-event value)
               (declare (ignore dx state))
               (when x
                 (case event
                   (#.lightfast:+event-push+
                    (let ((index (gallery-index-at x y))
                          (now (get-internal-real-time)))
                      (when index
                        (gallery-select index)
                        (cond
                          ((= button 3)
                           (let ((still (nth index gallery-stills)))
                             (when still (gallery-context-menu still))))
                          ((and (eql index (cdr gallery-click))
                                (< (- now (car gallery-click))
                                   (* 0.4 internal-time-units-per-second)))
                           (let ((still (nth index gallery-stills)))
                             (when still
                               (apply-still still))))
                          (t (setf gallery-click (cons now index)))))))
                   (#.lightfast:+event-wheel+
                    (setf gallery-scroll
                          (min (gallery-scroll-limit)
                               (max 0 (+ gallery-scroll (* dy 32)))))
                    (lightfast:redraw gallery-canvas))))))
           (sync-preset-action-label ()
             (let ((count (selected-photo-count))
                   (open (length (project-photos (gui-model-project model)))))
               (when preset-apply-button
                 (setf (lightfast:label preset-apply-button)
                       (format nil "Apply to ~D photo~:P" count)))
               (when photo-selection-label
                 (setf (lightfast:label photo-selection-label)
                       (format nil "Photos · ~D open · ~D selected" open count)))))
           (save-current-preset ()
             (handler-case
                 (let ((preset (gui-model-save-preset
                                model (lightfast:value preset-name-input))))
                   (remhash (processing-preset-name preset) gallery-thumbs)
                   (multiple-value-bind (stored condition)
                       ;; The name was typed on purpose, so saving over it is
                       ;; the intent; PERSIST-STILL refuses, since it guards
                       ;; auto-named grabs against clobbering a look.
                       (handler-case
                           (values (orfeus:still-store-write
                                    preset :if-exists :supersede)
                                   nil)
                         (error (condition) (values nil condition)))
                     (refresh-gallery)
                     (setf (lightfast:value preset-name-input)
                           (processing-preset-name preset))
                     (set-status
                      (if stored
                          (format nil "Saved preset ~A"
                                  (processing-preset-name preset))
                          (format nil "Saved preset ~A in project only: ~A"
                                  (processing-preset-name preset) condition)))))
               (error (condition)
                 (set-status (princ-to-string condition)))))
           (apply-current-preset ()
             (let ((still (gallery-selected-still gallery-stills
                                                  gallery-selected)))
               (if still
                   (apply-still still)
                   (set-status "Select a still in the gallery first"))))
           (sync-controls ()
             (dolist (entry controls)
               (let ((key (first entry)) (widget (second entry)))
                 (setf (lightfast:value widget)
                       (case key
                         ((:lens-correction-p :chromatic-aberration-correction-p)
                          (if (gui-model-setting model key) "1" "0"))
                         ;; An untouched temperature shows the one the camera
                         ;; balanced for rather than an empty box: it is the
                         ;; value this photograph is already being rendered
                         ;; at, and typing over it starts from the truth.
                         (:white-balance-temperature
                          ;; Whole kelvins: the camera balanced to 4668.18 K
                          ;; and no eye can tell the .18.
                          (let ((kelvin (or (gui-model-setting model key)
                                            (selected-as-shot-kelvin))))
                            (display-number (and kelvin (round kelvin)))))
                         (otherwise (display-number (gui-model-setting model key)))))))
             (setf (lightfast:value wb-choice)
                   (if (gui-model-setting model :white-balance-temperature)
                       "Custom" "As shot")
                   (lightfast:value target-choice)
                   (if (eq (gui-model-edit-target model) :defaults)
                       "Defaults" "Photo"))
             (when lut-choice
               (let ((path (gui-model-setting model :lut-path)))
                 (setf (lightfast:value lut-choice)
                       (if path (lut-choice-name path) "None"))))
             (setf (lightfast:label lens-name)
                   (let ((capture (selected-capture-description)))
                     (if capture
                         (format nil "Lens: ~A   |   ~A"
                                 (selected-lens-description) capture)
                         (format nil "Lens: ~A" (selected-lens-description)))))
             (sync-lens-profile-label)
             (when fringing-source-choice
               (setf (lightfast:value fringing-source-choice)
                     (or (first (find (gui-model-setting
                                       model :chromatic-aberration-source)
                                      *fringing-source-choices* :key #'rest))
                         "Measured")))
             (sync-export-controls)
             (sync-preset-action-label)
             (sync-window-title))
           (refresh-photo-groups ()
             ;; Reading a capture time spawns ExifTool the first time, and a
             ;; card's worth of photographs is a card's worth of spawns, so the
             ;; whole pass runs on the background queue and posts its answer.
             (let ((jobs (copy-list (project-photos project)))
                   (thumbs (let ((copy (make-hash-table :test #'eq)))
                             (maphash (lambda (job path)
                                        (setf (gethash job copy) path))
                                      thumbnail-files)
                             copy))
                   (sorting pending-sort)
                   (key (gui-model-sort-key model))
                   (reverse-p (gui-model-sort-reverse-p model)))
               (enqueue-gui-task
                background-queue :groups
                (lambda ()
                  (let* ((entries
                           (mapcar
                            (lambda (job)
                              (let ((input (photo-job-input-path job))
                                    (thumb (gethash job thumbs)))
                                (list job
                                      (ignore-errors
                                        (orfeus:photo-capture-seconds input))
                                      (and thumb
                                           (ignore-errors
                                             (orfeus:photo-signature thumb))))))
                            jobs))
                         ;; The same pass that reads every capture time is
                         ;; the one to sort by them: the bursts are then found
                         ;; in the order the filmstrip will show, which for a
                         ;; card shot in sequence is the only order they are
                         ;; consecutive in.
                         (order (when sorting
                                  (sort-photo-jobs
                                   jobs key
                                   :reverse-p reverse-p
                                   :seconds (lambda (job) (second (assoc job entries))))))
                         (entries (if order
                                      (mapcar (lambda (job) (assoc job entries)) order)
                                      entries)))
                    (queue-event queue
                                 (list :photo-groups
                                       (orfeus:group-index-of
                                        (orfeus:group-captures entries))
                                       order
                                       sorting)))))))
           (sort-photos (key)
             (setf (gui-model-sort-key model) key
                   pending-sort :chosen)
             (refresh-photo-groups))
           (reverse-photo-order ()
             (setf (gui-model-sort-reverse-p model)
                   (not (gui-model-sort-reverse-p model))
                   pending-sort :chosen)
             (refresh-photo-groups))
           (apply-photo-order (order how)
             ;; The order the background pass arrived at, put on the project
             ;; with the cursor and the marks staying on their photographs.
             ;; An order the photographer asked for is theirs to undo; the
             ;; default one a fresh card takes is not an edit.
             (when (gui-model-reorder-photos model order
                                             :checkpoint (eq how :chosen))
               (setf thumbnail-anchor (current-photo-row))
               (sync-window-title))
             (when (eq how :chosen)
               (set-status
                (format nil "Sorted by ~A~:[~;, newest first~]"
                        (ecase (gui-model-sort-key model)
                          (:capture-time "capture time")
                          (:name "name")
                          (:rating "rating"))
                        (gui-model-sort-reverse-p model))))
             (when (eq pending-sort how)
               (setf pending-sort nil)))
           (refresh-photo-focus (&key then)
             ;; Measuring focus develops the frame, which is half a second
             ;; each, so this runs on a thread of its own rather than on the
             ;; background queue: that queue renders previews with one worker,
             ;; and a card's worth of measuring would hold every preview behind
             ;; it for minutes. Answers arrive one at a time and the filmstrip
             ;; draws whatever has come in.
             (let ((jobs (remove-if (lambda (job) (gethash job photo-focus-reports))
                                    (copy-list (project-photos project)))))
               (cond
                 (focus-pass-running-p
                  (set-status "Still measuring focus"))
                 ((null jobs) (when then (funcall then)))
                 (t
                  (setf focus-pass-running-p t)
                  (set-status (format nil "Measuring focus of ~D photograph~:P"
                                      (length jobs)))
                  (let ((total (length jobs)))
                    (sb-thread:make-thread
                     (lambda ()
                       (unwind-protect
                            (loop for job in jobs
                                  for done from 1
                                  do (queue-event
                                      queue
                                      (list :photo-focus job
                                            (ignore-errors
                                              (orfeus:photo-focus
                                               (photo-job-input-path job)))
                                            done total)))
                         (queue-event queue (list :photo-focus-done then))))
                     :name "orfeus focus"))))))
           (select-blurry-photos ()
             (let* ((photos (project-photos project))
                    (indices (loop for job in photos
                                   for index from 0
                                   when (eq :soft (photo-focus-verdict job))
                                     collect index)))
               (if (null indices)
                   (set-status
                    (format nil "Nothing looks out of focus in ~D photograph~:P"
                            (count-if (lambda (job) (photo-focus-of job)) photos)))
                   (progn
                     ;; Open any burst holding one, or the selection would be a
                     ;; half-filled box on a folded row and the photographer
                     ;; could not see which frame was meant.
                     (loop for job in photos
                           for index from 0
                           when (and (member index indices) (photo-group-of job))
                             do (let ((place (photo-group-of job)))
                                  (let ((leader (nth (- index (second place))
                                                     photos)))
                                    (when (and leader (burst-leader-p leader))
                                      (remhash leader collapsed-bursts)))))
                     ;; A selection the interface built for the photographer
                     ;; is theirs to walk past with the arrows, like one they
                     ;; marked out by hand.
                     (setf (gui-model-marked-p model) t)
                     (apply-thumbnail-selection indices (first indices)
                                                (first indices))
                     (set-status
                      (format nil "Selected ~D of ~D photograph~:P that look soft"
                              (length indices) (length photos)))))))
           (find-and-select-blurry-photos ()
             (refresh-photo-focus :then #'select-blurry-photos))
           (replace-project (new-project &optional path)
             (incf preview-generation)
             (clear-previews)
             (setf project new-project
                   thumbnail-scroll 0
                   thumbnail-anchor 0)
             (clrhash thumbnail-files)
             (clrhash thumbnail-grades)
             (clrhash gallery-thumbs)
             (setf gallery-selected nil
                   gallery-scroll 0
                   pick-color-node nil)
             (set-preview-cursor :default)
             (gui-model-replace-project model new-project path)
             (setf (gui-model-selected-node model) nil)
             (sync-window-title)
             (refresh-gallery)
             (sync-node-tools)
             (when graph-canvas (lightfast:redraw graph-canvas))
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
                         (sync-node-tools)
                         (when graph-canvas (lightfast:redraw graph-canvas))
                         (redraw-thumbnails)
                         (schedule-initial-preview)
                         ;; New arrivals take the filmstrip's order once
                         ;; their capture times have been read.
                         (setf pending-sort :automatic)
                         (refresh-photo-groups)
                         (set-status (format nil "Added ~D photograph~:P" count)))
                       (set-status "All selected photographs are already in the project"))))))
           (remove-selected-photo ()
             (let ((removed (gui-model-remove-selected model)))
               (when removed
                 (incf preview-generation)
                 (dolist (job removed)
                   (remhash job lens-cache)
                   (remhash job capture-cache)
                   (remhash job thumbnail-files)
                   (remhash job thumbnail-grades))
                 (clear-previews)
                 (sync-controls)
                 (sync-node-tools)
                 (when graph-canvas (lightfast:redraw graph-canvas))
                 (redraw-thumbnails)
                 (if (selected-job)
                     (progn
                       (schedule-initial-preview)
                       (set-status (format nil "Removed ~D photograph~:P"
                                           (length removed))))
                     (set-status "Project contains no photographs")))))
           (open-project ()
             (when (confirm-discard "opening another project")
               (let ((path (lightfast:choose-file
                            :title "Open Orfeus project"
                            :filter (fltk-file-filter "Orfeus project" "*.sexp")
                            :preset-file (picker-preset))))
                 (when path
                   (remember-picked-path path)
                   (replace-project (project-read path) (pathname path))))))
           (new-project ()
             ;; An empty project to import into, named up front so its export
             ;; destination is settled before any photograph arrives.
             (when (or (null (project-photos project))
                       (= 1 (lightfast:choice-box
                             (format nil
                                     "Start a new project? ~D photograph~:P and any unsaved edits will be closed."
                                     (length (project-photos project)))
                             :button0 "Cancel" :button1 "New Project")))
               (let ((path (lightfast:choose-save-file
                            :title "New Orfeus project"
                            :filter (fltk-file-filter "Orfeus project" "*.sexp")
                            :preset-file
                            (namestring
                             (merge-pathnames "project.sexp" picker-directory)))))
                 (replace-project (gui-empty-project))
                 (cond
                   (path
                    (remember-picked-path path)
                    (gui-model-anchor-export-directory model (pathname path))
                    (handler-case
                        (progn
                          (project-write project (pathname path))
                          (setf (gui-model-project-path model) (pathname path))
                          (sync-export-controls)
                          (set-status
                           (format nil "New project at ~A; exports go to ~A"
                                   (file-namestring path)
                                   (namestring
                                    (project-output-directory project)))))
                      (error (condition)
                        (set-status (format nil "Could not write the project: ~A"
                                            condition)))))
                   (t
                    ;; Unnamed is still usable; exports settle when it is saved.
                    (set-status "New project"))))))
           (save-project (&optional choose-p)
             (let ((path (or (and (not choose-p) (gui-model-project-path model))
                             (lightfast:choose-save-file
                              :title "Save Orfeus project"
                              :filter (fltk-file-filter "Orfeus project" "*.sexp")
                              :preset-file
                              (namestring
                               (merge-pathnames "project.sexp"
                                                picker-directory))))))
               (when path
                 (remember-picked-path path)
                 ;; Exports belong beside the project. Anchoring only when the
                 ;; path changes leaves a destination the user chose alone on
                 ;; every later save.
                 (let ((moved (not (equal (pathname path)
                                          (gui-model-project-path model)))))
                   (when moved
                     (gui-model-anchor-export-directory model (pathname path)))
                   (project-write project path)
                   (setf (gui-model-project-path model) (pathname path)
                         (gui-model-modified-p model) nil)
                   (sync-export-controls)
                   (sync-window-title)
                   (set-status
                    (if moved
                        (format nil "Project saved; exports go to ~A"
                                (namestring
                                 (project-output-directory project)))
                        "Project saved"))
                   path))))
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
             ;; While a crop node is selected its rectangle is opened out to the
             ;; whole frame, but its *angle* is kept. So the preview shows the
             ;; photograph straightened exactly as the render will straighten
             ;; it, with everything outside the rectangle still visible to crop
             ;; from — and the rectangle itself can be drawn upright, which is
             ;; what straightening looks like. Rect changes still cost only a
             ;; redraw; only the angle asks for a render.
             (let ((settings (preview-recipe-snapshot
                              (settings-for-job (selected-job))))
                   (node (crop-editing-node)))
               (when (and node
                          (typep settings 'orfeus:processing-graph)
                          (not (orfeus:graph-node-bypassed-p node)))
                 (let ((twin (find (orfeus:graph-node-id node)
                                   (orfeus:processing-graph-nodes settings)
                                   :key #'orfeus:graph-node-id)))
                   (when twin
                     (let ((angle (getf (orfeus:graph-node-params twin)
                                        :angle 0.0)))
                       (if (zerop angle)
                           (setf (orfeus:graph-node-bypassed-p twin) t)
                           (setf (orfeus:graph-node-params twin)
                                 (list :left 0.0 :top 0.0 :width 1.0
                                       :height 1.0 :angle angle)))))))
               settings))
           (enqueue-render (target-queue role job index settings generation publish-p
                            &key front-p draft-p cache-p viewport)
             ;; Scheduling is constant-time on the FLTK thread. Content hashing,
             ;; cache lookup, rendering, and hit materialization all run here.
             (let* ((settings (preview-recipe-snapshot settings))
                    (input (photo-job-input-path job))
                    (file-role (if draft-p :draft role))
                    (bound (if draft-p
                               *gui-draft-preview-size*
                               (viewport-render-bound)))
                    (max-width bound)
                    (max-height bound)
                    ;; Part of the cache name, not decoration: two renders of
                    ;; the same photograph at the same settings and bound are
                    ;; different pictures when they cover different parts of it.
                    (geometry (recipe-geometry-key settings))
                    (region-token (if viewport
                                      (format nil "-w~{~4,'0D~^~}"
                                              (mapcar (lambda (value)
                                                        (round (* 1000 value)))
                                                      viewport))
                                      "")))
               (enqueue-gui-task
                target-queue role
                (lambda ()
                  (when (or (not publish-p) (= generation preview-generation))
                    (let ((render-started (get-internal-real-time))
                          (reporter
                            (and publish-p (eq role :after)
                                 (lambda (stage fraction)
                                   (queue-event queue
                                                (list :render-stage generation
                                                      stage fraction))))))
                      (loop for attempt below 3
                          for digest = (photo-content-key-for job generation
                                                              (plusp attempt))
                          for settings-key = (concatenate
                                              'string
                                              (preview-settings-key
                                               settings :max-width max-width
                                               :max-height max-height
                                               :jpeg-quality 88)
                                              region-token)
                          for output = (preview-pathname
                                        preview-directory digest file-role settings
                                        :max-width max-width :max-height max-height
                                        :jpeg-quality 88 :settings-key settings-key)
                          for dependencies-current-p =
                            (lambda ()
                              (string= settings-key
                                       (concatenate
                                        'string
                                        (preview-settings-key
                                         settings :max-width max-width
                                         :max-height max-height
                                         :jpeg-quality 88)
                                        region-token)))
                          do (handler-case
                                 (progn
                                   (call-with-preview-cache-fill
                                    output
                                    (lambda (temporary)
                                      (if (typep settings 'orfeus:processing-graph)
                                          (render-preview input temporary nil
                                                          :graph settings
                                                          :max-width max-width
                                                          :max-height max-height
                                                          :cache-p cache-p
                                                          :viewport viewport
                                                          :progress reporter
                                                          :if-exists :supersede)
                                          (render-preview input temporary settings
                                                          :max-width max-width
                                                          :max-height max-height
                                                          :cache-p cache-p
                                                          :viewport viewport
                                                          :progress reporter
                                                          :if-exists :supersede))
                                      (unless (string= digest (photo-content-key input))
                                        (error "RAW source changed during preview render")))
                                    :validation-function dependencies-current-p)
                                   (let ((display
                                           (materialize-preview-cache-hit
                                            output preview-session-directory
                                            :validation-function
                                            dependencies-current-p)))
                                     (unless (string= digest (photo-content-key input))
                                       (when display (ignore-errors (delete-file display)))
                                       (error "RAW source changed while loading cache hit"))
                                     (when (and display publish-p
                                                (= generation preview-generation))
                                       (queue-event queue
                                                    (list :preview generation index job
                                                          role display viewport
                                                          bound geometry))))
                                   (return))
                               (error (condition)
                                 (when (= attempt 2) (error condition)))))
                      ;; The interactive path adapts its debounce and draft
                      ;; use to how fast full previews actually come back.
                      (when (and publish-p (not draft-p) (eq role :after))
                        (setf last-render-ms
                              (/ (* 1000.0 (- (get-internal-real-time)
                                              render-started))
                                 internal-time-units-per-second))))))
                :front-p front-p :generation generation)))
           (enqueue-camera-thumbnail (job generation)
             ;; The camera's own JPEG, lifted out of the file in a few tens
             ;; of milliseconds, stands in until the develop is done: a card
             ;; of five hundred frames used to be a wall of grey squares for
             ;; minutes. Ahead of every develop in the queue, and no error is
             ;; reported — a file without a preview simply waits for its
             ;; develop like before.
             (enqueue-gui-task
              background-queue :camera-thumbnail
              (lambda ()
                (ignore-errors
                  (let* ((digest (photo-content-key-for job generation))
                         (output (camera-thumbnail-pathname preview-directory
                                                            digest)))
                    (call-with-preview-cache-fill
                     output
                     (lambda (temporary)
                       (orfeus:photo-embedded-preview
                        (photo-job-input-path job) temporary
                        :max-edge *thumbnail-preview-size* :quality 82)))
                    (let ((display (materialize-preview-cache-hit
                                    output preview-session-directory)))
                      (when display
                        (queue-event queue
                                     (list :thumbnail generation job display
                                           :camera)))))))
              :front-p t :generation generation))
           (enqueue-thumbnail (job generation)
             (enqueue-camera-thumbnail job generation)
             (enqueue-gui-task
              background-queue :thumbnail
              (lambda ()
                (handler-case
                    (loop for attempt below 3
                          for digest = (photo-content-key-for job generation
                                                              (plusp attempt))
                          for output = (thumbnail-pathname preview-directory digest)
                          do (handler-case
                                 (progn
                                   (call-with-preview-cache-fill
                                    output
                                    (lambda (temporary)
                                      (render-preview
                                       (photo-job-input-path job) temporary
                                       (neutral-preview-settings)
                                       :max-width *thumbnail-preview-size*
                                       :max-height *thumbnail-preview-size*
                                       :jpeg-quality 82 :if-exists :supersede)
                                      (unless (string= digest
                                                       (photo-content-key
                                                        (photo-job-input-path job)))
                                        (error "RAW source changed during thumbnail"))))
                                   (let ((display
                                           (materialize-preview-cache-hit
                                            output preview-session-directory)))
                                     (unless (string= digest
                                                      (photo-content-key
                                                       (photo-job-input-path job)))
                                       (when display (ignore-errors (delete-file display)))
                                       (error "RAW source changed while loading thumbnail"))
                                     (when display
                                       (queue-event queue
                                                    (list :thumbnail generation job
                                                          display :developed))))
                                   (return))
                               (error (condition)
                                 (when (= attempt 2) (error condition)))))
                  (error () nil)))))
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
                   (enqueue-thumbnail (nth index photos) generation)))
               (dolist (index indices)
                 (unless (= index selected-index)
                   (let ((job (nth index photos)))
                     (enqueue-render background-queue :after job index
                                     (settings-for-job job) nil nil))))))
           (draft-preview-fits-the-view-p ()
             (let* ((canvas (or after-canvas before-canvas))
                    (pixels (if canvas
                                (max (lightfast:widget-width canvas)
                                     (lightfast:widget-height canvas))
                                *gui-preview-minimum-bound*)))
               (draft-preview-fits-viewport-p pixels preview-zoom
                                              *gui-draft-preview-size*)))
           (enqueue-preview (initial-p)
             (let ((job (selected-job))
                   (index (gui-model-selected-index model))
                   (generation preview-generation))
               (when job
                 ;; A bounded draft lands quickly while the full-resolution
                 ;; preview renders behind it; both reuse the decoded RAW.
                 ;; Once checkpointed re-renders come back fast enough, the
                 ;; draft layer only adds churn, so it is skipped.
                 (let ((settings (current-settings))
                       (window (viewport-request)))
                   (when (and (>= last-render-ms 300) (not (live-view-p))
                              (null window)
                              (draft-preview-fits-the-view-p))
                     (enqueue-render queue :after job index settings
                                     generation t
                                     :front-p t :draft-p t :cache-p t))
                   (setf requested-viewport-region window)
                   (enqueue-render queue :after job index settings generation t
                                   :cache-p t :viewport window))
                 (enqueue-background-previews initial-p generation))))
           (schedule-initial-preview ()
             (when debounce-id
               (ignore-errors (lightfast:remove-timeout debounce-id))
               (setf debounce-id nil))
             ;; The render about to be abandoned will not report again, so its
             ;; last stage must not be left narrating a wait that is over.
             (setf render-stage nil
                   render-stage-fraction 0.0)
             (incf preview-generation)
             (setf progress-total 0
                   progress-generation preview-generation)
             (discard-gui-tasks queue :before)
             (discard-gui-tasks queue :after)
             (enqueue-preview t))
           (enqueue-live-preview ()
             ;; The drag hot path: bounded render straight into the back
             ;; buffer, swapped onto the canvas by the poll loop. Falls
             ;; back to the file path for photos still on flat settings.
             (let* ((job (selected-job))
                    (generation preview-generation)
                    (recipe (and job (current-settings))))
               (if (and job after-live-back
                        (<= preview-zoom 1.0001d0)
                        (typep recipe 'orfeus:processing-graph))
                   (let ((snapshot (preview-recipe-snapshot recipe))
                         (input (photo-job-input-path job)))
                     (enqueue-gui-task
                      queue :live
                      (lambda ()
                        (when (= generation preview-generation)
                          (let ((started (get-internal-real-time)))
                            (handler-case
                                (multiple-value-bind (width height)
                                    (orfeus:render-preview-rgb
                                     input snapshot
                                     after-live-back after-live-capacity
                                     :max-width *gui-live-preview-size*
                                     :max-height *gui-live-preview-size*
                                     :cache-p t)
                                  (setf live-render-ms
                                        (/ (* 1000.0
                                              (- (get-internal-real-time)
                                                 started))
                                           internal-time-units-per-second))
                                  (when (= generation preview-generation)
                                    (queue-event
                                     queue
                                     (list :live-preview generation job
                                           width height))))
                              (error (condition)
                                (queue-event
                                 queue
                                 (list :error
                                       (princ-to-string condition))))))))
                      :replace-kind :live :front-p t))
                   (enqueue-preview nil))))
           (schedule-live-settle ()
             ;; Once the drag quiets down, one file render persists the
             ;; result: the content-keyed cache, histogram, thumbnails,
             ;; and background previews all refresh from it.
             (when live-settle-id
               (ignore-errors (lightfast:remove-timeout live-settle-id)))
             (setf live-settle-id
                   (lightfast:add-timeout
                    0.5d0
                    (lambda ()
                      (setf live-settle-id nil)
                      (discard-gui-tasks queue :after)
                      (enqueue-preview nil)))))
           (schedule-edited-preview ()
             (incf preview-generation)
             (setf progress-total 0
                   progress-generation preview-generation)
             ;; Every edit comes through here, so the title's unsaved mark
             ;; follows every edit.
             (sync-window-title)
             (when debounce-id
               (ignore-errors (lightfast:remove-timeout debounce-id)))
             (setf debounce-id
                   (lightfast:add-timeout
                    ;; Checkpointed live renders come back in tens of
                    ;; milliseconds, so track drags tightly.
                    (if (< live-render-ms 250) 0.04d0
                        *preview-debounce-seconds*)
                    (lambda ()
                      (setf debounce-id nil)
                      (discard-gui-tasks queue :after)
                      (enqueue-live-preview)
                      (schedule-live-settle)))))
           (setting-changed (key widget &optional allow-empty)
             (handler-case
                 (let ((new-value (parse-number (lightfast:value widget)
                                                allow-empty)))
                   (when (and (eq key :white-balance-tint)
                              (null (gui-model-setting
                                     model :white-balance-temperature)))
                     (gui-model-set-setting
                      model :white-balance-temperature
                      (or (selected-as-shot-kelvin) 5500.0)))
                   (gui-model-set-setting model key new-value)
                   (sync-controls)
                   (when (member key '(:white-balance-temperature
                                       :white-balance-tint))
                     (setf (lightfast:value wb-choice) "Custom"))
                   (when graph-canvas (lightfast:redraw graph-canvas))
                   (schedule-edited-preview))
               (error (condition) (set-status (princ-to-string condition)))))
           (report-export-progress (index total job output)
             ;; A batch used to show one line for its whole run, so a long
             ;; export looked stalled. Name the file about to be written.
             (declare (ignore job))
             (queue-event queue
                          (list :status nil
                                (format nil "Exporting ~D of ~D: ~A"
                                        index total (file-namestring output))))
             (values))
           (render-selected ()
             (setf progress-total 0
                   progress-generation preview-generation)
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
             (setf progress-total 0
                   progress-generation preview-generation)
             (when (project-photos project)
               (ensure-directories-exist
                (merge-pathnames "placeholder" (project-output-directory project)))
               (enqueue-gui-task
                queue :export
                (lambda ()
                  (queue-event queue (list :status nil "Exporting all photos..."))
                  (multiple-value-bind (completed failures)
                      (project-render
                       project :if-exists :supersede :on-error :continue
                       :progress-callback #'report-export-progress)
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
           (render-jobs (jobs label)
             ;; Export an explicit set of photographs, reporting per-photo
             ;; failures without abandoning the rest of the batch.
             (setf progress-total 0
                   progress-generation preview-generation)
             (if (null jobs)
                 (set-status "No photographs selected to export")
                 (progn
                   (ensure-directories-exist
                    (merge-pathnames "placeholder"
                                     (project-output-directory project)))
                   (enqueue-gui-task
                    queue :export
                    (lambda ()
                      (queue-event queue
                                   (list :status nil
                                         (format nil "Exporting ~A..." label)))
                      (multiple-value-bind (completed failures)
                          (render-photo-jobs
                           project jobs
                           :if-exists :supersede :on-error :continue
                           :progress-callback #'report-export-progress)
                        (queue-event
                         queue
                         (if failures
                             (list :error
                                   (format nil "Exported ~D; ~D failed"
                                           (length completed)
                                           (length failures)))
                             (list :done
                                   (format nil "~D photograph~:P"
                                           (length completed)))))))))))
           (current-export-filename ()
             ;; What the current photograph would be written as, so the field
             ;; opens showing the name it is about to replace rather than blank.
             (let ((job (selected-job)))
               (when job
                 (let ((output (ignore-errors (gui-photo-output-path model job))))
                   (and output (file-namestring output))))))
           (apply-export-filename (format)
             ;; A name only, never a path: the folder has a field of its own,
             ;; and a relative output path is merged with it by the core. An
             ;; empty field puts the photograph back on its automatic name.
             (let ((job (selected-job))
                   (typed (string-trim '(#\Space #\Tab)
                                       (lightfast:value export-dialog-filename))))
               (when job
                 (setf (orfeus:photo-job-output-path job)
                       (when (plusp (length typed))
                         (let ((named (pathname typed)))
                           (make-pathname
                            :name (or (pathname-name named) typed)
                            :type (or (pathname-type named)
                                      (orfeus:export-format-extension format)))))))))
           (apply-export-dialog ()
             ;; Returns true once the dialog's choices are stored on the
             ;; project, so the caller may start the export.
             (handler-case
                 (progn
                   (update-project-export-settings
                    project
                    (lightfast:value export-dialog-destination)
                    (export-format-from-caption
                     (lightfast:value export-dialog-format))
                    (lightfast:value export-dialog-quality)
                    (lightfast:value export-dialog-width)
                    (lightfast:value export-dialog-height)
                    (string= "1" (lightfast:value export-dialog-metadata))
                    (string= "1" (lightfast:value export-dialog-timestamp)))
                   (sync-export-controls)
                   t)
               (error (condition)
                 (set-status (princ-to-string condition))
                 nil)))
           (start-dialog-export ()
             (when (apply-export-dialog)
               (lightfast:hide export-dialog)
               (let ((scope (lightfast:value export-dialog-scope)))
                 (when (string-equal scope "Current photograph")
                   (apply-export-filename
                    (export-format-from-caption
                     (lightfast:value export-dialog-format))))
                 (cond
                   ((string-equal scope "All photographs") (render-all))
                   ((string-equal scope "Selected photographs")
                    (render-jobs (gui-model-selected-jobs model)
                                 "the selection"))
                   (t (render-selected))))))
           (browse-export-destination ()
             (let ((chosen (lightfast:choose-directory
                            :title "Choose export folder"
                            :preset-path (lightfast:value
                                          export-dialog-destination))))
               (when (and chosen (plusp (length chosen)))
                 (setf (lightfast:value export-dialog-destination) chosen))))
           (build-export-dialog ()
             ;; Laid out by Lightfast's flex engine: rows of label, control and
             ;; trailing button, with the destination field taking the slack.
             ;; No pixel coordinates to keep in sync with one another.
             (let* ((dialog (lightfast:make-window :width 470 :height 390
                                                   :label "Export"))
                    (rows '())
                    (label-width 104)
                    (row-height 26))
               (setf export-dialog dialog)
               (labels ((field (label control &optional trailing)
                          ;; One labelled row; TRAILING is an optional button
                          ;; pinned to the right edge.
                          (push (lightfast:make-layout-row
                                 :basis row-height :shrink 0 :gap 8
                                 :children
                                 (append
                                  (list (lightfast:make-layout-item
                                         label :basis label-width :shrink 0)
                                        (lightfast:make-layout-item
                                         control :basis 0 :grow 1))
                                  (when trailing
                                    (list (lightfast:make-layout-item
                                           trailing :basis 88 :shrink 0)))))
                                rows)
                          control)
                        (text (caption)
                          (lightfast:make-label :parent dialog :x 0 :y 0
                                                :width label-width
                                                :height row-height
                                                :label caption))
                        (number-input ()
                          (lightfast:field-control
                           (lightfast:make-labeled-control
                            :int :parent dialog :x 0 :y 0 :width 120
                            :height row-height :label "" :label-width 0)))
                        (button (caption action width)
                          (lightfast:make-button
                           :parent dialog :x 0 :y 0 :width width
                           :height row-height :label caption
                           :callback (lambda (&rest ignored)
                                       (declare (ignore ignored))
                                       (funcall action)))))
                 (setf export-dialog-destination
                       (field (text "Destination")
                              (lightfast:make-input :parent dialog :x 0 :y 0
                                                    :width 200
                                                    :height row-height)
                              (lightfast:set-stock-icon
                               (button "Browse..."
                                       #'browse-export-destination 88)
                               :folder-open)))
                 ;; Only the single-photograph scope can honour a name; a batch
                 ;; has as many names as it has photographs. Left visible and
                 ;; labelled rather than hidden, so its scope is stated instead
                 ;; of discovered.
                 (setf export-dialog-filename
                       (field (text "File name")
                              (lightfast:make-input :parent dialog :x 0 :y 0
                                                    :width 200
                                                    :height row-height)))
                 (setf export-dialog-scope
                       (field (text "Photographs")
                              (lightfast:make-choice
                               :parent dialog :x 0 :y 0
                               :width 200 :height row-height
                               :items '("Current photograph"
                                        "Selected photographs"
                                        "All photographs"))))
                 (setf export-dialog-format
                       (field (text "Format")
                              (lightfast:make-choice
                               :parent dialog :x 0 :y 0
                               :width 200 :height row-height
                               :items (mapcar #'car
                                              *export-format-captions*))))
                 (setf export-dialog-quality
                       (field (text "JPEG quality") (number-input))
                       export-dialog-width
                       (field (text "Maximum width") (number-input))
                       export-dialog-height
                       (field (text "Maximum height") (number-input)))
                 (push (lightfast:make-layout-item
                        (text "0 keeps the full size")
                        :basis 20 :shrink 0)
                       rows)
                 (setf export-dialog-metadata
                       (lightfast:make-check-button
                        :parent dialog :x 0 :y 0 :width 200 :height 24
                        :label "Preserve metadata"))
                 (push (lightfast:make-layout-item export-dialog-metadata
                                                   :basis 24 :shrink 0)
                       rows)
                 (setf export-dialog-timestamp
                       (lightfast:make-check-button
                        :parent dialog :x 0 :y 0 :width 200 :height 24
                        :label "Timestamp filenames"))
                 (push (lightfast:make-layout-item export-dialog-timestamp
                                                   :basis 24 :shrink 0)
                       rows)
                 (push (lightfast:make-layout-row
                        :basis 28 :shrink 0 :gap 8 :justify :end
                        :children
                        (list (lightfast:make-layout-item
                               (button "Cancel"
                                       (lambda ()
                                         (lightfast:hide export-dialog))
                                       96)
                               :basis 96 :shrink 0)
                              (lightfast:make-layout-item
                               (lightfast:set-stock-icon
                                (button "Export" #'start-dialog-export 100)
                                :export)
                               :basis 100 :shrink 0)))
                       rows))
               (lightfast:layout-on-resize
                dialog
                (lightfast:make-layout-column :padding 12 :gap 8
                                              :children (nreverse rows)))
               dialog))
           (open-export-dialog (&optional scope)
             (unless export-dialog
               (build-export-dialog))
             (let ((settings (project-export-settings project)))
               (setf (lightfast:value export-dialog-destination)
                     (namestring (project-output-directory project))
                     (lightfast:value export-dialog-format)
                     (export-format-caption
                      (export-settings-format settings))
                     (lightfast:value export-dialog-quality)
                     (format nil "~D" (export-settings-jpeg-quality settings))
                     (lightfast:value export-dialog-width)
                     (format nil "~D" (or (export-settings-max-width settings)
                                          0))
                     (lightfast:value export-dialog-height)
                     (format nil "~D" (or (export-settings-max-height settings)
                                          0))
                     (lightfast:value export-dialog-metadata)
                     (if (export-settings-preserve-metadata-p settings) "1" "0")
                     (lightfast:value export-dialog-timestamp)
                     (if (export-settings-timestamp-filenames-p settings)
                         "1" "0")
                     (lightfast:value export-dialog-filename)
                     (or (current-export-filename) "")
                     (lightfast:value export-dialog-scope)
                     (or scope
                         (if (selected-job)
                             "Current photograph"
                             "All photographs"))))
             (place-dialog-over export-dialog window)
             (lightfast:show export-dialog))
           (selected-lens-profile-status ()
             ;; What the renderer will correct the current photograph with,
             ;; honouring a profile chosen by hand on its optics node.
             (let ((job (selected-job)))
               (when job
                 (handler-case
                     (photo-lens-profile-status
                      (photo-job-input-path job)
                      :profile (gui-model-setting model :lens-profile)
                      :focal-length (gui-model-setting model :lens-focal-length))
                   (error (condition)
                     (values nil (princ-to-string condition)))))))
           (lens-profile-summary (record)
             (format nil "~A~@[  (~{~A~^, ~})~]"
                     (getf record :display)
                     (remove nil
                             (list (and (getf record :distortion-p) "distortion")
                                   (and (getf record :chromatic-aberration-p)
                                        "colour fringing")
                                   (and (getf record :vignetting-p)
                                        "vignetting")))))
           (sync-lens-profile-label ()
             (when lens-profile-label
               (multiple-value-bind (text tooltip)
                   (let ((job (selected-job)))
                     (cond ((null job)
                            (values "No photograph selected" ""))
                           (t
                            (multiple-value-bind (record message)
                                (selected-lens-profile-status)
                              (cond (record
                                     (let ((summary (lens-profile-summary record)))
                                       (values
                                        (let ((prefix (if (gui-model-setting
                                                           model :lens-profile)
                                                          "Chosen profile: "
                                                          "Profile: ")))
                                          (format nil "~A~A" prefix
                                                  (if (> (+ (length prefix)
                                                            (length summary))
                                                         46)
                                                      (format nil "~A..."
                                                              (subseq summary 0
                                                                      (- 43 (length prefix))))
                                                      summary)))
                                        (format nil "~A~%~A" summary
                                                (or (getf record :maker) "")))))
                                    (t
                                     (values "Profile: none found for this lens"
                                             (or message ""))))))))
                 (setf (lightfast:label lens-profile-label) text)
                 (lightfast:set-tooltip lens-profile-label tooltip))))
           (clear-lens-profile ()
             (when (selected-job)
               (gui-model-set-setting model :lens-profile nil)
               (gui-model-set-setting model :lens-focal-length nil)
               (sync-controls)
               (schedule-edited-preview)
               (set-status "Lens profile follows the photograph's metadata")))
           (refresh-lens-picker ()
             ;; Everything the search names, mountable on this body first.
             (let* ((job (selected-job))
                    (path (and job (photo-job-input-path job)))
                    (focal (or (ignore-errors
                                 (parse-number (lightfast:value lens-picker-focal)))
                               (and path (ignore-errors (photo-focal-length path)))
                               0.0))
                    (records (handler-case
                                 (native-lens-profiles
                                  :make (and path (ignore-errors (photo-camera-make path)))
                                  :model (and path (ignore-errors (photo-camera-model path)))
                                  :query (lightfast:value lens-picker-search)
                                  :focal focal)
                               (error (condition)
                                 (set-status (princ-to-string condition))
                                 '())))
                    (chosen (gui-model-setting model :lens-profile)))
               (setf lens-picker-records records)
               (lightfast:clear lens-picker-list)
               (loop for record in records
                     for index from 0
                     do (lightfast:add-item
                         lens-picker-list
                         (format nil "~A~C~A~C~A~A~A"
                                 (getf record :display) #\Tab
                                 (getf record :maker) #\Tab
                                 (if (getf record :distortion-p) "D" "-")
                                 (if (getf record :chromatic-aberration-p) "T" "-")
                                 (if (getf record :vignetting-p) "V" "-")))
                        (when (and chosen (string= chosen (getf record :model)))
                          (lightfast:browser-select lens-picker-list index)))
               (setf (lightfast:label lens-picker-caption)
                     (format nil "~D profile~:P~@[ for ~A~]~@[; searching \"~A\"~]"
                             (length records)
                             (and path (ignore-errors (photo-camera-model path)))
                             (let ((query (lightfast:value lens-picker-search)))
                               (and (plusp (length query)) query))))))
           (accept-lens-picker ()
             (let* ((indices (lightfast:browser-selected-indices lens-picker-list))
                    (record (and indices (nth (first indices) lens-picker-records)))
                    (job (selected-job)))
               (cond ((null job)
                      (set-status "Select a photograph first"))
                     ((null record)
                      (set-status "Select a lens profile in the list"))
                     (t
                      (let* ((focal (ignore-errors
                                      (parse-number
                                       (lightfast:value lens-picker-focal))))
                             (path (photo-job-input-path job))
                             (description (ignore-errors
                                            (photo-lens-description path))))
                        (gui-model-set-setting model :lens-profile
                                               (getf record :model))
                        (gui-model-set-setting model :lens-focal-length
                                               (and focal (plusp focal)
                                                    (float focal 1.0)))
                        (when (and description
                                   (string/= "0"
                                             (lightfast:value lens-picker-remember)))
                          (handler-case
                              (progn
                                (lens-profile-alias-save
                                 description
                                 (getf record :model)
                                 (alias-body-crop-factor path
                                                         (getf record :model)
                                                         (and focal (plusp focal)
                                                              focal)))
                                (lens-profile-status-forget))
                            (error (condition)
                              (set-status (princ-to-string condition)))))
                        (lightfast:hide lens-picker)
                        (sync-controls)
                        (schedule-edited-preview)
                        (set-status (format nil "Lens profile: ~A"
                                            (getf record :display))))))))
           (build-lens-picker ()
             ;; A search box over the lens database, the matches beneath it,
             ;; and the two facts the profile needs that the photograph may
             ;; not carry: the focal length, and whether to remember the choice
             ;; for every photograph whose metadata describes the lens the same
             ;; way. Laid out by the flex engine like the export dialog.
             (let* ((dialog (lightfast:make-window :width 640 :height 470
                                                   :label "Choose Lens Profile"))
                    (rows '())
                    (row-height 26)
                    (label-width 130))
               (setf lens-picker dialog)
               (labels ((text (caption &optional (width label-width))
                          (lightfast:make-label :parent dialog :x 0 :y 0
                                                :width width :height row-height
                                                :label caption))
                        (field (label control)
                          (push (lightfast:make-layout-row
                                 :basis row-height :shrink 0 :gap 8
                                 :children
                                 (list (lightfast:make-layout-item
                                        label :basis label-width :shrink 0)
                                       (lightfast:make-layout-item
                                        control :basis 0 :grow 1)))
                                rows)
                          control)
                        (button (caption action width)
                          (lightfast:make-button
                           :parent dialog :x 0 :y 0 :width width
                           :height row-height :label caption
                           :callback (lambda (&rest ignored)
                                       (declare (ignore ignored))
                                       (funcall action)))))
                 (setf lens-picker-caption (text "" 600))
                 (push (lightfast:make-layout-item lens-picker-caption
                                                   :basis row-height :shrink 0)
                       rows)
                 (setf lens-picker-search
                       (field (text "Search")
                              (lightfast:make-input
                               :parent dialog :x 0 :y 0 :width 300
                               :height row-height
                               :callback (lambda (&rest ignored)
                                           (declare (ignore ignored))
                                           (refresh-lens-picker)))))
                 (setf lens-picker-list
                       (lightfast:make-browser :parent dialog :x 0 :y 0
                                               :width 600 :height 200
                                               :column-widths '(370 150 60)
                                               :callback
                                               (lambda (&rest ignored)
                                                 (declare (ignore ignored)))))
                 (push (lightfast:make-layout-item lens-picker-list
                                                   :basis 0 :grow 1)
                       rows)
                 (push (lightfast:make-layout-item
                        (text (format nil "D distortion, T colour fringing, V vignetting. ~
                                           Profiles this body can mount come first.")
                              600)
                        :basis 20 :shrink 0)
                       rows)
                 (setf lens-picker-focal
                       (field (text "Focal length (mm)")
                              (lightfast:make-input
                               :parent dialog :x 0 :y 0 :width 120
                               :height row-height)))
                 (setf lens-picker-remember
                       (lightfast:make-check-button
                        :parent dialog :x 0 :y 0 :width 600 :height 24
                        :label "Remember for every photograph of this lens"))
                 (push (lightfast:make-layout-item lens-picker-remember
                                                   :basis 24 :shrink 0)
                       rows)
                 (push (lightfast:make-layout-row
                        :basis 28 :shrink 0 :gap 8 :justify :end
                        :children
                        (list (lightfast:make-layout-item
                               (button "Cancel"
                                       (lambda () (lightfast:hide lens-picker))
                                       96)
                               :basis 96 :shrink 0)
                              (lightfast:make-layout-item
                               (lightfast:set-stock-icon
                                (button "Use Profile" #'accept-lens-picker 120)
                                :success)
                               :basis 120 :shrink 0)))
                       rows))
               (lightfast:layout-on-resize
                dialog
                (lightfast:make-layout-column :padding 12 :gap 8
                                              :children (nreverse rows)))
               dialog))
           (open-lens-picker ()
             (let ((job (selected-job)))
               (cond
                 ((null job)
                  (set-status "Select a photograph to choose a lens profile for"))
                 (t
                  (unless lens-picker
                    (build-lens-picker))
                  (let* ((path (photo-job-input-path job))
                         (description (ignore-errors (photo-lens-description path)))
                         (focal (or (gui-model-setting model :lens-focal-length)
                                    (ignore-errors (photo-focal-length path)))))
                    ;; Start the search from what the camera wrote down: the
                    ;; words of its description usually appear in the profile's
                    ;; name, however differently the two are punctuated.
                    (setf (lightfast:value lens-picker-search)
                          (or description "")
                          (lightfast:value lens-picker-focal)
                          (if focal (display-number focal) "")
                          (lightfast:label lens-picker-remember)
                          (if description
                              (format nil "Remember for every photograph described as ~S"
                                      description)
                              "This photograph names no lens, so the choice is its own")
                          (lightfast:value lens-picker-remember)
                          (if description "1" "0"))
                    (refresh-lens-picker)
                    (place-dialog-over lens-picker window)
                    (lightfast:show lens-picker))))))
           (choose-lut ()
             (let ((path (lightfast:choose-file
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
             (if (string-equal (lightfast:value widget) "As shot")
                 (progn
                   (gui-model-set-setting model :white-balance-temperature nil)
                   (gui-model-set-setting model :white-balance-tint 0.0))
                 ;; Custom starts at the frame's own temperature, which
                 ;; renders exactly what "as shot" rendered — so switching
                 ;; modes shows the number without moving the picture.
                 (unless (gui-model-setting model :white-balance-temperature)
                   (gui-model-set-setting
                    model :white-balance-temperature
                    (or (selected-as-shot-kelvin) 5500.0))))
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
             (push widget inspector-widgets)
             widget)
           (register-inspector-row (layout widgets parent x y height
                                    &optional (basis :root))
             ;; A row laid out by the flex engine rather than by per-widget
             ;; coordinates. Only its origin and height are fixed here; the
             ;; engine divides the width. PARENT must be the widgets' actual
             ;; FLTK parent: the flex engine places children relative to it and
             ;; refuses a mismatch, and :PAGE covers two different tab pages.
             (push (list layout parent x y height basis) inspector-rows)
             ;; WIDGETS is what the row places. It has to reach
             ;; INSPECTOR-WIDGETS or BUILD-GROUP cannot hide the row when its
             ;; correction is not the selected one, and panels pile up on top
             ;; of one another.
             (dolist (widget widgets)
               (push widget inspector-widgets))
             layout)
           (register-field (field y &optional (basis :root))
             (register-inspector-row
              (labeled-field-layout (lightfast:field-label field)
                                    (lightfast:field-control field)
                                    (if (eq basis :page) 88 96))
              (list (lightfast:field-label field)
                    (lightfast:field-control field))
              (lightfast:widget-parent (lightfast:field-label field))
              (if (eq basis :page) 12 8) y 26 basis)
             field)
           (build-base-panel (top tooltip)
             ;; The film base as three editable channels rather than behind a
             ;; modal chooser: it is the value being graded, so it belongs on
             ;; the panel where it can be nudged and read at a glance. Shared
             ;; by both inversions; returns the three fields and the swatch.
             (flet ((channel-field (key label y)
                      (register-inspector
                       (lightfast:make-label :parent node-page :x 12 :y y
                                             :width 88 :height 26 :label label)
                       12 y 88 26 :page)
                      (let ((spinner
                              (register-inspector
                               (lightfast:make-spinner
                                :parent node-page :x 110 :y y
                                :width 84 :height 26
                                :callback
                                (lambda (widget event value)
                                  (declare (ignore event value))
                                  (let ((node (gui-model-selected-graph-node
                                               model)))
                                    (when node
                                      (handler-case
                                          (set-base-channel
                                           node key
                                           (parse-number
                                            (lightfast:value widget)))
                                        (error (condition)
                                          (set-status
                                           (princ-to-string condition))))))))
                               110 y 84 26 :page)))
                        ;; Displayed encoded, the way the colour reads to the
                        ;; eye, while the node stores it scene-linear.
                        (lightfast:set-range spinner 0 100)
                        (lightfast:set-step spinner 0.5)
                        spinner))
                    (base-button (y label icon action)
                      (register-inspector
                       (lightfast:set-stock-icon
                        (lightfast:make-button
                         :parent node-page :x 12 :y y :width 292 :height 26
                         :label label
                         :callback
                         (lambda (&rest ignored)
                           (declare (ignore ignored))
                           (let ((node (gui-model-selected-graph-node model)))
                             (when node (funcall action node)))))
                        icon)
                       12 y :fill 26 :page)))
               (let ((red (channel-field :red "Base red %" top))
                     (green (channel-field :green "Base green %" (+ top 32)))
                     (blue (channel-field :blue "Base blue %" (+ top 64)))
                     (swatch (register-inspector
                              (lightfast:make-box :parent node-page
                                                  :x 204 :y top
                                                  :width 100 :height 90
                                                  :label "")
                              204 top '(:rest 100) 90 :page)))
                 (lightfast:set-box swatch lightfast:+box-flat-box+)
                 (lightfast:set-tooltip swatch tooltip)
                 (base-button (+ top 96) "Sample Base From Photo" :dropper
                              (lambda (node)
                                (setf pick-color-node node)
                                (set-preview-cursor :cross)
                                (set-status
                                 "Click the preview to sample the film base")))
                 (base-button (+ top 128) "Auto Base From Border" :film-border
                              #'auto-base-from-border)
                 (values red green blue swatch))))
           (build-group (kind builder)
             ;; Collects everything BUILDER registers into one Node panel
             ;; visibility group; SYNC-NODE-TOOLS shows exactly one of them.
             ;; Every registration path feeds INSPECTOR-WIDGETS, so whatever
             ;; BUILDER creates is captured however it chose to lay itself out.
             (let ((before (length inspector-widgets)))
               (funcall builder)
               (push (cons kind
                           (subseq inspector-widgets 0
                                   (- (length inspector-widgets) before)))
                     node-panel-groups)))
           (section-frame (parent title y height)
             ;; A classic engraved group frame whose title interrupts the
             ;; frame line, drawn behind the controls it surrounds.
             (let ((frame (lightfast:make-box :parent parent :x 4 :y y
                                            :width 300 :height height
                                            :label "")))
               (lightfast:set-box frame lightfast:+box-engraved-frame+)
               (register-inspector frame 4 y :frame height :page))
             (let ((title-width (+ 14 (* 7 (length title))))
                   (title-label (lightfast:make-label
                                 :parent parent :x 14 :y (- y 8)
                                 :width 80 :height 16 :label title)))
               (lightfast:set-box title-label lightfast:+box-flat-box+)
               (lightfast:set-label-font title-label
                                       lightfast:+font-helvetica-bold+)
               (register-inspector title-label 14 (- y 8) title-width 16
                                   :page)))
           (make-number-field (key label minimum maximum step y parent)
             (let* ((label-widget
                      (lightfast:make-label :parent parent :x 12 :y y
                                          :width 88 :height 26 :label label))
                    (callback (lambda (widget event value)
                                (declare (ignore event value))
                                (setting-changed key widget)))
                    (spinner (lightfast:make-spinner
                              :parent parent :x 202 :y y
                              :width 78 :height 26 :callback callback))
                    (slider (lightfast:make-slider
                             :parent parent :x 110 :y y
                             :width 84 :height 26 :callback callback)))
               (dolist (widget (list slider spinner))
                 (lightfast:set-range widget minimum maximum)
                 (lightfast:set-step widget step)
                 (push (list key widget) controls))
               (register-inspector-row
                (number-field-layout label-widget slider spinner)
                (list label-widget slider spinner)
                parent 12 y 26 :page)
               spinner))
           (make-node-number-field (key label minimum maximum step default y
                                    parent)
             ;; Like MAKE-NUMBER-FIELD, but for a node kind that carries its own
             ;; parameters rather than one of the flat pipeline's settings: the
             ;; value belongs to this node, so it is written straight to it.
             (let* ((label-widget
                      (lightfast:make-label :parent parent :x 12 :y y
                                            :width 88 :height 26 :label label))
                    (callback
                      (lambda (widget event value)
                        (declare (ignore event value))
                        (handler-case
                            (let ((node (gui-model-selected-graph-node model))
                                  (number (parse-number
                                           (lightfast:value widget))))
                              (when node
                                (gui-model-set-node-params
                                 model node
                                 (list key (float (max minimum
                                                       (min maximum number))
                                                  1.0)))
                                (sync-node-tools)
                                (when graph-canvas
                                  (lightfast:redraw graph-canvas))
                                (schedule-edited-preview)))
                          (error (condition)
                            (set-status (princ-to-string condition))))))
                    (spinner (lightfast:make-spinner
                              :parent parent :x 202 :y y
                              :width 78 :height 26 :callback callback))
                    (slider (lightfast:make-slider
                             :parent parent :x 110 :y y
                             :width 84 :height 26 :callback callback)))
               (dolist (widget (list slider spinner))
                 (lightfast:set-range widget minimum maximum)
                 (lightfast:set-step widget step)
                 (push (list key widget default) node-param-controls))
               (register-inspector-row
                (number-field-layout label-widget slider spinner)
                (list label-widget slider spinner)
                parent 12 y 26 :page)
               spinner))
           (make-tone-band (key short-label full-label)
             (let* ((callback (lambda (widget event value)
                                (declare (ignore event value))
                                (setting-changed key widget)))
                    (label (lightfast:make-label
                            :parent node-page :x 0 :y 92 :width 32 :height 22
                            :label short-label))
                    (slider (lightfast:make-vertical-slider
                             :parent node-page :x 0 :y 116 :width 20 :height 120
                             :callback callback))
                    (input (lightfast:make-value-input
                            :parent node-page :x 0 :y 240 :width 32 :height 24
                            :callback callback)))
               (dolist (widget (list slider input))
                 (lightfast:set-range widget -2 2)
                 (lightfast:set-step widget 0.1)
                 (lightfast:set-tooltip widget full-label)
                 (push (list key widget) controls))
               (dolist (widget (list label slider input))
                 (push widget inspector-widgets))
               (push (tone-band-layout label slider input) tone-items)))
           (layout-left-pane (&optional ignored)
             (declare (ignore ignored))
             (let ((width (lightfast:widget-width filmstrip-pane))
                   (height (lightfast:widget-height filmstrip-pane))
                   (header-height 28))
               (when photo-selection-label
                 (lightfast:resize-widget photo-selection-label :x 8 :y 2
                                        :width (- width 16) :height 24))
               (lightfast:resize-widget
                thumbnail-canvas :x 0 :y header-height
                :width (max 40 (- width 16)) :height (- height header-height))
               (when thumbnail-scrollbar
                 (lightfast:resize-widget
                  thumbnail-scrollbar :x (max 40 (- width 16)) :y header-height
                  :width 16 :height (- height header-height))))
             (redraw-thumbnails)
             (lightfast:redraw filmstrip-pane))
           (layout-center-pane (&optional ignored)
             (declare (ignore ignored))
             (let* ((center (lightfast:widget-width center-pane))
                    (main-height (lightfast:widget-height center-pane))
                    (caption-height 22)
                    (viewer-height (max 100 (- main-height caption-height)))
                    (gutter 6)
                    (pane-width (floor (- center gutter) 2)))
               (if comparison-p
                   (progn
                     (lightfast:show before-caption)
                     (lightfast:show before-canvas)
                     (lightfast:resize-widget before-caption :x 0 :y 0
                                            :width pane-width :height caption-height)
                     (lightfast:resize-widget before-canvas :x 0 :y caption-height
                                            :width pane-width :height viewer-height)
                     (lightfast:resize-widget after-caption
                                            :x (+ pane-width gutter) :y 0
                                            :width (- center pane-width gutter)
                                            :height caption-height)
                     (lightfast:resize-widget after-canvas
                                            :x (+ pane-width gutter) :y caption-height
                                            :width (- center pane-width gutter)
                                            :height viewer-height))
                   (progn
                     (lightfast:hide before-caption)
                     (lightfast:hide before-canvas)
                     (lightfast:resize-widget after-caption :x 0 :y 0
                                            :width center :height caption-height)
                     (lightfast:resize-widget after-canvas :x 0 :y caption-height
                                            :width center :height viewer-height)))
               (lightfast:redraw center-pane)))
           (layout-graph-pane (&optional ignored)
             (declare (ignore ignored))
             (let* ((width (lightfast:widget-width graph-pane))
                    (height (lightfast:widget-height graph-pane))
                    (button-width (max 48 (floor (- width 32) 3))))
               ;; A drag of the tile divider is the only thing that may change
               ;; the remembered share; our own layout pass must not ratchet it.
               (unless applying-layout-p
                 (let ((column (lightfast:widget-height right-column)))
                   (when (plusp column)
                     (setf graph-height-fraction
                           (max 1/5 (min 4/5 (/ height column)))))))
               (when graph-title
                 (lightfast:resize-widget graph-title :x 8 :y 4
                                        :width (- width 16) :height 18))
               (when still-button
                 (lightfast:resize-widget still-button
                                        :x 8 :y 24
                                        :width button-width :height 22)
                 (lightfast:resize-widget copy-grade-button
                                        :x (+ 12 button-width) :y 24
                                        :width button-width :height 22)
                 (lightfast:resize-widget paste-grade-button
                                        :x (+ 16 (* 2 button-width)) :y 24
                                        :width button-width :height 22))
               (when graph-canvas
                 (lightfast:resize-widget graph-canvas :x 2 :y 52
                                        :width (max 40 (- width 22))
                                        :height (max 60 (- height 54))))
               (when graph-scrollbar
                 (lightfast:resize-widget graph-scrollbar
                                        :x (max 42 (- width 18)) :y 52
                                        :width 16
                                        :height (max 60 (- height 54))))
               (lightfast:redraw graph-pane)))
           (layout-gallery-pane (&optional ignored)
             (declare (ignore ignored))
             (let* ((width (lightfast:widget-width gallery-pane))
                    (height (lightfast:widget-height gallery-pane))
                    (save-width 88)
                    (apply-width (max 120 (- width save-width 24))))
               (when gallery-title
                 (lightfast:resize-widget gallery-title :x 8 :y 2
                                        :width (- width 16) :height 18))
               (when gallery-canvas
                 (lightfast:resize-widget gallery-canvas :x 4 :y 22
                                        :width (- width 8)
                                        :height (max 60 (- height 88))))
               (when preset-name-input
                 (lightfast:resize-widget preset-name-input
                                        :x 8 :y (- height 60)
                                        :width (- width 16) :height 24))
               (when preset-apply-button
                 (lightfast:resize-widget preset-apply-button
                                        :x (+ 16 save-width) :y (- height 32)
                                        :width apply-width :height 26))
               (when preset-save-button
                 (lightfast:resize-widget preset-save-button
                                        :x 8 :y (- height 32)
                                        :width save-width :height 26))
               (lightfast:redraw gallery-pane)))
           (layout-inspector-pane (&optional ignored)
             (declare (ignore ignored))
             (let* ((right (lightfast:widget-width inspector))
                    (main-height (lightfast:widget-height inspector))
                    (page-height (max 150 (- main-height 108)))
                    ;; The curves panel splits whatever the page has left
                    ;; between the waveform and the chart, with the reset
                    ;; button pinned to the last row.
                    (curve-space (max 130 (- page-height 76 38)))
                    (scope-height (max 56 (floor (* curve-space 2) 5)))
                    (chart-height (max 70 (- curve-space scope-height 8))))
               (lightfast:resize-widget tabs :x 4 :y 40
                                      :width (- right 8)
                                      :height (- main-height 80))
               (dolist (page (list node-page export-page))
                 (lightfast:resize-widget page :x 2 :y 24
                                        :width (- right 12)
                                        :height page-height))
               (dolist (item inspector-items)
                 (destructuring-bind
                     (widget x y width-mode item-height basis) item
                   (let* ((basis-width (if (eq basis :page)
                                           (- right 12)
                                           right))
                          ;; Widths that follow the page rather than the
                          ;; designer: a control that stopped at a fixed pixel
                          ;; hung out of a narrowed sidebar. Numbers stay as
                          ;; they are; the keywords and lists are measured
                          ;; from the page's 12-pixel margins.
                          (item-width
                            (cond
                              ((eq width-mode :scope-control)
                               (max 100 (- right 98)))
                              ((eq width-mode :control)
                               (max 100 (- basis-width 118)))
                              ((eq width-mode :frame) (max 120 (- basis-width 8)))
                              ((eq width-mode :fill) (max 100 (- basis-width 24)))
                              ;; (:rest LIMIT): from X to the right margin, at
                              ;; most LIMIT wide.
                              ((and (consp width-mode) (eq (first width-mode) :rest))
                               (max 24 (min (second width-mode)
                                            (- basis-width 12 x))))
                              ;; (:before WIDTH): from X up to a right-anchored
                              ;; neighbour WIDTH wide, a gap between.
                              ((and (consp width-mode) (eq (first width-mode) :before))
                               (max 60 (- basis-width 12 (second width-mode) 8 x)))
                              ;; (:share N): one of N equal columns with gaps.
                              ((and (consp width-mode) (eq (first width-mode) :share))
                               (max 40 (floor (- basis-width 24
                                                 (* 8 (1- (second width-mode))))
                                              (second width-mode))))
                              (t width-mode)))
                          (item-x (cond
                                    ;; Against the right margin.
                                    ((eq x :right) (- basis-width 12 item-width))
                                    ;; (:column I) of a (:share N) row.
                                    ((and (consp x) (eq (first x) :column))
                                     (+ 12 (* (second x) (+ item-width 8))))
                                    (t x)))
                          (item-y (case y
                                    (:action-row (- main-height 34))
                                    (:chart-row (+ 84 scope-height))
                                    (:page-action-row (- page-height 30))
                                    (otherwise y)))
                          (item-height (case item-height
                                         (:scope scope-height)
                                         (:chart chart-height)
                                         (otherwise item-height))))
                     (lightfast:resize-widget
                      widget :x item-x :y item-y
                      :width item-width :height item-height))))
               (dolist (row inspector-rows)
                 (destructuring-bind (layout parent x y height basis) row
                   (let ((width (if (eq basis :page) (- right 12) right)))
                     (lightfast:apply-layout
                      layout
                      (lightfast:make-rect :x x
                                           :y (case y
                                                (:action-row (- main-height 34))
                                                (:page-action-row
                                                 (- page-height 30))
                                                (otherwise y))
                                           :width (max 120 (- width x 8))
                                           :height height)
                      :parent parent))))
               (when tone-items
                 (lightfast:apply-layout
                  (tone-bands-layout (reverse tone-items))
                  (lightfast:make-rect :x 14 :y 92
                                       :width (max 70 (- right 40))
                                       :height (max 100 (- page-height 100)))
                  :parent node-page))
               (lightfast:redraw inspector)))
           (layout-ui (&optional ignored)
             (declare (ignore ignored))
             (setf applying-layout-p t)
             (unwind-protect
             (let* ((width (lightfast:widget-width window))
                    (height (lightfast:widget-height window)))
               (lightfast:apply-layout
                root-layout
                (lightfast:make-rect :width width :height height)
                :parent window)
               (let* ((main-height (lightfast:widget-height main-tile))
                      (left (min (max *left-sidebar-min-width* (- width 580))
                               480
                               (max *left-sidebar-min-width*
                                    (if layout-initialized-p
                                        (lightfast:widget-width left-column)
                                        *left-sidebar-initial-width*))))
                    (right (min (max *right-sidebar-min-width*
                                     (- width left 300))
                                480
                                (max *right-sidebar-min-width*
                                     (if layout-initialized-p
                                         (lightfast:widget-width right-column)
                                         (floor (* width 3) 8)))))
                    (center (- width left right)))
               (when toolbar-bottom-rule
                 (lightfast:resize-widget toolbar-bottom-rule :x 0 :y 38
                                        :width width :height 2))
               (when lens-name
                 ;; Kept in step with the toolbar's last button rather than
                 ;; pinned: the resize runs on every window change and would
                 ;; otherwise drag the label back over the zoom controls.
                 (lightfast:resize-widget lens-name :x 402 :y 6
                                        :width (max 120 (- width 412)) :height 28))
               (lightfast:resize-widget left-column :x 0 :y 0
                                      :width left :height main-height)
               (lightfast:resize-widget center-pane :x left :y 0
                                      :width center :height main-height)
               (lightfast:resize-widget right-column :x (+ left center) :y 0
                                      :width right :height main-height)
               ;; The left column splits into filmstrip and stills gallery,
               ;; the right into the Node panel and the graph editor.
               (let ((gallery-height
                       (min (- main-height 200)
                            (max 160
                                 (if layout-initialized-p
                                     (lightfast:widget-height gallery-pane)
                                     (min 300 (floor main-height 3))))))
                     (graph-height
                       (min (- main-height *inspector-min-height*)
                            (max 160
                                 (round (* main-height
                                           graph-height-fraction))))))
                 (lightfast:resize-widget filmstrip-pane :x 0 :y 0
                                        :width left
                                        :height (- main-height
                                                   gallery-height))
                 (lightfast:resize-widget gallery-pane :x 0
                                        :y (- main-height gallery-height)
                                        :width left :height gallery-height)
                 (lightfast:resize-widget inspector :x 0 :y 0
                                        :width right
                                        :height (- main-height graph-height))
                 (lightfast:resize-widget graph-pane :x 0
                                        :y (- main-height graph-height)
                                        :width right :height graph-height))
               (layout-left-pane)
               (layout-gallery-pane)
               (layout-center-pane)
               (layout-inspector-pane)
               (layout-graph-pane)
               (unless layout-initialized-p
                 (lightfast:init-sizes main-tile)
                 (lightfast:init-sizes left-column)
                 (lightfast:init-sizes right-column))
               (setf layout-initialized-p t)))
               (setf applying-layout-p nil)))
           (poll ()
             (dolist (event (drain-events queue))
               (case (first event)
                 (:status
                  (when (or (null (second event))
                            (= (second event) preview-generation))
                    (set-status (third event))))
                 (:preview
                  (when (gui-preview-event-current-p model event preview-generation)
                    (publish-preview (fifth event) (sixth event) (second event)
                                     (published-region (sixth event)
                                                       (seventh event)
                                                       (eighth event))
                                     (ninth event))
                    (setf render-stage nil
                          render-stage-fraction 0.0)
                    (set-status (render-summary-text))))
                 (:live-preview
                  (when (and (= (second event) preview-generation)
                             (eq (third event) (selected-job)))
                    (rotatef after-live-front after-live-back)
                    (setf after-live-width (fourth event)
                          after-live-height (fifth event)
                          after-live-p t)
                    (incf after-live-generation)
                    (when after-canvas (lightfast:redraw after-canvas))
                    ;; The scope now trails the image, so repaint its notice.
                    (when scope-canvas (lightfast:redraw scope-canvas))))
                 (:histogram
                  (if (and (= (second event) preview-generation)
                           (equal (third event) after-preview-file))
                      (progn
                        (release-waveform)
                        (setf curve-histogram (fourth event)
                              waveform-buffer (fifth event))
                        (when curve-canvas (lightfast:redraw curve-canvas))
                        (when scope-canvas (lightfast:redraw scope-canvas)))
                      (cffi:foreign-free (fifth event))))
                 (:thumbnail
                  (destructuring-bind (generation job display
                                       &optional (grade :developed))
                      (rest event)
                    (declare (ignore generation))
                    (when (and (member job (project-photos project) :test #'eq)
                               ;; The camera's preview stands in only while
                               ;; there is nothing; the develop replaces it.
                               (or (null (gethash job thumbnail-files))
                                   (and (eq grade :developed)
                                        (not (eq :developed
                                                 (gethash job thumbnail-grades))))))
                      (setf (gethash job thumbnail-files) display
                            (gethash job thumbnail-grades) grade)
                      ;; A thumbnail is also the first time this photograph can
                      ;; be compared with its neighbours, so the bursts are worth
                      ;; asking about again.
                      (refresh-photo-groups)
                      (redraw-thumbnails))))
                 (:photo-groups
                  (when (third event)
                    (apply-photo-order (third event) (fourth event)))
                  (setf photo-groups (second event))
                  (redraw-thumbnails))
                 (:render-stage
                  ;; Only the render the interface is waiting for; a stale one
                  ;; still finishing must not narrate over it.
                  (when (= (second event) preview-generation)
                    (setf render-stage (third event)
                          render-stage-fraction (fourth event))
                    (set-status (render-activity-text))
                    (show-render-progress)))
                 (:photo-focus
                  (when (member (second event) (project-photos project) :test #'eq)
                    (setf (gethash (second event) photo-focus-reports)
                          (or (third event) :unmeasurable))
                    (when (zerop (mod (fourth event) 8))
                      (set-status (format nil "Measuring focus ~D of ~D"
                                          (fourth event) (fifth event))))
                    (redraw-thumbnails)))
                 (:photo-focus-done
                  (setf focus-pass-running-p nil)
                  (when (second event) (funcall (second event))))
                 (:still-error
                  (when (gallery-generation-event-current-p event
                                                            gallery-generation)
                    (set-status (third event))))
                 (:still-thumb
                  (let ((still (and (gallery-generation-event-current-p
                                     event gallery-generation)
                                    (find (third event) gallery-stills
                                          :key #'gallery-still-key
                                          :test #'equal))))
                    (when still
                      (when (fifth event)
                        (ignore-errors
                          (orfeus:still-store-write-thumbnail
                           (processing-preset-name
                            (gallery-still-preset still))
                           (fourth event))))
                      (setf (gethash (third event) gallery-thumbs)
                            (fourth event))
                      (when gallery-canvas (lightfast:redraw gallery-canvas)))))
                 (:done
                  (set-status (format nil "Exported ~A" (second event))))
                 (:error
                  (set-status (format nil "Error: ~A" (second event))))))
             ;; The node graph repaints whenever what it should show
             ;; changes, so no edit path can leave a stale graph on screen.
             (let* ((graph-job (selected-job))
                    (signature (graph-view-signature
                                graph-job
                                (and graph-job
                                     (orfeus:photo-job-graph graph-job))
                                (gui-model-selected-graph-node model))))
               (unless (equal signature graph-view-state)
                 (setf graph-view-state signature)
                 (sync-node-tools)
                 (when graph-canvas (lightfast:redraw graph-canvas))))
             ;; Progress belongs to the current preview generation or export
             ;; batch. Histogram and eviction maintenance are deliberately
             ;; excluded so cancellation cannot inherit an old high-water mark.
             (let* ((export-load (gui-queue-load queue :include-kinds '(:export)))
                    (load (if (plusp export-load)
                              export-load
                              (gui-queue-load queue
                                              :exclude-kinds '(:histogram :evict :export)
                                              :generation preview-generation))))
               (multiple-value-bind (percent total generation)
                   (preview-progress-state load progress-total preview-generation
                                           progress-generation)
                 (setf progress-total total
                       progress-generation generation)
                 ;; A render that is reporting its own stages knows better than
                 ;; the queue does: the queue counts jobs, and one job is the
                 ;; whole wait. Its own fraction wins for as long as it lasts,
                 ;; and an export batch — many jobs, each short — keeps the
                 ;; count.
                 (if (and render-stage (zerop export-load))
                     (show-render-progress)
                     (setf (lightfast:value progress)
                           (format nil "~D" percent)))))))
        (setf after-live-capacity
              (* *gui-live-preview-size* *gui-live-preview-size* 3)
              after-live-front (cffi:foreign-alloc
                                :uint8 :count after-live-capacity)
              after-live-back (cffi:foreign-alloc
                               :uint8 :count after-live-capacity))
        (setf window (lightfast:make-window :width 1280 :height 800
                                          :label "Orfeus"
                                          :app-id "org.orfeus.Orfeus"))
        ;; FLTK closes a window on Escape unless told otherwise. A dialog wants
        ;; that; the main window, with a project open in it, does not.
        (lightfast:window-escape-closes window nil)
        (lightfast:window-set-icon window :orfeus-32)
        (lightfast:apply-classic-theme)
        ;; The core keeps focus measurements wherever this points, and only a
        ;; frontend knows where that should be.
        (setf orfeus:*focus-cache-directory*
              (ignore-errors (gui-focus-cache-directory)))
        (lightfast:set-size-range window :min-width 960 :min-height 700)
        (setf menu (lightfast:make-menu-bar :parent window :x 0 :y 0
                                          :width 1280 :height 24))
        (lightfast:add-menu-item menu "&File/New Project"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (new-project))
                               :shortcut (logior +menu-ctrl+ (char-code #\n)))
        (lightfast:add-menu-item menu "&File/Open Photo" (lambda (&rest ignored)
                                                          (declare (ignore ignored))
                                                          (open-photo))
                               :shortcut (logior +menu-ctrl+ (char-code #\o)))
        (lightfast:add-menu-item menu "&File/Add Photos to Project"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (add-photos))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\a)))
        (lightfast:add-menu-item menu "&File/Open Project" (lambda (&rest ignored)
                                                            (declare (ignore ignored))
                                                            (open-project))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\o)))
        (lightfast:add-menu-item menu "&File/Save Project" (lambda (&rest ignored)
                                                            (declare (ignore ignored))
                                                            (save-project))
                               :shortcut (logior +menu-ctrl+ (char-code #\s)))
        (lightfast:add-menu-item menu "&File/Save Project As" (lambda (&rest ignored)
                                                               (declare (ignore ignored))
                                                               (save-project t))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\s)))
        (lightfast:add-menu-item menu "&File/Export..."
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (open-export-dialog))
                               :shortcut (logior +menu-ctrl+ (char-code #\e)))
        (lightfast:add-menu-item menu "&File/Export Current Photo"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (render-selected))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\e)))
        (lightfast:add-menu-item menu "&File/Export All Photos"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (render-all)))
        (lightfast:add-menu-item menu "&File/Quit" (lambda (&rest ignored)
                                                    (declare (ignore ignored))
                                                    (quit-application))
                               :shortcut (logior +menu-ctrl+ (char-code #\q)))
        ;; Closing the window asks the same question as quitting does.
        (lightfast:on window
                      (lambda (widget event value)
                        (declare (ignore widget event value))
                        (unless (confirm-discard "closing")
                          (lightfast:window-cancel-close window)))
                      :event lightfast:+event-close+)
        (lightfast:add-menu-item menu "&Edit/Undo"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (step-history :undo))
                               :shortcut (logior +menu-ctrl+ (char-code #\z)))
        (lightfast:add-menu-item menu "&Edit/Redo"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (step-history :redo))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\z)))
        (lightfast:add-menu-item menu "&Edit/Copy Grade"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (copy-grade))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\c)))
        (lightfast:add-menu-item menu "&Edit/Paste Grade to Selected"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (paste-grade))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\v)))
        (lightfast:add-menu-item menu "&Edit/Remove Selected Photos"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (remove-selected-photo-with-confirmation))
                               :shortcut +key-delete+)
        (lightfast:add-menu-item menu "&Edit/Select Photo"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (toggle-photo-mark))
                               :shortcut +key-space+)
        (lightfast:add-menu-item menu "&Edit/Select All Photos"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (select-all-thumbnails))
                               :shortcut (logior +menu-ctrl+ (char-code #\a)))
        ;; The arrows walk the filmstrip, as they do any list; the menu names
        ;; one key per command and the others answer unseen. Page Down and
        ;; Page Up still work for anyone whose hands learnt them here.
        (lightfast:add-menu-item menu "&Edit/Next Photo"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (step-photo 1))
                               :shortcut +key-right+)
        (lightfast:add-menu-item menu "&Edit/Previous Photo"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (step-photo -1))
                               :shortcut +key-left+)
        (loop for (path key delta) in `(("&Edit/Next Photo (Down)" ,+key-down+ 1)
                                        ("&Edit/Next Photo (Page Down)" ,+key-page-down+ 1)
                                        ("&Edit/Previous Photo (Up)" ,+key-up+ -1)
                                        ("&Edit/Previous Photo (Page Up)" ,+key-page-up+ -1))
              do (let ((delta delta))
                   (lightfast:add-menu-item menu path
                                            (lambda (&rest ignored)
                                              (declare (ignore ignored))
                                              (step-photo delta))
                                            :shortcut key :hidden t)))
        (lightfast:add-menu-item menu "&Edit/First Photo"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (move-to-photo-row 0))
                               :shortcut +key-home+)
        (lightfast:add-menu-item menu "&Edit/Last Photo"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (move-to-photo-row most-positive-fixnum))
                               :shortcut +key-end+)
        (lightfast:add-menu-item menu "&Edit/Select Blurry Photos"
                                 (lambda (&rest ignored)
                                   (declare (ignore ignored))
                                   (find-and-select-blurry-photos)))
        (lightfast:add-menu-item menu "&Edit/Reset Selected Photos"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (gui-model-reset-selected model)
                                 (sync-controls)
                                              (schedule-edited-preview)))
        (lightfast:add-menu-item menu "&Edit/Reset Defaults"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (setf (project-defaults project)
                                       (gui-default-processing-settings))
                                 (sync-controls)
                                              (schedule-edited-preview)))
        (lightfast:add-menu-item menu "&View/Before and After"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (toggle-comparison))
                               :shortcut (logior +menu-ctrl+ (char-code #\b)))
        (lightfast:add-menu-item menu "&View/Refresh Preview"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (schedule-initial-preview))
                               :shortcut +key-f5+)
        (lightfast:add-menu-item menu "&View/Rebuild Previews"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (discard-previews))
                               :shortcut (logior +menu-shift+ +key-f5+))
        (lightfast:add-menu-item menu "&View/Zoom In"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (zoom-preview 1.25d0))
                               :shortcut (logior +menu-ctrl+ (char-code #\=)))
        (lightfast:add-menu-item menu "&View/Zoom Out"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (zoom-preview .8d0))
                               :shortcut (logior +menu-ctrl+ (char-code #\-)))
        (lightfast:add-menu-item menu "&View/Fit Preview"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (reset-preview-view))
                               :shortcut (logior +menu-ctrl+ (char-code #\0)))
        (lightfast:add-menu-item menu "&View/Actual Pixels"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (preview-one-to-one))
                               :shortcut (logior +menu-ctrl+ (char-code #\1)))
        (lightfast:add-menu-item menu "&View/Expand All Bursts"
                                 (lambda (&rest ignored)
                                   (declare (ignore ignored))
                                   (set-bursts-expanded t)))
        (lightfast:add-menu-item menu "&View/Collapse All Bursts"
                                 (lambda (&rest ignored)
                                   (declare (ignore ignored))
                                   (set-bursts-expanded nil)))
        ;; The filmstrip's order. FLTK keeps the bullet and the tick itself
        ;; when an item is picked; the bullet starts on the order in force.
        (loop for (path key) in '(("&View/Sort Photos/By Capture Time" :capture-time)
                                  ("&View/Sort Photos/By Name" :name)
                                  ("&View/Sort Photos/By Rating" :rating))
              do (let ((key key))
                   (lightfast:add-menu-item
                    menu path
                    (lambda (&rest ignored)
                      (declare (ignore ignored))
                      (sort-photos key))
                    :radio t
                    :checked (eq key (gui-model-sort-key model)))))
        (lightfast:add-menu-item menu "&View/Sort Photos/Reverse Order"
                                 (lambda (&rest ignored)
                                   (declare (ignore ignored))
                                   (reverse-photo-order))
                                 :toggle t)
        (lightfast:add-menu-item menu "&Process/Grab Still"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (grab-still))
                               :shortcut (logior +menu-ctrl+ (char-code #\g)))
        (lightfast:add-menu-item menu "&Help/About Orfeus"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (lightfast:message-box
                                  ;; Common Lisp does not read \n as a newline;
                                  ;; the backslash just escapes the n.
                                  (format nil "Orfeus ~A~%~
                                               RAW processor for the Olympus ~
                                               PEN-F and OM-1~%~%~
                                               Built from ~A"
                                          (orfeus:orfeus-version)
                                          (or (orfeus:orfeus-build-commit)
                                              "an unknown revision")))))
        (setf toolbar (lightfast:make-panel :parent window :x 0 :y 24
                                          :width 1280 :height 40 :label ""))
        (lightfast:set-box toolbar lightfast:+box-flat-box+)
        (flet ((rule (x y width height red green blue)
                 (let ((box (lightfast:make-box :parent toolbar :x x :y y
                                              :width width :height height
                                              :label "")))
                   (lightfast:set-box box lightfast:+box-flat-box+)
                   (lightfast:set-color-rgb box :red red :green green :blue blue)
                   box))
               (toolbar-button (x icon tooltip action)
                 (let ((button (lightfast:make-button
                                :parent toolbar :x x :y 5 :width 28 :height 28
                                :label ""
                                :callback (lambda (&rest ignored)
                                            (declare (ignore ignored))
                                            (funcall action)))))
                   (lightfast:set-box button lightfast:+box-flat-box+)
                   (lightfast:set-stock-icon button icon)
                   (lightfast:set-tooltip button tooltip)
                   button)))
          (rule 6 10 2 18 130 130 130)
          (rule 10 10 2 18 245 245 245)
          ;; A photograph and a project are not the same thing and must not
          ;; wear the same picture. They did, because the toolkit aliased
          ;; folder-open to the icon it uses for open, so both buttons drew a
          ;; folder with papers in it.
          (toolbar-button 18 :photo "Add RAW photographs to project" #'add-photos)
          (toolbar-button 48 :folder-open "Open project" #'open-project)
          (toolbar-button 78 :save "Save project" #'save-project)
          (toolbar-button 108 :delete "Remove selected photographs"
                          #'remove-selected-photo)
          (rule 142 7 1 24 150 150 150)
          (toolbar-button 150 :export "Export photographs..." #'open-export-dialog)
          (toolbar-button 180 :compare "Show or hide Before and After"
                          #'toggle-comparison)
          (toolbar-button 210 :focus "Select photographs that look out of focus"
                          #'find-and-select-blurry-photos)
          (rule 244 7 1 24 150 150 150)
          (toolbar-button 252 :zoom-out "Zoom out" (lambda () (zoom-preview .8d0)))
          (toolbar-button 282 :fit "Fit preview" #'reset-preview-view)
          (toolbar-button 312 :zoom-in "Zoom in" (lambda () (zoom-preview 1.25d0)))
          (toolbar-button 342 :actual-pixels "Show image pixels at 1:1"
                          #'preview-one-to-one)
          (rule 376 7 1 24 150 150 150)
          (setf toolbar-bottom-rule (rule 0 38 1280 2 145 145 145)))
        (setf lens-name (lightfast:make-label :parent toolbar :x 402 :y 6
                                            :width 852 :height 28
                                            :label "Lens: No photograph selected"))
        (lightfast:set-label-font lens-name 1)
        (setf main-tile (lightfast:make-tile :parent window :x 0 :y 64
                                           :width 1280 :height 708)
              left-column (lightfast:make-tile :parent main-tile :x 0 :y 0
                                             :width 240 :height 708)
              filmstrip-pane (lightfast:make-panel :parent left-column
                                                 :x 0 :y 0
                                                 :width 240 :height 448
                                                 :label "")
              gallery-pane (lightfast:make-panel :parent left-column
                                               :x 0 :y 448
                                               :width 240 :height 260
                                               :label "")
              center-pane (lightfast:make-panel :parent main-tile :x 240 :y 0
                                              :width 720 :height 708 :label ""))
        (lightfast:set-box gallery-pane lightfast:+box-flat-box+)
        (setf photo-selection-label
              (lightfast:make-label
               :parent filmstrip-pane :x 8 :y 2 :width 224 :height 24
               :label "Photos · 0 open · 0 selected"))
        (lightfast:set-label-font photo-selection-label lightfast:+font-helvetica-bold+)
        (setf thumbnail-canvas
              (lightfast:make-canvas
               :parent filmstrip-pane :x 0 :y 28 :width 240 :height 420
               :callback
               (lambda (widget event value)
                 (declare (ignore event value))
                 (let ((x (lightfast:widget-x widget))
                       (y (lightfast:widget-y widget))
                       (width (lightfast:widget-width widget))
                       (height (lightfast:widget-height widget))
                       (row-height (thumbnail-row-height)))
                   (lightfast:draw-color-rgb :red 42 :green 44 :blue 46)
                   (lightfast:draw-filled-rect x y width height)
                   (loop with photos = (coerce (project-photos project) 'vector)
                         with selected = (gui-model-selected-indices model)
                         for (indices . leader) in (thumbnail-display-rows)
                         for row from 0
                         for row-y = (+ y (* row row-height) (- thumbnail-scroll))
                         when (and (< row-y (+ y height))
                                   (> (+ row-y row-height) y))
                           do (let* ((job (aref photos (first indices)))
                                     (chosen (count-if (lambda (place)
                                                         (member place selected))
                                                       indices))
                                     (all-chosen (= chosen (length indices))))
                                (when (plusp chosen)
                                  (lightfast:draw-color-rgb :red 0 :green 0
                                                            :blue (if all-chosen 128 72))
                                  (lightfast:draw-filled-rect x row-y width row-height))
                                ;; The cursor: where the arrows are, whatever
                                ;; the selection is doing. On a selected row it
                                ;; frames the blue; on an unselected one it is
                                ;; the only mark of where the preview stands.
                                (when (member (gui-model-selected-index model) indices)
                                  (lightfast:draw-color-rgb :red 240 :green 190 :blue 90)
                                  (lightfast:draw-rect (+ x 2) (+ row-y 2)
                                                       (- width 4) (- row-height 4))
                                  (lightfast:draw-rect (+ x 3) (+ row-y 3)
                                                       (- width 6) (- row-height 6)))
                                (let ((path (gethash job thumbnail-files)))
                                  (if path
                                      (draw-thumbnail-file widget path
                                                           (+ x 6) (+ row-y 6)
                                                           88 (- row-height 12))
                                      (progn
                                        (lightfast:draw-color-rgb :red 62 :green 64 :blue 66)
                                        (lightfast:draw-filled-rect
                                         (+ x 6) (+ row-y 6) 88 (- row-height 12)))))
                                (when (photo-interned-cached-p job)
                                  (draw-interned-badge (+ x 76) (+ row-y 8)))
                                (draw-burst-bar job x row-y row-height)
                                (lightfast:draw-color-rgb :red 235 :green 235 :blue 235)
                                (lightfast:draw-text
                                 (orfeus:photo-display-name
                                  (photo-job-input-path job)
                                  :interned-p (photo-interned-cached-p job))
                                 (+ x 102) (+ row-y 30))
                                ;; Filename, then the stars beneath it, then the
                                ;; focus mark, then the burst line: a star is
                                ;; drawn downwards from its corner and text
                                ;; upwards from its baseline, so the two need
                                ;; clear air between them.
                                (draw-photo-rating job (+ x 102) (+ row-y 42))
                                (draw-focus-mark job (+ x 102) (+ row-y 62))
                                (if leader
                                    (draw-burst-chevron x row-y t (length indices))
                                    (progn
                                      (when (burst-leader-p job)
                                        (draw-burst-chevron x row-y nil 0))
                                      (draw-burst-position job (+ x 102) (+ row-y 78))))
                                (let ((check-x (thumbnail-checkbox-x x width))
                                      (check-y (+ row-y 10)))
                                  (lightfast:draw-color-rgb :red 225 :green 225 :blue 225)
                                  (lightfast:draw-filled-rect check-x check-y 14 14)
                                  (when (plusp chosen)
                                    (lightfast:draw-color-rgb :red 0 :green 0 :blue 128)
                                    (if all-chosen
                                        (lightfast:draw-filled-rect
                                         (+ check-x 3) (+ check-y 3) 8 8)
                                        ;; Part of a folded burst, not all of
                                        ;; it: a bar rather than a box, so the
                                        ;; row does not claim more than it has.
                                        (lightfast:draw-filled-rect
                                         (+ check-x 3) (+ check-y 6) 8 3))))
                                (lightfast:draw-color-rgb :red 125 :green 127 :blue 129)
                                (lightfast:draw-filled-rect x (+ row-y row-height -1)
                                                            width 1)))))))
        (lightfast:set-tooltip
         thumbnail-canvas
         "Click boxes to toggle; Shift-click selects a range; right-click for actions")
        (dolist (event (list lightfast:+event-push+ lightfast:+event-wheel+))
          (lightfast:on thumbnail-canvas #'handle-thumbnail-mouse :event event))
        (setf thumbnail-scrollbar
              (lightfast:make-scrollbar
               :parent filmstrip-pane :x 224 :y 28 :width 16 :height 420
               :value "0"
               :callback
               (lambda (widget event value)
                 (declare (ignore event))
                 (let ((position (ignore-errors
                                   (round (parse-number
                                           (if (plusp (length (or value "")))
                                               value
                                               (lightfast:value widget)))))))
                   (when position
                     (setf thumbnail-scroll position)
                     (clamp-thumbnail-scroll)
                     (lightfast:redraw thumbnail-canvas))))))
        (lightfast:scrollbar-set-orientation thumbnail-scrollbar :vertical)
        (lightfast:set-step thumbnail-scrollbar 36)
        (setf before-caption
              (lightfast:make-label :parent center-pane :x 0 :y 0
                                  :width 357 :height 22
                                  :label "  Before · neutral RAW")
              after-caption
              (lightfast:make-label :parent center-pane :x 363 :y 0
                                  :width 357 :height 22
                                  :label "  After · current adjustments"))
        (dolist (caption (list before-caption after-caption))
          (lightfast:set-box caption lightfast:+box-thin-up-box+)
          (lightfast:set-label-font caption lightfast:+font-helvetica-bold+))
        (flet ((make-preview-canvas (role x)
                 (lightfast:make-canvas
                  :parent center-pane :x x :y 22 :width 357 :height 686
                  :callback
                  (lambda (widget event value)
                    (declare (ignore event value))
                    (lightfast:draw-color-rgb :red 30 :green 32 :blue 34)
                    (lightfast:draw-filled-rect
                     (lightfast:widget-x widget) (lightfast:widget-y widget)
                     (lightfast:widget-width widget) (lightfast:widget-height widget))
                    (let ((path (ecase role
                                  (:before before-preview-file)
                                  (:after after-preview-file)))
                          (live-p (and (eq role :after)
                                       (live-view-p widget))))
                      (cond
                        (live-p
                         (draw-preview-buffer widget after-live-front
                                              after-live-width
                                              after-live-height
                                              after-live-generation
                                              :zoom preview-zoom
                                              :center-x preview-center-x
                                              :center-y preview-center-y)
                         (draw-crop-overlay widget))
                        (path
                         (draw-current-preview widget path)
                         (when (eq role :after)
                           (draw-crop-overlay widget)))
                        ;; Nothing developed yet. The sidebar has already
                        ;; rendered this photograph small, so show that scaled up
                        ;; rather than an empty panel: it is the right image, a
                        ;; moment early, and it costs nothing to draw. The full
                        ;; preview replaces it in place when it arrives.
                        ((thumbnail-preview-for (selected-job))
                         (draw-preview-file
                          widget (thumbnail-preview-for (selected-job))
                          :zoom preview-zoom
                          :center-x preview-center-x
                          :center-y preview-center-y)
                         (lightfast:draw-color-rgb :red 235 :green 200 :blue 120)
                         (lightfast:draw-text "Developing..."
                                              (+ (lightfast:widget-x widget) 20)
                                              (+ (lightfast:widget-y widget) 36)))
                        (t
                         (lightfast:draw-color-rgb :red 205 :green 208 :blue 210)
                         (lightfast:draw-text "Developing RAW preview..."
                                            (+ (lightfast:widget-x widget) 20)
                                            (+ (lightfast:widget-y widget) 36)))))))))
          (setf before-canvas (make-preview-canvas :before 0)
                after-canvas (make-preview-canvas :after 363))
          (dolist (canvas (list before-canvas after-canvas))
            (dolist (event (list lightfast:+event-push+
                                 lightfast:+event-drag+
                                 lightfast:+event-release+
                                 lightfast:+event-wheel+))
              (lightfast:on canvas
                          (lambda (widget callback-event value)
                            (handle-preview-mouse widget callback-event value))
                          :event event))))
        (setf right-column (lightfast:make-tile :parent main-tile
                                              :x 960 :y 0
                                              :width 320 :height 708)
              inspector (lightfast:make-panel :parent right-column :x 0 :y 0
                                            :width 320 :height 425
                                            :label "")
              graph-pane (lightfast:make-panel :parent right-column
                                             :x 0 :y 425
                                             :width 320 :height 283
                                             :label ""))
        (lightfast:set-box graph-pane lightfast:+box-flat-box+)
        (setf graph-title (lightfast:make-label :parent graph-pane
                                              :x 8 :y 4
                                              :width 304 :height 18
                                              :label "Node Graph"))
        (lightfast:set-label-font graph-title lightfast:+font-helvetica-bold+)
        (flet ((grade-button (x label icon tooltip action)
                 (let ((button (lightfast:make-button
                                :parent graph-pane :x x :y 24
                                :width 96 :height 22 :label label
                                :callback (lambda (&rest ignored)
                                            (declare (ignore ignored))
                                            (funcall action)))))
                   (lightfast:set-stock-icon button icon)
                   (lightfast:set-tooltip button tooltip)
                   button)))
          (setf still-button
                (grade-button 8 "Still" :camera
                              "Grab a still of the current grade"
                              #'grab-still)
                copy-grade-button
                (grade-button 108 "Copy" :copy
                              "Copy the current photo's node graph"
                              #'copy-grade)
                paste-grade-button
                (grade-button 208 "Paste" :paste
                              "Paste the copied node graph to the selection"
                              #'paste-grade)))
        (setf graph-canvas
              (lightfast:make-canvas
               :parent graph-pane :x 2 :y 52 :width 316 :height 229
               :callback (lambda (widget event value)
                           (declare (ignore event value))
                           (draw-graph-editor widget))))
        (lightfast:set-box graph-canvas lightfast:+box-flat-box+)
        (lightfast:set-tooltip
         graph-canvas
         "Green optics comes before any crop, cool nodes grade in scene-linear, warm ones follow the film transform; drag to arrange, drag a port to rewire, right-click to add")
        (dolist (event (list lightfast:+event-push+
                             lightfast:+event-drag+
                             lightfast:+event-release+
                             lightfast:+event-wheel+))
          (lightfast:on graph-canvas #'handle-graph-mouse :event event))
        ;; The default graph alone is seven nodes tall and the canvas shows
        ;; four: without a bar the output well simply was not there, and the
        ;; wheel that would have found it was nothing anyone was told about.
        (setf graph-scrollbar
              (lightfast:make-scrollbar
               :parent graph-pane :x 302 :y 52 :width 16 :height 229
               :value "0"
               :callback
               (lambda (widget event value)
                 (declare (ignore event))
                 (let ((position (ignore-errors
                                   (round (parse-number
                                           (if (plusp (length (or value "")))
                                               value
                                               (lightfast:value widget)))))))
                   (when position
                     (setf graph-scroll-y (max 0 position))
                     (lightfast:redraw graph-canvas))))))
        (lightfast:scrollbar-set-orientation graph-scrollbar :vertical)
        (lightfast:set-step graph-scrollbar 32)
        ;; The inspector lays its children out manually, so it must paint its
        ;; own background; a boxless group smears stale pixels during tile
        ;; drags and window resizes.
        (lightfast:set-box inspector lightfast:+box-flat-box+)
        (let ((scope-field
                (lightfast:make-labeled-choice
                 :parent inspector :x 8 :y 8 :width 300 :height 26
                 :label "Apply to" :label-width 96
                 :items '("Photo" "Defaults")
                 :callback (lambda (widget event value)
                             (declare (ignore event value))
                             (setf (gui-model-edit-target model)
                                   (if (string-equal (lightfast:value widget)
                                                     "Defaults")
                                       :defaults :photo))
                             (sync-controls)))))
          (register-field scope-field 8)
          (setf target-choice (lightfast:field-control scope-field)))
        (setf tabs (lightfast:make-tabs :parent inspector :x 4 :y 40
                                      :width 312 :height 380)
              node-page (lightfast:make-tab-page :parent tabs :x 2 :y 24
                                               :width 308 :height 352
                                               :label "Node")
              export-page (lightfast:make-tab-page :parent tabs :x 2 :y 24
                                                 :width 308 :height 352
                                                 :label "Export"))
        (section-frame export-page "Output" 16 172)
        (flet ((export-integer-field (key label y)
                 (lightfast:field-control
                  (register-field
                   (lightfast:make-labeled-control
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
                 (lightfast:make-check-button
                  :parent export-page :x 110 :y 122 :width 182 :height 26
                  :label "Preserve metadata"
                  :callback (lambda (widget event value)
                              (declare (ignore event value))
                              (export-setting-changed :preserve-metadata-p widget)))
                 110 122 :control 26 :page)
                export-timestamp
                (register-inspector
                 (lightfast:make-check-button
                  :parent export-page :x 110 :y 154 :width 182 :height 26
                  :label "Timestamp filenames"
                  :callback (lambda (widget event value)
                              (declare (ignore event value))
                              (export-setting-changed :timestamp-filenames-p
                                                      widget)))
                 110 154 :control 26 :page)))
        ;; The stills gallery lives on the left column, PowerGrade style.
        (setf gallery-title (lightfast:make-label :parent gallery-pane
                                                :x 8 :y 2
                                                :width 224 :height 18
                                                :label "Stills Gallery"))
        (lightfast:set-label-font gallery-title lightfast:+font-helvetica-bold+)
        (setf gallery-canvas
              (lightfast:make-canvas
               :parent gallery-pane :x 4 :y 22 :width 232 :height 172
               :callback (lambda (widget event value)
                           (declare (ignore event value))
                           (draw-gallery widget))))
        (dolist (event (list lightfast:+event-push+ lightfast:+event-wheel+))
          (lightfast:on gallery-canvas #'handle-gallery-mouse :event event))
        (setf preset-name-input
              (lightfast:make-input :parent gallery-pane :x 8 :y 200
                                  :width 224 :height 24))
        (lightfast:set-tooltip preset-name-input "Still or preset name")
        (setf preset-save-button
              (lightfast:make-button
               :parent gallery-pane :x 8 :y 228 :width 108 :height 26
               :label "Save still"
               :callback (lambda (&rest ignored)
                           (declare (ignore ignored))
                           (save-current-preset))))
        (setf preset-apply-button
              (lightfast:make-button
               :parent gallery-pane :x 124 :y 228 :width 108 :height 26
               :label "Apply to 1 photo"
               :callback (lambda (&rest ignored)
                           (declare (ignore ignored))
                           (apply-current-preset))))
        (lightfast:set-stock-icon preset-save-button :save)
        (lightfast:set-stock-icon preset-apply-button :send)
        (refresh-gallery)
        ;; The Node panel: pick a correction, see only that category's
        ;; controls beneath it.
        (build-group
         :picker
         (lambda ()
           (setf kind-choice
                 (lightfast:field-control
                  (register-field
                   (lightfast:make-labeled-choice
                    :parent node-page :x 12 :y 8 :width 292 :height 26
                    :label "Correction" :label-width 88
                    :items (mapcar #'first *node-kind-choices*)
                    :callback
                    (lambda (widget event value)
                      (declare (ignore event value))
                      (let ((node (gui-model-selected-graph-node model))
                            (kind (rest (assoc (lightfast:value widget)
                                               *node-kind-choices*
                                               :test #'string-equal))))
                        (when (and node kind
                                   (not (eq kind
                                            (orfeus:graph-node-kind
                                             node))))
                          (handler-case
                              (progn
                                (multiple-value-bind (changed moved)
                                    (gui-model-set-node-kind model node kind)
                                  (declare (ignore changed))
                                  (after-graph-edit
                                   (if moved
                                       ;; Say so, or the node appearing further
                                       ;; up the chain looks like a glitch.
                                       (format nil "~A moved before the film tail"
                                               (node-kind-label kind))
                                       (format nil "Correction set to ~A"
                                               (node-kind-label kind))))))
                            (error (condition)
                              (sync-node-tools)
                              (set-status
                               (princ-to-string condition))))))))
                   8 :page)))))
        (build-group
         :none
         (lambda ()
           (register-inspector
            (lightfast:make-label
             :parent node-page :x 12 :y 44 :width 292 :height 26
             :label "Select or create a node in the graph")
            12 44 :fill 26 :page)))
        (build-group
         :node
         (lambda ()
           (register-inspector
            (lightfast:make-label
             :parent node-page :x 12 :y 44 :width 292 :height 26
             :label "Pick a correction type above")
            12 44 :fill 26 :page)))
        (build-group
         :white-balance
         (lambda ()
           (setf wb-choice
                 (lightfast:field-control
                  (register-field
                   (lightfast:make-labeled-choice
                    :parent node-page :x 12 :y 44 :width 292 :height 26
                    :label "Mode" :label-width 88
                    :items '("As shot" "Custom")
                    :callback (lambda (widget event value)
                                (declare (ignore event value))
                                (set-wb-mode widget)))
                   44 :page)))
           (make-number-field :white-balance-temperature "Temperature (K)"
                              2000 15000 50 76 node-page)
           (make-number-field :white-balance-tint "Tint" -20 20 0.1 108
                              node-page)))
        (build-group
         :exposure
         (lambda ()
           (make-number-field :exposure "Exposure EV" -10 10 0.1 44
                              node-page)))
        (build-group
         :noise-reduction
         (lambda ()
           (make-number-field :noise-reduction "Edge-aware" 0 1 0.05 44
                              node-page)
           (make-number-field :neural-noise-reduction "Neural" 0 1 0.05 76
                              node-page)))
        (build-group
         :tone
         (lambda ()
           (register-inspector
            (lightfast:make-label :parent node-page :x 12 :y 44
                                :width 292 :height 26
                                :label "Tone Equalizer")
            12 44 :fill 26 :page)
           (make-tone-band :tone-blacks "Blk" "Blacks")
           (make-tone-band :tone-shadows "Shd" "Shadows")
           (make-tone-band :tone-dark-mids "DkM" "Dark mids")
           (make-tone-band :tone-midtones "Mid" "Midtones")
           (make-tone-band :tone-light-mids "LtM" "Light mids")
           (make-tone-band :tone-highlights "Hi" "Highlights")
           (make-tone-band :tone-whites "Wht" "Whites")))
        (build-group
         :optics
         (lambda ()
           (let ((lens (register-inspector
                        (lightfast:make-check-button
                         :parent node-page :x 12 :y 44
                         :width 292 :height 26
                         :label "Apply lens distortion correction"
                         :callback
                         (lambda (widget event value)
                           (declare (ignore event value))
                           (gui-model-set-setting
                            model :lens-correction-p
                            (string/= "0" (lightfast:value widget)))
                           (schedule-edited-preview)))
                        12 44 :fill 26 :page)))
             (push (list :lens-correction-p lens) controls))
           (make-number-field :lens-correction-strength "Strength"
                              0 2 0.05 76 node-page)
           (let ((tca (register-inspector
                       (lightfast:make-check-button
                        :parent node-page :x 12 :y 108
                        :width 170 :height 26
                        :label "Remove colour fringing"
                        :callback
                        (lambda (widget event value)
                          (declare (ignore event value))
                          (gui-model-set-setting
                           model :chromatic-aberration-correction-p
                           (string/= "0" (lightfast:value widget)))
                          (schedule-edited-preview)))
                       12 108 '(:before 110) 26 :page)))
             (lightfast:set-tooltip
              tca "Draw red and blue back onto green where the lens spread them")
             (push (list :chromatic-aberration-correction-p tca)
                   controls))
           ;; Where the fringing correction is read from. Measured from the
           ;; frame by default; the database entry is there for when it is
           ;; wanted, since it describes somebody else's copy of the lens.
           (setf fringing-source-choice
                 (register-inspector
                  (lightfast:make-choice
                   :parent node-page :x 190 :y 108 :width 110 :height 26
                   :items (mapcar #'first *fringing-source-choices*)
                   :callback
                   (lambda (widget event value)
                     (declare (ignore event value))
                     (let ((source (rest (assoc (lightfast:value widget)
                                                *fringing-source-choices*
                                                :test #'string=))))
                       (when source
                         (gui-model-set-setting
                          model :chromatic-aberration-source source)
                         (schedule-edited-preview)))))
                  :right 108 110 26 :page))
           (lightfast:set-tooltip
            fringing-source-choice
            "Measured: read from this photograph. Lens profile: trust the database entry")
           ;; The hand-set correction, for lenses no profile describes.
           (let ((distortion (make-number-field :lens-distortion "By hand"
                                                -0.5 0.5 0.01 140 node-page)))
             (lightfast:set-tooltip
              distortion
              "Barrel or pincushion correction set by eye, for a lens no profile describes"))
           ;; Which profile the corrections above read, and a way to pick one
           ;; when the photograph's metadata names a lens the database does not
           ;; know by that name — or names none at all.
           (setf lens-profile-label
                 (register-inspector
                  (lightfast:make-label
                   :parent node-page :x 12 :y 172 :width 292 :height 26
                   :label "")
                  12 172 :fill 26 :page))
           (register-inspector
            (lightfast:set-stock-icon
             (lightfast:make-button
              :parent node-page :x 12 :y 204 :width 140 :height 26
              :label "Choose Profile..."
              :callback (lambda (&rest ignored)
                          (declare (ignore ignored))
                          (open-lens-picker)))
             :search)
            '(:column 0) 204 '(:share 2) 26 :page)
           (register-inspector
            (lightfast:set-stock-icon
             (lightfast:make-button
              :parent node-page :x 160 :y 204 :width 140 :height 26
              :label "Photograph's Own"
              :callback (lambda (&rest ignored)
                          (declare (ignore ignored))
                          (clear-lens-profile)))
             :photo)
            '(:column 1) 204 '(:share 2) 26 :page)))
        (build-group
         :film
         (lambda ()
           (register-inspector
            (lightfast:make-label :parent node-page :x 12 :y 44
                                :width 88 :height 26 :label "3D LUT")
            12 44 88 26 :page)
           (let ((items '("None")))
             (dolist (path (gui-bundled-lut-paths))
               (let ((name (file-namestring path)))
                 (setf (gethash name lut-paths) (namestring path)
                       items (append items (list name)))))
             (setf items (append items '("Browse..."))
                   lut-choice
                   (register-inspector
                    (lightfast:make-choice
                     :parent node-page :x 110 :y 44 :width 190 :height 26
                     :items items
                     :callback
                     (lambda (widget event value)
                       (declare (ignore event value))
                       (let ((selection (lightfast:value widget)))
                         (cond ((string= selection "Browse...")
                                (choose-lut)
                                (sync-controls))
                               ((string= selection "None") (clear-lut))
                               (t
                                (gui-model-set-setting
                                 model :lut-path
                                 (gethash selection lut-paths))
                                (schedule-edited-preview))))))
                    110 44 :control 26 :page)))
           (make-number-field :lut-strength "Strength" 0 1 0.05 76
                              node-page)
           (make-number-field :grain-amount "Grain amount" 0 1 0.05 108
                              node-page)
           (make-number-field :grain-size "Grain size" 0.25 16 0.25 140
                              node-page)))
        (build-group
         :blend
         (lambda ()
           (register-inspector
            (lightfast:make-label :parent node-page :x 12 :y 44
                                :width 88 :height 26 :label "Opacity")
            12 44 88 26 :page)
           (setf blend-opacity-input
                 (register-inspector
                  (lightfast:make-spinner
                   :parent node-page :x 110 :y 44 :width 84 :height 26
                   :callback
                   (lambda (widget event value)
                     (declare (ignore event value))
                     (let ((node (gui-model-selected-graph-node model)))
                       (when (and node (orfeus:graph-node-blend-p node))
                         (handler-case
                             (let ((opacity (parse-number
                                             (lightfast:value widget))))
                               (setf (orfeus:graph-node-opacity node)
                                     (float (max 0 (min 1 opacity)) 1.0))
                               (when graph-canvas
                                 (lightfast:redraw graph-canvas))
                               (schedule-edited-preview))
                           (error (condition)
                             (set-status
                              (princ-to-string condition))))))))
                  110 44 84 26 :page))
           (lightfast:set-range blend-opacity-input 0 1)
           (lightfast:set-step blend-opacity-input 0.05)))
        (build-group
         :contrast
         (lambda ()
           (register-inspector
            (lightfast:make-label :parent node-page :x 12 :y 44
                                  :width 292 :height 26
                                  :label "Slope about a fixed tone")
            12 44 :fill 26 :page)
           (make-node-number-field :contrast "Contrast" 0.2 4.0 0.05 1.0 76
                                   node-page)
           (make-node-number-field :pivot "Pivot" 0.05 0.95 0.005 0.435 108
                                   node-page)))
        (build-group
         :sharpen
         (lambda ()
           (register-inspector
            (lightfast:make-label :parent node-page :x 12 :y 44
                                  :width 292 :height 26
                                  :label "Unsharp mask on brightness, halos held in")
            12 44 :fill 26 :page)
           (make-number-field :sharpen-amount "Amount" 0.0 3.0 0.05 76
                              node-page)
           (make-number-field :sharpen-radius "Radius (px)" 0.3 5.0 0.1 108
                              node-page)
           (make-number-field :sharpen-threshold "Noise floor" 0.0 8.0 0.25 140
                              node-page)))
        (build-group
         :color-subtract
         (lambda ()
           (multiple-value-setq (base-red-input base-green-input
                                 base-blue-input base-swatch)
             (build-base-panel 44 "The film base being subtracted"))))
        (build-group
         :negative
         (lambda ()
           ;; The print's controls first, the film base under them: the base
           ;; is picked once from the border and then left alone, while the
           ;; print is what gets adjusted.
           (make-node-number-field :gamma "Print contrast" 1.0 4.0 0.05
                                   orfeus:*negative-default-gamma* 44 node-page)
           (make-node-number-field :balance "Neutral whites" 0.0 1.0 0.05
                                   orfeus:*negative-default-balance* 76
                                   node-page)
           (multiple-value-setq (negative-red-input negative-green-input
                                 negative-blue-input negative-swatch)
             (build-base-panel
              108 "The film base divided out; zero measures it from the frame"))))
        (build-group
         :rotate
         (lambda ()
           (let ((field
                   (lightfast:make-labeled-choice
                    :parent node-page :x 12 :y 44 :width 292 :height 26
                    :label "Turn" :label-width 88
                    :items (mapcar #'rest orfeus:*quarter-turn-labels*)
                    :callback
                    (lambda (widget event value)
                      (declare (ignore event value))
                      (let ((node (gui-model-selected-graph-node model))
                            (turns (quarter-turns-for-label
                                    (lightfast:value widget))))
                        (when (and node turns)
                          (handler-case
                              (progn
                                (gui-model-set-node-params
                                 model node (list :quarter-turns turns))
                                (after-graph-edit
                                 (format nil "Rotated ~A"
                                         (quarter-turn-label turns))))
                            (error (condition)
                              (set-status (princ-to-string condition))))))))))
             (setf rotate-turn-input (lightfast:field-control field))
             (register-field field 44 :page))
           (register-inspector
            (lightfast:make-label
             :parent node-page :x 12 :y 76 :width 292 :height 26
             :label "Whole turns keep every photosite")
            12 76 :fill 26 :page)))
        (build-group
         :flip
         (lambda ()
           ;; One choice rather than two checkboxes: a mirror has four states
           ;; and naming them reads better than two boxes whose combination the
           ;; reader has to work out.
           (let ((field
                   (lightfast:make-labeled-choice
                    :parent node-page :x 12 :y 44 :width 292 :height 26
                    :label "Mirror" :label-width 88
                    :items (mapcar #'first *flip-choices*)
                    :callback
                    (lambda (widget event value)
                      (declare (ignore event value))
                      (let ((node (gui-model-selected-graph-node model))
                            (axes (rest (assoc (lightfast:value widget)
                                               *flip-choices* :test #'string=))))
                        (when node
                          (handler-case
                              (progn
                                (gui-model-set-node-params
                                 model node
                                 (list :horizontal (first axes)
                                       :vertical (second axes)))
                                (after-graph-edit
                                 (format nil "Mirror: ~A"
                                         (lightfast:value widget))))
                            (error (condition)
                              (set-status (princ-to-string condition))))))))))
             (setf flip-axis-input (lightfast:field-control field))
             (register-field field 44 :page))
           (register-inspector
            (lightfast:make-label
             :parent node-page :x 12 :y 76 :width 292 :height 26
             :label "A mirror keeps every photosite")
            12 76 :fill 26 :page)))
        (build-group
         :crop
         (lambda ()
           (let ((angle-label
                   (lightfast:make-label :parent node-page :x 12 :y 44
                                         :width 88 :height 26 :label "Angle"))
                 (angle-callback
                   (lambda (widget event value)
                     (declare (ignore event value))
                     (let ((node (crop-editing-node)))
                       (when node
                         (handler-case
                             (let ((angle (parse-number
                                           (lightfast:value widget))))
                               (set-crop-node-angle
                                node
                                (float (max -45 (min 45 angle)) 1.0)))
                           (error (condition)
                             (set-status
                              (princ-to-string condition)))))))))
             ;; A slider as well as a number: levelling a horizon is a thing
             ;; you nudge until it looks right, not a figure anybody knows in
             ;; advance.
             (let ((slider (lightfast:make-slider
                            :parent node-page :x 110 :y 44
                            :width 84 :height 26 :callback angle-callback)))
               (lightfast:set-range slider -45 45)
               (lightfast:set-step slider 0.1)
               (push (list :crop-angle slider) crop-angle-controls)
               (setf crop-angle-input
                     (lightfast:make-spinner
                      :parent node-page :x 202 :y 44 :width 78 :height 26
                      :callback angle-callback))
               (lightfast:set-range crop-angle-input -45 45)
               (lightfast:set-step crop-angle-input 0.1)
               (push (list :crop-angle crop-angle-input) crop-angle-controls)
               (register-inspector-row
                (number-field-layout angle-label slider crop-angle-input)
                (list angle-label slider crop-angle-input)
                node-page 12 44 26 :page)))
           (let ((aspect-field
                   (lightfast:make-labeled-choice
                    :parent node-page :x 12 :y 76 :width 292 :height 26
                    :label "Aspect" :label-width 88
                    :items (mapcar #'first *crop-aspect-choices*)
                    :callback
                    (lambda (widget event value)
                      (declare (ignore event value))
                      (setf crop-aspect
                            (crop-aspect-for-label (lightfast:value widget)))
                      (let ((node (crop-editing-node)))
                        (when node
                          (reshape-crop-to-aspect node)
                          (sync-node-tools)
                          (schedule-edited-preview)))
                      (set-status
                       (if crop-aspect
                           (format nil "Crop locked to ~A"
                                   (crop-aspect-label crop-aspect))
                           "Crop proportions free"))))))
             (setf crop-aspect-input (lightfast:field-control aspect-field))
             (register-field aspect-field 76 :page))
           ;; Sizes as a percentage of the frame, so a crop can be set to an
           ;; exact amount rather than only dragged to it.
           (flet ((size-field (axis label y)
                    (register-inspector
                     (lightfast:make-label :parent node-page :x 12 :y y
                                         :width 88 :height 26 :label label)
                     12 y 88 26 :page)
                    (let ((spinner
                            (register-inspector
                             (lightfast:make-spinner
                              :parent node-page :x 110 :y y
                              :width 84 :height 26
                              :callback
                              (lambda (widget event value)
                                (declare (ignore event value))
                                (let ((node (crop-editing-node)))
                                  (when node
                                    (handler-case
                                        (let ((percent
                                                (parse-number
                                                 (lightfast:value widget))))
                                          (set-crop-size-fraction
                                           node axis (/ percent 100.0d0))
                                          (sync-node-tools)
                                          (schedule-edited-preview))
                                      (error (condition)
                                        (set-status
                                         (princ-to-string condition))))))))
                             110 y 84 26 :page)))
                      (lightfast:set-range spinner 5 100)
                      (lightfast:set-step spinner 0.5)
                      spinner)))
             (setf crop-width-input (size-field :width "Width %" 108)
                   crop-height-input (size-field :height "Height %" 140)))
           ;; Straightening without the slider. The camera's level gauge is
           ;; written into the maker notes as the roll at exposure, so the
           ;; exact correction is usually in the file already; the picture's
           ;; own straight edges are the fallback for bodies that write none.
           (lightfast:set-tooltip
            (register-inspector
             (lightfast:set-stock-icon
              (lightfast:make-button
               :parent node-page :x 12 :y 172 :width 140 :height 26
               :label "Level From Camera"
               :callback (lambda (&rest ignored)
                           (declare (ignore ignored))
                           (let ((node (crop-editing-node)))
                             (when node (level-crop-from-camera node)))))
              :camera)
             '(:column 0) 172 '(:share 2) 26 :page)
            "Turn the picture back by the roll the camera's level gauge recorded")
           (lightfast:set-tooltip
            (register-inspector
             (lightfast:set-stock-icon
              (lightfast:make-button
               :parent node-page :x 160 :y 172 :width 140 :height 26
               :label "Level From Photo"
               :callback (lambda (&rest ignored)
                           (declare (ignore ignored))
                           (let ((node (crop-editing-node)))
                             (when node (level-crop-from-photo node)))))
              :photo)
             '(:column 1) 172 '(:share 2) 26 :page)
            "Stand the picture's strongest straight edges upright")
           (register-inspector
            (lightfast:set-stock-icon
             (lightfast:make-button
              :parent node-page :x 12 :y 204 :width 292 :height 26
              :label "Autocrop Negative"
              :callback (lambda (&rest ignored)
                          (declare (ignore ignored))
                          (let ((node (crop-editing-node)))
                            (when node (autocrop-negative node)))))
             :crop)
            12 204 :fill 26 :page)
           (register-inspector
            (lightfast:make-button
             :parent node-page :x 12 :y 236 :width 292 :height 26
             :label "Reset Crop"
             :callback (lambda (&rest ignored)
                         (declare (ignore ignored))
                         (let ((node (crop-editing-node)))
                           (when node
                             ;; Back to the inset a fresh crop starts on, not to
                             ;; the frame edge: resetting is for starting over,
                             ;; and a rectangle whose handles sit on the edge of
                             ;; the picture cannot be grabbed to start with.
                             (gui-model-set-node-params
                              model node (default-crop-params))
                             (setf crop-aspect :original)
                             (after-graph-edit "Crop reset")))))
            12 236 :fill 26 :page)))
        (build-group
         :curves
         (lambda ()
           ;; Y R G B as four lit buttons, the way the reference panel selects
           ;; a channel: the chart shows all four curves at once, so this only
           ;; says which one a click on empty chart adds a point to and which
           ;; one is drawn on top.
           (let ((buttons
                   (loop for key in '(:master-points :red-points
                                      :green-points :blue-points)
                         for column from 0
                         collect
                         (let ((key key))
                           (cons
                            key
                            (lightfast:make-button
                             :parent node-page
                             :x (+ 12 (* column 74)) :y 44
                             :width 70 :height 26
                             :label (curve-channel-button-label key)
                             :callback
                             (lambda (&rest ignored)
                               (declare (ignore ignored))
                               (setf curve-channel key)
                               (sync-curve-channel-buttons)
                               (when curve-canvas
                                 (lightfast:redraw curve-canvas)))))))))
             (setf curve-channel-buttons buttons)
             (register-inspector-row
              (lightfast:make-layout-row
               :gap 4
               :children (mapcar (lambda (entry)
                                   (lightfast:make-layout-item (rest entry)
                                                               :basis 0 :grow 1))
                                 buttons))
              (mapcar #'rest buttons)
              node-page 12 44 26 :page)
             (sync-curve-channel-buttons))
           (setf scope-canvas
                 (register-inspector
                  (lightfast:make-canvas
                   :parent node-page :x 12 :y 76 :width 292 :height 104
                   :callback (lambda (widget event value)
                               (declare (ignore event value))
                               (draw-scope widget)))
                  12 76 :fill :scope :page))
           (lightfast:set-tooltip
            scope-canvas
            "Waveform: highlights at the top, shadows at the bottom")
           (setf curve-canvas
                 (register-inspector
                  (lightfast:make-canvas
                   :parent node-page :x 12 :y 188 :width 292 :height 150
                   :callback (lambda (widget event value)
                               (declare (ignore event value))
                               (draw-curve-editor widget)))
                  12 :chart-row :fill :chart :page))
           (lightfast:set-tooltip
            curve-canvas
            "Drag a point to shape its channel, click the chart to add one, right-click a point to remove it")
           (dolist (event (list lightfast:+event-push+
                                lightfast:+event-drag+
                                lightfast:+event-release+))
             (lightfast:on curve-canvas #'handle-curve-mouse :event event))
           (register-inspector
            (lightfast:make-button
             :parent node-page :x 12 :y 346 :width 292 :height 26
             :label "Reset Channel"
             :callback (lambda (&rest ignored)
                         (declare (ignore ignored))
                         (reset-curve-channel)))
            12 :page-action-row :fill 26 :page)))
        ;; The two footer buttons split the inspector's width evenly, which the
        ;; flex engine does directly instead of two mirrored width modes that
        ;; each had to recompute the same half.
        (register-inspector-row
         (lightfast:make-layout-row
          :gap 12
          :children
          (list (lightfast:make-layout-item
                 (lightfast:set-stock-icon
                  (lightfast:make-button
                   :parent inspector :x 12 :y 674
                   :width 140 :height 26 :label "Reset selected"
                   :callback (lambda (&rest ignored)
                               (declare (ignore ignored))
                               (gui-model-reset-selected model)
                               (sync-controls)
                               (schedule-edited-preview)))
                  :reload)
                 :basis 0 :grow 1)
                (lightfast:make-layout-item
                 (lightfast:set-stock-icon
                  (lightfast:make-button
                   :parent inspector :x 166 :y 674
                   :width 142 :height 26 :label "Export current"
                   :callback (lambda (&rest ignored)
                               (declare (ignore ignored))
                               (render-selected)))
                  :export)
                 :basis 0 :grow 1)))
         '() inspector 12 :action-row 26)
        (setf progress (lightfast:make-progress :parent window :x 0 :y 772
                                              :width 180 :height 28 :value "0")
              status (lightfast:make-status-bar :parent window :x 180 :y 772
                                               :width 1100 :height 28
                                               :value "Ready")
              root-layout
              (make-root-layout menu toolbar main-tile progress status))
        (lightfast:tile-size-range main-tile left-column
                                 :min-width 180 :max-width 420)
        (lightfast:tile-size-range main-tile center-pane :min-width 300)
        (lightfast:tile-size-range main-tile right-column
                                 :min-width *right-sidebar-min-width*
                                 :max-width 480)
        (lightfast:tile-size-range left-column filmstrip-pane :min-height 140)
        (lightfast:tile-size-range left-column gallery-pane :min-height 140)
        (lightfast:tile-size-range right-column inspector
                                 :min-height *inspector-min-height*)
        (lightfast:tile-size-range right-column graph-pane :min-height 140)
        (lightfast:on-resize window #'layout-ui)
        (lightfast:on-resize filmstrip-pane #'layout-left-pane)
        (lightfast:on-resize gallery-pane #'layout-gallery-pane)
        (lightfast:on-resize center-pane #'layout-center-pane)
        (lightfast:on-resize inspector #'layout-inspector-pane)
        (lightfast:on-resize graph-pane #'layout-graph-pane)
        (gui-model-set-selected-indices
         model (if (project-photos project) '(0) '()))
        (sync-controls)
        (sync-node-tools)
        (layout-ui)
        (setf poll-id (lightfast:add-timeout 0.08d0 #'poll :repeat t))
        ;; Best-effort housekeeping of the persistent preview cache.
        (enqueue-gui-task background-queue :evict
                          (lambda ()
                            (ignore-errors
                              (evict-stale-previews preview-directory))))
        (when (selected-job)
          (schedule-initial-preview))
        (unwind-protect
             (progn (lightfast:show window) (lightfast:run))
          (when poll-id (ignore-errors (lightfast:remove-timeout poll-id)))
          (when debounce-id (ignore-errors (lightfast:remove-timeout debounce-id)))
          (stop-gui-queue queue)
          (stop-gui-queue background-queue)
          (stop-gui-queue histogram-queue)
          (when after-live-front (cffi:foreign-free after-live-front))
          (when after-live-back (cffi:foreign-free after-live-back))
          (setf after-live-front nil after-live-back nil)
          (release-waveform)
          (clear-preview-cache)
          (ignore-errors
            (uiop:delete-directory-tree preview-session-directory :validate t
                                                                  :if-does-not-exist :ignore))
          (when window (ignore-errors (lightfast:destroy window))))))))

(defun main (&optional pathname)
  "Launch Orfeus with optional PHOTO or PROJECT PATHNAME."
  (let ((argument (or pathname (first (uiop:command-line-arguments)))))
    (run-gui argument)))
