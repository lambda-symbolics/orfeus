;;;; lightroom-preset-lut.lisp -- bake a Lightroom / Camera Raw preset into a
;;;; 3D CUBE LUT for Orfeus's Film node.
;;;;
;;;;   sbcl --script scripts/lightroom-preset-lut.lisp OUT-DIRECTORY PRESET.xmp...
;;;;
;;;; Self-contained SBCL; no dependencies. One .cube per preset, named after the
;;;; preset's crs:Name, 33 steps a side.
;;;;
;;;; What is baked: the colour work of the preset -- camera calibration
;;;; (primary hue and saturation), a custom white balance taken relative to
;;;; daylight, exposure, the Basic panel's tone controls, the parametric and
;;;; point tone curves (RGB and per channel), the HSL panel, vibrance and
;;;; saturation, and split toning. What is not, because a LUT cannot hold it:
;;;; sharpening, noise reduction, grain, the post-crop vignette, clarity and
;;;; dehaze. Orfeus has nodes for those; the values a preset asked for are
;;;; written into the .cube's header as comments.
;;;;
;;;; Adobe's pipeline is not public. Each control here is a documented
;;;; approximation of what the slider does to an already developed picture, in
;;;; the order Camera Raw applies them, with ranges chosen so that a slider at
;;;; its limit does about what Lightroom's does. The LUT is applied by the Film
;;;; node to display-referred sRGB, so that is the domain everything works in.

(require :asdf)

(defpackage #:lightroom-preset-lut
  (:use #:cl)
  (:export #:convert #:main))

(in-package #:lightroom-preset-lut)

(defparameter *lut-size* 33)

(defparameter *daylight-kelvin* 5500d0
  "The light a preset's custom white balance is taken relative to: a preset
that sets 4600 K makes a daylight photograph cooler by that much.")

;;; ------------------------------------------------------------- XMP reading

(defun slurp (path)
  (with-open-file (stream path :external-format :utf-8)
    (let ((text (make-string (file-length stream))))
      (subseq text 0 (read-sequence text stream)))))

(defun attribute (text name)
  "The value of the crs:NAME attribute in TEXT, or NIL."
  (let* ((key (format nil "crs:~A=\"" name))
         (start (search key text)))
    (when start
      (let* ((from (+ start (length key)))
             (end (position #\" text :start from)))
        (subseq text from end)))))

(defun number-attribute (text name &optional (default 0d0))
  (let ((value (attribute text name)))
    (if (and value (plusp (length value)))
        (let ((*read-default-float-format* 'double-float))
          (coerce (read-from-string (string-left-trim "+" value)) 'double-float))
        default)))

(defun element-text (text name)
  "The body of the <crs:NAME> element in TEXT, or NIL."
  (let* ((open (format nil "<crs:~A>" name))
         (close (format nil "</crs:~A>" name))
         (start (search open text)))
    (when start
      (let ((end (search close text :start2 start)))
        (when end
          (subseq text (+ start (length open)) end))))))

(defun curve-points (text name)
  "The (x . y) points of the tone curve element NAME, on 0..255, or NIL."
  (let ((body (element-text text name))
        (points '()))
    (when body
      (loop with start = 0
            for li = (search "<rdf:li>" body :start2 start)
            while li
            do (let* ((from (+ li 8))
                      (end (search "</rdf:li>" body :start2 from))
                      (item (subseq body from end))
                      (comma (position #\, item)))
                 (push (cons (parse-integer item :end comma)
                             (parse-integer item :start (1+ comma)))
                       points)
                 (setf start end)))
      (nreverse points))))

(defun read-preset-name (text)
  (let ((body (element-text text "Name")))
    (if body
        (let* ((start (search "x-default\">" body))
               (from (+ start 11))
               (end (search "</rdf:li>" body :start2 from)))
          (string-trim " " (subseq body from end)))
        "preset")))

;;; -------------------------------------------------------------- arithmetic

(defun clamp (value low high)
  (max low (min high value)))

(defun smoothstep (edge0 edge1 x)
  (let ((t0 (clamp (/ (- x edge0) (- edge1 edge0)) 0d0 1d0)))
    (* t0 t0 (- 3d0 (* 2d0 t0)))))

(defun srgb-encode (linear)
  (let ((v (max 0d0 linear)))
    (if (<= v 0.0031308d0)
        (* 12.92d0 v)
        (- (* 1.055d0 (expt v (/ 1d0 2.4d0))) 0.055d0))))

(defun srgb-decode (encoded)
  (let ((v (max 0d0 encoded)))
    (if (<= v 0.04045d0)
        (/ v 12.92d0)
        (expt (/ (+ v 0.055d0) 1.055d0) 2.4d0))))

(defun luminance (r g b)
  (+ (* 0.2126d0 r) (* 0.7152d0 g) (* 0.0722d0 b)))

;;; Monotone cubic interpolation (Fritsch-Carlson) through a point curve, so
;;; the curve never overshoots the points it was drawn through.

(defun monotone-curve (points)
  "Return a function of 0..1 through POINTS, each (x . y) on 0..255."
  (let* ((n (length points))
         (xs (map 'vector (lambda (p) (/ (car p) 255d0)) points))
         (ys (map 'vector (lambda (p) (/ (cdr p) 255d0)) points))
         (slopes (make-array n :initial-element 0d0)))
    (when (< n 2)
      (return-from monotone-curve #'identity))
    (let ((deltas (make-array (1- n))))
      (dotimes (i (1- n))
        (setf (aref deltas i)
              (/ (- (aref ys (1+ i)) (aref ys i))
                 (max 1d-9 (- (aref xs (1+ i)) (aref xs i))))))
      (setf (aref slopes 0) (aref deltas 0)
            (aref slopes (1- n)) (aref deltas (- n 2)))
      (loop for i from 1 below (1- n)
            do (setf (aref slopes i)
                     (if (<= (* (aref deltas (1- i)) (aref deltas i)) 0d0)
                         0d0
                         (/ 2d0 (+ (/ 1d0 (aref deltas (1- i)))
                                   (/ 1d0 (aref deltas i))))))))
    (lambda (x)
      (cond ((<= x (aref xs 0)) (aref ys 0))
            ((>= x (aref xs (1- n))) (aref ys (1- n)))
            (t
             (let ((i (loop for k from 0 below (1- n)
                            when (<= (aref xs k) x (aref xs (1+ k)))
                              return k)))
               (let* ((h (- (aref xs (1+ i)) (aref xs i)))
                      (u (/ (- x (aref xs i)) h))
                      (h00 (+ (* 2 u u u) (* -3 u u) 1))
                      (h10 (+ (* u u u) (* -2 u u) u))
                      (h01 (+ (* -2 u u u) (* 3 u u)))
                      (h11 (- (* u u u) (* u u))))
                 (clamp (+ (* h00 (aref ys i))
                           (* h10 h (aref slopes i))
                           (* h01 (aref ys (1+ i)))
                           (* h11 h (aref slopes (1+ i))))
                        0d0 1d0))))))))

;;; ------------------------------------------------------------ white balance

(defun planckian-xy (kelvin)
  "CIE xy of the Planckian locus at KELVIN (Kim et al.'s cubic fit)."
  (let* ((tk (clamp kelvin 1667d0 25000d0))
         (x (if (<= tk 4000d0)
                (+ (/ -0.2661239d9 (expt tk 3)) (/ -0.2343589d6 (expt tk 2))
                   (/ 0.8776956d3 tk) 0.179910d0)
                (+ (/ -3.0258469d9 (expt tk 3)) (/ 2.1070379d6 (expt tk 2))
                   (/ 0.2226347d3 tk) 0.240390d0)))
         (y (cond ((<= tk 2222d0)
                   (+ (* -1.1063814d0 (expt x 3)) (* -1.34811020d0 (expt x 2))
                      (* 2.18555832d0 x) -0.20219683d0))
                  ((<= tk 4000d0)
                   (+ (* -0.9549476d0 (expt x 3)) (* -1.37418593d0 (expt x 2))
                      (* 2.09137015d0 x) -0.16748867d0))
                  (t
                   (+ (* 3.0817580d0 (expt x 3)) (* -5.87338670d0 (expt x 2))
                      (* 3.75112997d0 x) -0.37001483d0)))))
    (values x y)))

(defun kelvin-rgb (kelvin)
  "Linear sRGB of a white at KELVIN, green scaled to one."
  (multiple-value-bind (x y) (planckian-xy kelvin)
    (let* ((bx (/ x y)) (by 1d0) (bz (/ (- 1d0 x y) y))
           (r (+ (* 3.2406d0 bx) (* -1.5372d0 by) (* -0.4986d0 bz)))
           (g (+ (* -0.9689d0 bx) (* 1.8758d0 by) (* 0.0415d0 bz)))
           (b (+ (* 0.0557d0 bx) (* -0.2040d0 by) (* 1.0570d0 bz))))
      (values (/ r g) 1d0 (/ b g)))))

(defun white-balance-gains (kelvin tint)
  "Channel gains that render a daylight photograph as a preset set to KELVIN
and TINT would: the ratio of the daylight white to the chosen white, tint
pulling green down for magenta and up for green."
  (multiple-value-bind (dr dg db) (kelvin-rgb *daylight-kelvin*)
    (multiple-value-bind (tr tg tb) (kelvin-rgb kelvin)
      (list (/ dr tr) (* (/ dg tg) (- 1d0 (* 0.35d0 (/ tint 150d0)))) (/ db tb)))))

;;; -------------------------------------------------------------- calibration

(defun rotate-hue-linear (r g b degrees)
  "Rotate the hue of a linear RGB colour by DEGREES about the grey axis,
keeping its luminance: the colour's two chroma differences turn as a vector."
  (let* ((theta (* degrees (/ pi 180d0)))
         (c (cos theta)) (s (sin theta))
         (y (luminance r g b))
         (i-axis (- r y)) (q-axis (- b y))
         (ci (- (* i-axis c) (* q-axis s)))
         (cq (+ (* i-axis s) (* q-axis c)))
         (nr (+ y ci)) (nb (+ y cq))
         (ng (/ (- y (* 0.2126d0 nr) (* 0.0722d0 nb)) 0.7152d0)))
    (values nr ng nb)))

(defun calibration-matrix (text)
  "The 3x3 matrix Camera Raw's calibration sliders amount to: each primary's
hue turned by up to thirty degrees and its saturation scaled by up to a
factor of two, rows normalised so white stays white."
  (flet ((primary (name r g b)
           (let* ((hue (number-attribute text (format nil "~AHue" name)))
                  (sat (number-attribute text (format nil "~ASaturation" name)))
                  (factor (expt 2d0 (/ sat 100d0))))
             (multiple-value-bind (nr ng nb) (rotate-hue-linear r g b (* 30d0 (/ hue 100d0)))
               (let ((y (luminance nr ng nb)))
                 (list (+ y (* factor (- nr y)))
                       (+ y (* factor (- ng y)))
                       (+ y (* factor (- nb y)))))))))
    (let* ((red (primary "Red" 1d0 0d0 0d0))
           (green (primary "Green" 0d0 1d0 0d0))
           (blue (primary "Blue" 0d0 0d0 1d0))
           ;; Columns are the new primaries; normalise each row so that
           ;; (1 1 1) maps to (1 1 1).
           (rows (loop for i below 3
                       collect (let* ((row (list (nth i red) (nth i green) (nth i blue)))
                                      (sum (reduce #'+ row)))
                                 (mapcar (lambda (v) (/ v sum)) row)))))
      rows)))

(defun apply-matrix (rows r g b)
  (flet ((row (k) (let ((w (nth k rows)))
                    (+ (* (first w) r) (* (second w) g) (* (third w) b)))))
    (values (row 0) (row 1) (row 2))))

;;; ---------------------------------------------------------- basic tone panel

(defun contrast (v amount)
  "Camera Raw's Contrast about middle grey: positive is an S-curve that keeps
black and white where they are; negative flattens the midtones most and lifts
black and lowers white by less -- about a fifth of the range at -100 -- which
is what a flat, faded Lightroom look is made of."
  (let ((k (/ amount 100d0)) (p 0.5d0))
    (if (>= k 0d0)
        (+ p (* (- v p) (+ 1d0 (* 0.6d0 k (- 1d0 (/ (abs (- v p)) p))))))
        (+ p (* (- v p) (+ 1d0 (* 0.5d0 k (- 1d0 (* 0.5d0 (/ (abs (- v p)) p))))))))))

(defun highlights (v amount)
  "Highlights: the upper tones roll off from a third of the way up, white
itself moving by at most 0.14, which is about what Lightroom's -100 does to a
clipped white with nothing left to recover."
  (+ v (* (/ amount 100d0) 0.14d0 (smoothstep 0.35d0 0.95d0 v))))

(defun shadows (v amount)
  (let ((u (clamp (/ v 0.6d0) 0d0 1d0)))
    (+ v (* (/ amount 100d0) 0.12d0 4d0 u (- 1d0 u)))))

(defun whites (v amount)
  (+ v (* (/ amount 100d0) 0.12d0 (smoothstep 0.55d0 1d0 v))))

(defun blacks (v amount)
  (+ v (* (/ amount 100d0) 0.08d0 (- 1d0 (smoothstep 0d0 0.45d0 v)))))

(defun parametric (v shadows darks lights highlights s1 s2 s3)
  "The parametric curve's four regions as tents between the region centres,
each moving its region by up to 0.15, the ends held."
  (let* ((c0 (/ s1 2d0)) (c1 (/ (+ s1 s2) 2d0)) (c2 (/ (+ s2 s3) 2d0)) (c3 (/ (+ s3 1d0) 2d0))
         (centres (list c0 c1 c2 c3))
         (amounts (list shadows darks lights highlights))
         (edge (* (smoothstep 0d0 0.08d0 v) (smoothstep 1d0 0.92d0 v)))
         (shift 0d0))
    (loop for i below 4
          for c = (nth i centres)
          for a = (nth i amounts)
          for w = (cond ((and (= i 0) (<= v c)) 1d0)
                        ((and (= i 3) (>= v c)) 1d0)
                        ((and (> i 0) (<= (nth (1- i) centres) v c))
                         (/ (- v (nth (1- i) centres)) (- c (nth (1- i) centres))))
                        ((and (< i 3) (<= c v (nth (1+ i) centres)))
                         (/ (- (nth (1+ i) centres) v) (- (nth (1+ i) centres) c)))
                        (t 0d0))
          do (incf shift (* w (/ a 100d0) 0.15d0)))
    (+ v (* shift edge))))

;;; ------------------------------------------------------------------- HSL

(defparameter *hsl-centres* '(0d0 30d0 60d0 120d0 180d0 240d0 280d0 320d0)
  "Where Lightroom's eight colour bands sit on the hue circle: red, orange,
yellow, green, aqua, blue, purple, magenta.")

(defparameter *hsl-names* '("Red" "Orange" "Yellow" "Green" "Aqua" "Blue" "Purple" "Magenta"))

(defun band-weights (hue)
  "Tent weights of the eight bands at HUE (degrees), summing to one."
  (let ((weights (make-list 8 :initial-element 0d0))
        (h (mod hue 360d0)))
    (loop for i below 8
          for c = (nth i *hsl-centres*)
          for next = (if (= i 7) 360d0 (nth (1+ i) *hsl-centres*))
          when (<= c h next)
            do (let ((u (/ (- h c) (- next c))))
                 (incf (nth i weights) (- 1d0 u))
                 (incf (nth (mod (1+ i) 8) weights) u)))
    weights))

(defun band-hue-shift (i amount)
  "Degrees a band moves at AMOUNT: +100 puts it on the next band, -100 on the
one before, which is what the slider's own colour ramp promises."
  (let* ((c (nth i *hsl-centres*))
         (next (if (= i 7) 360d0 (nth (1+ i) *hsl-centres*)))
         (previous (if (= i 0) (- (nth 7 *hsl-centres*) 360d0) (nth (1- i) *hsl-centres*))))
    (if (>= amount 0d0)
        (* (/ amount 100d0) (- next c))
        (* (/ amount 100d0) (- c previous)))))

(defun rgb->hsv (r g b)
  (let* ((mx (max r g b)) (mn (min r g b)) (d (- mx mn))
         (h (cond ((<= d 1d-12) 0d0)
                  ((= mx r) (mod (* 60d0 (/ (- g b) d)) 360d0))
                  ((= mx g) (+ 120d0 (* 60d0 (/ (- b r) d))))
                  (t (+ 240d0 (* 60d0 (/ (- r g) d))))))
         (s (if (<= mx 1d-12) 0d0 (/ d mx))))
    (values h s mx)))

(defun hsv->rgb (h s v)
  (let* ((hh (/ (mod h 360d0) 60d0))
         (i (floor hh))
         (f (- hh i))
         (p (* v (- 1d0 s)))
         (q (* v (- 1d0 (* s f))))
         (u (* v (- 1d0 (* s (- 1d0 f))))))
    (ecase (mod i 6)
      (0 (values v u p)) (1 (values q v p)) (2 (values p v u))
      (3 (values p q v)) (4 (values u p v)) (5 (values v p q)))))

(defun hsl-adjust (r g b hues sats lums)
  "Lightroom's HSL panel on an encoded colour: hue turned, saturation scaled,
luminance moved by up to a stop, each by the bands the colour belongs to."
  (multiple-value-bind (h s v) (rgb->hsv r g b)
    (if (< s 1d-6)
        (values r g b)
        (let* ((weights (band-weights h))
               (membership (smoothstep 0d0 0.35d0 s))
               (shift (loop for i below 8 sum (* (nth i weights) (band-hue-shift i (nth i hues)))))
               (sat-factor (loop for i below 8 sum (* (nth i weights) (+ 1d0 (/ (nth i sats) 100d0)))))
               (lum-stops (loop for i below 8 sum (* (nth i weights) (/ (nth i lums) 100d0)))))
          (multiple-value-bind (nr ng nb)
              (hsv->rgb (+ h (* shift membership))
                        (clamp (* s (+ 1d0 (* membership (- sat-factor 1d0)))) 0d0 1d0)
                        v)
            (let ((factor (expt 2d0 (/ (* lum-stops membership) 2.2d0))))
              (values (* nr factor) (* ng factor) (* nb factor))))))))

(defun saturation-vibrance (r g b saturation vibrance)
  (multiple-value-bind (h s v) (rgb->hsv r g b)
    (let* ((skin (smoothstep 10d0 25d0 h))
           (skin-protect (- 1d0 (* 0.6d0 (* skin (smoothstep 55d0 40d0 h)))))
           (s1 (* s (+ 1d0 (/ saturation 100d0))))
           (s2 (+ s1 (* (/ vibrance 100d0) 2d0 s1 (- 1d0 s1) skin-protect))))
      (hsv->rgb h (clamp s2 0d0 1d0) v))))

(defun split-tone (r g b shadow-hue shadow-sat highlight-hue highlight-sat balance)
  (if (and (zerop shadow-sat) (zerop highlight-sat))
      (values r g b)
      (let* ((l (luminance r g b))
             (pivot (+ 0.5d0 (* 0.25d0 (/ balance 100d0))))
             (shadow-weight (- 1d0 (smoothstep 0d0 (+ pivot 0.1d0) l)))
             (highlight-weight (smoothstep (- pivot 0.1d0) 1d0 l)))
        (flet ((tint (hue sat weight)
                 (multiple-value-bind (tr tg tb) (hsv->rgb hue 1d0 1d0)
                   (let ((grey (luminance tr tg tb))
                         (k (* 0.3d0 (/ sat 100d0) weight)))
                     (list (* k (- tr grey)) (* k (- tg grey)) (* k (- tb grey)))))))
          (let ((sh (tint shadow-hue shadow-sat shadow-weight))
                (hi (tint highlight-hue highlight-sat highlight-weight)))
            (values (+ r (first sh) (first hi))
                    (+ g (second sh) (second hi))
                    (+ b (third sh) (third hi))))))))

;;; ---------------------------------------------------------------- pipeline

(defstruct preset
  name gains matrix exposure contrast highlights shadows whites blacks
  parametric point-curve red-curve green-curve blue-curve
  hues sats lums saturation vibrance split notes)

(defun read-preset (path)
  (let* ((text (slurp path))
         (custom-wb (equal (attribute text "WhiteBalance") "Custom"))
         (band (lambda (prefix)
                 (mapcar (lambda (name)
                           (number-attribute text (format nil "~A~A" prefix name)))
                         *hsl-names*)))
         (curve (lambda (name)
                  (let ((points (curve-points text name)))
                    (if (and points
                             (not (equal points '((0 . 0) (255 . 255)))))
                        (monotone-curve points)
                        nil)))))
    (make-preset
     :name (read-preset-name text)
     :gains (when custom-wb
              (white-balance-gains (number-attribute text "Temperature" *daylight-kelvin*)
                                   (number-attribute text "Tint")))
     :matrix (calibration-matrix text)
     :exposure (number-attribute text "Exposure2012")
     :contrast (number-attribute text "Contrast2012")
     :highlights (number-attribute text "Highlights2012")
     :shadows (number-attribute text "Shadows2012")
     :whites (number-attribute text "Whites2012")
     :blacks (number-attribute text "Blacks2012")
     :parametric (list (number-attribute text "ParametricShadows")
                       (number-attribute text "ParametricDarks")
                       (number-attribute text "ParametricLights")
                       (number-attribute text "ParametricHighlights")
                       (/ (number-attribute text "ParametricShadowSplit" 25d0) 100d0)
                       (/ (number-attribute text "ParametricMidtoneSplit" 50d0) 100d0)
                       (/ (number-attribute text "ParametricHighlightSplit" 75d0) 100d0))
     :point-curve (funcall curve "ToneCurvePV2012")
     :red-curve (funcall curve "ToneCurvePV2012Red")
     :green-curve (funcall curve "ToneCurvePV2012Green")
     :blue-curve (funcall curve "ToneCurvePV2012Blue")
     :hues (funcall band "HueAdjustment")
     :sats (funcall band "SaturationAdjustment")
     :lums (funcall band "LuminanceAdjustment")
     :saturation (number-attribute text "Saturation")
     :vibrance (number-attribute text "Vibrance")
     :split (list (number-attribute text "SplitToningShadowHue")
                  (number-attribute text "SplitToningShadowSaturation")
                  (number-attribute text "SplitToningHighlightHue")
                  (number-attribute text "SplitToningHighlightSaturation")
                  (number-attribute text "SplitToningBalance"))
     :notes (remove nil
                    (list (let ((v (number-attribute text "Clarity2012")))
                            (unless (zerop v) (format nil "Clarity ~@D" (round v))))
                          (let ((v (number-attribute text "Dehaze")))
                            (unless (zerop v) (format nil "Dehaze ~@D" (round v))))
                          (let ((v (number-attribute text "PostCropVignetteAmount")))
                            (unless (zerop v)
                              (format nil "Vignette ~@D, midpoint ~D, feather ~D, roundness ~@D"
                                      (round v)
                                      (round (number-attribute text "PostCropVignetteMidpoint" 50d0))
                                      (round (number-attribute text "PostCropVignetteFeather" 50d0))
                                      (round (number-attribute text "PostCropVignetteRoundness")))))
                          (let ((v (number-attribute text "GrainAmount")))
                            (unless (zerop v)
                              (format nil "Grain ~D, size ~D" (round v)
                                      (round (number-attribute text "GrainSize" 25d0)))))
                          (let ((v (number-attribute text "Sharpness")))
                            (unless (zerop v)
                              (format nil "Sharpening ~D, radius ~,1F" (round v)
                                      (number-attribute text "SharpenRadius" 1d0))))
                          (when custom-wb
                            (format nil "White balance ~D K, tint ~@D, taken relative to ~D K daylight"
                                    (round (number-attribute text "Temperature"))
                                    (round (number-attribute text "Tint"))
                                    (round *daylight-kelvin*))))))))

(defun develop (preset r g b)
  "PRESET applied to one display-referred sRGB colour; returns three values."
  (let ((lr (srgb-decode r)) (lg (srgb-decode g)) (lb (srgb-decode b)))
    ;; White balance, relative to daylight.
    (when (preset-gains preset)
      (destructuring-bind (kr kg kb) (preset-gains preset)
        (setf lr (* lr kr) lg (* lg kg) lb (* lb kb))))
    ;; Calibration.
    (multiple-value-setq (lr lg lb) (apply-matrix (preset-matrix preset) lr lg lb))
    ;; Exposure.
    (let ((gain (expt 2d0 (preset-exposure preset))))
      (setf lr (max 0d0 (* lr gain)) lg (max 0d0 (* lg gain)) lb (max 0d0 (* lb gain))))
    ;; The Basic panel and the curves, per channel, on the encoded signal.
    (flet ((tone (linear)
             (let ((v (srgb-encode linear)))
               (setf v (contrast v (preset-contrast preset)))
               (setf v (highlights v (preset-highlights preset)))
               (setf v (shadows v (preset-shadows preset)))
               (setf v (whites v (preset-whites preset)))
               (setf v (blacks v (preset-blacks preset)))
               (setf v (clamp v 0d0 1d0))
               (setf v (apply #'parametric v (preset-parametric preset)))
               (setf v (clamp v 0d0 1d0))
               (when (preset-point-curve preset)
                 (setf v (funcall (preset-point-curve preset) v)))
               v)))
      (let ((er (tone lr)) (eg (tone lg)) (eb (tone lb)))
        (when (preset-red-curve preset) (setf er (funcall (preset-red-curve preset) er)))
        (when (preset-green-curve preset) (setf eg (funcall (preset-green-curve preset) eg)))
        (when (preset-blue-curve preset) (setf eb (funcall (preset-blue-curve preset) eb)))
        ;; Colour: HSL bands, then vibrance and saturation, then split toning.
        (multiple-value-setq (er eg eb)
          (hsl-adjust er eg eb (preset-hues preset) (preset-sats preset) (preset-lums preset)))
        (multiple-value-setq (er eg eb)
          (saturation-vibrance (clamp er 0d0 1d0) (clamp eg 0d0 1d0) (clamp eb 0d0 1d0)
                               (preset-saturation preset) (preset-vibrance preset)))
        (multiple-value-setq (er eg eb)
          (apply #'split-tone er eg eb (preset-split preset)))
        (values (clamp er 0d0 1d0) (clamp eg 0d0 1d0) (clamp eb 0d0 1d0))))))

;;; ------------------------------------------------------------------ output

(defun write-cube (preset path)
  (with-open-file (stream path :direction :output :if-exists :supersede
                               :external-format :utf-8)
    (format stream "# Baked from the Lightroom preset ~S by scripts/lightroom-preset-lut.lisp~%"
            (preset-name preset))
    (format stream "# Colour only; not in this LUT (add the node instead):~%")
    (dolist (note (preset-notes preset))
      (format stream "#   ~A~%" note))
    (format stream "TITLE ~S~%~%LUT_3D_SIZE ~D~%~%DOMAIN_MIN 0.0 0.0 0.0~%DOMAIN_MAX 1.0 1.0 1.0~%~%"
            (preset-name preset) *lut-size*)
    (let ((steps (1- *lut-size*)))
      (dotimes (bi *lut-size*)
        (dotimes (gi *lut-size*)
          (dotimes (ri *lut-size*)
            (multiple-value-bind (r g b)
                (develop preset (/ ri steps) (/ gi steps) (/ bi steps))
              (format stream "~,6F ~,6F ~,6F~%" r g b))))))))

(defun convert (xmp-path out-directory)
  (let* ((preset (read-preset xmp-path))
         (out (merge-pathnames (make-pathname :name (preset-name preset) :type "cube")
                               (uiop:ensure-directory-pathname out-directory))))
    (write-cube preset out)
    (format t "~A -> ~A~@[  (not baked: ~{~A~^; ~})~]~%" xmp-path out (preset-notes preset))
    out))

(defun main (arguments)
  (when (< (length arguments) 2)
    (format *error-output* "usage: lightroom-preset-lut.lisp OUT-DIRECTORY PRESET.xmp...~%")
    (uiop:quit 2))
  (let ((out (first arguments)))
    (ensure-directories-exist (uiop:ensure-directory-pathname out))
    (dolist (xmp (rest arguments))
      (convert xmp out)))
  (uiop:quit 0))

(when (and (boundp 'sb-ext:*posix-argv*)
           (not (find-package "SWANK"))
           (not (find-package "SLYNK")))
  (main (rest sb-ext:*posix-argv*)))
