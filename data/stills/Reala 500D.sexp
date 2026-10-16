;; Reala 500D: the whole look for Orfeus's gallery, hand-calibrated from
;; measured reference photographs (not converted from a Lightroom preset)
;; and baked by scripts/lightroom-preset-lut.lisp.
(:orfeus-still 1
 (:name "Reala 500D" :graph
  (:nodes
   ((:id 1 :kind :white-balance :inputs (0) :params
     (:white-balance-temperature 5200.0 :white-balance-tint 0.0))
    (:id 2 :kind :optics :inputs (1) :params
     (:lens-correction-p t :lens-correction-strength 1.0
      :chromatic-aberration-correction-p t :chromatic-aberration-source
      :measured :lens-distortion 0.0 :lens-profile nil :lens-focal-length nil
      :demosaic :rcd))
    (:id 3 :kind :noise-reduction :inputs (2) :params
     (:noise-reduction 0.45 :neural-noise-reduction 0.0))
    (:id 4 :kind :sharpen :inputs (3) :params
     (:sharpen-amount 0.5 :sharpen-radius 1.5 :sharpen-threshold 2.0))
    (:id 5 :kind :film :inputs (4) :params
     (:lut-path "luts/Reala 500D.cube" :lut-strength 1.0 :grain-amount 0.0
      :grain-size 1.0)))
   :output 5)))
