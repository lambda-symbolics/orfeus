;; Reala 500D: the whole look for Orfeus's gallery, hand-calibrated from
;; measured reference photographs (not converted from a Lightroom preset)
;; and baked by scripts/lightroom-preset-lut.lisp.
(:orfeus-still 1
 (:name "Reala 500D" :graph
  (:nodes
   ((:id 1 :kind :optics :inputs (0) :params
     (:lens-correction-p t :lens-correction-strength 1.0
      :chromatic-aberration-correction-p t :chromatic-aberration-source
      :measured :lens-distortion 0.0 :lens-profile nil :lens-focal-length nil
      :demosaic :rcd))
    (:id 2 :kind :noise-reduction :inputs (1) :params
     (:noise-reduction 0.45 :neural-noise-reduction 0.0))
    (:id 3 :kind :white-balance :inputs (2) :params
     (:white-balance-temperature 5200.0 :white-balance-tint 0.0))
    (:id 4 :kind :sharpen :inputs (3) :params
     (:sharpen-amount 0.5 :sharpen-radius 1.5 :sharpen-threshold 2.0))
    (:id 5 :kind :film :inputs (4) :params
     (:lut-path "luts/Reala 500D.cube" :lut-strength 1.0 :grain-amount 0.3
      :grain-size 1.3)))
   :output 5)))
