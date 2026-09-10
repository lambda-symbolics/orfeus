;; Agfalike D: the whole look for Orfeus's gallery, written by
;; scripts/lightroom-preset-lut.lisp from the Lightroom preset "JSZ-CEDAR D".
(:orfeus-still 1
 (:name "Agfalike D" :graph
  (:nodes
   ((:id 1 :kind :optics :inputs (0) :params
     (:lens-correction-p t :lens-correction-strength 1.0
      :chromatic-aberration-correction-p t :chromatic-aberration-source
      :measured :lens-distortion 0.0 :lens-profile nil :lens-focal-length nil
      :demosaic :rcd))
    (:id 2 :kind :dehaze :inputs (1) :params (:amount 0.21))
    (:id 3 :kind :noise-reduction :inputs (2) :params
     (:noise-reduction 0.45 :neural-noise-reduction 0.0))
    (:id 4 :kind :sharpen :inputs (3) :params
     (:sharpen-amount 0.5 :sharpen-radius 1.5 :sharpen-threshold 2.0))
    (:id 5 :kind :clarity :inputs (4) :params (:amount 0.33 :radius 150.0))
    (:id 6 :kind :vignette :inputs (5) :params
     (:amount -0.1 :midpoint 0.0 :feather 1.0 :roundness 0.07))
    (:id 7 :kind :film :inputs (6) :params
     (:lut-path "luts/Agfalike D.cube" :lut-strength 1.0 :grain-amount 0.12
      :grain-size 1.32)))
   :output 7)))
