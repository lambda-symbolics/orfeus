;; Agfalike C: the whole look for Orfeus's gallery, written by
;; scripts/lightroom-preset-lut.lisp from the Lightroom preset "JSZ-CEDAR C".
(:orfeus-still 1
 (:name "Agfalike C" :graph
  (:nodes
   ((:id 1 :kind :optics :inputs (0) :params
     (:lens-correction-p t :lens-correction-strength 1.0
      :chromatic-aberration-correction-p t :chromatic-aberration-source
      :measured :lens-distortion 0.0 :lens-profile nil :lens-focal-length nil
      :demosaic :rcd))
    (:id 2 :kind :noise-reduction :inputs (1) :params
     (:noise-reduction 0.45 :neural-noise-reduction 0.0))
    (:id 3 :kind :sharpen :inputs (2) :params
     (:sharpen-amount 0.5 :sharpen-radius 1.5 :sharpen-threshold 2.0))
    (:id 4 :kind :clarity :inputs (3) :params (:amount 0.3 :radius 150.0))
    (:id 5 :kind :vignette :inputs (4) :params
     (:amount -0.12 :midpoint 0.0 :feather 0.74 :roundness 0.14))
    (:id 6 :kind :film :inputs (5) :params
     (:lut-path "luts/Agfalike C.cube" :lut-strength 1.0 :grain-amount 0.08
      :grain-size 1.32)))
   :output 6)))
