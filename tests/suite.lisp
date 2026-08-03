(in-package #:orfeus/tests)

(defun test-temporary-pathname (suffix)
  (merge-pathnames
   (format nil "orfeus-test-~D-~D.~A"
           (get-universal-time) (random most-positive-fixnum) suffix)
   #P"/tmp/"))

(defun project-test-value ()
  (make-project
   :output-directory #P"exports/"
   :defaults (make-processing-settings :exposure 0.75 :grain-amount 0.12
                                       :tone-shadows 0.6 :tone-highlights -0.4
                                       :lens-correction-strength 0.65)
   :photos (list (make-photo-job
                  :input-path #P"input/example.orf"
                  :overrides '(:exposure -0.25 :lut-strength 0.8)))))

(defun project-round-trip-p ()
  (let* ((original (project-test-value))
         (decoded (sexp->project (project->sexp original))))
    (and (= 3 (second (project->sexp original)))
         (= 0.75 (processing-settings-exposure
                  (project-defaults decoded)))
         (= 0.65 (processing-settings-lens-correction-strength
                  (project-defaults decoded)))
         (= 0.6 (orfeus:processing-settings-tone-shadows
                 (project-defaults decoded)))
         (= -0.4 (orfeus:processing-settings-tone-highlights
                  (project-defaults decoded)))
         (= 1 (length (project-photos decoded)))
         (equal '(:exposure -0.25 :lut-strength 0.8)
                (photo-job-overrides (first (project-photos decoded)))))))

(defun export-settings-round-trip-p ()
  (let* ((project (make-project
                   :output-directory #P"exports/"
                   :export-settings (make-export-settings
                                     :jpeg-quality 84
                                     :max-width 2400
                                     :max-height 1600
                                     :preserve-metadata-p nil)))
         (decoded (sexp->project (project->sexp project)))
         (settings (project-export-settings decoded)))
    (and (= 84 (export-settings-jpeg-quality settings))
         (= 2400 (export-settings-max-width settings))
         (= 1600 (export-settings-max-height settings))
         (null (export-settings-preserve-metadata-p settings)))))

(defun processing-presets-round-trip-p ()
  (let* ((project
           (make-project
            :output-directory #P"exports/"
            :presets (list (make-processing-preset
                            :name "Night"
                            :settings (make-processing-settings
                                       :exposure 1.25
                                       :noise-reduction 0.8)
                            :disabled-stages '(:film)))))
         (decoded (sexp->project (project->sexp project)))
         (preset (first (project-presets decoded))))
    (and (= 1 (length (project-presets decoded)))
         (string= "Night" (processing-preset-name preset))
         (= 1.25 (processing-settings-exposure
                  (processing-preset-settings preset)))
         (equal '(:film) (processing-preset-disabled-stages preset)))))

(defun neural-noise-reduction-round-trip-p ()
  (let* ((project
           (make-project
            :output-directory #P"exports/"
            :defaults (make-processing-settings :neural-noise-reduction 0.4)
            :photos (list (make-photo-job
                           :input-path #P"input/example.orf"
                           :overrides '(:neural-noise-reduction 0.9)))))
         (decoded (sexp->project (project->sexp project)))
         (effective (processing-settings-with-overrides
                     (project-defaults decoded)
                     (photo-job-overrides (first (project-photos decoded))))))
    (and (= 0.4 (processing-settings-neural-noise-reduction
                 (project-defaults decoded)))
         (= 0.9 (processing-settings-neural-noise-reduction effective))
         (handler-case
             (progn (sexp->processing-settings '(:neural-noise-reduction 1.5))
                    nil)
           (invalid-project-data () t))
         (= 0.0 (processing-settings-neural-noise-reduction
                 (make-processing-settings))))))

(defun grade-stages-partition-setting-keys-p ()
  (let ((stage-keys (loop for stage in (grade-stages)
                          append (grade-stage-keys stage)))
        (identity-keys (loop for (key nil) on orfeus::*stage-identity-plist*
                             by #'cddr collect key)))
    (and (null (set-difference stage-keys orfeus::*processing-setting-keys*))
         (null (set-difference orfeus::*processing-setting-keys* stage-keys))
         (= (length stage-keys) (length (remove-duplicates stage-keys)))
         (null (set-exclusive-or stage-keys identity-keys)))))

(defun stage-bypass-renders-identity-p ()
  (let* ((project
           (make-project
            :output-directory #P"exports/"
            :photos (list (make-photo-job
                           :input-path #P"input/example.orf"
                           :overrides '(:exposure 1.5 :noise-reduction 0.8
                                        :lut-path "film.cube" :lut-strength 1.0)
                           :disabled-stages '(:exposure :film)))))
         (photo (first (project-photos project)))
         (rendered (photo-render-settings project photo)))
    (and (= 0.0 (processing-settings-exposure rendered))
         (null (processing-settings-lut-path rendered))
         (= 0.0 (processing-settings-lut-strength rendered))
         (= 0.8 (processing-settings-noise-reduction rendered))
         ;; The underlying grade is remembered, only rendering bypasses it.
         (equal '(:exposure 1.5 :noise-reduction 0.8
                  :lut-path "film.cube" :lut-strength 1.0)
                (photo-job-overrides photo))
         (handler-case
             (progn (orfeus::disabled-stages-validate '(:no-such-stage)) nil)
           (invalid-project-data () t)))))

(defun disabled-stages-round-trip-p ()
  (let* ((original
           (make-project
            :output-directory #P"exports/"
            :photos (list (make-photo-job
                           :input-path #P"input/example.orf"
                           :disabled-stages '(:tone :optics)))))
         (decoded (sexp->project (project->sexp original))))
    (and (equal '(:tone :optics)
                (photo-job-disabled-stages (first (project-photos decoded))))
         (handler-case
             (progn
               (sexp->project
                '(:orfeus-project 1
                  :output-directory "exports/"
                  :defaults (:exposure 0.0)
                  :export-settings (:jpeg-quality 92)
                  :photos ((:input "a.orf" :disabled-stages (:bogus)))))
               nil)
           (invalid-project-data () t)))))

(defun grade-copy-paste-p ()
  (let* ((settings (make-processing-settings :exposure 0.7 :tone-shadows 0.4
                                             :grain-amount 0.3))
         (grade (settings-grade-plist settings '(:exposure :tone)))
         (target (make-photo-job :input-path #P"b.orf"
                                 :overrides '(:noise-reduction 0.5))))
    (photo-job-apply-grade target grade '(:film))
    (and (= 0.7 (getf grade :exposure))
         (= 0.4 (getf grade :tone-shadows))
         (null (member :grain-amount grade))
         (= 0.7 (getf (photo-job-overrides target) :exposure))
         (= 0.5 (getf (photo-job-overrides target) :noise-reduction))
         (equal '(:film) (photo-job-disabled-stages target))
         (= 0.7 (processing-settings-exposure
                 (photo-render-settings
                  (make-project :output-directory #P"exports/"
                                :photos (list target))
                  target))))))

(defun still-preset-source-photo-round-trip-p ()
  (let ((pathname (test-temporary-pathname "sexp")))
    (unwind-protect
         (progn
           (project-write
            (make-project
             :output-directory #P"exports/"
             :presets (list (make-processing-preset
                             :name "Still 001 (example)"
                             :settings (make-processing-settings :exposure 0.5)
                             :source-photo #P"input/example.orf")))
            pathname)
           (let* ((project (project-read pathname))
                  (preset (first (project-presets project)))
                  (base (uiop:pathname-directory-pathname pathname)))
             (equal (processing-preset-source-photo preset)
                    (merge-pathnames #P"input/example.orf" base))))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun still-names-increment-p ()
  (let* ((photo (make-photo-job :input-path #P"input/example.orf"))
         (project (make-project
                   :output-directory #P"exports/"
                   :photos (list photo)
                   :presets (list (make-processing-preset
                                   :name "Still 001 (example)")))))
    (string= "Still 002 (example)"
             (next-still-preset-name project photo))))

(defun branched-test-graph ()
  "Source -> exposure -> (tone branch) blended back over the exposure result."
  (make-processing-graph
   :nodes (list (make-graph-node :id 1 :kind :exposure
                                 :params '(:exposure 1.0) :inputs '(0))
                (make-graph-node :id 2 :kind :tone
                                 :params '(:tone-shadows 0.5) :inputs '(1))
                (make-graph-node :id 3 :kind :blend
                                 :opacity 0.25 :inputs '(1 2)))
   :output 3))

(defun graph-round-trip-p ()
  (let* ((graph (graph-validate (branched-test-graph)))
         (decoded (sexp->graph (graph->sexp graph)))
         (blend (graph-find-node decoded 3)))
    (and (= 3 (length (processing-graph-nodes decoded)))
         (equal '(1 2) (graph-node-inputs blend))
         (= 0.25 (graph-node-opacity blend))
         (equal '(:exposure 1.0)
                (graph-node-params (graph-find-node decoded 1)))
         (= 3 (processing-graph-output decoded)))))

(defun graph-validation-rejects-p ()
  (flet ((rejected-p (graph)
           (handler-case (progn (graph-validate graph) nil)
             (invalid-project-data () t))))
    (and
     ;; Forward reference.
     (rejected-p (make-processing-graph
                  :nodes (list (make-graph-node :id 1 :kind :exposure
                                                :params '() :inputs '(2)))
                  :output 1))
     ;; Key from a foreign stage.
     (rejected-p (make-processing-graph
                  :nodes (list (make-graph-node :id 1 :kind :exposure
                                                :params '(:tone-whites 1.0)
                                                :inputs '(0)))
                  :output 1))
     ;; Filter consuming film output.
     (rejected-p (make-processing-graph
                  :nodes (list (make-graph-node :id 1 :kind :film
                                                :params '(:grain-amount 0.5)
                                                :inputs '(0))
                               (make-graph-node :id 2 :kind :tone
                                                :params '() :inputs '(1)))
                  :output 2))
     ;; Blend consuming film output.
     (rejected-p (make-processing-graph
                  :nodes (list (make-graph-node :id 1 :kind :film
                                                :params '(:grain-amount 0.5)
                                                :inputs '(0))
                               (make-graph-node :id 2 :kind :blend
                                                :opacity 0.5 :inputs '(0 1)))
                  :output 2))
     ;; Out-of-range blend opacity.
     (rejected-p (make-processing-graph
                  :nodes (list (make-graph-node :id 1 :kind :blend
                                                :opacity 1.5 :inputs '(0 0)))
                  :output 1))
     ;; Film chained after film stays legal.
     (not (rejected-p (make-processing-graph
                       :nodes (list (make-graph-node :id 1 :kind :film
                                                     :params '(:grain-amount 0.2)
                                                     :inputs '(0))
                                    (make-graph-node :id 2 :kind :film
                                                     :params '(:grain-amount 0.1)
                                                     :inputs '(1)))
                       :output 2))))))

(defun settings-graph-conversion-p ()
  (let* ((settings (make-processing-settings :exposure 0.5
                                             :noise-reduction 0.4
                                             :lens-correction-p nil
                                             :chromatic-aberration-correction-p nil
                                             :lut-strength 0.0))
         (graph (settings->graph settings '(:noise-reduction)))
         (nodes (processing-graph-nodes graph)))
    (and (= 2 (length nodes))
         (eq :exposure (graph-node-kind (first nodes)))
         (eq :noise-reduction (graph-node-kind (second nodes)))
         (graph-node-bypassed-p (second nodes))
         (not (graph-node-bypassed-p (first nodes)))
         ;; The bypassed node drops out of the effective plan.
         (= 1 (length (graph-effective-nodes graph))))))

(defun graph-editing-p ()
  (let ((graph (graph-validate
                (make-processing-graph
                 :nodes (list (make-graph-node :id 1 :kind :exposure
                                               :params '(:exposure 1.0)
                                               :inputs '(0))
                              (make-graph-node :id 2 :kind :tone
                                               :params '(:tone-shadows 0.5)
                                               :inputs '(1)))
                 :output 2))))
    ;; Insert white balance right after the source.
    (graph-insert-node graph 0 :white-balance
                       :params '(:white-balance-tint 3.0))
    (unless (equal '(:white-balance :exposure :tone)
                   (mapcar #'graph-node-kind (processing-graph-nodes graph)))
      (return-from graph-editing-p nil))
    ;; Reorder: pull tone above exposure.
    (let ((tone (find :tone (processing-graph-nodes graph)
                      :key #'graph-node-kind)))
      (unless (graph-swap-with-upstream graph (graph-node-id tone))
        (return-from graph-editing-p nil)))
    (unless (equal '(:white-balance :tone :exposure)
                   (mapcar #'graph-node-kind (processing-graph-nodes graph)))
      (return-from graph-editing-p nil))
    ;; Drop the white balance again; tone must inherit the source input.
    (let ((wb (find :white-balance (processing-graph-nodes graph)
                    :key #'graph-node-kind)))
      (graph-delete-node graph (graph-node-id wb)))
    (unless (and (equal '(:tone :exposure)
                        (mapcar #'graph-node-kind
                                (processing-graph-nodes graph)))
                 (equal '(0) (graph-node-inputs
                              (first (processing-graph-nodes graph)))))
      (return-from graph-editing-p nil))
    ;; Blend the graded result against the untouched source.
    (let ((exposure (find :exposure (processing-graph-nodes graph)
                          :key #'graph-node-kind)))
      (graph-insert-node graph (graph-node-id exposure) :blend
                         :opacity 0.5))
    (let ((blend (find :blend (processing-graph-nodes graph)
                       :key #'graph-node-kind)))
      (and blend
           (eql (processing-graph-output graph) (graph-node-id blend))
           (= 3 (length (graph-effective-nodes graph)))
           (progn (graph-validate graph) t)))))

(defun photo-graph-round-trip-p ()
  (let* ((original
           (make-project
            :output-directory #P"exports/"
            :photos (list (make-photo-job
                           :input-path #P"input/example.orf"
                           :graph (branched-test-graph)))))
         (decoded (sexp->project (project->sexp original)))
         (graph (photo-job-graph (first (project-photos decoded)))))
    (and graph
         (= 3 (length (processing-graph-nodes graph)))
         (eql 3 (second (project->sexp original)))
         ;; Version 2 projects without graphs still read.
         (null (photo-job-graph
                (first (project-photos
                        (sexp->project
                         '(:orfeus-project 2
                           :output-directory "exports/"
                           :defaults (:exposure 0.0)
                           :export-settings (:jpeg-quality 92)
                           :photos ((:input "a.orf")))))))))))

(defun old-project-export-defaults-p ()
  (let* ((decoded
           (sexp->project
            '(:orfeus-project 1
              :output-directory "exports/"
              :defaults (:exposure 0.0
                         :white-balance-temperature nil
                         :white-balance-tint 0.0
                         :noise-reduction 0.35
                         :lens-correction-p t
                         :lens-correction-strength 1.0
                         :chromatic-aberration-correction-p t
                         :lut-path nil :lut-strength 1.0
                         :grain-amount 0.0 :grain-size 1.0)
              :photos ())))
         (settings (project-export-settings decoded)))
    (and (= 92 (export-settings-jpeg-quality settings))
         (null (export-settings-max-width settings))
         (null (export-settings-max-height settings))
         (export-settings-preserve-metadata-p settings)
         (null (project-presets decoded)))))

(defun project-file-round-trip-p ()
  (let ((pathname (test-temporary-pathname "sexp")))
    (unwind-protect
         (progn
           (project-write (project-test-value) pathname)
           (and (not (search "#A(" (uiop:read-file-string pathname)))
                (search "\"exports/\"" (uiop:read-file-string pathname))
                (project-round-trip-p-from (project-read pathname))))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun project-round-trip-p-from (project)
  (and (= 0.75 (processing-settings-exposure (project-defaults project)))
       (= 1 (length (project-photos project)))))

(defun project-reader-evaluation-disabled-p ()
  (let ((pathname (test-temporary-pathname "sexp")))
    (unwind-protect
         (progn
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede)
             (write-string "#.(error \"reader evaluation escaped\")" stream))
           (handler-case
               (progn (project-read pathname) nil)
             (reader-error () t)))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun invalid-project-rejected-p ()
  (handler-case
      (progn
        (sexp->project '(:orfeus-project 99))
        nil)
    (invalid-project-data () t)))

(defun lens-description-selection-p ()
  (and (string= "Olympus M.Zuiko 12-45mm"
                (uiop:symbol-call '#:orfeus '#:preferred-lens-description
                                  (format nil "Unknown~%Olympus M.Zuiko 12-45mm~%")))
       (string= "Ultron 0.7x"
                (uiop:symbol-call '#:orfeus '#:preferred-lens-description
                                  (format nil "None~%Ultron 0.7x~%")))))

(defun capture-description-formatting-p ()
  (and (string= "OM-1 | ISO 200 | f/5.6 | 1/50"
                (uiop:symbol-call '#:orfeus '#:capture-description
                                  "OM-1" "200" "5.6" "1/50"))
       (string= "PEN-F | ISO 800"
                (uiop:symbol-call '#:orfeus '#:capture-description
                                  "PEN-F" "800" "-" "-"))
       (null (uiop:symbol-call '#:orfeus '#:capture-description
                               "-" "" "none" "unknown"))))

(defun adapted-lens-aliases-p ()
  (multiple-value-bind (model reducer crop-factor)
      (resolve-lens-profile-alias "Ultron 0.7x")
    (and (string= model "Voigtlander Ultron 40mm f/2 SLII Aspherical")
         (= reducer 0.71)
         (= crop-factor 2.0))))

(defun lens-alias-reader-evaluation-disabled-p ()
  (let ((pathname (test-temporary-pathname "sexp")))
    (unwind-protect
         (progn
           (with-open-file (stream pathname :direction :output
                                           :if-exists :supersede)
             (write-string "#.(error \"reader evaluation escaped\")" stream))
           (handler-case
               (progn (lens-profile-aliases-read pathname) nil)
             (reader-error () t)))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun processing-overrides-p ()
  (let ((settings
          (processing-settings-with-overrides
           (make-processing-settings :exposure 0.5 :grain-amount 0.0)
           '(:exposure -1.0 :grain-amount 0.2))))
    (and (= -1.0 (processing-settings-exposure settings))
         (= 0.2 (processing-settings-grain-amount settings)))))

(defun tonal-settings-default-and-validation-p ()
  (let* ((old (sexp->project
               '(:orfeus-project 1 :output-directory "exports/"
                 :defaults (:exposure 0.0) :photos ())))
         (settings (project-defaults old)))
    (and (zerop (orfeus:processing-settings-tone-blacks settings))
         (zerop (orfeus:processing-settings-tone-midtones settings))
         (zerop (orfeus:processing-settings-tone-whites settings))
         (handler-case
             (progn
               (processing-settings-with-overrides settings '(:tone-shadows 2.1))
               nil)
           (invalid-project-data () t)))))

(defun project-relative-paths-p ()
  (let ((pathname (test-temporary-pathname "sexp")))
    (unwind-protect
         (progn
           (project-write
            (make-project
             :output-directory #P"exports/"
             :photos (list (make-photo-job :input-path #P"input.orf")))
            pathname)
           (let* ((project (project-read pathname))
                  (base (uiop:pathname-directory-pathname pathname)))
             (and (equal (project-output-directory project)
                         (merge-pathnames #P"exports/" base))
                  (equal (photo-job-input-path (first (project-photos project)))
                         (merge-pathnames #P"input.orf" base)))))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun photo-job-render-output-semantics-p ()
  (let* ((project (make-project :output-directory #P"/tmp/orfeus-exports/"))
         (automatic (make-photo-job :input-path #P"source/photo.orf"))
         (relative (make-photo-job :input-path #P"source/photo.orf"
                                   :output-path #P"edited.tiff"))
         (absolute (make-photo-job :input-path #P"source/photo.orf"
                                   :output-path #P"/tmp/custom-output.jpg")))
    (and (equal (photo-job-render-output project automatic)
                #P"/tmp/orfeus-exports/photo.jpg")
         (equal (photo-job-render-output project relative)
                #P"/tmp/orfeus-exports/edited.tiff")
         (equal (photo-job-render-output project absolute)
                #P"/tmp/custom-output.jpg"))))

(defun negative-workflow-graph-p ()
  (let* ((graph (graph-validate
                 (make-processing-graph
                  :nodes (list (make-graph-node
                                :id 1 :kind :crop
                                :params '(:left 0.1 :top 0.1
                                          :width 0.8 :height 0.8)
                                :inputs '(0))
                               (make-graph-node
                                :id 2 :kind :color-subtract
                                :params '(:red 0.9 :green 0.62 :blue 0.5)
                                :inputs '(1))
                               (make-graph-node
                                :id 3 :kind :white-balance
                                :params '(:white-balance-temperature 5500.0
                                          :white-balance-tint 0.0)
                                :inputs '(2)))
                  :output 3)))
         (decoded (sexp->graph (graph->sexp graph)))
         (bytes (orfeus::graph->program-bytes decoded)))
    (and (= 3 (length (processing-graph-nodes decoded)))
         (equal '(:red 0.9 :green 0.62 :blue 0.5)
                (graph-node-params (graph-find-node decoded 2)))
         ;; Wire codes 9 (crop) and 8 (color subtract) in program order.
         (= 9 (elt bytes 12))
         (plusp (length bytes))
         (flet ((rejected-p (nodes)
                  (handler-case
                      (progn (graph-validate (make-processing-graph
                                              :nodes nodes
                                              :output (graph-node-id
                                                       (first (last nodes)))))
                             nil)
                    (invalid-project-data () t))))
           (and
            ;; Color components outside 0..4 are rejected.
            (rejected-p (list (make-graph-node
                               :id 1 :kind :color-subtract
                               :params '(:red 5.0) :inputs '(0))))
            ;; Crops cannot leave the frame.
            (rejected-p (list (make-graph-node
                               :id 1 :kind :crop
                               :params '(:left 0.8 :width 0.5)
                               :inputs '(0))))
            ;; Color subtraction stays scene-linear.
            (rejected-p (list (make-graph-node
                               :id 1 :kind :film
                               :params '(:grain-amount 0.2) :inputs '(0))
                              (make-graph-node
                               :id 2 :kind :color-subtract
                               :params '(:red 1.0) :inputs '(1)))))))))

(defun graph-program-bytes-p ()
  (let* ((graph (make-processing-graph
                 :nodes (list (make-graph-node :id 1 :kind :exposure
                                               :params '(:exposure 0.5)
                                               :inputs '(0)))
                 :output 1))
         (bytes (orfeus::graph->program-bytes graph)))
    (equalp bytes
            ;; magic "ORFG", version 1, one node: exposure(2), input 0,
            ;; no second input, one parameter 0.5f0, no string.
            (coerce #(#x4F #x52 #x46 #x47  1 0 0 0  1 0 0 0
                      2 0 0 0  0 0 0 0  #xFF #xFF #xFF #xFF
                      1 0 0 0  0 0 0 #x3F  0 0 0 0)
                    '(simple-array (unsigned-byte 8) (*))))))

(defun graph-program-prunes-bypassed-nodes-p ()
  (let* ((graph (make-processing-graph
                 :nodes (list (make-graph-node :id 1 :kind :exposure
                                               :params '(:exposure 0.5)
                                               :inputs '(0)
                                               :bypassed-p t)
                              (make-graph-node :id 2 :kind :tone
                                               :params '(:tone-whites 0.3)
                                               :inputs '(1)))
                 :output 2))
         (bytes (orfeus::graph->program-bytes graph)))
    ;; Only the tone node survives, reading straight from the source.
    (and (= 1 (elt bytes 8))
         (= 4 (elt bytes 12))
         (= 0 (elt bytes 16)))))

(defun graph-render-rejects-input-as-output-p ()
  (let ((pathname (test-temporary-pathname "orf"))
        (contents "source pixels must survive"))
    (unwind-protect
         (progn
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede)
             (write-string contents stream))
           (let ((rejected
                   (handler-case
                       (progn
                         (render-photo pathname pathname nil
                                       :graph (default-processing-graph)
                                       :if-exists :supersede)
                         nil)
                     (raw-render-error () t))))
             (and rejected
                  (string= contents
                           (uiop:read-file-string pathname)))))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun render-rejects-input-as-output-p ()
  (let ((pathname (test-temporary-pathname "orf"))
        (contents "source pixels must survive"))
    (unwind-protect
         (progn
           (with-open-file (stream pathname
                                   :direction :output
                                   :if-exists :supersede)
             (write-string contents stream))
           (let ((rejected
                   (handler-case
                       (progn
                         (render-photo pathname pathname
                                       (make-processing-settings)
                                       :if-exists :supersede)
                         nil)
                     (raw-render-error () t))))
             (and rejected
                  (with-open-file (stream pathname :direction :input)
                    (string= contents (read-line stream nil ""))))))
      (when (probe-file pathname)
        (delete-file pathname)))))

(defun render-accepts-unbounded-nil-dimensions-p ()
  (let ((input (test-temporary-pathname "orf"))
        (output (test-temporary-pathname "jpg")))
    (unwind-protect
         (progn
           (with-open-file (stream input :direction :output :if-exists :supersede)
             (write-string "not a raw" stream))
           (handler-case
               (progn
                 (render-photo input output (make-processing-settings)
                               :max-width nil :max-height nil
                               :preserve-metadata-p nil)
                 nil)
             (raw-render-error () t)
             (type-error () nil)))
      (dolist (pathname (list input output))
        (when (probe-file pathname)
          (delete-file pathname))))))

(defun preview-does-not-overwrite-p ()
  (let ((input (test-temporary-pathname "orf"))
        (output (test-temporary-pathname "jpg"))
        (contents "keep this export"))
    (unwind-protect
         (progn
           (with-open-file (stream input :direction :output
                                         :if-exists :supersede)
             (write-string "raw" stream))
           (with-open-file (stream output :direction :output
                                          :if-exists :supersede)
             (write-string contents stream))
           (let ((rejected
                   (handler-case
                       (progn
                         (render-preview input output
                                         (make-processing-settings))
                         nil)
                     (output-file-exists () t))))
             (and rejected
                  (with-open-file (stream output :direction :input)
                    (string= contents (read-line stream nil ""))))))
      (dolist (pathname (list input output))
        (when (probe-file pathname)
          (delete-file pathname))))))

(defun embedded-preview-rejects-missing-image-p ()
  (let ((input (test-temporary-pathname "txt"))
        (output (test-temporary-pathname "jpg")))
    (unwind-protect
         (progn
           (with-open-file (stream input :direction :output
                                         :if-exists :supersede)
             (write-string "not a photograph" stream))
           (and (handler-case
                    (progn
                      (photo-extract-embedded-preview input output
                                                      :if-exists :supersede)
                      nil)
                  (raw-render-error () t))
                (not (probe-file output))))
      (dolist (pathname (list input output))
        (when (probe-file pathname)
          (delete-file pathname))))))

(defun publish-race-does-not-clobber-p ()
  (let ((temporary (test-temporary-pathname "jpg"))
        (output (test-temporary-pathname "jpg")))
    (unwind-protect
         (progn
           (with-open-file (stream temporary :direction :output
                                             :if-exists :supersede)
             (write-string "new render" stream))
           (with-open-file (stream output :direction :output
                                          :if-exists :supersede)
             (write-string "racing writer" stream))
           (let ((rejected
                   (handler-case
                       (progn
                         (uiop:symbol-call '#:orfeus '#:render-publish
                                           temporary output :error)
                         nil)
                     (output-file-exists () t))))
             (and rejected
                  (with-open-file (stream output :direction :input)
                    (string= "racing writer" (read-line stream nil ""))))))
      (dolist (pathname (list temporary output))
        (when (probe-file pathname)
          (delete-file pathname))))))

(defun write-test-bytes (pathname bytes)
  (with-open-file (stream pathname :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :element-type '(unsigned-byte 8))
    (write-sequence bytes stream)))

(defun copy-test-bytes (input output)
  (with-open-file (source input :direction :input
                                :element-type '(unsigned-byte 8))
    (with-open-file (target output :direction :output
                                   :if-exists :error
                                   :if-does-not-exist :create
                                   :element-type '(unsigned-byte 8))
      (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
        (loop for count = (read-sequence buffer source)
              while (plusp count)
              do (write-sequence buffer target :end count))))))

(defun dng-render-source-cache-reuses-content-p ()
  (let ((input (test-temporary-pathname "dng"))
        (extract-count 0)
        first-path second-path changed-path cache-directory result)
    (unwind-protect
         (setf result
               (progn
           (orfeus::clear-render-source-cache)
           (write-test-bytes input #(1 2 3 4 5 6 7 8))
           (let ((orfeus::*render-source-name-reader*
                   (lambda (ignored)
                     (declare (ignore ignored))
                     "embedded.orf"))
                 (orfeus::*render-source-extractor*
                   (lambda (source output)
                     (incf extract-count)
                     (copy-test-bytes source output))))
             (orfeus::call-with-render-source
              input (lambda (source)
                      (setf first-path source)
                      (and (probe-file source) t)))
             (orfeus::call-with-render-source
              input (lambda (source)
                      (setf second-path source)
                      (and (probe-file source) t)))
             (write-test-bytes input #(8 7 6 5 4 3 2 1))
             (orfeus::call-with-render-source
              input (lambda (source)
                      (setf changed-path source)
                      (and (probe-file source) t)))
             (setf cache-directory orfeus::*render-source-cache-directory*)
             (and (= extract-count 2)
                  (equal first-path second-path)
                  (not (equal first-path changed-path))
                  (probe-file first-path)
                  (probe-file changed-path)))))
      (orfeus::clear-render-source-cache)
      (when (probe-file input)
        (delete-file input)))
    (and result
         cache-directory
         (not (probe-file cache-directory)))))

(defun dng-render-source-cache-coalesces-concurrent-p ()
  (let ((input (test-temporary-pathname "dng"))
        (extract-count 0)
        (paths (make-array 4 :initial-element nil))
        (old-name-reader orfeus::*render-source-name-reader*)
        (old-extractor orfeus::*render-source-extractor*)
        threads result)
    (unwind-protect
         (progn
           (orfeus::clear-render-source-cache)
           (write-test-bytes input #(10 20 30 40 50 60))
           (setf orfeus::*render-source-name-reader*
                 (lambda (ignored)
                   (declare (ignore ignored))
                   "embedded.orf")
                 orfeus::*render-source-extractor*
                 (lambda (source output)
                   (incf extract-count)
                   (sleep 0.1)
                   (copy-test-bytes source output)))
           (setf threads
                 (loop for index below (length paths)
                       collect
                       (let ((slot index))
                         (sb-thread:make-thread
                          (lambda ()
                            (orfeus::call-with-render-source
                             input
                             (lambda (source)
                               (setf (aref paths slot) source))))))))
           (dolist (thread threads)
             (sb-thread:join-thread thread))
           (setf result
                 (and (= extract-count 1)
                      (every (lambda (path) (equal path (aref paths 0)))
                             paths))))
      (setf orfeus::*render-source-name-reader* old-name-reader
            orfeus::*render-source-extractor* old-extractor)
      (orfeus::clear-render-source-cache)
      (when (probe-file input)
        (delete-file input)))
    result))

(defun distinct-dng-render-sources-extract-concurrently-p ()
  (let ((inputs (list (test-temporary-pathname "dng")
                      (test-temporary-pathname "dng")))
        (activity-lock (sb-thread:make-mutex :name "DNG test activity"))
        (active 0)
        (maximum-active 0)
        (old-name-reader orfeus::*render-source-name-reader*)
        (old-extractor orfeus::*render-source-extractor*)
        threads result)
    (unwind-protect
         (progn
           (orfeus::clear-render-source-cache)
           (write-test-bytes (first inputs) #(1 3 5 7))
           (write-test-bytes (second inputs) #(2 4 6 8))
           (setf orfeus::*render-source-name-reader*
                 (lambda (ignored)
                   (declare (ignore ignored))
                   "embedded.orf")
                 orfeus::*render-source-extractor*
                 (lambda (source output)
                   (sb-thread:with-mutex (activity-lock)
                     (incf active)
                     (setf maximum-active (max maximum-active active)))
                   (unwind-protect
                        (progn
                          (sleep 0.15)
                          (copy-test-bytes source output))
                     (sb-thread:with-mutex (activity-lock)
                       (decf active)))))
           (setf threads
                 (mapcar
                  (lambda (input)
                    (sb-thread:make-thread
                     (lambda ()
                       (orfeus::call-with-render-source input #'probe-file))))
                  inputs))
           (dolist (thread threads)
             (sb-thread:join-thread thread))
           (setf result (= maximum-active 2)))
      (setf orfeus::*render-source-name-reader* old-name-reader
            orfeus::*render-source-extractor* old-extractor)
      (orfeus::clear-render-source-cache)
      (dolist (input inputs)
        (when (probe-file input)
          (delete-file input))))
    result))

(defun bundled-film-luts-match-pinned-digests-p ()
  (let* ((directory (asdf:system-relative-pathname "orfeus" #P"data/luts/"))
         (expected
           '(("agfa_apx_100.cube" . "61f692f928d3809a77af2249035f771e47150964ac7243e9a217438cd2ca2858")
             ("agfa_apx_25.cube" . "5779cd44a2ea912085653054f12ae270343b87e918e27c63113afc52cebbb2cd")
             ("agfa_precisa_100.cube" . "712dd96ca82535164751ee432548ea4f619218d3025ffcbd2411eb1fbd10c2bb")
             ("agfa_ultra_color_100.cube" . "5c15b98ef31e836ce371a7725f1740ba7ecbe1e8aef6aaba89024b1d3b00e17b")
             ("agfa_vista_200.cube" . "b65848b3e217bbb57ca998f0460fe0d6c631228872ad49574d85e07ffeb3548e")
             ("kodak_kodachrome_200.cube" . "0026d0685796cf2f5cef438633e109f2895abed96a33e614575528f540dbf0cd")
             ("kodak_kodachrome_25.cube" . "70a9f8106de3a5e87fb2b5e478d708407429d064945b769be8913a814653523e")
             ("kodak_kodachrome_64.cube" . "6fce987295dac264a689a8ef1b1b3a407f26ffcb3f6d81a356ebc5d9c565d0b4")
             ("kodak_kodachrome_64_generic.cube" . "19cacda68131b43e153cee643c02e9fd015ec1f6e7b74fa95b7c2168b0979935"))))
    (and (= (length expected) (length (directory (merge-pathnames "*.cube" directory))))
         (every (lambda (entry)
                  (let ((path (merge-pathnames (first entry) directory)))
                    (and (probe-file path)
                         (string-equal
                          (rest entry)
                          (ironclad:byte-array-to-hex-string
                           (ironclad:digest-file :sha256 path))))))
                expected))))

(defun cli-version-p ()
  (let ((output (make-string-output-stream))
        (errors (make-string-output-stream)))
    (and (zerop (cli-run '("--version")
                         :output-stream output
                         :error-stream errors))
         (search (orfeus-version) (get-output-stream-string output))
         (string= "" (get-output-stream-string errors)))))

(defun cli-rejects-unknown-command-p ()
  (let ((output (make-string-output-stream))
        (errors (make-string-output-stream)))
    (and (= 2 (cli-run '("wat")
                       :output-stream output
                       :error-stream errors))
         (search "Unknown command" (get-output-stream-string errors)))))

(defun run-tests ()
  "Run the dependency-free Orfeus test suite."
  (let ((failures 0))
    (flet ((check (description predicate)
             (format t "~:[not ok~;ok~] - ~A~%" predicate description)
             (unless predicate
               (incf failures))))
      (check "system exposes a version"
             (and (stringp (orfeus-version))
                  (plusp (length (orfeus-version)))))
      (check "project S-expressions round trip" (project-round-trip-p))
      (check "project files round trip" (project-file-round-trip-p))
      (check "export settings round trip" (export-settings-round-trip-p))
      (check "processing presets round trip" (processing-presets-round-trip-p))
      (check "neural noise reduction round trips and validates"
             (neural-noise-reduction-round-trip-p))
      (check "grade stages partition the setting keys"
             (grade-stages-partition-setting-keys-p))
      (check "bypassed stages render as identity yet keep their grade"
             (stage-bypass-renders-identity-p))
      (check "disabled stages round trip and reject unknown names"
             (disabled-stages-round-trip-p))
      (check "grades copy selectively and paste onto photos"
             (grade-copy-paste-p))
      (check "still presets remember and resolve their source photo"
             (still-preset-source-photo-round-trip-p))
      (check "still names increment past taken gallery names"
             (still-names-increment-p))
      (check "graphs round trip through S-expressions"
             (graph-round-trip-p))
      (check "graph validation rejects malformed graphs"
             (graph-validation-rejects-p))
      (check "settings convert to equivalent linear graphs"
             (settings-graph-conversion-p))
      (check "graph editing inserts, reorders, deletes, and blends"
             (graph-editing-p))
      (check "photo graphs round trip as project version 3"
             (photo-graph-round-trip-p))
      (check "graphs serialize to the native program format"
             (graph-program-bytes-p))
      (check "bypassed nodes drop out of serialized programs"
             (graph-program-prunes-bypassed-nodes-p))
      (check "graph rendering never replaces its input"
             (graph-render-rejects-input-as-output-p))
      (check "negative-inversion graphs validate and serialize"
             (negative-workflow-graph-p))
      (check "old projects receive export defaults" (old-project-export-defaults-p))
      (check "project-relative paths resolve beside the project"
             (project-relative-paths-p))
      (check "project reads disable reader evaluation"
             (project-reader-evaluation-disabled-p))
      (check "invalid project versions are rejected"
             (invalid-project-rejected-p))
      (check "lens metadata skips unidentified values"
             (lens-description-selection-p))
      (check "capture metadata formats compactly"
             (capture-description-formatting-p))
      (check "adapted lens nicknames resolve portable Lensfun mappings"
             (adapted-lens-aliases-p))
      (check "lens alias reads disable reader evaluation"
             (lens-alias-reader-evaluation-disabled-p))
      (check "per-photo overrides produce effective settings"
             (processing-overrides-p))
      (check "tonal settings default safely and reject invalid ranges"
             (tonal-settings-default-and-validation-p))
      (check "photo outputs follow project path semantics"
             (photo-job-render-output-semantics-p))
      (check "rendering never replaces its input"
             (render-rejects-input-as-output-p))
      (check "NIL export bounds mean unbounded dimensions"
             (render-accepts-unbounded-nil-dimensions-p))
      (check "preview does not overwrite an existing export"
             (preview-does-not-overwrite-p))
      (check "embedded preview extraction rejects files without an image"
             (embedded-preview-rejects-missing-image-p))
      (check "a racing writer is not clobbered at publish time"
             (publish-race-does-not-clobber-p))
      (check "DNG render sources reuse content and invalidate by digest"
             (dng-render-source-cache-reuses-content-p))
      (check "concurrent DNG render requests share one extraction"
             (dng-render-source-cache-coalesces-concurrent-p))
      (check "distinct DNG render sources extract concurrently"
             (distinct-dng-render-sources-extract-concurrently-p))
      (check "bundled film LUTs match pinned digests"
             (bundled-film-luts-match-pinned-digests-p))
      (check "CLI reports its version" (cli-version-p))
      (check "CLI rejects unknown commands" (cli-rejects-unknown-command-p)))
    (zerop failures)))
