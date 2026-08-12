(in-package #:orfeus/tests)

(defun project-deep-copy-is-independent-p ()
  "A deep project copy must share no structure an edit could reach."
  (let* ((graph (orfeus:default-processing-graph))
         (job (orfeus:make-photo-job :input-path #P"one.orf"
                                     :overrides '(:exposure 1.0)
                                     :disabled-stages '(:film)
                                     :graph graph))
         (preset (orfeus:make-processing-preset
                  :name "look"
                  :settings (orfeus:make-processing-settings :exposure 0.5)
                  :disabled-stages '(:optics)
                  :graph (orfeus:graph-copy graph)))
         (project (orfeus:make-project :output-directory #P"exports/"
                                       :defaults (orfeus:make-processing-settings
                                                  :exposure 2.0)
                                       :presets (list preset)
                                       :photos (list job)))
         (copy (orfeus:copy-project-deep project)))
    ;; Mutate every mutable place in the original.
    (setf (orfeus:photo-job-overrides job) '(:exposure 9.0)
          (orfeus:photo-job-disabled-stages job) '()
          (orfeus:processing-settings-exposure (orfeus:project-defaults project)) 9.0
          (orfeus:processing-preset-disabled-stages preset) '()
          (orfeus:processing-settings-exposure
           (orfeus:processing-preset-settings preset)) 9.0)
    (dolist (node (orfeus:processing-graph-nodes graph))
      (setf (orfeus:graph-node-bypassed-p node) t))
    (let ((copied-job (first (orfeus:project-photos copy)))
          (copied-preset (first (orfeus:project-presets copy))))
      (and (equal '(:exposure 1.0) (orfeus:photo-job-overrides copied-job))
           (equal '(:film) (orfeus:photo-job-disabled-stages copied-job))
           (= 2.0 (orfeus:processing-settings-exposure
                   (orfeus:project-defaults copy)))
           (equal '(:optics) (orfeus:processing-preset-disabled-stages
                              copied-preset))
           (= 0.5 (orfeus:processing-settings-exposure
                   (orfeus:processing-preset-settings copied-preset)))
           (notany #'orfeus:graph-node-bypassed-p
                   (orfeus:processing-graph-nodes
                    (orfeus:photo-job-graph copied-job)))
           (notany #'orfeus:graph-node-bypassed-p
                   (orfeus:processing-graph-nodes
                    (orfeus:processing-preset-graph copied-preset)))))))

(defun project-render-batches-in-order-p ()
  "PROJECT-RENDER must report completions and failures in project order.

Every photograph here is missing, so each render fails: that exercises ordering,
the failure list, and both ON-ERROR modes without needing real RAW files."
  (let* ((photos (loop for index below 7
                       collect (orfeus:make-photo-job
                                :input-path (make-pathname
                                             :name (format nil "absent-~D" index)
                                             :type "orf"))))
         (project (orfeus:make-project
                   :output-directory (merge-pathnames
                                      "orfeus-render-order/"
                                      (uiop:temporary-directory))
                   :photos photos))
         (seen '()))
    (multiple-value-bind (completed failures)
        (orfeus:project-render project :on-error :continue
                                       :progress-callback
                                       (lambda (index total photo output)
                                         (declare (ignore total output))
                                         (push (cons index photo) seen)))
      (and (null completed)
           ;; Every photograph attempted exactly once, and reported in order.
           (= (length photos) (length failures))
           (equal photos (mapcar #'car failures))
           (= (length photos) (length seen))
           (equal (loop for index from 1 to (length photos) collect index)
                  (sort (mapcar #'car seen) #'<))
           ;; Aborting still signals, and never reports a completion.
           (handler-case
               (progn (orfeus:project-render project :on-error :abort) nil)
             (error () t))
           ))))

(defun interned-raw-store-behaves-p ()
  "Interning must be idempotent, collision-safe, atomic, and repoint the job."
  (let* ((root (merge-pathnames "orfeus-intern-check/" (uiop:temporary-directory)))
         (store (merge-pathnames "store/" root))
         (first-card (merge-pathnames "card-a/_6040106.ORF" root))
         (second-card (merge-pathnames "card-b/_6040106.ORF" root)))
    (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)
    (dolist (path (list first-card second-card))
      (ensure-directories-exist path))
    (ensure-directories-exist store)
    (with-open-file (stream first-card :direction :output)
      (write-string "one" stream))
    (with-open-file (stream second-card :direction :output)
      (write-string "another" stream))
    (unwind-protect
         (let ((first-interned (orfeus:intern-raw-file first-card
                                                       :directory store))
               (second-interned (orfeus:intern-raw-file second-card
                                                        :directory store)))
           (and
            ;; Two cards hold the same filename and different photographs.
            (not (equal first-interned second-interned))
            (orfeus:photo-interned-p first-interned :directory store)
            (not (orfeus:photo-interned-p first-card :directory store))
            ;; Interning twice, or interning what is already interned, copies
            ;; nothing further.
            (equal first-interned
                   (orfeus:intern-raw-file first-card :directory store))
            (equal first-interned
                   (orfeus:intern-raw-file first-interned :directory store))
            (string= "one" (with-open-file (stream first-interned)
                             (read-line stream)))
            ;; The atomic rename must leave nothing behind.
            (null (remove-if-not
                   (lambda (path) (equal "tmp" (pathname-type path)))
                   (directory (merge-pathnames "*.*" store))))
            ;; A job follows its copy, which is the whole point.
            (let ((job (orfeus:make-photo-job :input-path second-card)))
              (orfeus:intern-photo-job job :directory store)
              (and (orfeus:photo-interned-p (orfeus:photo-job-input-path job)
                                            :directory store)
                   (equal second-interned (orfeus:photo-job-input-path job))))
            ;; A photograph that is not there cannot be interned.
            (handler-case
                (progn (orfeus:intern-raw-file
                        (merge-pathnames "absent.ORF" root) :directory store)
                       nil)
              (error () t))))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

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
    (and (= 4 (second (project->sexp original)))
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

(defun still-store-round-trip-p ()
  (let ((directory (merge-pathnames
                    (format nil "orfeus-still-store-~D-~D/"
                            (get-universal-time)
                            (random most-positive-fixnum))
                    #P"/tmp/")))
    (ensure-directories-exist directory)
    (unwind-protect
         (let* ((lut-target (merge-pathnames "target.cube" directory))
                (lut-link (merge-pathnames "linked.cube" directory))
                (graph (settings->graph
                        (make-processing-settings
                         :lut-path (namestring lut-link)
                         :lut-strength 1.0)))
                (first (make-processing-preset
                        :name "A B" :source-photo #P"input/example.orf"
                        :settings (make-processing-settings :lut-path "film.cube")
                        :graph graph))
                (second (make-processing-preset :name "A-B"))
                (thumbnail-source (merge-pathnames "source.jpg" directory)))
           (with-open-file (stream lut-target :direction :output
                                              :if-exists :supersede)
             (write-line "TITLE test" stream))
           (sb-posix:symlink (namestring lut-target) (namestring lut-link))
           (setf (orfeus::graph-node-kind-states
                  (first (processing-graph-nodes graph)))
                 `((:film :params (:lut-path ,(namestring lut-link)))))
           (with-open-file (stream thumbnail-source :direction :output
                                                   :element-type '(unsigned-byte 8)
                                                   :if-exists :supersede)
             (write-sequence #(1 2 3 4 5) stream))
           (still-store-write first :directory directory)
           (still-store-write second :directory directory)
           (still-store-write-thumbnail "A B" thumbnail-source
                                        :directory directory)
           (let* ((loaded (still-store-list :directory directory))
                  (stored-first (find "A B" loaded
                                      :key #'processing-preset-name
                                      :test #'string=))
                  (stored-nodes (processing-graph-nodes
                                 (processing-preset-graph stored-first)))
                  (film (find :film stored-nodes :key #'graph-node-kind))
                  (dormant (assoc :film
                                  (orfeus::graph-node-kind-states
                                   (first stored-nodes)))))
             (and (= 2 (length loaded))
                  (not (string= (still-store-identity "A B")
                                (still-store-identity "A-B")))
                  (handler-case
                      (progn
                        (still-store-write second :directory directory
                                                  :if-exists :error)
                        nil)
                    (error () t))
                  (uiop:absolute-pathname-p
                   (processing-preset-source-photo stored-first))
                  (equal (namestring (truename lut-target))
                         (getf (graph-node-params film) :lut-path))
                  (equal (namestring (truename lut-target))
                         (getf (getf (rest dormant) :params) :lut-path))
                  (progn
                    (setf (processing-preset-name first) "Renamed")
                    (still-store-rename first "A B" :directory directory)
                    (let ((names (mapcar #'processing-preset-name
                                        (still-store-list :directory directory))))
                      (and (member "Renamed" names :test #'string=)
                           (member "A-B" names :test #'string=)
                           (probe-file
                            (orfeus:still-store-thumbnail-pathname
                             "Renamed" :directory directory))
                           (not (probe-file
                                 (orfeus:still-store-thumbnail-pathname
                                  "A B" :directory directory))))))
                  (progn
                    (setf (processing-preset-name second) "Renamed")
                    (handler-case
                        (progn
                          (still-store-rename second "A-B"
                                              :directory directory)
                          nil)
                      (error () t)))
                  (progn
                    (still-store-delete "Renamed" :directory directory)
                    (let ((remaining (still-store-list :directory directory)))
                      (and (= 1 (length remaining))
                           (string= "A-B" (processing-preset-name
                                            (first remaining)))))))))
      (uiop:delete-directory-tree directory :validate t
                                            :if-does-not-exist :ignore))))

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
     ;; Lens correction must run in the uncropped sensor geometry.
     (rejected-p (make-processing-graph
                  :nodes (list (make-graph-node
                                :id 1 :kind :crop
                                :params '(:left 0.1 :top 0.1
                                          :width 0.8 :height 0.8)
                                :inputs '(0))
                               (make-graph-node :id 2 :kind :optics
                                                :params '()
                                                :inputs '(1)))
                  :output 2))
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

(defun content-digest-is-stable-and-complete-p ()
  "Content keys are pinned, and every byte still reaches them.

The key names cached previews and interned originals on disk, so a change to the
algorithm silently orphans every artefact a user already has. These values were
captured from the implementation that shipped them; a mismatch here is a
deliberate decision to invalidate caches, not a passing detail. The lengths
straddle the chunk boundary and whole-word alignment, because the second lane
mixes in each word's index within its chunk and the tail is folded separately."
  (let ((pinned '((7 . "5C4123D7FEAD98FC000367000002E4DC")
                  (8 . "8B09C3E001C270DF86385708DAD08C13")
                  (9 . "0408CCA2FD65F3B586385608DAD08A60")
                  (1048576 . "C1C8FDAE075223251B42222ECCDEE329")
                  (1048583 . "D6848E6898BD98FC1B42232ECCDEE4DC")
                  (3145731 . "DE8E80DBACCCA74C180179F9A4D6DE10"))))
    (flet ((write-pattern (path length &key (edit-at nil))
             (with-open-file (stream path :direction :output
                                          :element-type '(unsigned-byte 8)
                                          :if-exists :supersede)
               (dotimes (index length)
                 (write-byte (if (eql index edit-at)
                                 (mod (1+ (* index 37)) 256)
                                 (mod (* index 37) 256))
                             stream)))
             path))
      (and
       (loop for (length . digest) in pinned
             always (let ((path (test-temporary-pathname "bin")))
                      (unwind-protect
                           (progn (write-pattern path length)
                                  (forget-content-key path)
                                  (string= digest (file-content-key path)))
                        (when (probe-file path) (delete-file path)))))
       ;; A single byte changed in the middle has to move the key: a sampled
       ;; digest once let exactly this through.
       (let ((path (test-temporary-pathname "bin")))
         (unwind-protect
              (let (before after)
                (write-pattern path 3145731)
                (forget-content-key path)
                (setf before (file-content-key path))
                (write-pattern path 3145731 :edit-at 1572866)
                (forget-content-key path)
                (setf after (file-content-key path))
                (not (string= before after)))
           (when (probe-file path) (delete-file path))))))))

(defun graph-curves-p ()
  (let* ((graph (default-processing-graph))
         (points (list 0.0 0.05 0.3 0.5 0.7 0.9 1.0 1.0))
         (node (graph-insert-node graph (processing-graph-output graph)
                                  :curves
                                  :params (list :red-points points))))
    (and (graph-validate graph)
         ;; The wire format leads with one point count per channel, then the
         ;; points, so a shaped red can sit beside three untouched channels
         ;; that each cost only their two endpoints.
         (let ((params (orfeus::graph-node-program-parameters node)))
           (and (= 24 (length params))
                (equal '(4.0 2.0 2.0 2.0) (subseq params 0 4))
                (= 0.05 (nth 5 params))
                (equal '(0.0 0.0 1.0 1.0) (subseq params 12 16))
                (equal '(0.0 0.0 1.0 1.0) (subseq params 20 24))))
         (eql 10 (rest (assoc :curves orfeus::*graph-node-kind-codes*)))
         ;; Points survive the project S-expression round trip.
         (let* ((decoded (sexp->graph (graph->sexp graph)))
                (twin (find :curves (processing-graph-nodes decoded)
                            :key #'graph-node-kind)))
           (equal points (getf (graph-node-params twin) :red-points)))
         ;; A channel may be as short as its two endpoints: four channels of
         ;; two points is sixteen floats behind the four counts.
         (let ((two (graph-insert-node (default-processing-graph) 2 :curves
                                       :params '(:blue-points (0.0 0.0 0.4 1.0)))))
           (= 20 (length (orfeus::graph-node-program-parameters two))))
         ;; Descending positions are rejected.
         (handler-case
             (progn
               (graph-insert-node
                (default-processing-graph) 2 :curves
                :params '(:green-points
                          (0.0 0.0 0.6 0.5 0.3 0.7 1.0 1.0)))
               nil)
           (invalid-project-data () t))
         ;; So is a lone point, and so is a channel past the native ceiling.
         (handler-case
             (progn (graph-insert-node (default-processing-graph) 2 :curves
                                       :params '(:red-points (0.5 0.5)))
                    nil)
           (invalid-project-data () t))
         (handler-case
             (let ((over (1+ *maximum-curve-points*)))
               (graph-insert-node
                (default-processing-graph) 2 :curves
                :params (list :red-points
                              (loop for index below over
                                    append (list (/ index (float over))
                                                 (/ index (float over))))))
               nil)
           (invalid-project-data () t)))))

(defun graph-domain-placement-p ()
  "Display-space tracking, and the insertion point derived from it.

A film node converts its branch to display space; only crops, further film
nodes, and untyped containers may read that. Placement has to know the same rule
GRAPH-VALIDATE enforces, or it puts nodes where validation then refuses them."
  (let* ((graph (default-processing-graph))
         (linear (processing-graph-output graph))
         (film (graph-node-id
                (graph-insert-node graph linear :film
                                   :params '(:lut-path nil :lut-strength 0.0
                                             :grain-amount 0.2
                                             :grain-size 1.0))))
         (crop (graph-node-id
                (graph-insert-node graph film :crop
                                   :params '(:left 0.1 :top 0.1 :width 0.8
                                             :height 0.8 :angle 0.0)))))
    (and
     ;; The source and the grade chain are scene-linear; film and anything
     ;; downstream that keeps its domain are not.
     (not (graph-display-domain-p graph *graph-source-id*))
     (not (graph-display-domain-p graph linear))
     (graph-display-domain-p graph film)
     (graph-display-domain-p graph crop)
     ;; Kinds that may live in display space keep the position asked for.
     (graph-kind-accepts-display-p :crop)
     (graph-kind-accepts-display-p :film)
     (graph-kind-accepts-display-p :node)
     (not (graph-kind-accepts-display-p :curves))
     (not (graph-kind-accepts-display-p :exposure))
     (eql crop (graph-insertion-point graph crop :crop))
     (eql crop (graph-insertion-point graph crop :film))
     ;; A scene-linear kind walks upstream past the crop and the film node to
     ;; the last position it is legal, rather than failing where it was asked.
     (eql linear (graph-insertion-point graph crop :curves))
     (eql linear (graph-insertion-point graph film :exposure))
     ;; Already legal positions are left alone.
     (eql linear (graph-insertion-point graph linear :tone))
     (eql *graph-source-id*
          (graph-insertion-point graph *graph-source-id* :white-balance)))))

(defun graph-insertable-kinds-p ()
  "The kinds a position will accept, which is what the context menu offers.

Decided by trial insertion rather than by restating the rules, so it tracks all
of them at once: the film domain, optics refusing a cropped branch, and a blend
whose two branches would disagree about geometry. A menu that offered the rest
and then quietly relocated them would be lying about where a click puts things."
  (let* ((graph (default-processing-graph))
         (linear (processing-graph-output graph))
         (film (graph-node-id
                (graph-insert-node graph linear :film
                                   :params '(:lut-path nil :lut-strength 0.0
                                             :grain-amount 0.2
                                             :grain-size 1.0))))
         (display-crop (graph-node-id
                        (graph-insert-node graph film :crop
                                           :params '(:left 0.1 :top 0.1
                                                     :width 0.8 :height 0.8
                                                     :angle 0.0))))
         (all (graph-node-kinds)))
    (and
     ;; A scene-linear, uncropped position accepts everything.
     (null (set-difference all (graph-insertable-kinds graph linear)))
     ;; Below the film transform only the display-space kinds remain: another
     ;; film node, a crop, and a rotation, all of which are indifferent to which
     ;; domain they reframe.
     (null (set-difference (graph-insertable-kinds graph film)
                           '(:film :crop :rotate)))
     (null (set-difference '(:film :crop :rotate)
                           (graph-insertable-kinds graph film)))
     (null (set-difference (graph-insertable-kinds graph display-crop)
                           '(:film :crop :rotate)))
     ;; Every offered kind really does insert, and every withheld one really
     ;; does not: the menu and the validator must not disagree either way.
     (every (lambda (kind) (graph-can-insert-p graph film kind))
            (graph-insertable-kinds graph film))
     (notany (lambda (kind) (graph-can-insert-p graph film kind))
             (set-difference all (graph-insertable-kinds graph film)))
     ;; On a scene-linear branch a crop still excludes optics, which cannot read
     ;; a cropped input, and a blend, whose other branch would be uncropped.
     (let* ((plain (default-processing-graph))
            (crop (graph-node-id
                   (graph-insert-node plain (processing-graph-output plain)
                                      :crop
                                      :params '(:left 0.1 :top 0.1 :width 0.8
                                                :height 0.8 :angle 0.0))))
            (offered (graph-insertable-kinds plain crop)))
       (and (not (member :optics offered))
            (not (member :blend offered))
            (member :curves offered)
            (member :exposure offered)))
     ;; A rotation reframes for the same reason, so it excludes the same two.
     (let* ((turned (default-processing-graph))
            (rotate (graph-node-id
                     (graph-insert-node turned (processing-graph-output turned)
                                        :rotate
                                        :params '(:quarter-turns 1))))
            (offered (graph-insertable-kinds turned rotate)))
       (and (not (member :optics offered))
            (not (member :blend offered))
            (member :curves offered)
            (member :crop offered))))))

(defun graph-node-workflow-p ()
  ;; The Resolve-style flow: New Node (untyped) -> assign a correction.
  (let* ((graph (default-processing-graph))
         (node (graph-insert-node graph (processing-graph-output graph)
                                  :node)))
    (and (graph-validate graph)
         ;; Untyped nodes are invisible to the render program.
         (= 2 (length (graph-effective-nodes graph)))
         (graph-set-node-kind graph (graph-node-id node) :exposure)
         (eq :exposure (graph-node-kind node))
         (= 3 (length (graph-effective-nodes graph)))
         ;; Retyping into a blend gains a source-fed second branch.
         (graph-set-node-kind graph (graph-node-id node) :blend)
         (= 2 (length (graph-node-inputs node)))
         (eql 0 (second (graph-node-inputs node)))
         ;; Positions ride through S-expression round trips.
         (progn (setf (orfeus:graph-node-position node) (list 40.0 96.0))
                (let* ((decoded (sexp->graph (graph->sexp graph)))
                       (twin (find :blend (processing-graph-nodes decoded)
                                   :key #'graph-node-kind)))
                  (equal '(40.0 96.0) (orfeus:graph-node-position twin))))
         ;; Unknown kinds are rejected.
         (handler-case
             (progn (graph-set-node-kind graph (graph-node-id node) :bogus)
                    nil)
           (invalid-project-data () t)))))

(defun graph-position-validation-p ()
  (let* ((graph (default-processing-graph))
         (node (first (processing-graph-nodes graph))))
    (and
     (every (lambda (position)
              (setf (orfeus:graph-node-position node) position)
              (handler-case (progn (graph->sexp graph) nil)
                (invalid-project-data () t)))
            (list '(1.0) '(1.0 2.0 3.0) '(1.0 bogus) '(100001 0)
                   #(1.0 2.0) '(1.0 . 2.0)))
     (progn
       (setf (orfeus:graph-node-position node) '(12.5 -8.0))
       (equal '(12.5 -8.0)
              (orfeus:graph-node-position
               (first (processing-graph-nodes
                       (sexp->graph (graph->sexp graph))))))))))

(defun graph-render-identity-p ()
  (let* ((graph (default-processing-graph))
         (node (first (processing-graph-nodes graph)))
         (before (orfeus:graph->render-sexp graph)))
    (setf (orfeus:graph-node-position node) '(200.0 -75.0)
          (orfeus::graph-node-kind-states node)
          '((:film :params (:lut-path "inactive.cube" :lut-strength 1.0))))
    (and (equal before (orfeus:graph->render-sexp graph))
         (not (equal (graph->sexp (default-processing-graph))
                     (graph->sexp graph))))))

(defun graph-kind-state-recovery-p ()
  (let* ((graph (graph-validate
                 (make-processing-graph
                  :nodes (list
                          (make-graph-node :id 1 :kind :tone
                                           :params '(:tone-shadows 0.2)
                                           :inputs '(0))
                          (make-graph-node :id 2 :kind :white-balance
                                           :params '(:white-balance-tint 2.0)
                                           :inputs '(0))
                          (make-graph-node :id 3 :kind :exposure
                                           :params '(:exposure 1.25)
                                           :inputs '(1)))
                  :output 3)))
         (node (graph-find-node graph 3)))
    (and
     (graph-set-node-kind graph 3 :blend)
     (graph-set-blend-input graph 3 2)
     (progn (setf (graph-node-opacity node) 0.7) t)
     (graph-set-node-kind graph 3 :exposure)
     (equal '(:exposure 1.25) (graph-node-params node))
     (graph-set-node-kind graph 3 :tone)
     (progn (setf (graph-node-params node) '(:tone-shadows 0.6)) t)
     (graph-set-node-kind graph 3 :blend)
     (= 0.7 (graph-node-opacity node))
     (eql 2 (second (graph-node-inputs node)))
     ;; Saved configurations remain portable, including across another switch.
     (let* ((decoded (sexp->graph (graph->sexp graph)))
            (twin (graph-find-node decoded 3)))
       (and (graph-set-node-kind decoded 3 :tone)
            (equal '(:tone-shadows 0.6) (graph-node-params twin))
            (graph-set-node-kind decoded 3 :exposure)
            (equal '(:exposure 1.25) (graph-node-params twin)))))))

(defun graph-kind-state-deletion-remap-p ()
  (let* ((graph
           (graph-validate
            (make-processing-graph
             :nodes (list
                     (make-graph-node :id 5 :kind :tone
                                      :params '(:tone-shadows 0.2) :inputs '(0))
                     (make-graph-node :id 10 :kind :exposure
                                      :params '(:exposure 0.5) :inputs '(5))
                     (make-graph-node
                      :id 20 :kind :tone :params '(:tone-highlights 0.3)
                      :inputs '(10)
                      :kind-states
                      '((:blend :opacity 0.6 :second-input 10))))
             :output 20)))
         (tail (graph-find-node graph 20)))
    (graph-delete-node graph 10)
    (let* ((renumbered (find tail (processing-graph-nodes graph)))
           (blend (assoc :blend (orfeus::graph-node-kind-states renumbered))))
      (and (= 2 (graph-node-id renumbered))
           (eql 1 (getf (rest blend) :second-input))))))

(defun graph-rewire-inputs-p ()
  (let* ((graph (default-processing-graph))
         (optics (first (processing-graph-nodes graph)))
         (nr (second (processing-graph-nodes graph))))
    (and ;; NR reads the source directly; optics hangs as a side branch.
         (graph-set-primary-input graph (graph-node-id nr)
                                  *graph-source-id*)
         (eql 0 (first (graph-node-inputs nr)))
         ;; The output can move to any node.
         (graph-set-output graph (graph-node-id optics))
         (eql (graph-node-id optics) (processing-graph-output graph))
         ;; Wiring a genuine cycle rolls back untouched.
         (graph-set-primary-input graph (graph-node-id optics)
                                  (graph-node-id nr))
         (let ((before (graph->sexp graph)))
           (and (handler-case
                    (progn (graph-set-primary-input
                            graph (graph-node-id nr)
                            (graph-node-id optics))
                           nil)
                  (invalid-project-data () t))
                (equal before (graph->sexp graph)))))))

(defun graph-rewire-p ()
  (let ((graph (graph-validate
                (make-processing-graph
                 :nodes (list (make-graph-node :id 1 :kind :optics
                                               :params '(:lens-correction-p t)
                                               :inputs '(0))
                              (make-graph-node :id 2 :kind :noise-reduction
                                               :params '(:noise-reduction 0.4)
                                               :inputs '(1))
                              (make-graph-node :id 3 :kind :tone
                                               :params '(:tone-shadows 0.5)
                                               :inputs '(2)))
                 :output 3))))
    (flet ((kind-node (kind)
             (find kind (processing-graph-nodes graph)
                   :key #'graph-node-kind))
           (kinds ()
             (mapcar #'graph-node-kind (processing-graph-nodes graph))))
      ;; Drag the tail node to the head of the chain.
      (unless (graph-move-node-after graph (graph-node-id (kind-node :tone))
                                     *graph-source-id*)
        (return-from graph-rewire-p nil))
      (unless (equal '(:tone :optics :noise-reduction) (kinds))
        (return-from graph-rewire-p nil))
      ;; Moving a node onto its own input is a no-op.
      (when (graph-move-node-after graph (graph-node-id (kind-node :optics))
                                   (graph-node-id (kind-node :tone)))
        (return-from graph-rewire-p nil))
      ;; Drag the head node back into the middle.
      (unless (graph-move-node-after graph (graph-node-id (kind-node :tone))
                                     (graph-node-id (kind-node :optics)))
        (return-from graph-rewire-p nil))
      (unless (equal '(:optics :tone :noise-reduction) (kinds))
        (return-from graph-rewire-p nil))
      ;; A mid-chain blend can rewire its second branch to an upstream
      ;; node, and wiring it downstream (a cycle) rolls back untouched.
      (let ((blend (graph-insert-node
                    graph (graph-node-id (kind-node :optics)) :blend
                    :opacity 0.5)))
        (unless (graph-set-blend-input
                 graph (graph-node-id blend)
                 (graph-node-id (kind-node :optics)))
          (return-from graph-rewire-p nil))
        (let ((before (graph->sexp graph)))
          (and (handler-case
                   (progn (graph-set-blend-input
                           graph (graph-node-id blend)
                           (graph-node-id (kind-node :noise-reduction)))
                          nil)
                 (error () t))
               (equal before (graph->sexp graph))
               (progn (graph-validate graph) t)))))))

(defun graph-edit-rollback-and-copy-p ()
  (let* ((graph (graph-validate
                 (make-processing-graph
                  :nodes (list
                          (make-graph-node :id 1 :kind :exposure
                                           :params '(:exposure 0.5)
                                           :inputs '(0))
                          (make-graph-node :id 2 :kind :film
                                           :params '(:lut-path "film.cube")
                                           :inputs '(1)))
                  :output 2)))
         (before (graph->sexp graph))
         (points (list 0.0 0.0 0.33 0.4 0.67 0.8 1.0 1.0))
         (curve-graph (make-processing-graph))
         (curve (graph-insert-node curve-graph 0 :curves
                                   :params (list :red-points points)))
         (copy (orfeus:graph-copy curve-graph))
         (copied-curve (first (processing-graph-nodes copy))))
    (and
     ;; Inserting a scene-linear node after film fails without mutating GRAPH.
     (handler-case
         (progn (graph-insert-node graph 2 :exposure
                                   :params '(:exposure 1.0))
                nil)
       (invalid-project-data () t))
     (equal before (graph->sexp graph))
     ;; Film nodes cannot cross either side of a swap.
     (null (graph-swap-with-upstream graph 2))
     (equal before (graph->sexp graph))
     ;; Insertion and graph copies own nested curve point lists.
     (progn
       (setf (second points) 0.2)
       (= 0.0 (second (getf (graph-node-params curve) :red-points))))
     (progn
       (setf (second (getf (graph-node-params copied-curve) :red-points)) 0.3)
       (= 0.0 (second (getf (graph-node-params curve) :red-points)))))))

(defun graph-crop-branch-compatibility-p ()
  (let* ((crop '(:left 0.1 :top 0.1 :width 0.8 :height 0.8))
         (graph (graph-validate
                 (make-processing-graph
                  :nodes (list
                          (make-graph-node :id 1 :kind :crop
                                           :params (copy-list crop) :inputs '(0))
                          (make-graph-node :id 2 :kind :crop
                                           :params (copy-list crop) :inputs '(0))
                          (make-graph-node :id 3 :kind :blend
                                           :inputs '(1 2) :opacity 0.5))
                  :output 3)))
         (before (graph->sexp graph)))
    (and
     ;; Equal crops on separate branches have compatible image geometry.
     (graph-validate graph)
     ;; Removing one branch's crop is rejected and leaves every wire intact.
     (handler-case
         (progn (graph-delete-node graph 1) nil)
       (invalid-project-data () t))
     (equal before (graph->sexp graph))
     ;; Moving one crop below the other is likewise incompatible and atomic.
     (handler-case
         (progn (graph-move-node-after graph 2 1) nil)
       (invalid-project-data () t))
     (equal before (graph->sexp graph))
     (graph-validate graph)
     ;; A bypassed crop resolves to its input, so it cannot disguise a
     ;; geometry mismatch against an active crop.
     (handler-case
         (progn
           (graph-validate
            (make-processing-graph
             :nodes (list
                     (make-graph-node :id 1 :kind :crop
                                      :params (copy-list crop) :inputs '(0))
                     (make-graph-node :id 2 :kind :crop
                                      :params (copy-list crop) :inputs '(0)
                                      :bypassed-p t)
                     (make-graph-node :id 3 :kind :blend
                                      :inputs '(1 2) :opacity 0.5))
             :output 3))
           nil)
       (invalid-project-data () t))
     ;; Bypassed filters and blends both inherit only their first input's
     ;; effective geometry, irrespective of their inactive crop branches.
     (graph-validate
      (make-processing-graph
       :nodes (list
               (make-graph-node :id 1 :kind :crop
                                :params (copy-list crop) :inputs '(0)
                                :bypassed-p t)
               (make-graph-node :id 2 :kind :crop
                                :params '(:left 0.2 :top 0.2
                                          :width 0.6 :height 0.6)
                                :inputs '(0) :bypassed-p t)
               (make-graph-node :id 3 :kind :blend
                                :inputs '(1 2) :opacity 0.5)
               (make-graph-node :id 4 :kind :crop
                                :params (copy-list crop) :inputs '(3))
               (make-graph-node :id 5 :kind :blend
                                :inputs '(4 0) :opacity 0.5
                                :bypassed-p t))
       :output 5)))))

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
         (eql 4 (second (project->sexp original)))
         ;; Versions 1 through 3 migrate without graph kind history.
         (every (lambda (version)
                  (null (photo-job-graph
                         (first (project-photos
                                 (sexp->project
                                  `(:orfeus-project ,version
                                    :output-directory "exports/"
                                    :defaults (:exposure 0.0)
                                    :export-settings (:jpeg-quality 92)
                                    :photos ((:input "a.orf")))))))))
                '(1 2 3)))))

(defun timestamped-output-names-p ()
  (let* ((first (make-photo-job :input-path #P"one/photo.orf"))
         (second (make-photo-job :input-path #P"two/photo.orf"))
         (project (make-project
                   :output-directory #P"/tmp/orfeus-exports/"
                   :export-settings (make-export-settings
                                     :timestamp-filenames-p t)
                   :photos (list first second))))
    (and (string= "20260802-183512"
                  (orfeus::timestamp-token "2026:08:02 18:35:12"))
         (null (orfeus::timestamp-token "no digits here"))
         ;; Without readable EXIF the original name survives untouched.
         (equal #P"/tmp/orfeus-exports/photo.jpg"
                (photo-job-render-output project first))
         ;; Equal timestamp/name pairs from different directories cannot collide.
         (let ((original (symbol-function 'orfeus::photo-capture-timestamp)))
           (unwind-protect
                (progn
                  (setf (symbol-function 'orfeus::photo-capture-timestamp)
                        (lambda (pathname)
                          (declare (ignore pathname))
                          "20260802-183512"))
                  (let* ((duplicate (make-photo-job
                                     :input-path #P"three/foo.orf"))
                         (same (make-photo-job
                                :input-path #P"four/foo.orf"))
                         (literal-suffix (make-photo-job
                                          :input-path #P"five/foo-2.orf"))
                         (collision-project
                           (make-project
                            :output-directory #P"/tmp/orfeus-exports/"
                            :export-settings (make-export-settings
                                              :timestamp-filenames-p t)
                            :photos (list duplicate same literal-suffix))))
                    (and (equal #P"/tmp/orfeus-exports/20260802-183512-photo.jpg"
                                (photo-job-render-output project first))
                         (equal #P"/tmp/orfeus-exports/20260802-183512-photo-2.jpg"
                                (photo-job-render-output project second))
                         ;; The duplicate's generated -2 name reserves that
                         ;; final stem against a later literal foo-2 input.
                         (equal #P"/tmp/orfeus-exports/20260802-183512-foo.jpg"
                                (photo-job-render-output collision-project duplicate))
                         (equal #P"/tmp/orfeus-exports/20260802-183512-foo-2.jpg"
                                (photo-job-render-output collision-project same))
                         (equal #P"/tmp/orfeus-exports/20260802-183512-foo-2-2.jpg"
                                (photo-job-render-output collision-project
                                                         literal-suffix)))))
             (setf (symbol-function 'orfeus::photo-capture-timestamp) original)))
         (let ((decoded (sexp->project (project->sexp project))))
           (export-settings-timestamp-filenames-p
            (project-export-settings decoded))))))

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
             :presets (list
                       (make-processing-preset
                        :name "Graph preset"
                        :graph (graph-validate
                                (make-processing-graph
                                 :nodes (list
                                         (make-graph-node
                                          :id 1 :kind :film
                                          :params '(:lut-path "preset.cube")
                                          :inputs '(0)
                                          :kind-states
                                          '((:film :params
                                             (:lut-path "preset-dormant.cube")))))
                                 :output 1))))
             :photos (list
                      (make-photo-job
                       :input-path #P"input.orf"
                       :graph (graph-validate
                               (make-processing-graph
                                :nodes (list
                                        (make-graph-node
                                         :id 1 :kind :film
                                         :params '(:lut-path "photo.cube")
                                         :inputs '(0)
                                         :kind-states
                                         '((:film :params
                                            (:lut-path "photo-dormant.cube")))))
                                :output 1)))))
            pathname)
           (let* ((project (project-read pathname))
                  (base (uiop:pathname-directory-pathname pathname))
                  (photo (first (project-photos project)))
                  (preset (first (project-presets project)))
                  (photo-node (first (processing-graph-nodes
                                      (photo-job-graph photo))))
                  (preset-node (first (processing-graph-nodes
                                       (processing-preset-graph preset)))))
             (and (equal (project-output-directory project)
                         (merge-pathnames #P"exports/" base))
                  (equal (photo-job-input-path photo)
                         (merge-pathnames #P"input.orf" base))
                  (string= (getf (graph-node-params photo-node) :lut-path)
                           (namestring (merge-pathnames #P"photo.cube" base)))
                  (string= (getf (getf (rest (assoc :film
                                                    (orfeus::graph-node-kind-states
                                                     photo-node)))
                                        :params)
                                 :lut-path)
                           (namestring
                            (merge-pathnames #P"photo-dormant.cube" base)))
                  (string= (getf (graph-node-params preset-node) :lut-path)
                           (namestring (merge-pathnames #P"preset.cube" base)))
                  (string= (getf (getf (rest (assoc :film
                                                    (orfeus::graph-node-kind-states
                                                     preset-node)))
                                        :params)
                                 :lut-path)
                           (namestring
                            (merge-pathnames #P"preset-dormant.cube" base))))))
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
            ;; magic "ORFG", version 4, one node: exposure(2), input 0,
            ;; no second input, one parameter 0.5f0, no string.
            (coerce #(#x4F #x52 #x46 #x47  4 0 0 0  1 0 0 0
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

(defun bundled-film-luts-match-pinned-digests-p ()
  (let* ((directory (asdf:system-relative-pathname "orfeus" #P"data/luts/"))
         (expected
           '(("IWLTBAP K25.cube" . "4065c4c9cdbe1df5e37f43538710ae340e877879ccf31875f91e0702d2bc03c4")
             ("IWLTBAP K64.cube" . "da2e5731e0bff919d56fe188b78dcde963034dd1f9ba76d23a1f0cbefc49c852")
             ("IWLTBAP K99.cube" . "2ad878a5611621f9f5748a3d58e14b8832ccf2d23d788bab3f904c56f1b804f6")
             ("agfa_apx_100.cube" . "61f692f928d3809a77af2249035f771e47150964ac7243e9a217438cd2ca2858")
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
         (string= "" (get-output-stream-string errors))
         ;; The release number is declared once, in the system definition, and
         ;; everything that reports a build starts from it.
         (let ((version (orfeus-version))
               (description (orfeus-build-description)))
           (and (not (string= "unknown" version))
                (eql 0 (search version description))
                ;; A revision, when one can be found, is named after the version
                ;; rather than instead of it.
                (or (null (orfeus-build-commit))
                    (search (orfeus-build-commit) description)))))))

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
      (check "deep project copies share nothing an edit can reach"
             (project-deep-copy-is-independent-p))
      (check "interning copies a RAW off its card exactly once"
             (interned-raw-store-behaves-p))
      (check "batch renders report in project order"
             (project-render-batches-in-order-p))
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
      (check "the local still store writes, renames, and deletes stills"
             (still-store-round-trip-p))
      (check "graphs round trip through S-expressions"
             (graph-round-trip-p))
      (check "graph validation rejects malformed graphs"
             (graph-validation-rejects-p))
      (check "settings convert to equivalent linear graphs"
             (settings-graph-conversion-p))
      (check "graph editing inserts, reorders, deletes, and blends"
             (graph-editing-p))
      (check "nodes splice anywhere and invalid wires roll back"
             (graph-rewire-p))
      (check "a position offers exactly the kinds it accepts"
             (graph-insertable-kinds-p))
      (check "display-space tracking places nodes where they are legal"
             (graph-domain-placement-p))
      (check "untyped nodes pass through until a correction is assigned"
             (graph-node-workflow-p))
      (check "graph positions validate before portable serialization"
             (graph-position-validation-p))
      (check "render graph identity ignores editor-only positions"
             (graph-render-identity-p))
      (check "node kind switching preserves portable prior settings"
             (graph-kind-state-recovery-p))
      (check "deletion remaps dormant blend inputs through the replacement"
             (graph-kind-state-deletion-remap-p))
      (check "primary inputs and the output rewire with rollback"
             (graph-rewire-inputs-p))
      (check "failed graph edits roll back and graph copies own nested params"
             (graph-edit-rollback-and-copy-p))
      (check "blends require compatible crop geometry on both branches"
             (graph-crop-branch-compatibility-p))
      (check "content digests are pinned and cover every byte"
             (content-digest-is-stable-and-complete-p))
      (check "curves nodes validate, serialize, and round trip"
             (graph-curves-p))
      (check "photo graphs round trip as project version 4"
             (photo-graph-round-trip-p))
      (check "graphs serialize to the native program format"
             (graph-program-bytes-p))
      (check "bypassed nodes drop out of serialized programs"
             (graph-program-prunes-bypassed-nodes-p))
      (check "graph rendering never replaces its input"
             (graph-render-rejects-input-as-output-p))
      (check "negative-inversion graphs validate and serialize"
             (negative-workflow-graph-p))
      (check "timestamped output names format and round trip"
             (timestamped-output-names-p))
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
      (check "bundled film LUTs match pinned digests"
             (bundled-film-luts-match-pinned-digests-p))
      (check "CLI reports its version" (cli-version-p))
      (check "CLI rejects unknown commands" (cli-rejects-unknown-command-p)))
    (zerop failures)))
