(in-package #:orfeus/gui)

(defparameter *preview-debounce-seconds* 0.15d0
  "Delay used to coalesce interactive control changes.")

(defparameter *gui-preview-max-width* 0
  "Maximum GUI preview width; zero preserves full source resolution.")

(defparameter *gui-preview-max-height* 0
  "Maximum GUI preview height; zero preserves full source resolution.")

(defparameter *gui-draft-preview-size* 2048
  "Bounding size of the fast draft preview rendered before the full one.")

(defparameter *thumbnail-preview-size* 320
  "Maximum width and height for orientation-correct thumbnail renders.")

(defun available-processor-count ()
  "Return the number of logical processors reported by Linux, or one."
  (handler-case
      (with-open-file (stream #P"/proc/cpuinfo" :direction :input)
        (max 1 (loop for line = (read-line stream nil)
                     while line
                     count (uiop:string-prefix-p "processor" line))))
    (error () 1)))

(defparameter *background-preview-workers* (available-processor-count)
  "Concurrent thumbnail and full-project preview workers, one per CPU.")

(defconstant +thumbnail-shift-mask+ 1)
(defconstant +thumbnail-control-mask+ 4)

(defconstant +menu-shift+ #x00010000
  "FLTK FL_SHIFT modifier for menu shortcuts.")
(defconstant +menu-ctrl+ #x00040000
  "FLTK FL_CTRL modifier for menu shortcuts.")
(defconstant +key-f5+ #xffc2
  "FLTK key code for the F5 function key.")

(defparameter *node-kind-labels*
  '((:white-balance . "WB")
    (:exposure . "Expo")
    (:noise-reduction . "NR")
    (:tone . "Tone")
    (:optics . "Optics")
    (:film . "Film")
    (:blend . "Blend")
    (:color-subtract . "Subtr")
    (:crop . "Crop")
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
    ("Crop" . :crop)
    ("Curves" . :curves))
  "Correction picker entries for the Node panel, in menu order.")

(defparameter *graph-node-width* 96
  "Width of a node box on the graph editor canvas.")

(defparameter *graph-node-height* 30
  "Height of a node box on the graph editor canvas.")

(defparameter *graph-row-pitch* 46
  "Vertical distance between auto-laid-out nodes on the graph canvas.")

(defun node-kind-label (kind)
  (or (rest (assoc kind *node-kind-labels*)) (string kind)))

(defun curve-spline-value (points x)
  "Evaluate the monotone cubic through POINTS (four x y pairs) at X.

Mirrors the native executor's Fritsch-Carlson spline so the editor draws
exactly the curve the render applies."
  (let ((xs (make-array 4)) (ys (make-array 4)))
    (dotimes (index 4)
      (setf (aref xs index) (float (nth (* index 2) points) 1d0)
            (aref ys index) (float (nth (1+ (* index 2)) points) 1d0)))
    (cond
      ((<= x (aref xs 0)) (aref ys 0))
      ((>= x (aref xs 3)) (aref ys 3))
      (t
       (let ((slopes (make-array 3))
             (tangents (make-array 4)))
         (dotimes (segment 3)
           (setf (aref slopes segment)
                 (/ (- (aref ys (1+ segment)) (aref ys segment))
                    (max 1.0d-4 (- (aref xs (1+ segment))
                                   (aref xs segment))))))
         (setf (aref tangents 0) (aref slopes 0)
               (aref tangents 3) (aref slopes 2))
         (loop for point from 1 to 2
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
         (let* ((segment (cond ((< x (aref xs 1)) 0)
                               ((< x (aref xs 2)) 1)
                               (t 2)))
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
    (:color-subtract t)
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
  "One gallery row with an explicit project or local origin."
  origin
  identity
  preset)

(defun gallery-still-key (still)
  "Return the cache/task key for STILL, including its provenance."
  (list (gallery-still-origin still) (gallery-still-identity still)))

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

(defun preview-priority-indices (count selected)
  "Return every photo index ordered from SELECTED outward."
  (when (and (plusp count) (<= 0 selected) (< selected count))
    (stable-sort (loop for index below count collect index)
                 #'< :key (lambda (index) (abs (- index selected))))))

(defun thumbnail-selection-after-click (selected row anchor state)
  "Return the selection and anchor produced by clicking ROW with modifier STATE."
  (cond
    ((logtest +thumbnail-shift-mask+ state)
     (values (loop for index from (min anchor row) to (max anchor row)
                   collect index)
             anchor))
    ((logtest +thumbnail-control-mask+ state)
     (values (if (member row selected)
                 (remove row selected)
                 (cons row selected))
             row))
    (t (values (list row) row))))

(defun crop-preview-current-p (preview-generation published-generation)
  "True when the displayed preview uses the current crop-edit generation."
  (and published-generation
       (= preview-generation published-generation)))

(defun fltk-file-filter (label pattern)
  "Build FLTK's LABEL<TAB>PATTERN native file chooser syntax."
  (format nil "~A~C~A" label #\Tab pattern))

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

(defun gui-preview-cache-directory ()
  "The persistent preview cache under XDG, keyed by photo content."
  (let ((directory (uiop:xdg-cache-home "orfeus/previews/")))
    (ensure-directories-exist directory)
    directory))

(defun photo-content-key (pathname)
  "A quick content fingerprint: size plus head and tail samples, hashed.

Renaming or moving a photograph keeps its previews valid; editing the file
invalidates them."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let* ((size (file-length stream))
           (sample (min 65536 size))
           (digest (ironclad:make-digest :sha256))
           (buffer (make-array sample :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      (ironclad:update-digest digest buffer)
      (when (> size sample)
        (file-position stream (- size sample))
        (let ((count (read-sequence buffer stream)))
          (ironclad:update-digest digest buffer :end count)))
      (let ((size-bytes (make-array 8 :element-type '(unsigned-byte 8))))
        (dotimes (index 8)
          (setf (aref size-bytes index) (ldb (byte 8 (* 8 index)) size)))
        (ironclad:update-digest digest size-bytes))
      (subseq (ironclad:byte-array-to-hex-string
               (ironclad:produce-digest digest))
              0 16))))

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
        (if (< date cutoff)
            (ignore-errors (delete-file file))
            (push (list file date size) entries))))
    (let ((total (reduce #'+ entries :key #'third)))
      (dolist (entry (sort entries #'< :key #'second))
        (when (<= total max-bytes)
          (return))
        (ignore-errors (delete-file (first entry)))
        (decf total (third entry))))))

(defun preview-recipe-snapshot (recipe)
  "Return an immutable deep copy of a settings or graph render RECIPE."
  (etypecase recipe
    (orfeus:processing-graph
     (orfeus:sexp->graph (orfeus:graph->sexp recipe)))
    (orfeus:processing-settings
     (orfeus::sexp->processing-settings
      (orfeus::processing-settings->sexp recipe)))))

(defun preview-settings-key (recipe)
  "Return a content hash covering a settings or graph render RECIPE."
  (let* ((*print-readably* t)
         (text (prin1-to-string
                (etypecase recipe
                  (orfeus:processing-graph (orfeus:graph->sexp recipe))
                  (orfeus:processing-settings
                   (orfeus::processing-settings->sexp recipe)))))
         (digest (ironclad:digest-sequence
                  :sha256
                  (sb-ext:string-to-octets text :external-format :utf-8))))
    (subseq (ironclad:byte-array-to-hex-string digest) 0 16)))

(defun preview-pathname (preview-directory content-key role settings)
  (merge-pathnames
   (make-pathname :name (format nil "~(~A~)-~A-~A"
                                role content-key
                                (preview-settings-key settings))
                  :type "jpg")
   preview-directory))

(defun thumbnail-pathname (preview-directory content-key)
  (merge-pathnames
   (make-pathname :name (format nil "thumbnail-~A" content-key)
                  :type "jpg")
   preview-directory))

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
          (format nil "RAW preview  |  WB: ~A  |  NR: ~D%~@[  |  Neural: ~D%~]  |  LUT: ~A"
                  (if temperature "Custom" "As shot")
                  (round (* 100 noise-reduction))
                  (when (plusp neural) (round (* 100 neural)))
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
    ;; Load the CFFI bridge before render workers can race to initialize it.
    (orfeus:native-bridge-version)
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
           (lut-paths (make-hash-table :test #'equal))
           (content-keys (make-hash-table :test #'eq))
           window menu toolbar toolbar-bottom-rule main-tile left-column
           filmstrip-pane gallery-pane center-pane
           thumbnail-canvas thumbnail-scrollbar
           before-canvas after-canvas before-caption after-caption
           still-button copy-grade-button paste-grade-button
           right-column graph-pane graph-canvas graph-title
           (graph-scroll-x 0)
           (graph-scroll-y 0)
           pick-color-node crop-drag node-drag
           (node-click (cons 0 nil))
           grade-clipboard
           inspector tabs node-page export-page
           kind-choice
           (node-panel-groups '())
           blend-opacity-input crop-angle-input
           curve-canvas curve-drag
           (curve-channel :red-points)
           curve-histogram
           status progress before-preview-file after-preview-file
           after-preview-generation
           lens-name controls inspector-items tone-items lut-choice wb-choice target-choice
           export-quality export-max-width export-max-height export-metadata
           export-timestamp
           gallery-canvas gallery-title preset-name-input
           preset-apply-button preset-save-button
           (gallery-scroll 0)
           (gallery-selected nil)
           (gallery-stills '())
           (gallery-generation 0)
           (gallery-thumbs (make-hash-table :test #'equal))
           (gallery-click (cons 0 -1))
           debounce-id poll-id comparison-p layout-initialized-p
           (progress-total 0)
           (thumbnail-scroll 0)
           (thumbnail-anchor 0)
           (preview-generation 0)
           (preview-zoom 1d0)
           (preview-center-x .5d0)
           (preview-center-y .5d0)
           preview-drag-p preview-drag-x preview-drag-y
           preview-drag-center-x preview-drag-center-y)
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
             (when before-canvas (cl-fltk:redraw before-canvas))
             (when after-canvas (cl-fltk:redraw after-canvas)))
           (reset-preview-view ()
             (setf preview-zoom 1d0
                   preview-center-x .5d0
                   preview-center-y .5d0)
             (redraw-previews)
             (set-status "Preview fitted to window"))
           (preview-scaled-size (canvas path zoom)
             (multiple-value-bind (source-width source-height)
                 (preview-file-size path)
               (when source-width
                 (let ((fit (min (/ (cl-fltk:widget-width canvas)
                                    (float source-width 1d0))
                                 (/ (cl-fltk:widget-height canvas)
                                    (float source-height 1d0)))))
                   (let ((scale (min (max fit 2d0) (* fit zoom))))
                     (values (* source-width scale)
                             (* source-height scale)
                             fit))))))
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
                     (let* ((canvas-center-x (/ (cl-fltk:widget-width canvas) 2d0))
                            (canvas-center-y (/ (cl-fltk:widget-height canvas) 2d0))
                            (source-x (+ preview-center-x
                                         (/ (- pointer-x canvas-center-x) old-width)))
                            (source-y (+ preview-center-y
                                         (/ (- pointer-y canvas-center-y) old-height)))
                            (ratio (/ old-zoom new-zoom)))
                       (setf preview-center-x
                             (- source-x (* (- source-x preview-center-x) ratio))
                             preview-center-y
                             (- source-y (* (- source-y preview-center-y) ratio)))))))
               (setf preview-zoom new-zoom)
               (redraw-previews)
               (set-status (format nil "Preview zoom: ~,0F%" (* 100 new-zoom)))))
           (preview-one-to-one ()
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
                     (set-status "Preview at 1:1 pixels"))))))
           (crop-node-rect (node)
             (let ((params (orfeus:graph-node-params node)))
               (values (float (getf params :left 0.0) 1d0)
                       (float (getf params :top 0.0) 1d0)
                       (float (getf params :width 1.0) 1d0)
                       (float (getf params :height 1.0) 1d0)
                       (float (getf params :angle 0.0) 1d0))))
           (preview-image-frame (canvas path)
             ;; Widget-relative placement of the drawn preview, mirroring
             ;; the native adapter's pan clamping so overlays land on the
             ;; same pixels the image occupies.
             (multiple-value-bind (scaled-width scaled-height)
                 (preview-scaled-size canvas path preview-zoom)
               (when scaled-width
                 (let* ((width (cl-fltk:widget-width canvas))
                        (height (cl-fltk:widget-height canvas))
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
                       (let ((radians (* angle (/ pi 180))))
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
                   (progn
                     (gui-model-set-node-params
                      model node
                      (list :left (float left 1.0) :top (float top 1.0)
                            :width (float width 1.0)
                            :height (float height 1.0)))
                     (when after-canvas (cl-fltk:redraw after-canvas))
                     (set-status
                      (format nil "Crop ~,2F ~,2F ~,2Fx~,2F"
                              left top width height)))
                 (error (condition)
                   (set-status (princ-to-string condition))))))
           (set-crop-node-angle (node angle)
             ;; The crop stage is bypassed while its node is selected, so
             ;; an angle change only moves the overlay; no re-render.
             (handler-case
                 (progn
                   (gui-model-set-node-params model node (list :angle angle))
                   (when after-canvas (cl-fltk:redraw after-canvas))
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
                             (span-x (min frame-width
                                          (max (* frame-width 0.05)
                                               (* sign-x local-x))))
                             (span-y (min frame-height
                                          (max (* frame-height 0.05)
                                               (* sign-y local-y))))
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
                 (let* ((x (cl-fltk:widget-x widget))
                        (y (cl-fltk:widget-y widget))
                        (corners
                          (loop for (dx dy) in (crop-corner-offsets
                                                half-width half-height)
                                collect
                                (multiple-value-bind (px py)
                                    (crop-corner-point center-x center-y
                                                       cosine sine dx dy)
                                  (list (round (+ x px))
                                        (round (+ y py)))))))
                   (cl-fltk:draw-push-clip x y
                                           (cl-fltk:widget-width widget)
                                           (cl-fltk:widget-height widget))
                   (loop for (from to) in '((0 1) (1 2) (2 3) (3 0))
                         do (destructuring-bind (x1 y1) (nth from corners)
                              (destructuring-bind (x2 y2) (nth to corners)
                                (cl-fltk:draw-color-rgb :red 20 :green 22
                                                        :blue 26)
                                (cl-fltk:draw-line (1+ x1) (1+ y1)
                                                   (1+ x2) (1+ y2))
                                (cl-fltk:draw-color-rgb :red 235 :green 235
                                                        :blue 240)
                                (cl-fltk:draw-line x1 y1 x2 y2))))
                   (loop for (px py) in corners
                         do (cl-fltk:draw-color-rgb :red 20 :green 22
                                                    :blue 26)
                            (cl-fltk:draw-filled-circle px py 5)
                            (cl-fltk:draw-color-rgb :red 235 :green 235
                                                    :blue 240)
                            (cl-fltk:draw-filled-circle px py 4))
                   (cl-fltk:draw-pop-clip)))))
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
                                 (values 60 75 125)))))
           (curve-channel-label (key)
             (ecase key
               (:red-points "Red")
               (:green-points "Green")
               (:blue-points "Blue")))
           (schedule-curve-histogram (path generation)
             ;; Histogram decoding is bounded to one pending background task;
             ;; draw callbacks only consume the last completed result.
             (discard-gui-tasks histogram-queue :histogram)
             (setf curve-histogram nil)
             (enqueue-gui-task
              histogram-queue :histogram
              (lambda ()
                (multiple-value-bind (red green blue)
                    (ignore-errors (preview-histogram path))
                  (when red
                    (queue-event queue
                                 (list :histogram generation path
                                       (list red green blue))))))))
           (curve-chart-geometry ()
             ;; The chart rectangle, widget-relative.
             (values 12 8
                     (- (cl-fltk:widget-width curve-canvas) 24)
                     (- (cl-fltk:widget-height curve-canvas) 16)))
           (draw-curve-editor (widget)
             (let* ((x (cl-fltk:widget-x widget))
                    (y (cl-fltk:widget-y widget))
                    (width (cl-fltk:widget-width widget))
                    (height (cl-fltk:widget-height widget))
                    (node (curves-editing-node)))
               (cl-fltk:draw-color-rgb :red 44 :green 44 :blue 48)
               (cl-fltk:draw-filled-rect x y width height)
               (multiple-value-bind (chart-x chart-y chart-w chart-h)
                   (curve-chart-geometry)
                 (let ((left (+ x chart-x))
                       (top (+ y chart-y)))
                   (cl-fltk:draw-color-rgb :red 16 :green 16 :blue 18)
                   (cl-fltk:draw-filled-rect left top chart-w chart-h)
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
                                (cl-fltk:draw-color-rgb :red red :green green
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
                                           (cl-fltk:draw-filled-rect
                                            bar-x (+ top chart-h (- bar))
                                            bar-w bar))))))
                   (cl-fltk:draw-color-rgb :red 58 :green 58 :blue 64)
                   (dotimes (line 2)
                     (let ((grid-x (+ left (floor (* (1+ line) chart-w) 3)))
                           (grid-y (+ top (floor (* (1+ line) chart-h) 3))))
                       (cl-fltk:draw-line grid-x top grid-x (+ top chart-h))
                       (cl-fltk:draw-line left grid-y (+ left chart-w)
                                          grid-y)))
                   (cl-fltk:draw-color-rgb :red 84 :green 84 :blue 92)
                   (cl-fltk:draw-line left (+ top chart-h) (+ left chart-w)
                                      top)
                   (if (null node)
                       (progn
                         (cl-fltk:draw-color-rgb :red 205 :green 205
                                                 :blue 210)
                         (cl-fltk:draw-font :size 11)
                         (cl-fltk:draw-text "Right-click the node strip"
                                            (+ left 14) (+ top 26))
                         (cl-fltk:draw-text "and add a Curves node"
                                            (+ left 14) (+ top 42))
                         (cl-fltk:draw-font :size 12))
                       (progn
                         ;; Draw the two inactive curves, then the active
                         ;; channel on top with its control points.
                         (dolist (key (append (remove curve-channel
                                                      orfeus:*curve-channel-keys*)
                                              (list curve-channel)))
                           (let ((points (curve-node-points node key)))
                             (multiple-value-bind (red green blue)
                                 (curve-channel-color
                                  key (eq key curve-channel))
                               (cl-fltk:draw-color-rgb :red red :green green
                                                       :blue blue))
                             (loop with steps = 48
                                   for step from 0 below steps
                                   for x0 = (/ step (float steps))
                                   for x1 = (/ (1+ step) (float steps))
                                   for y0 = (curve-spline-value points x0)
                                   for y1 = (curve-spline-value points x1)
                                   do (cl-fltk:draw-line
                                       (+ left (round (* x0 (1- chart-w))))
                                       (+ top (round (* (- 1 y0)
                                                        (1- chart-h))))
                                       (+ left (round (* x1 (1- chart-w))))
                                       (+ top (round (* (- 1 y1)
                                                        (1- chart-h))))))))
                         (let ((points (curve-node-points node
                                                          curve-channel)))
                           (dotimes (index 4)
                             (let ((point-x
                                     (+ left
                                        (round (* (nth (* index 2) points)
                                                  (1- chart-w)))))
                                   (point-y
                                     (+ top
                                        (round (* (- 1 (nth (1+ (* index 2))
                                                            points))
                                                  (1- chart-h))))))
                               (cl-fltk:draw-color-rgb :red 20 :green 20
                                                       :blue 24)
                               (cl-fltk:draw-filled-circle point-x point-y 5)
                               (multiple-value-bind (red green blue)
                                   (curve-channel-color curve-channel t)
                                 (cl-fltk:draw-color-rgb :red red
                                                         :green green
                                                         :blue blue))
                               (cl-fltk:draw-filled-circle point-x point-y
                                                           4))))))))))
           (update-curve-drag (node x y)
             (multiple-value-bind (chart-x chart-y chart-w chart-h)
                 (curve-chart-geometry)
               (let* ((points (copy-list (curve-node-points node
                                                            curve-channel)))
                      (index curve-drag)
                      (raw-x (/ (- x chart-x) (float (1- chart-w))))
                      (raw-y (- 1 (/ (- y chart-y) (float (1- chart-h)))))
                      (new-y (max 0.0 (min 1.0 raw-y)))
                      (new-x (cond ((= index 0) 0.0)
                                   ((= index 3) 1.0)
                                   (t (max (+ (nth (* 2 (1- index)) points)
                                              0.02)
                                          (min (- (nth (* 2 (1+ index))
                                                       points)
                                                  0.02)
                                               raw-x))))))
                 (setf (nth (* index 2) points) (float new-x 1.0)
                       (nth (1+ (* index 2)) points) (float new-y 1.0))
                 (handler-case
                     (progn
                       (gui-model-set-node-params
                        model node (list curve-channel points))
                       (cl-fltk:redraw curve-canvas)
                       (schedule-edited-preview)
                       (set-status
                        (format nil "~A point ~D: ~,2F ~,2F"
                                (curve-channel-label curve-channel)
                                (1+ index) new-x new-y)))
                   (error (condition)
                     (set-status (princ-to-string condition)))))))
           (handle-curve-mouse (widget event value)
             (declare (ignore widget))
             (multiple-value-bind (x y) (parse-preview-event value)
               (when x
                 (let ((node (curves-editing-node)))
                   (when node
                     (case event
                       (#.cl-fltk:+event-push+
                        (multiple-value-bind (chart-x chart-y chart-w
                                              chart-h)
                            (curve-chart-geometry)
                          (let ((points (curve-node-points node
                                                           curve-channel)))
                            (setf curve-drag nil)
                            (dotimes (index 4)
                              (let ((point-x
                                      (+ chart-x
                                         (round (* (nth (* index 2) points)
                                                   (1- chart-w)))))
                                    (point-y
                                      (+ chart-y
                                         (round (* (- 1
                                                      (nth (1+ (* index 2))
                                                           points))
                                                   (1- chart-h))))))
                                (when (and (<= (abs (- x point-x)) 8)
                                           (<= (abs (- y point-y)) 8))
                                  (setf curve-drag index)))))))
                       (#.cl-fltk:+event-drag+
                        (when curve-drag
                          (update-curve-drag node x y)))
                       (#.cl-fltk:+event-release+
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
                   (#.cl-fltk:+event-push+
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
                   (#.cl-fltk:+event-drag+
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
                               (redraw-previews))))))))
                   (#.cl-fltk:+event-release+
                    (setf crop-drag nil
                          preview-drag-p nil))
                   (#.cl-fltk:+event-wheel+
                    (zoom-preview (if (minusp dy) 1.25d0 .8d0)
                                  canvas x y))))))
           (set-status (text)
             (setf (cl-fltk:value status) text))
           (selected-job ()
             (gui-model-selected-job model))
           (photo-content-key-for (job)
             ;; Memoized per job object; the key survives renames and moves
             ;; because it fingerprints file content, not the path.
             (or (gethash job content-keys)
                 (setf (gethash job content-keys)
                       (handler-case
                           (photo-content-key (photo-job-input-path job))
                         (error ()
                           (format nil "path~16,'0X"
                                   (ldb (byte 64 0)
                                        (sxhash
                                         (namestring
                                          (photo-job-input-path
                                           job))))))))))
           (thumbnail-row-height () 104)
           (thumbnail-scroll-limit ()
             (if thumbnail-canvas
                 (max 0 (- (* (length (project-photos project))
                              (thumbnail-row-height))
                           (cl-fltk:widget-height thumbnail-canvas)))
                 0))
           (clamp-thumbnail-scroll ()
             (setf thumbnail-scroll
                   (min (thumbnail-scroll-limit) (max 0 thumbnail-scroll))))
           (redraw-thumbnails ()
             (when thumbnail-canvas
               (clamp-thumbnail-scroll)
               (when thumbnail-scrollbar
                 (cl-fltk:set-range thumbnail-scrollbar
                                    0 (thumbnail-scroll-limit))
                 (setf (cl-fltk:value thumbnail-scrollbar)
                       (format nil "~D" thumbnail-scroll)))
               (cl-fltk:redraw thumbnail-canvas)))
           (select-thumbnail-row (row state)
             (when (and (>= row 0) (< row (length (project-photos project))))
               (multiple-value-bind (selection anchor)
                   (thumbnail-selection-after-click
                    (gui-model-selected-indices model) row thumbnail-anchor state)
                 (setf thumbnail-anchor anchor)
                 (gui-model-set-selected-indices model selection)
                 (when (member row selection)
                   (setf (gui-model-selected-index model) row))
                 (setf (gui-model-selected-node model) nil
                       pick-color-node nil)
                 (clear-previews)
                 (sync-controls)
                 (sync-node-tools)
                 (when graph-canvas (cl-fltk:redraw graph-canvas))
                 (if selection
                     (schedule-initial-preview)
                     (progn
                       (incf preview-generation)
                       (when debounce-id
                         (ignore-errors (cl-fltk:remove-timeout debounce-id))
                         (setf debounce-id nil))
                       (discard-gui-tasks queue :before)
                       (discard-gui-tasks queue :after)
                       (discard-gui-tasks background-queue :before)
                       (discard-gui-tasks background-queue :after)
                       (discard-gui-tasks histogram-queue :histogram)
                       (setf after-preview-generation nil
                             curve-histogram nil)))
                 (redraw-thumbnails))))
           (handle-thumbnail-mouse (canvas event value)
             (declare (ignore canvas))
             (multiple-value-bind (x y button dx dy state)
                 (parse-preview-event value)
               (declare (ignore x button dx))
               (when y
                 (case event
                   (#.cl-fltk:+event-push+
                    (select-thumbnail-row
                     (thumbnail-row-at y thumbnail-scroll
                                       (thumbnail-row-height))
                     state))
                   (#.cl-fltk:+event-wheel+
                    (incf thumbnail-scroll (* dy 36))
                    (redraw-thumbnails))))))
           (clear-previews ()
             (setf preview-zoom 1d0
                   preview-center-x .5d0
                   preview-center-y .5d0
                   preview-drag-p nil)
             (dolist (path (list before-preview-file after-preview-file))
               (when path (forget-preview-file path)))
             (setf before-preview-file nil
                   after-preview-file nil)
             (when before-canvas (cl-fltk:redraw before-canvas))
             (when after-canvas (cl-fltk:redraw after-canvas)))
           (publish-preview (role path generation)
             (let ((old-path (ecase role
                               (:before before-preview-file)
                               (:after after-preview-file))))
               (when (and old-path (not (equal old-path path)))
                 (forget-preview-file old-path))
               (forget-preview-file path)
               (ecase role
                 (:before (setf before-preview-file path)
                          (cl-fltk:redraw before-canvas))
                 (:after (setf after-preview-file path
                               after-preview-generation generation
                               (gethash (selected-job) thumbnail-files) path)
                         (schedule-curve-histogram path generation)
                         (cl-fltk:redraw after-canvas)
                         (when curve-canvas
                           (cl-fltk:redraw curve-canvas))
                         (redraw-thumbnails)))))
           (sync-export-controls ()
             (when export-quality
               (let ((settings (project-export-settings project)))
                 (setf (cl-fltk:value export-quality)
                       (format nil "~D" (export-settings-jpeg-quality settings))
                       (cl-fltk:value export-max-width)
                       (format nil "~D" (or (export-settings-max-width settings) 0))
                       (cl-fltk:value export-max-height)
                       (format nil "~D" (or (export-settings-max-height settings) 0))
                       (cl-fltk:value export-metadata)
                       (if (export-settings-preserve-metadata-p settings) "1" "0")
                       (cl-fltk:value export-timestamp)
                       (if (export-settings-timestamp-filenames-p settings)
                           "1" "0")))))
           (export-setting-changed (key widget)
             (handler-case
                 (let ((settings (project-export-settings project)))
                   (ecase key
                     (:jpeg-quality
                      (let ((value (round (parse-number (cl-fltk:value widget)))))
                        (unless (<= 1 value 100)
                          (error "JPEG quality must be from 1 to 100."))
                        (setf (export-settings-jpeg-quality settings) value)))
                     (:max-width
                      (let ((value (round (parse-number (cl-fltk:value widget)))))
                        (when (minusp value) (error "Maximum width cannot be negative."))
                        (setf (export-settings-max-width settings)
                              (unless (zerop value) value))))
                     (:max-height
                      (let ((value (round (parse-number (cl-fltk:value widget)))))
                        (when (minusp value) (error "Maximum height cannot be negative."))
                        (setf (export-settings-max-height settings)
                              (unless (zerop value) value))))
                     (:preserve-metadata-p
                      (setf (export-settings-preserve-metadata-p settings)
                            (gui-boolean-value (cl-fltk:value widget))))
                     (:timestamp-filenames-p
                      (setf (export-settings-timestamp-filenames-p settings)
                            (gui-boolean-value (cl-fltk:value widget)))))
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
                     (when lut-choice (cl-fltk:add-item lut-choice name))
                     name))))
           (selected-photo-count ()
             (length (or (gui-model-selected-indices model)
                         (and (selected-job) '(0)))))
           (editor-nodes ()
             (let ((graph (gui-model-display-graph model)))
               (if graph (orfeus:processing-graph-nodes graph) '())))
           (crop-editing-node ()
             (let ((node (gui-model-selected-graph-node model)))
               (and node (eq :crop (orfeus:graph-node-kind node)) node)))
           (sync-node-tools ()
             ;; The Node panel follows the selection: the correction picker
             ;; plus only the active kind's controls, DaVinci style.
             (let* ((node (gui-model-selected-graph-node model))
                    (kind (and node (orfeus:graph-node-kind node))))
               (when kind-choice
                 (setf (cl-fltk:value kind-choice)
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
                         (cl-fltk:show widget)
                         (cl-fltk:hide widget)))))
               (when (and node blend-opacity-input
                          (orfeus:graph-node-blend-p node))
                 (setf (cl-fltk:value blend-opacity-input)
                       (format nil "~,2F"
                               (orfeus:graph-node-opacity node))))
               (when (and node crop-angle-input (eq kind :crop))
                 (setf (cl-fltk:value crop-angle-input)
                       (format nil "~,1F"
                               (getf (orfeus:graph-node-params node)
                                     :angle 0.0))))))
           (ensure-node-positions (nodes)
             ;; Fresh nodes stack top-down in chain order; dragged nodes
             ;; keep wherever the user put them.
             (loop for node in nodes
                   for index from 1
                   unless (orfeus:graph-node-position node)
                     do (setf (orfeus:graph-node-position node)
                              (list 18.0
                                    (float (+ 6 (* index *graph-row-pitch*))
                                           1.0))))
             nodes)
           (graph-node-box (node)
             (let ((place (orfeus:graph-node-position node)))
               (values (round (first place)) (round (second place))
                       *graph-node-width* *graph-node-height*)))
           (graph-output-box (nodes)
             (let ((bottom (loop for node in nodes
                                 maximize
                                 (multiple-value-bind (x y) (graph-node-box
                                                             node)
                                   (declare (ignore x))
                                   (+ y *graph-node-height*)))))
               (values 18 (+ (max bottom 28) 22) *graph-node-width* 22)))
           (graph-editor-hit (gx gy)
             ;; What lies at graph coordinates: a node body, its output
             ;; port, a blend's branch port, the source, or the output box.
             (let ((nodes (ensure-node-positions (editor-nodes))))
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
                          (<= 6 gy 28))
                 (return-from graph-editor-hit
                   (values (if (>= gy 22) :source-port :source) nil)))
               (multiple-value-bind (x y w h) (graph-output-box nodes)
                 (when (and (<= x gx (+ x w)) (<= y gy (+ y h)))
                   (return-from graph-editor-hit (values :output nil))))
               (values nil nil)))
           (draw-editor-box (x y w h label style selected)
             ;; STYLE :terminal draws the darker RAW/OUT wells.
             (ecase style
               (:terminal (cl-fltk:draw-color-rgb :red 78 :green 80 :blue 86))
               (:bypassed (cl-fltk:draw-color-rgb :red 148 :green 148
                                                  :blue 148))
               (:normal (cl-fltk:draw-color-rgb :red 208 :green 208
                                                :blue 208)))
             (cl-fltk:draw-filled-rect x y w h)
             (cl-fltk:draw-color-rgb :red 240 :green 240 :blue 244)
             (cl-fltk:draw-filled-rect x y w 1)
             (cl-fltk:draw-filled-rect x y 1 h)
             (cl-fltk:draw-color-rgb :red 70 :green 70 :blue 74)
             (cl-fltk:draw-filled-rect x (+ y h -1) w 1)
             (cl-fltk:draw-filled-rect (+ x w -1) y 1 h)
             (when selected
               (cl-fltk:draw-color-rgb :red 0 :green 0 :blue 128)
               (cl-fltk:draw-rect x y w h)
               (cl-fltk:draw-rect (1+ x) (1+ y) (- w 2) (- h 2)))
             (if (eq style :terminal)
                 (cl-fltk:draw-color-rgb :red 225 :green 225 :blue 230)
                 (cl-fltk:draw-color-rgb :red 10 :green 10 :blue 12))
             (cl-fltk:draw-font :size 11)
             (cl-fltk:draw-text label (+ x 6) (+ y (- h 9)))
             (cl-fltk:draw-font :size 12))
           (draw-graph-editor (widget)
             (let* ((wx (cl-fltk:widget-x widget))
                    (wy (cl-fltk:widget-y widget))
                    (width (cl-fltk:widget-width widget))
                    (height (cl-fltk:widget-height widget))
                    (nodes (ensure-node-positions (editor-nodes)))
                    (selected (gui-model-selected-graph-node model))
                    (ox (- wx graph-scroll-x))
                    (oy (- wy graph-scroll-y)))
               (cl-fltk:draw-push-clip wx wy width height)
               (cl-fltk:draw-color-rgb :red 52 :green 54 :blue 58)
               (cl-fltk:draw-filled-rect wx wy width height)
               ;; A quiet dot grid anchored to graph space.
               (cl-fltk:draw-color-rgb :red 66 :green 68 :blue 72)
               (loop for gy from (* 24 (ceiling graph-scroll-y 24))
                       below (+ graph-scroll-y height) by 24
                     do (loop for gx from (* 24 (ceiling graph-scroll-x 24))
                                below (+ graph-scroll-x width) by 24
                              do (cl-fltk:draw-filled-rect (+ ox gx)
                                                           (+ oy gy) 1 1)))
               (when (null nodes)
                 (cl-fltk:draw-color-rgb :red 168 :green 170 :blue 174)
                 (cl-fltk:draw-font :size 11)
                 (cl-fltk:draw-text
                  (if (selected-job)
                      "Right-click for a New Node"
                      "Open a photograph to grade")
                  (+ wx 14) (+ wy 52))
                 (cl-fltk:draw-font :size 12))
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
                                (+ oy 28))))
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
                           (cl-fltk:draw-color-rgb :red 132 :green 134
                                                   :blue 140)
                           (cl-fltk:draw-line fx fy tx ty))))
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
                             (cl-fltk:draw-color-rgb :red 90 :green 110
                                                     :blue 190)
                             (cl-fltk:draw-line fx fy (+ tx 8) ty)
                             (cl-fltk:draw-line (+ tx 8) ty tx ty)))))))
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
                         (cl-fltk:draw-color-rgb :red 132 :green 134
                                                 :blue 140)
                         (cl-fltk:draw-line fx fy
                                            (+ ox out-x (floor out-w 2))
                                            (+ oy out-y)))))
                   ;; Terminal wells.
                   (when (selected-job)
                     (draw-editor-box (+ ox 18) (+ oy 6)
                                      *graph-node-width* 22
                                      "RAW" :terminal nil)
                     (draw-editor-box (+ ox out-x) (+ oy out-y)
                                      out-w 22 "OUT" :terminal nil)))
                 ;; Node boxes.
                 (dolist (node nodes)
                   (multiple-value-bind (x y w h) (graph-node-box node)
                     (let ((bx (+ ox x)) (by (+ oy y))
                           (bypassed (orfeus:graph-node-bypassed-p node)))
                       (draw-editor-box bx by w h
                                        (node-kind-label
                                         (orfeus:graph-node-kind node))
                                        (if bypassed :bypassed :normal)
                                        (eq node selected))
                       (if (graph-node-active-p node)
                           (cl-fltk:draw-color-rgb :red 40 :green 150
                                                   :blue 40)
                           (cl-fltk:draw-color-rgb :red 150 :green 150
                                                   :blue 150))
                       (cl-fltk:draw-filled-rect (+ bx w -11) (+ by 4) 7 7)
                       ;; The output port nub.
                       (cl-fltk:draw-color-rgb :red 235 :green 235 :blue 240)
                       (cl-fltk:draw-filled-rect (+ bx (floor w 2) -3)
                                                 (+ by h -3) 7 6)
                       (when (orfeus:graph-node-blend-p node)
                         (cl-fltk:draw-color-rgb :red 90 :green 110
                                                 :blue 190)
                         (cl-fltk:draw-filled-rect (+ bx w -3)
                                                   (+ by (floor h 2) -3)
                                                   6 7))
                       (when bypassed
                         (cl-fltk:draw-color-rgb :red 165 :green 40 :blue 40)
                         (cl-fltk:draw-line bx (+ by h -1)
                                            (+ bx w -1) by)))))
                 ;; Source port nub.
                 (when (selected-job)
                   (cl-fltk:draw-color-rgb :red 235 :green 235 :blue 240)
                   (multiple-value-bind (sx sy) (source-bottom)
                     (cl-fltk:draw-filled-rect (- sx 3) (- sy 3) 7 6)))
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
                         (cl-fltk:draw-color-rgb :red 0 :green 0 :blue 128)
                         (cl-fltk:draw-line
                          fx fy
                          (+ ox (getf (rest node-drag) :x))
                          (+ oy (getf (rest node-drag) :y))))))))
               (cl-fltk:draw-pop-clip)))
           (after-graph-edit (message)
             (sync-controls)
             (sync-node-tools)
             (when graph-canvas (cl-fltk:redraw graph-canvas))
             (when curve-canvas (cl-fltk:redraw curve-canvas))
             (redraw-previews)
             (schedule-edited-preview)
             (when message (set-status message)))
           (select-graph-node (index)
             ;; Selecting a node upgrades flat photos to graph grading so the
             ;; selection can drive the Node panel and later edits. Entering
             ;; or leaving crop editing changes the preview recipe.
             (when (selected-job)
               (let* ((graph (gui-model-ensure-graph model))
                      (node (nth index
                                 (orfeus:processing-graph-nodes graph)))
                      (was-cropping (crop-editing-node)))
                 (when node
                   (setf (gui-model-selected-node model) node)
                   (unless (eq was-cropping (crop-editing-node))
                     (schedule-edited-preview))
                   (sync-controls)
                   (sync-node-tools)
                   (when graph-canvas (cl-fltk:redraw graph-canvas))
                   (when curve-canvas (cl-fltk:redraw curve-canvas))
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
                 (when graph-canvas (cl-fltk:redraw graph-canvas))
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
                      (cons "Move Earlier"
                            (lambda ()
                              (if (gui-model-move-node model node :earlier)
                                  (after-graph-edit "Node moved earlier")
                                  (set-status "This node cannot move earlier"))))
                      (cons "Move Later"
                            (lambda ()
                              (if (gui-model-move-node model node :later)
                                  (after-graph-edit "Node moved later")
                                  (set-status "This node cannot move later")))))
                (when (eq kind :crop)
                  (list (cons "-" nil)
                        (cons "Autocrop Negative"
                              (lambda () (autocrop-negative node)))
                        (cons "Reset Crop"
                              (lambda ()
                                (gui-model-set-node-params
                                 model node '(:left 0.0 :top 0.0
                                              :width 1.0 :height 1.0
                                              :angle 0.0))
                                (after-graph-edit "Crop reset to full frame")))))
                (when (eq kind :color-subtract)
                  (list (cons "-" nil)
                        (cons "Pick Base Color..."
                              (lambda () (pick-base-color node)))
                        (cons "Sample Base From Photo"
                              (lambda ()
                                (setf pick-color-node node)
                                (set-status
                                 "Click the preview to sample the film base")))
                        (cons "Auto Base From Border"
                              (lambda () (auto-base-from-border node)))))
                (list (cons "-" nil)
                      (cons "Delete Node"
                            (lambda ()
                              (gui-model-delete-node model node)
                              (after-graph-edit "Node deleted")))))))
           (add-node-menu-actions (after-node)
             ;; The Resolve flow first: New Node makes an untyped container
             ;; the Node panel then assigns a correction to. The direct
             ;; per-kind entries remain as shortcuts.
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
                 (cons (format nil "Add ~A" (node-kind-label kind))
                       (lambda ()
                         (handler-case
                             (progn
                               (gui-model-add-node
                                model kind
                                :after (and after-node
                                            (orfeus:graph-node-id
                                             after-node)))
                               (after-graph-edit
                                (format nil "Added ~A node"
                                        (node-kind-label kind))))
                           (error (condition)
                             (set-status (princ-to-string condition)))))))
               (orfeus:graph-node-kinds))))
           (show-node-menu (actions)
             (let ((chosen (cl-fltk:popup-menu (mapcar #'first actions))))
               (when chosen
                 (let ((action (rest (nth chosen actions))))
                   (when action (funcall action))))))
           (graph-content-extent ()
             ;; (values right bottom) of everything drawn, in graph space.
             (let ((nodes (ensure-node-positions (editor-nodes))))
               (multiple-value-bind (out-x out-y out-w out-h)
                   (graph-output-box nodes)
                 (values (max (+ out-x out-w)
                              (loop for node in nodes
                                    maximize
                                    (multiple-value-bind (x y w)
                                        (graph-node-box node)
                                      (declare (ignore y))
                                      (+ x w))))
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
                                (cl-fltk:redraw graph-canvas)
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
                                    (cl-fltk:redraw graph-canvas)))))
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
                                    (cl-fltk:redraw graph-canvas))
                                (cl-fltk:redraw graph-canvas))))
                         (:output
                          (if source-node
                              (if (gui-model-set-output model source-node)
                                  (after-graph-edit
                                   (format nil "Output is now ~A"
                                           (node-kind-label
                                            (orfeus:graph-node-kind
                                             source-node))))
                                  (cl-fltk:redraw graph-canvas))
                              (cl-fltk:redraw graph-canvas)))
                         (t (cl-fltk:redraw graph-canvas))))
                   (error (condition)
                     (cl-fltk:redraw graph-canvas)
                     (set-status (princ-to-string condition)))))))
           (handle-graph-mouse (widget event value)
             (declare (ignore widget))
             (multiple-value-bind (x y button dx dy) (parse-preview-event
                                                      value)
               (declare (ignore dx))
               (when (and x (selected-job))
                 (let ((gx (+ x graph-scroll-x))
                       (gy (+ y graph-scroll-y)))
                   (case event
                     (#.cl-fltk:+event-push+
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
                     (#.cl-fltk:+event-drag+
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
                               (cl-fltk:redraw graph-canvas)))
                            (:wire
                             (setf (getf plist :x) gx
                                   (getf plist :y) gy
                                   (getf plist :moved-p) t)
                             (cl-fltk:redraw graph-canvas))
                            (:pan
                             (setf (getf plist :moved-p) t
                                   graph-scroll-x
                                   (max 0 (- (getf plist :sx)
                                             (- x (getf plist :x))))
                                   graph-scroll-y
                                   (max 0 (- (getf plist :sy)
                                             (- y (getf plist :y)))))
                             (cl-fltk:redraw graph-canvas))))))
                     (#.cl-fltk:+event-release+
                      (let ((drag node-drag))
                        (setf node-drag nil)
                        (when drag
                          (case (first drag)
                            (:wire
                             (if (getf (rest drag) :moved-p)
                                 (drop-graph-wire drag gx gy)
                                 (cl-fltk:redraw graph-canvas)))
                            (:move (cl-fltk:redraw graph-canvas))
                            (:pan
                             (unless (getf (rest drag) :moved-p)
                               (deselect-graph-node)))))))
                     (#.cl-fltk:+event-wheel+
                      (multiple-value-bind (right bottom)
                          (graph-content-extent)
                        (declare (ignore right))
                        (setf graph-scroll-y
                              (max 0 (min (max 0 (- bottom 60))
                                          (+ graph-scroll-y (* dy 32))))))
                      (cl-fltk:redraw graph-canvas)))))))
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
           (pick-base-color (node)
             (let ((params (orfeus:graph-node-params node)))
               (multiple-value-bind (red green blue)
                   (cl-fltk:choose-color
                    :title "Film base color"
                    :red (srgb-encode-component (getf params :red 1.0))
                    :green (srgb-encode-component (getf params :green 1.0))
                    :blue (srgb-encode-component (getf params :blue 1.0)))
                 (when red
                   (gui-model-set-node-params
                    model node
                    (list :red (srgb-decode-component red)
                          :green (srgb-decode-component green)
                          :blue (srgb-decode-component blue)))
                   (after-graph-edit "Film base color set")))))
           (sample-base-at (canvas x y)
             ;; The eyedropper: map the click through the current fit and pan
             ;; to normalized image coordinates, then sample linear color.
             (let ((node pick-color-node)
                   (job (selected-job))
                   (path (preview-path-for-canvas canvas)))
               (setf pick-color-node nil)
               (when (and node job path)
                 (multiple-value-bind (scaled-width scaled-height)
                     (preview-scaled-size canvas path preview-zoom)
                   (when scaled-width
                     (let* ((normalized-x
                              (+ preview-center-x
                                 (/ (- x (/ (cl-fltk:widget-width canvas) 2d0))
                                    scaled-width)))
                            (normalized-y
                              (+ preview-center-y
                                 (/ (- y (/ (cl-fltk:widget-height canvas)
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
             (incf gallery-generation)
             (discard-gui-tasks background-queue :still)
             (clrhash gallery-thumbs)
             (setf gallery-stills
                   (append (mapcar #'make-project-gallery-still
                                   (project-presets project))
                           (handler-case
                               (mapcar #'make-local-gallery-still
                                       (orfeus:still-store-list))
                             (error (condition)
                               (set-status (princ-to-string condition))
                               '()))))
             (when (and gallery-selected
                        (>= gallery-selected (length gallery-stills)))
               (setf gallery-selected nil)))
           (persist-still (preset)
             ;; The local gallery copy survives removable source media and
             ;; unsaved projects, but never replaces an unrelated exact name.
             (handler-case
                 (values (orfeus:still-store-write preset :if-exists :error) nil)
               (error (condition)
                 (values nil condition))))
           (grab-still ()
             (let ((preset (gui-model-grab-still model)))
               (if preset
                   (multiple-value-bind (stored condition)
                       (persist-still preset)
                     (refresh-gallery)
                     (setf (cl-fltk:value preset-name-input)
                           (processing-preset-name preset))
                     (sync-preset-action-label)
                     (set-status
                      (if stored
                          (format nil "Grabbed ~A"
                                  (processing-preset-name preset))
                          (format nil "Grabbed ~A in project only: ~A"
                                  (processing-preset-name preset) condition))))
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
               (merge-pathnames
                (make-pathname
                 :name (format nil "still-~(~A~)-~A-~A-~A"
                               (gallery-still-origin still)
                               (gallery-still-identity still)
                               source-identity
                               (preview-settings-key (still-recipe preset)))
                 :type "jpg")
                preview-directory)))
           (request-still-thumbnail (still)
             (let* ((preset (gallery-still-preset still))
                    (key (gallery-still-key still))
                    (generation gallery-generation)
                    (name (processing-preset-name preset))
                    (source (processing-preset-source-photo preset))
                    (stored (when (eq :local (gallery-still-origin still))
                              (ignore-errors
                                (orfeus:still-store-thumbnail-pathname name)))))
               (when (null (gethash key gallery-thumbs))
                 (cond
                   ((and source (probe-file source))
                    (let* ((output (still-thumbnail-pathname still))
                           (recipe (still-recipe preset))
                           (graph-p (typep recipe
                                           'orfeus:processing-graph)))
                      (if (probe-file output)
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
                               (error () nil)))))))
                   ;; Source gone (card ejected, file moved): fall back to
                   ;; the persisted local copy of the thumbnail.
                   ((and stored (probe-file stored))
                    (setf (gethash key gallery-thumbs) stored))))))
           (refresh-gallery ()
             (reload-gallery-stills)
             (when gallery-canvas
               (dolist (still gallery-stills)
                 (request-still-thumbnail still))
               (cl-fltk:redraw gallery-canvas)))
           (gallery-columns ()
             (max 1 (floor (- (cl-fltk:widget-width gallery-canvas) 8)
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
                         (- (cl-fltk:widget-height gallery-canvas) 8)))))
           (draw-gallery (widget)
             (let ((x (cl-fltk:widget-x widget))
                   (y (cl-fltk:widget-y widget))
                   (width (cl-fltk:widget-width widget))
                   (height (cl-fltk:widget-height widget))
                   (columns (gallery-columns)))
               (cl-fltk:draw-color-rgb :red 255 :green 255 :blue 255)
               (cl-fltk:draw-filled-rect x y width height)
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
                              (cl-fltk:draw-color-rgb :red 0 :green 0 :blue 128)
                              (cl-fltk:draw-filled-rect
                               cell-x cell-y (- *gallery-cell-width* 6)
                               (- *gallery-cell-height* 6)))
                            (let ((thumb (gethash (gallery-still-key still) gallery-thumbs)))
                              (if thumb
                                  (draw-thumbnail-file
                                   widget thumb (+ cell-x 3) (+ cell-y 3)
                                   (- *gallery-cell-width* 12) 62)
                                  (progn
                                    (cl-fltk:draw-color-rgb
                                     :red 205 :green 205 :blue 205)
                                    (cl-fltk:draw-filled-rect
                                     (+ cell-x 3) (+ cell-y 3)
                                     (- *gallery-cell-width* 12) 62))))
                            (if selected
                                (cl-fltk:draw-color-rgb :red 255 :green 255
                                                        :blue 255)
                                (cl-fltk:draw-color-rgb :red 0 :green 0
                                                        :blue 0))
                            (cl-fltk:draw-font :size 10)
                            (cl-fltk:draw-text
                             (if (> (length name) 14)
                                 (subseq name 0 14)
                                 name)
                             (+ cell-x 3) (+ cell-y 80))
                            (cl-fltk:draw-font :size 12)))))
           (gallery-select (index)
             (setf gallery-selected index)
             (let* ((still (nth index gallery-stills))
                    (preset (and still (gallery-still-preset still))))
               (when preset
                 (setf (cl-fltk:value preset-name-input)
                       (processing-preset-name preset))))
             (cl-fltk:redraw gallery-canvas))
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
           (apply-still (preset &key bypass-kinds description)
             (handler-case
                 (let ((count (gui-model-apply-preset-graph
                               model preset :bypass-kinds bypass-kinds)))
                   (after-graph-edit
                    (format nil "Applied ~A~@[ ~A~] to ~D photo~:P~@[~A~]"
                            (processing-preset-name preset)
                            description count (still-lens-note preset)))
                   (redraw-thumbnails))
               (error (condition)
                 (set-status (princ-to-string condition)))))
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
                    (answer (cl-fltk:input-dialog "Still name"
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
                                 (setf (cl-fltk:value preset-name-input) new-name)
                                 (refresh-gallery)
                                 (set-status (format nil "Renamed ~A to ~A"
                                                     old-name new-name)))
                             (error (condition)
                               (set-status (princ-to-string condition)))))))))))
           (gallery-context-menu (still)
             (let* ((preset (gallery-still-preset still))
                    (actions
                     (list (cons (format nil "Apply Graph to ~D Photo~:P"
                                         (selected-photo-count))
                                 (lambda () (apply-still preset)))
                           (cons "Apply Without Optics"
                                 (lambda ()
                                   (apply-still preset
                                                :bypass-kinds '(:optics)
                                                :description
                                                "without optics")))
                           (cons "Apply Without White Balance"
                                 (lambda ()
                                   (apply-still preset
                                                :bypass-kinds
                                                '(:white-balance)
                                                :description
                                                "without white balance")))
                           (cons "-" nil)
                           (cons "Rename Still..."
                                 (lambda () (rename-still still)))
                           (cons "Delete Still"
                                 (lambda () (delete-still still))))))
               (let ((chosen (cl-fltk:popup-menu (mapcar #'first actions))))
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
                   (#.cl-fltk:+event-push+
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
                               (apply-still (gallery-still-preset still)))))
                          (t (setf gallery-click (cons now index)))))))
                   (#.cl-fltk:+event-wheel+
                    (setf gallery-scroll
                          (min (gallery-scroll-limit)
                               (max 0 (+ gallery-scroll (* dy 32)))))
                    (cl-fltk:redraw gallery-canvas))))))
           (sync-preset-action-label ()
             (when preset-apply-button
               (setf (cl-fltk:label preset-apply-button)
                     (format nil "Apply to ~D photo~:P"
                             (selected-photo-count)))))
           (save-current-preset ()
             (handler-case
                 (let ((preset (gui-model-save-preset
                                model (cl-fltk:value preset-name-input))))
                   (remhash (processing-preset-name preset) gallery-thumbs)
                   (multiple-value-bind (stored condition)
                       (persist-still preset)
                     (refresh-gallery)
                     (setf (cl-fltk:value preset-name-input)
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
             (handler-case
                 (let* ((name (cl-fltk:value preset-name-input))
                        (count (gui-model-apply-preset model name)))
                   (sync-controls)
                   (schedule-edited-preview)
                   (set-status (format nil "Applied ~A to ~D photo~:P"
                                       name count)))
               (error (condition)
                 (set-status (princ-to-string condition)))))
           (sync-controls ()
             (dolist (entry controls)
               (let ((key (first entry)) (widget (second entry)))
                 (setf (cl-fltk:value widget)
                       (case key
                         ((:lens-correction-p :chromatic-aberration-correction-p)
                          (if (gui-model-setting model key) "1" "0"))
                         (otherwise (display-number (gui-model-setting model key)))))))
             (setf (cl-fltk:value wb-choice)
                   (if (gui-model-setting model :white-balance-temperature)
                       "Custom" "As shot")
                   (cl-fltk:value target-choice)
                   (if (eq (gui-model-edit-target model) :defaults)
                       "Defaults" "Photo"))
             (when lut-choice
               (let ((path (gui-model-setting model :lut-path)))
                 (setf (cl-fltk:value lut-choice)
                       (if path (lut-choice-name path) "None"))))
             (setf (cl-fltk:label lens-name)
                   (let ((capture (selected-capture-description)))
                     (if capture
                         (format nil "Lens: ~A   |   ~A"
                                 (selected-lens-description) capture)
                         (format nil "Lens: ~A" (selected-lens-description)))))
             (sync-export-controls)
             (sync-preset-action-label))
           (replace-project (new-project &optional path)
             (incf preview-generation)
             (clear-previews)
             (setf project new-project
                   thumbnail-scroll 0
                   thumbnail-anchor 0)
             (clrhash thumbnail-files)
             (clrhash gallery-thumbs)
             (setf gallery-selected nil
                   gallery-scroll 0
                   pick-color-node nil)
             (gui-model-replace-project model new-project path)
             (setf (gui-model-selected-node model) nil)
             (clrhash content-keys)
               (refresh-gallery)
             (sync-node-tools)
             (when graph-canvas (cl-fltk:redraw graph-canvas))
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
                         (schedule-initial-preview)
                         (set-status (format nil "Added ~D photograph~:P" count)))
                       (set-status "All selected photographs are already in the project"))))))
           (remove-selected-photo ()
             (let ((removed (gui-model-remove-selected model)))
               (when removed
                 (incf preview-generation)
                 (dolist (job removed)
                   (remhash job lens-cache)
                   (remhash job capture-cache)
                   (remhash job thumbnail-files))
                 (clear-previews)
                 (sync-controls)
                 (if (selected-job)
                     (progn
                       (schedule-initial-preview)
                       (set-status (format nil "Removed ~D photograph~:P"
                                           (length removed))))
                     (set-status "Project contains no photographs")))))
           (open-project ()
             (let ((path (cl-fltk:choose-file
                          :title "Open Orfeus project"
                          :filter (fltk-file-filter "Orfeus project" "*.sexp")
                          :preset-file (picker-preset))))
               (when path
                 (remember-picked-path path)
                 (replace-project (project-read path) (pathname path)))))
           (save-project (&optional choose-p)
             (let ((path (or (and (not choose-p) (gui-model-project-path model))
                             (cl-fltk:choose-save-file
                              :title "Save Orfeus project"
                              :filter (fltk-file-filter "Orfeus project" "*.sexp")
                              :preset-file
                              (namestring
                               (merge-pathnames "project.sexp"
                                                picker-directory))))))
               (when path
                 (remember-picked-path path)
                 (project-write project path)
                 (setf (gui-model-project-path model) (pathname path))
                 (set-status "Project saved"))))
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
             ;; While a crop node is selected its stage is bypassed in the
             ;; preview: the full frame stays visible under the editing
             ;; overlay, and rect changes cost a redraw, not a render.
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
                     (setf (orfeus:graph-node-bypassed-p twin) t))))
               settings))
           (enqueue-render (target-queue role job index settings generation publish-p
                            &key front-p draft-p cache-p)
             ;; Freeze once before deriving the cache path, then pass that same
             ;; immutable recipe to the worker.
             (let* ((settings (preview-recipe-snapshot settings))
                    (input (photo-job-input-path job))
                    (file-role (if draft-p :draft role))
                    (output (preview-pathname preview-directory
                                              (photo-content-key-for job)
                                              file-role settings))
                    (max-width (if draft-p
                                   *gui-draft-preview-size*
                                   *gui-preview-max-width*))
                    (max-height (if draft-p
                                    *gui-draft-preview-size*
                                    *gui-preview-max-height*)))
               (if (probe-file output)
                   (when publish-p
                     (queue-event queue
                                  (list :preview generation index job role output)))
                   (enqueue-gui-task
                    target-queue role
                    (lambda ()
                      (when (or (not publish-p)
                                (= generation preview-generation))
                        (if (typep settings 'orfeus:processing-graph)
                            (render-preview input output nil
                                            :graph settings
                                            :max-width max-width
                                            :max-height max-height
                                            :cache-p cache-p
                                            :if-exists :supersede)
                            (render-preview input output settings
                                            :max-width max-width
                                            :max-height max-height
                                            :cache-p cache-p
                                            :if-exists :supersede))
                        (when (and publish-p
                                   (= generation preview-generation))
                          (queue-event queue
                                       (list :preview generation index job role
                                             output)))))
                    :front-p front-p))))
           (enqueue-thumbnail (job index generation)
             (let ((output (thumbnail-pathname preview-directory
                                               (photo-content-key-for job))))
               (if (probe-file output)
                   (queue-event queue (list :thumbnail generation job output))
                   (enqueue-gui-task
                    background-queue :thumbnail
                    (lambda ()
                      (handler-case
                          (progn
                            (render-preview
                              (photo-job-input-path job) output
                              (neutral-preview-settings)
                              :max-width *thumbnail-preview-size*
                              :max-height *thumbnail-preview-size*
                              :jpeg-quality 82
                              :if-exists :supersede)
                            (queue-event queue
                                         (list :thumbnail generation job output)))
                        (error () nil)))))))
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
                   (enqueue-thumbnail (nth index photos) index generation)))
               (dolist (index indices)
                 (unless (= index selected-index)
                   (let ((job (nth index photos)))
                     (enqueue-render background-queue :after job index
                                     (settings-for-job job) nil nil))))))
           (enqueue-preview (initial-p)
             (let ((job (selected-job))
                   (index (gui-model-selected-index model))
                   (generation preview-generation))
               (when job
                 ;; A bounded draft lands quickly while the full-resolution
                 ;; preview renders behind it; both reuse the decoded RAW.
                 (let ((settings (current-settings)))
                   (unless (probe-file
                             (preview-pathname preview-directory
                                               (photo-content-key-for job)
                                               :after
                                               (preview-recipe-snapshot
                                                settings)))
                     (enqueue-render queue :after job index settings generation t
                                     :front-p t :draft-p t :cache-p t))
                   (enqueue-render queue :after job index settings generation t
                                   :cache-p t))
                 (enqueue-background-previews initial-p generation))))
           (schedule-initial-preview ()
             (when debounce-id
               (ignore-errors (cl-fltk:remove-timeout debounce-id))
               (setf debounce-id nil))
             (incf preview-generation)
             (discard-gui-tasks queue :before)
             (discard-gui-tasks queue :after)
             (enqueue-preview t))
           (schedule-edited-preview ()
             (incf preview-generation)
             (when debounce-id
               (ignore-errors (cl-fltk:remove-timeout debounce-id)))
             (setf debounce-id
                   (cl-fltk:add-timeout
                    *preview-debounce-seconds*
                    (lambda ()
                      (setf debounce-id nil)
                      (discard-gui-tasks queue :after)
                      (enqueue-preview nil)))))
           (setting-changed (key widget &optional allow-empty)
             (handler-case
                 (let ((new-value (parse-number (cl-fltk:value widget)
                                                allow-empty)))
                   (when (and (eq key :white-balance-tint)
                              (null (gui-model-setting
                                     model :white-balance-temperature)))
                     (gui-model-set-setting model
                                            :white-balance-temperature 5500.0))
                   (gui-model-set-setting model key new-value)
                   (sync-controls)
                   (when (member key '(:white-balance-temperature
                                       :white-balance-tint))
                     (setf (cl-fltk:value wb-choice) "Custom"))
                   (schedule-edited-preview))
               (error (condition) (set-status (princ-to-string condition)))))
           (render-selected ()
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
             (when (project-photos project)
               (ensure-directories-exist
                (merge-pathnames "placeholder" (project-output-directory project)))
               (enqueue-gui-task
                queue :export
                (lambda ()
                  (queue-event queue (list :status nil "Exporting all photos..."))
                  (multiple-value-bind (completed failures)
                      (project-render project :if-exists :supersede :on-error :continue)
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
           (choose-lut ()
             (let ((path (cl-fltk:choose-file
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
             (if (string-equal (cl-fltk:value widget) "As shot")
                 (progn
                   (gui-model-set-setting model :white-balance-temperature nil)
                   (gui-model-set-setting model :white-balance-tint 0.0))
                 (unless (gui-model-setting model :white-balance-temperature)
                   (gui-model-set-setting model :white-balance-temperature 5500.0)))
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
             widget)
           (register-field (field y &optional (basis :root))
             (let ((label-x (if (eq basis :page) 12 8))
                   (label-width (if (eq basis :page) 88 96)))
               (register-inspector (cl-fltk:field-label field)
                                   label-x y label-width 26 basis)
               (register-inspector (cl-fltk:field-control field) 110 y
                                   :control 26 basis))
             field)
           (build-group (kind builder)
             ;; Collects everything BUILDER registers into one Node panel
             ;; visibility group; SYNC-NODE-TOOLS shows exactly one of them.
             (let ((items-before (length inspector-items))
                   (tone-before (length tone-items)))
               (funcall builder)
               (push (cons kind
                           (append
                            (mapcar #'first
                                    (subseq inspector-items 0
                                            (- (length inspector-items)
                                               items-before)))
                            (mapcar #'first
                                    (subseq tone-items 0
                                            (- (length tone-items)
                                               tone-before)))))
                     node-panel-groups)))
           (section-frame (parent title y height)
             ;; A classic engraved group frame whose title interrupts the
             ;; frame line, drawn behind the controls it surrounds.
             (let ((frame (cl-fltk:make-box :parent parent :x 4 :y y
                                            :width 300 :height height
                                            :label "")))
               (cl-fltk:set-box frame cl-fltk:+box-engraved-frame+)
               (register-inspector frame 4 y :frame height :page))
             (let ((title-width (+ 14 (* 7 (length title))))
                   (title-label (cl-fltk:make-label
                                 :parent parent :x 14 :y (- y 8)
                                 :width 80 :height 16 :label title)))
               (cl-fltk:set-box title-label cl-fltk:+box-flat-box+)
               (cl-fltk:set-label-font title-label
                                       cl-fltk:+font-helvetica-bold+)
               (register-inspector title-label 14 (- y 8) title-width 16
                                   :page)))
           (make-number-field (key label minimum maximum step y parent)
             (let* ((label-widget
                      (cl-fltk:make-label :parent parent :x 12 :y y
                                          :width 88 :height 26 :label label))
                    (callback (lambda (widget event value)
                                (declare (ignore event value))
                                (setting-changed key widget)))
                    (spinner (cl-fltk:make-spinner
                              :parent parent :x 202 :y y
                              :width 78 :height 26 :callback callback))
                    (slider (cl-fltk:make-slider
                             :parent parent :x 110 :y y
                             :width 84 :height 26 :callback callback)))
               (dolist (widget (list slider spinner))
                 (cl-fltk:set-range widget minimum maximum)
                 (cl-fltk:set-step widget step)
                 (push (list key widget) controls))
               (register-inspector label-widget 12 y 88 26 :page)
               (register-inspector slider 110 y :slider 26 :page)
               (register-inspector spinner 202 y :number 26 :page)
               spinner))
           (make-tone-band (key short-label full-label index)
             (let* ((callback (lambda (widget event value)
                                (declare (ignore event value))
                                (setting-changed key widget)))
                    (label (cl-fltk:make-label
                            :parent node-page :x 0 :y 92 :width 32 :height 22
                            :label short-label))
                    (slider (cl-fltk:make-vertical-slider
                             :parent node-page :x 0 :y 116 :width 20 :height 120
                             :callback callback))
                    (input (cl-fltk:make-value-input
                            :parent node-page :x 0 :y 240 :width 32 :height 24
                            :callback callback)))
               (dolist (widget (list slider input))
                 (cl-fltk:set-range widget -2 2)
                 (cl-fltk:set-step widget 0.1)
                 (cl-fltk:set-tooltip widget full-label)
                 (push (list key widget) controls))
               (push (list label index :label) tone-items)
               (push (list slider index :slider) tone-items)
               (push (list input index :input) tone-items)))
           (layout-left-pane (&optional ignored)
             (declare (ignore ignored))
             (let ((width (cl-fltk:widget-width filmstrip-pane))
                   (height (cl-fltk:widget-height filmstrip-pane)))
               (cl-fltk:resize-widget
                thumbnail-canvas :x 0 :y 0
                :width (max 40 (- width 16)) :height height)
               (when thumbnail-scrollbar
                 (cl-fltk:resize-widget
                  thumbnail-scrollbar :x (max 40 (- width 16)) :y 0
                  :width 16 :height height)))
             (redraw-thumbnails)
             (cl-fltk:redraw filmstrip-pane))
           (layout-center-pane (&optional ignored)
             (declare (ignore ignored))
             (let* ((center (cl-fltk:widget-width center-pane))
                    (main-height (cl-fltk:widget-height center-pane))
                    (caption-height 22)
                    (viewer-height (max 100 (- main-height caption-height)))
                    (gutter 6)
                    (pane-width (floor (- center gutter) 2)))
               (if comparison-p
                   (progn
                     (cl-fltk:show before-caption)
                     (cl-fltk:show before-canvas)
                     (cl-fltk:resize-widget before-caption :x 0 :y 0
                                            :width pane-width :height caption-height)
                     (cl-fltk:resize-widget before-canvas :x 0 :y caption-height
                                            :width pane-width :height viewer-height)
                     (cl-fltk:resize-widget after-caption
                                            :x (+ pane-width gutter) :y 0
                                            :width (- center pane-width gutter)
                                            :height caption-height)
                     (cl-fltk:resize-widget after-canvas
                                            :x (+ pane-width gutter) :y caption-height
                                            :width (- center pane-width gutter)
                                            :height viewer-height))
                   (progn
                     (cl-fltk:hide before-caption)
                     (cl-fltk:hide before-canvas)
                     (cl-fltk:resize-widget after-caption :x 0 :y 0
                                            :width center :height caption-height)
                     (cl-fltk:resize-widget after-canvas :x 0 :y caption-height
                                            :width center :height viewer-height)))
               (cl-fltk:redraw center-pane)))
           (layout-graph-pane (&optional ignored)
             (declare (ignore ignored))
             (let* ((width (cl-fltk:widget-width graph-pane))
                    (height (cl-fltk:widget-height graph-pane))
                    (button-width (max 48 (floor (- width 32) 3))))
               (when graph-title
                 (cl-fltk:resize-widget graph-title :x 8 :y 4
                                        :width (- width 16) :height 18))
               (when still-button
                 (cl-fltk:resize-widget still-button
                                        :x 8 :y 24
                                        :width button-width :height 22)
                 (cl-fltk:resize-widget copy-grade-button
                                        :x (+ 12 button-width) :y 24
                                        :width button-width :height 22)
                 (cl-fltk:resize-widget paste-grade-button
                                        :x (+ 16 (* 2 button-width)) :y 24
                                        :width button-width :height 22))
               (when graph-canvas
                 (cl-fltk:resize-widget graph-canvas :x 2 :y 52
                                        :width (- width 4)
                                        :height (max 60 (- height 54))))
               (cl-fltk:redraw graph-pane)))
           (layout-gallery-pane (&optional ignored)
             (declare (ignore ignored))
             (let* ((width (cl-fltk:widget-width gallery-pane))
                    (height (cl-fltk:widget-height gallery-pane))
                    (half (floor (- width 24) 2)))
               (when gallery-title
                 (cl-fltk:resize-widget gallery-title :x 8 :y 2
                                        :width (- width 16) :height 18))
               (when gallery-canvas
                 (cl-fltk:resize-widget gallery-canvas :x 4 :y 22
                                        :width (- width 8)
                                        :height (max 60 (- height 88))))
               (when preset-name-input
                 (cl-fltk:resize-widget preset-name-input
                                        :x 8 :y (- height 60)
                                        :width (- width 16) :height 24))
               (when preset-apply-button
                 (cl-fltk:resize-widget preset-apply-button
                                        :x (+ 16 half) :y (- height 32)
                                        :width half :height 26))
               (when preset-save-button
                 (cl-fltk:resize-widget preset-save-button
                                        :x 8 :y (- height 32)
                                        :width half :height 26))
               (cl-fltk:redraw gallery-pane)))
           (layout-inspector-pane (&optional ignored)
             (declare (ignore ignored))
             (let ((right (cl-fltk:widget-width inspector))
                   (main-height (cl-fltk:widget-height inspector)))
               (cl-fltk:resize-widget tabs :x 4 :y 40
                                      :width (- right 8)
                                      :height (- main-height 80))
               (dolist (page (list node-page export-page))
                 (cl-fltk:resize-widget page :x 2 :y 24
                                        :width (- right 12)
                                        :height (- main-height 108)))
               (dolist (item inspector-items)
                 (destructuring-bind
                     (widget x y width-mode item-height basis) item
                   (let* ((basis-width (if (eq basis :page)
                                           (- right 12)
                                           right))
                          (item-width
                            (case width-mode
                              (:scope-control (max 100 (- right 98)))
                              (:control (max 100 (- basis-width 118)))
                              (:slider (max 64 (- basis-width 204)))
                              (:number 78)
                              (:frame (max 120 (- basis-width 8)))
                              (:fill (max 100 (- basis-width 24)))
                              (:half-left (max 70 (floor (- right 30) 2)))
                              (:half-right (max 70 (floor (- right 30) 2)))
                              (otherwise width-mode)))
                          (item-x
                            (case width-mode
                              (:number (- basis-width 86))
                              (:half-right (+ 18 (floor (- right 30) 2)))
                              (otherwise x)))
                          (item-y (if (eq y :action-row)
                                      (- main-height 34)
                                      y)))
                     (cl-fltk:resize-widget
                      widget :x item-x :y item-y
                      :width item-width :height item-height))))
               (let* ((page-width (- right 12))
                      (column-width (max 34 (floor (- page-width 32) 7))))
                 (dolist (item tone-items)
                   (destructuring-bind (widget index role) item
                     (let* ((column-x (+ 14 (* index column-width)))
                            (item-width (ecase role
                                          (:label column-width)
                                          (:slider 20)
                                          (:input (max 30 (- column-width 6)))))
                            (item-x (ecase role
                                      (:label (+ column-x 4))
                                      (:slider (+ column-x (floor (- column-width 20) 2)))
                                      (:input (+ column-x 2))))
                            (item-y (ecase role (:label 92) (:slider 116) (:input 240)))
                            (item-height (ecase role (:label 22) (:slider 120) (:input 24))))
                       (cl-fltk:resize-widget widget :x item-x :y item-y
                                              :width item-width :height item-height)))))
               (cl-fltk:redraw inspector)))
           (layout-ui (&optional ignored)
             (declare (ignore ignored))
             (let* ((width (cl-fltk:widget-width window))
                    (height (cl-fltk:widget-height window))
                    (top 64) (bottom 28)
                    (main-height (max 200 (- height top bottom)))
                    (left (min (max 180 (- width 580))
                               420
                               (max 180
                                    (if layout-initialized-p
                                        (cl-fltk:widget-width left-column)
                                        (floor width 5)))))
                    (right (min (max 280 (- width left 300))
                                480
                                (max 280
                                     (if layout-initialized-p
                                         (cl-fltk:widget-width right-column)
                                         (floor width 3)))))
                    (center (- width left right)))
               (cl-fltk:resize-widget menu :x 0 :y 0 :width width :height 24)
               (cl-fltk:resize-widget toolbar :x 0 :y 24 :width width :height 40)
               (when toolbar-bottom-rule
                 (cl-fltk:resize-widget toolbar-bottom-rule :x 0 :y 38
                                        :width width :height 2))
               (when lens-name
                 (cl-fltk:resize-widget lens-name :x 342 :y 6
                                        :width (max 120 (- width 352)) :height 28))
               (cl-fltk:resize-widget main-tile :x 0 :y top
                                      :width width :height main-height)
               (cl-fltk:resize-widget left-column :x 0 :y 0
                                      :width left :height main-height)
               (cl-fltk:resize-widget center-pane :x left :y 0
                                      :width center :height main-height)
               (cl-fltk:resize-widget right-column :x (+ left center) :y 0
                                      :width right :height main-height)
               ;; The left column splits into filmstrip and stills gallery,
               ;; the right into the Node panel and the graph editor.
               (let ((gallery-height
                       (min (- main-height 200)
                            (max 160
                                 (if layout-initialized-p
                                     (cl-fltk:widget-height gallery-pane)
                                     (min 300 (floor main-height 3))))))
                     (graph-height
                       (min (- main-height 240)
                            (max 160
                                 (if layout-initialized-p
                                     (cl-fltk:widget-height graph-pane)
                                     (min 340
                                          (floor (* main-height 2) 5)))))))
                 (cl-fltk:resize-widget filmstrip-pane :x 0 :y 0
                                        :width left
                                        :height (- main-height
                                                   gallery-height))
                 (cl-fltk:resize-widget gallery-pane :x 0
                                        :y (- main-height gallery-height)
                                        :width left :height gallery-height)
                 (cl-fltk:resize-widget inspector :x 0 :y 0
                                        :width right
                                        :height (- main-height graph-height))
                 (cl-fltk:resize-widget graph-pane :x 0
                                        :y (- main-height graph-height)
                                        :width right :height graph-height))
               (cl-fltk:resize-widget progress :x 0 :y (+ top main-height)
                                      :width 180 :height bottom)
               (cl-fltk:resize-widget status :x 180 :y (+ top main-height)
                                      :width (- width 180) :height bottom)
               (layout-left-pane)
               (layout-gallery-pane)
               (layout-center-pane)
               (layout-inspector-pane)
               (layout-graph-pane)
               (unless layout-initialized-p
                 (cl-fltk:init-sizes main-tile)
                 (cl-fltk:init-sizes left-column)
                 (cl-fltk:init-sizes right-column))
               (setf layout-initialized-p t)))
           (poll ()
             (dolist (event (drain-events queue))
               (case (first event)
                 (:status
                  (when (or (null (second event))
                            (= (second event) preview-generation))
                    (set-status (third event))))
                 (:preview
                  (when (gui-preview-event-current-p model event preview-generation)
                    (publish-preview (fifth event) (sixth event) (second event))
                    (set-status (preview-status-text model))))
                 (:histogram
                  (when (and (= (second event) preview-generation)
                             (equal (third event) after-preview-file))
                    (setf curve-histogram (fourth event))
                    (when curve-canvas (cl-fltk:redraw curve-canvas))))
                 (:thumbnail
                  (when (and (member (third event) (project-photos project) :test #'eq)
                             (null (gethash (third event) thumbnail-files)))
                    (setf (gethash (third event) thumbnail-files) (fourth event))
                    (redraw-thumbnails)))
                 (:still-thumb
                  (let ((still (and (= (second event) gallery-generation)
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
                      (when gallery-canvas (cl-fltk:redraw gallery-canvas)))))
                 (:done
                  (set-status (format nil "Exported ~A" (second event))))
                 (:error
                  (set-status (format nil "Error: ~A" (second event))))))
             ;; Honest progress: completed over the batch's high-water
             ;; total, across every render queue.
             (let ((load (+ (gui-queue-load queue)
                            (gui-queue-load background-queue)
                            (gui-queue-load histogram-queue))))
               (if (zerop load)
                   (progn
                     (setf progress-total 0)
                     (setf (cl-fltk:value progress) "0"))
                   (progn
                     (setf progress-total (max progress-total load))
                     (setf (cl-fltk:value progress)
                           (format nil "~D"
                                   (max 2
                                        (min 99
                                             (round (* 100
                                                       (- progress-total
                                                          load))
                                                    progress-total))))))))))
        (setf window (cl-fltk:make-window :width 1280 :height 800
                                          :label "Orfeus"
                                          :app-id "org.orfeus.Orfeus"))
        (cl-fltk:apply-classic-theme)
        (cl-fltk:set-size-range window :min-width 960 :min-height 700)
        (setf menu (cl-fltk:make-menu-bar :parent window :x 0 :y 0
                                          :width 1280 :height 24))
        (cl-fltk:add-menu-item menu "File/Open Photo" (lambda (&rest ignored)
                                                          (declare (ignore ignored))
                                                          (open-photo))
                               :shortcut (logior +menu-ctrl+ (char-code #\o)))
        (cl-fltk:add-menu-item menu "File/Add Photos to Project"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (add-photos))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\a)))
        (cl-fltk:add-menu-item menu "File/Open Project" (lambda (&rest ignored)
                                                            (declare (ignore ignored))
                                                            (open-project))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\o)))
        (cl-fltk:add-menu-item menu "File/Save Project" (lambda (&rest ignored)
                                                            (declare (ignore ignored))
                                                            (save-project))
                               :shortcut (logior +menu-ctrl+ (char-code #\s)))
        (cl-fltk:add-menu-item menu "File/Save Project As" (lambda (&rest ignored)
                                                               (declare (ignore ignored))
                                                               (save-project t))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\s)))
        (cl-fltk:add-menu-item menu "File/Export Current Photo"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (render-selected))
                               :shortcut (logior +menu-ctrl+ (char-code #\e)))
        (cl-fltk:add-menu-item menu "File/Export All Photos"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (render-all))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\e)))
        (cl-fltk:add-menu-item menu "File/Quit" (lambda (&rest ignored)
                                                    (declare (ignore ignored))
                                                    (cl-fltk:quit))
                               :shortcut (logior +menu-ctrl+ (char-code #\q)))
        (cl-fltk:add-menu-item menu "Edit/Copy Grade"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (copy-grade))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\c)))
        (cl-fltk:add-menu-item menu "Edit/Paste Grade to Selected"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (paste-grade))
                               :shortcut (logior +menu-ctrl+ +menu-shift+
                                                 (char-code #\v)))
        (cl-fltk:add-menu-item menu "Edit/Remove Selected Photos"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (remove-selected-photo)))
        (cl-fltk:add-menu-item menu "Edit/Reset Selected Photos"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (gui-model-reset-selected model)
                                 (sync-controls)
                                              (schedule-edited-preview)))
        (cl-fltk:add-menu-item menu "Edit/Reset Defaults"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (setf (project-defaults project)
                                       (gui-default-processing-settings))
                                 (sync-controls)
                                              (schedule-edited-preview)))
        (cl-fltk:add-menu-item menu "View/Before and After"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (toggle-comparison))
                               :shortcut (logior +menu-ctrl+ (char-code #\b)))
        (cl-fltk:add-menu-item menu "View/Refresh Preview"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (schedule-initial-preview))
                               :shortcut +key-f5+)
        (cl-fltk:add-menu-item menu "Process/Grab Still"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (grab-still))
                               :shortcut (logior +menu-ctrl+ (char-code #\g)))
        (cl-fltk:add-menu-item menu "Process/Export Current Photo"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (render-selected)))
        (cl-fltk:add-menu-item menu "Process/Export All Photos"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (render-all)))
        (cl-fltk:add-menu-item menu "Help/About Orfeus"
                               (lambda (&rest ignored)
                                 (declare (ignore ignored))
                                 (cl-fltk:message-box
                                  "Orfeus RAW processor\nOlympus PEN-F and OM-1")))
        (setf toolbar (cl-fltk:make-panel :parent window :x 0 :y 24
                                          :width 1280 :height 40 :label ""))
        (cl-fltk:set-box toolbar cl-fltk:+box-flat-box+)
        (flet ((rule (x y width height red green blue)
                 (let ((box (cl-fltk:make-box :parent toolbar :x x :y y
                                              :width width :height height
                                              :label "")))
                   (cl-fltk:set-box box cl-fltk:+box-flat-box+)
                   (cl-fltk:set-color-rgb box :red red :green green :blue blue)
                   box))
               (toolbar-button (x icon tooltip action)
                 (let ((button (cl-fltk:make-button
                                :parent toolbar :x x :y 5 :width 28 :height 28
                                :label ""
                                :callback (lambda (&rest ignored)
                                            (declare (ignore ignored))
                                            (funcall action)))))
                   (cl-fltk:set-box button cl-fltk:+box-flat-box+)
                   (cl-fltk:set-stock-icon button icon)
                   (cl-fltk:set-tooltip button tooltip)
                   button))
               (toolbar-text-button (x width label tooltip action)
                 (let ((button (cl-fltk:make-button
                                :parent toolbar :x x :y 5 :width width :height 28
                                :label label
                                :callback (lambda (&rest ignored)
                                            (declare (ignore ignored))
                                            (funcall action)))))
                   (cl-fltk:set-box button cl-fltk:+box-flat-box+)
                   (cl-fltk:set-tooltip button tooltip)
                   button)))
          (rule 6 10 2 18 130 130 130)
          (rule 10 10 2 18 245 245 245)
          (toolbar-button 18 :open "Add RAW photographs to project" #'add-photos)
          (toolbar-button 48 :folder-open "Open project" #'open-project)
          (toolbar-button 78 :delete "Remove selected photographs" #'remove-selected-photo)
          (rule 112 7 1 24 150 150 150)
          (toolbar-button 120 :export "Export current photograph" #'render-selected)
          (toolbar-button 150 :pipeline "Show or hide Before and After" #'toggle-comparison)
          (rule 184 7 1 24 150 150 150)
          (toolbar-text-button 192 28 "−" "Zoom out" (lambda () (zoom-preview .8d0)))
          (toolbar-text-button 222 38 "Fit" "Fit preview" #'reset-preview-view)
          (toolbar-text-button 262 28 "+" "Zoom in" (lambda () (zoom-preview 1.25d0)))
          (toolbar-text-button 292 38 "1:1" "Show image pixels at 1:1" #'preview-one-to-one)
          (rule 334 7 1 24 150 150 150)
          (setf toolbar-bottom-rule (rule 0 38 1280 2 145 145 145)))
        (setf lens-name (cl-fltk:make-label :parent toolbar :x 342 :y 6
                                            :width 912 :height 28
                                            :label "Lens: No photograph selected"))
        (cl-fltk:set-label-font lens-name 1)
        (setf main-tile (cl-fltk:make-tile :parent window :x 0 :y 64
                                           :width 1280 :height 708)
              left-column (cl-fltk:make-tile :parent main-tile :x 0 :y 0
                                             :width 240 :height 708)
              filmstrip-pane (cl-fltk:make-panel :parent left-column
                                                 :x 0 :y 0
                                                 :width 240 :height 448
                                                 :label "")
              gallery-pane (cl-fltk:make-panel :parent left-column
                                               :x 0 :y 448
                                               :width 240 :height 260
                                               :label "")
              center-pane (cl-fltk:make-panel :parent main-tile :x 240 :y 0
                                              :width 720 :height 708 :label ""))
        (cl-fltk:set-box gallery-pane cl-fltk:+box-flat-box+)
        (setf thumbnail-canvas
              (cl-fltk:make-canvas
               :parent filmstrip-pane :x 0 :y 0 :width 240 :height 448
               :callback
               (lambda (widget event value)
                 (declare (ignore event value))
                 (let ((x (cl-fltk:widget-x widget))
                       (y (cl-fltk:widget-y widget))
                       (width (cl-fltk:widget-width widget))
                       (height (cl-fltk:widget-height widget))
                       (row-height (thumbnail-row-height)))
                   (cl-fltk:draw-color-rgb :red 42 :green 44 :blue 46)
                   (cl-fltk:draw-filled-rect x y width height)
                   (loop for job in (project-photos project)
                         for row from 0
                         for row-y = (+ y (* row row-height) (- thumbnail-scroll))
                         when (and (< row-y (+ y height))
                                   (> (+ row-y row-height) y))
                           do (when (member row (gui-model-selected-indices model))
                                (cl-fltk:draw-color-rgb :red 0 :green 0 :blue 128)
                                (cl-fltk:draw-filled-rect x row-y width row-height))
                              (let ((path (gethash job thumbnail-files)))
                                (if path
                                    (draw-thumbnail-file widget path
                                                         (+ x 6) (+ row-y 6)
                                                         88 (- row-height 12))
                                    (progn
                                      (cl-fltk:draw-color-rgb :red 62 :green 64 :blue 66)
                                      (cl-fltk:draw-filled-rect
                                       (+ x 6) (+ row-y 6) 88 (- row-height 12)))))
                              (cl-fltk:draw-color-rgb :red 235 :green 235 :blue 235)
                              (cl-fltk:draw-text
                               (file-namestring (photo-job-input-path job))
                               (+ x 102) (+ row-y 30))
                              (cl-fltk:draw-color-rgb :red 125 :green 127 :blue 129)
                              (cl-fltk:draw-filled-rect x (+ row-y row-height -1)
                                                       width 1))))))
        (dolist (event (list cl-fltk:+event-push+ cl-fltk:+event-wheel+))
          (cl-fltk:on thumbnail-canvas #'handle-thumbnail-mouse :event event))
        (setf thumbnail-scrollbar
              (cl-fltk:make-scrollbar
               :parent filmstrip-pane :x 224 :y 0 :width 16 :height 448
               :value "0"
               :callback
               (lambda (widget event value)
                 (declare (ignore event))
                 (let ((position (ignore-errors
                                   (round (parse-number
                                           (if (plusp (length (or value "")))
                                               value
                                               (cl-fltk:value widget)))))))
                   (when position
                     (setf thumbnail-scroll position)
                     (clamp-thumbnail-scroll)
                     (cl-fltk:redraw thumbnail-canvas))))))
        (cl-fltk:scrollbar-set-orientation thumbnail-scrollbar :vertical)
        (cl-fltk:set-step thumbnail-scrollbar 36)
        (setf before-caption
              (cl-fltk:make-label :parent center-pane :x 0 :y 0
                                  :width 357 :height 22
                                  :label "  Before · neutral RAW")
              after-caption
              (cl-fltk:make-label :parent center-pane :x 363 :y 0
                                  :width 357 :height 22
                                  :label "  After · current adjustments"))
        (dolist (caption (list before-caption after-caption))
          (cl-fltk:set-box caption cl-fltk:+box-thin-up-box+)
          (cl-fltk:set-label-font caption cl-fltk:+font-helvetica-bold+))
        (flet ((make-preview-canvas (role x)
                 (cl-fltk:make-canvas
                  :parent center-pane :x x :y 22 :width 357 :height 686
                  :callback
                  (lambda (widget event value)
                    (declare (ignore event value))
                    (cl-fltk:draw-color-rgb :red 30 :green 32 :blue 34)
                    (cl-fltk:draw-filled-rect
                     (cl-fltk:widget-x widget) (cl-fltk:widget-y widget)
                     (cl-fltk:widget-width widget) (cl-fltk:widget-height widget))
                    (let ((path (ecase role
                                  (:before before-preview-file)
                                  (:after after-preview-file))))
                      (if path
                          (progn
                            (draw-preview-file widget path
                                               :zoom preview-zoom
                                               :center-x preview-center-x
                                               :center-y preview-center-y)
                            (when (eq role :after)
                              (draw-crop-overlay widget)))
                          (progn
                            (cl-fltk:draw-color-rgb :red 205 :green 208 :blue 210)
                            (cl-fltk:draw-text "Developing RAW preview..."
                                               (+ (cl-fltk:widget-x widget) 20)
                                               (+ (cl-fltk:widget-y widget) 36)))))))))
          (setf before-canvas (make-preview-canvas :before 0)
                after-canvas (make-preview-canvas :after 363))
          (dolist (canvas (list before-canvas after-canvas))
            (dolist (event (list cl-fltk:+event-push+
                                 cl-fltk:+event-drag+
                                 cl-fltk:+event-release+
                                 cl-fltk:+event-wheel+))
              (cl-fltk:on canvas
                          (lambda (widget callback-event value)
                            (handle-preview-mouse widget callback-event value))
                          :event event))))
        (setf right-column (cl-fltk:make-tile :parent main-tile
                                              :x 960 :y 0
                                              :width 320 :height 708)
              inspector (cl-fltk:make-panel :parent right-column :x 0 :y 0
                                            :width 320 :height 425
                                            :label "")
              graph-pane (cl-fltk:make-panel :parent right-column
                                             :x 0 :y 425
                                             :width 320 :height 283
                                             :label ""))
        (cl-fltk:set-box graph-pane cl-fltk:+box-flat-box+)
        (setf graph-title (cl-fltk:make-label :parent graph-pane
                                              :x 8 :y 4
                                              :width 304 :height 18
                                              :label "Node Graph"))
        (cl-fltk:set-label-font graph-title cl-fltk:+font-helvetica-bold+)
        (flet ((grade-button (x label tooltip action)
                 (let ((button (cl-fltk:make-button
                                :parent graph-pane :x x :y 24
                                :width 96 :height 22 :label label
                                :callback (lambda (&rest ignored)
                                            (declare (ignore ignored))
                                            (funcall action)))))
                   (cl-fltk:set-tooltip button tooltip)
                   button)))
          (setf still-button
                (grade-button 8 "Still" "Grab a still of the current grade"
                              #'grab-still)
                copy-grade-button
                (grade-button 108 "Copy"
                              "Copy the current photo's node graph"
                              #'copy-grade)
                paste-grade-button
                (grade-button 208 "Paste"
                              "Paste the copied node graph to the selection"
                              #'paste-grade)))
        (setf graph-canvas
              (cl-fltk:make-canvas
               :parent graph-pane :x 2 :y 52 :width 316 :height 229
               :callback (lambda (widget event value)
                           (declare (ignore event value))
                           (draw-graph-editor widget))))
        (cl-fltk:set-box graph-canvas cl-fltk:+box-flat-box+)
        (cl-fltk:set-tooltip
         graph-canvas
         "Drag nodes to arrange; drag a port to rewire; right-click to add")
        (dolist (event (list cl-fltk:+event-push+
                             cl-fltk:+event-drag+
                             cl-fltk:+event-release+
                             cl-fltk:+event-wheel+))
          (cl-fltk:on graph-canvas #'handle-graph-mouse :event event))
        ;; The inspector lays its children out manually, so it must paint its
        ;; own background; a boxless group smears stale pixels during tile
        ;; drags and window resizes.
        (cl-fltk:set-box inspector cl-fltk:+box-flat-box+)
        (let ((scope-field
                (cl-fltk:make-labeled-choice
                 :parent inspector :x 8 :y 8 :width 300 :height 26
                 :label "Apply to" :label-width 96
                 :items '("Photo" "Defaults")
                 :callback (lambda (widget event value)
                             (declare (ignore event value))
                             (setf (gui-model-edit-target model)
                                   (if (string-equal (cl-fltk:value widget)
                                                     "Defaults")
                                       :defaults :photo))
                             (sync-controls)))))
          (register-field scope-field 8)
          (setf target-choice (cl-fltk:field-control scope-field)))
        (setf tabs (cl-fltk:make-tabs :parent inspector :x 4 :y 40
                                      :width 312 :height 380)
              node-page (cl-fltk:make-tab-page :parent tabs :x 2 :y 24
                                               :width 308 :height 352
                                               :label "Node")
              export-page (cl-fltk:make-tab-page :parent tabs :x 2 :y 24
                                                 :width 308 :height 352
                                                 :label "Export"))
        (section-frame export-page "Output" 16 172)
        (flet ((export-integer-field (key label y)
                 (cl-fltk:field-control
                  (register-field
                   (cl-fltk:make-labeled-control
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
                 (cl-fltk:make-check-button
                  :parent export-page :x 110 :y 122 :width 182 :height 26
                  :label "Preserve metadata"
                  :callback (lambda (widget event value)
                              (declare (ignore event value))
                              (export-setting-changed :preserve-metadata-p widget)))
                 110 122 :control 26 :page)
                export-timestamp
                (register-inspector
                 (cl-fltk:make-check-button
                  :parent export-page :x 110 :y 154 :width 182 :height 26
                  :label "Timestamp filenames"
                  :callback (lambda (widget event value)
                              (declare (ignore event value))
                              (export-setting-changed :timestamp-filenames-p
                                                      widget)))
                 110 154 :control 26 :page)))
        ;; The stills gallery lives on the left column, PowerGrade style.
        (setf gallery-title (cl-fltk:make-label :parent gallery-pane
                                                :x 8 :y 2
                                                :width 224 :height 18
                                                :label "Stills Gallery"))
        (cl-fltk:set-label-font gallery-title cl-fltk:+font-helvetica-bold+)
        (setf gallery-canvas
              (cl-fltk:make-canvas
               :parent gallery-pane :x 4 :y 22 :width 232 :height 172
               :callback (lambda (widget event value)
                           (declare (ignore event value))
                           (draw-gallery widget))))
        (dolist (event (list cl-fltk:+event-push+ cl-fltk:+event-wheel+))
          (cl-fltk:on gallery-canvas #'handle-gallery-mouse :event event))
        (setf preset-name-input
              (cl-fltk:make-input :parent gallery-pane :x 8 :y 200
                                  :width 224 :height 24))
        (cl-fltk:set-tooltip preset-name-input "Still or preset name")
        (setf preset-save-button
              (cl-fltk:make-button
               :parent gallery-pane :x 8 :y 228 :width 108 :height 26
               :label "Save current"
               :callback (lambda (&rest ignored)
                           (declare (ignore ignored))
                           (save-current-preset))))
        (setf preset-apply-button
              (cl-fltk:make-button
               :parent gallery-pane :x 124 :y 228 :width 108 :height 26
               :label "Apply to 1 photo"
               :callback (lambda (&rest ignored)
                           (declare (ignore ignored))
                           (apply-current-preset))))
        (refresh-gallery)
        ;; The Node panel: pick a correction, see only that category's
        ;; controls beneath it.
        (build-group
         :picker
         (lambda ()
           (setf kind-choice
                 (cl-fltk:field-control
                  (register-field
                   (cl-fltk:make-labeled-choice
                    :parent node-page :x 12 :y 8 :width 292 :height 26
                    :label "Correction" :label-width 88
                    :items (mapcar #'first *node-kind-choices*)
                    :callback
                    (lambda (widget event value)
                      (declare (ignore event value))
                      (let ((node (gui-model-selected-graph-node model))
                            (kind (rest (assoc (cl-fltk:value widget)
                                               *node-kind-choices*
                                               :test #'string-equal))))
                        (when (and node kind
                                   (not (eq kind
                                            (orfeus:graph-node-kind
                                             node))))
                          (handler-case
                              (progn
                                (gui-model-set-node-kind model node kind)
                                (after-graph-edit
                                 (format nil "Correction set to ~A"
                                         (node-kind-label kind))))
                            (error (condition)
                              (sync-node-tools)
                              (set-status
                               (princ-to-string condition))))))))
                   8 :page)))))
        (build-group
         :none
         (lambda ()
           (register-inspector
            (cl-fltk:make-label
             :parent node-page :x 12 :y 44 :width 292 :height 26
             :label "Select or create a node in the graph")
            12 44 :fill 26 :page)))
        (build-group
         :node
         (lambda ()
           (register-inspector
            (cl-fltk:make-label
             :parent node-page :x 12 :y 44 :width 292 :height 26
             :label "Pick a correction type above")
            12 44 :fill 26 :page)))
        (build-group
         :white-balance
         (lambda ()
           (setf wb-choice
                 (cl-fltk:field-control
                  (register-field
                   (cl-fltk:make-labeled-choice
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
            (cl-fltk:make-label :parent node-page :x 12 :y 44
                                :width 292 :height 26
                                :label "Tone Equalizer")
            12 44 :fill 26 :page)
           (make-tone-band :tone-blacks "Blk" "Blacks" 0)
           (make-tone-band :tone-shadows "Shd" "Shadows" 1)
           (make-tone-band :tone-dark-mids "DkM" "Dark mids" 2)
           (make-tone-band :tone-midtones "Mid" "Midtones" 3)
           (make-tone-band :tone-light-mids "LtM" "Light mids" 4)
           (make-tone-band :tone-highlights "Hi" "Highlights" 5)
           (make-tone-band :tone-whites "Wht" "Whites" 6)))
        (build-group
         :optics
         (lambda ()
           (let ((lens (register-inspector
                        (cl-fltk:make-check-button
                         :parent node-page :x 12 :y 44
                         :width 292 :height 26
                         :label "Apply lens distortion correction"
                         :callback
                         (lambda (widget event value)
                           (declare (ignore event value))
                           (gui-model-set-setting
                            model :lens-correction-p
                            (string/= "0" (cl-fltk:value widget)))
                           (schedule-edited-preview)))
                        12 44 :fill 26 :page)))
             (push (list :lens-correction-p lens) controls))
           (make-number-field :lens-correction-strength "Strength"
                              0 2 0.05 76 node-page)
           (let ((tca (register-inspector
                       (cl-fltk:make-check-button
                        :parent node-page :x 12 :y 108
                        :width 292 :height 26
                        :label "Remove chromatic aberration"
                        :callback
                        (lambda (widget event value)
                          (declare (ignore event value))
                          (gui-model-set-setting
                           model :chromatic-aberration-correction-p
                           (string/= "0" (cl-fltk:value widget)))
                          (schedule-edited-preview)))
                       12 108 :fill 26 :page)))
             (push (list :chromatic-aberration-correction-p tca)
                   controls))))
        (build-group
         :film
         (lambda ()
           (register-inspector
            (cl-fltk:make-label :parent node-page :x 12 :y 44
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
                    (cl-fltk:make-choice
                     :parent node-page :x 110 :y 44 :width 190 :height 26
                     :items items
                     :callback
                     (lambda (widget event value)
                       (declare (ignore event value))
                       (let ((selection (cl-fltk:value widget)))
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
            (cl-fltk:make-label :parent node-page :x 12 :y 44
                                :width 88 :height 26 :label "Opacity")
            12 44 88 26 :page)
           (setf blend-opacity-input
                 (register-inspector
                  (cl-fltk:make-spinner
                   :parent node-page :x 110 :y 44 :width 84 :height 26
                   :callback
                   (lambda (widget event value)
                     (declare (ignore event value))
                     (let ((node (gui-model-selected-graph-node model)))
                       (when (and node (orfeus:graph-node-blend-p node))
                         (handler-case
                             (let ((opacity (parse-number
                                             (cl-fltk:value widget))))
                               (setf (orfeus:graph-node-opacity node)
                                     (float (max 0 (min 1 opacity)) 1.0))
                               (when graph-canvas
                                 (cl-fltk:redraw graph-canvas))
                               (schedule-edited-preview))
                           (error (condition)
                             (set-status
                              (princ-to-string condition))))))))
                  110 44 84 26 :page))
           (cl-fltk:set-range blend-opacity-input 0 1)
           (cl-fltk:set-step blend-opacity-input 0.05)))
        (build-group
         :color-subtract
         (lambda ()
           (flet ((subtract-button (y label action)
                    (register-inspector
                     (cl-fltk:make-button
                      :parent node-page :x 12 :y y :width 292 :height 26
                      :label label
                      :callback
                      (lambda (&rest ignored)
                        (declare (ignore ignored))
                        (let ((node (gui-model-selected-graph-node
                                     model)))
                          (when node (funcall action node)))))
                     12 y :fill 26 :page)))
             (subtract-button 44 "Pick Base Color..." #'pick-base-color)
             (subtract-button 76 "Sample Base From Photo"
                              (lambda (node)
                                (setf pick-color-node node)
                                (set-status
                                 "Click the preview to sample the film base")))
             (subtract-button 108 "Auto Base From Border"
                              #'auto-base-from-border))))
        (build-group
         :crop
         (lambda ()
           (register-inspector
            (cl-fltk:make-label :parent node-page :x 12 :y 44
                                :width 88 :height 26 :label "Angle")
            12 44 88 26 :page)
           (setf crop-angle-input
                 (register-inspector
                  (cl-fltk:make-spinner
                   :parent node-page :x 110 :y 44 :width 84 :height 26
                   :callback
                   (lambda (widget event value)
                     (declare (ignore event value))
                     (let ((node (crop-editing-node)))
                       (when node
                         (handler-case
                             (let ((angle (parse-number
                                           (cl-fltk:value widget))))
                               (set-crop-node-angle
                                node
                                (float (max -45 (min 45 angle)) 1.0)))
                           (error (condition)
                             (set-status
                              (princ-to-string condition))))))))
                  110 44 84 26 :page))
           (cl-fltk:set-range crop-angle-input -45 45)
           (cl-fltk:set-step crop-angle-input 0.1)
           (register-inspector
            (cl-fltk:make-button
             :parent node-page :x 12 :y 76 :width 292 :height 26
             :label "Autocrop Negative"
             :callback (lambda (&rest ignored)
                         (declare (ignore ignored))
                         (let ((node (crop-editing-node)))
                           (when node (autocrop-negative node)))))
            12 76 :fill 26 :page)
           (register-inspector
            (cl-fltk:make-button
             :parent node-page :x 12 :y 108 :width 292 :height 26
             :label "Reset Crop"
             :callback (lambda (&rest ignored)
                         (declare (ignore ignored))
                         (let ((node (crop-editing-node)))
                           (when node
                             (gui-model-set-node-params
                              model node '(:left 0.0 :top 0.0
                                           :width 1.0 :height 1.0
                                           :angle 0.0))
                             (after-graph-edit
                              "Crop reset to full frame")))))
            12 108 :fill 26 :page)
           (register-inspector
            (cl-fltk:make-label
             :parent node-page :x 12 :y 140 :width 292 :height 26
             :label "Drag the rectangle on the preview")
            12 140 :fill 26 :page)))
        (build-group
         :curves
         (lambda ()
           (let ((channel-field
                   (cl-fltk:make-labeled-choice
                    :parent node-page :x 12 :y 44 :width 292 :height 26
                    :label "Channel" :label-width 88
                    :items '("Red" "Green" "Blue")
                    :callback
                    (lambda (widget event value)
                      (declare (ignore event value))
                      (setf curve-channel
                            (cond ((string-equal
                                    (cl-fltk:value widget) "Green")
                                   :green-points)
                                  ((string-equal
                                    (cl-fltk:value widget) "Blue")
                                   :blue-points)
                                  (t :red-points)))
                      (when curve-canvas
                        (cl-fltk:redraw curve-canvas))))))
             (register-field channel-field 44 :page))
           (setf curve-canvas
                 (register-inspector
                  (cl-fltk:make-canvas
                   :parent node-page :x 12 :y 76 :width 292 :height 160
                   :callback (lambda (widget event value)
                               (declare (ignore event value))
                               (draw-curve-editor widget)))
                  12 76 :fill 160 :page))
           (cl-fltk:set-tooltip
            curve-canvas
            "Drag the points to shape the channel; the histogram shows occupancy")
           (dolist (event (list cl-fltk:+event-push+
                                cl-fltk:+event-drag+
                                cl-fltk:+event-release+))
             (cl-fltk:on curve-canvas #'handle-curve-mouse :event event))
           (register-inspector
            (cl-fltk:make-button
             :parent node-page :x 12 :y 244 :width 292 :height 26
             :label "Reset Channel"
             :callback (lambda (&rest ignored)
                         (declare (ignore ignored))
                         (reset-curve-channel)))
            12 244 :fill 26 :page)))
        (register-inspector
         (cl-fltk:make-button :parent inspector :x 12 :y 674
                              :width 140 :height 26 :label "Reset selected"
                              :callback (lambda (&rest ignored)
                                          (declare (ignore ignored))
                                          (gui-model-reset-selected model)
                                          (sync-controls)
                                                                (schedule-edited-preview)))
         12 :action-row :half-left 26)
        (register-inspector
         (cl-fltk:make-button :parent inspector :x 166 :y 674
                              :width 142 :height 26 :label "Export current"
                              :callback (lambda (&rest ignored)
                                          (declare (ignore ignored))
                                          (render-selected)))
         166 :action-row :half-right 26)
        (setf progress (cl-fltk:make-progress :parent window :x 0 :y 772
                                              :width 180 :height 28 :value "0")
              status (cl-fltk:make-status-bar :parent window :x 180 :y 772
                                               :width 1100 :height 28
                                               :value "Ready"))
        (cl-fltk:tile-size-range main-tile left-column
                                 :min-width 180 :max-width 420)
        (cl-fltk:tile-size-range main-tile center-pane :min-width 300)
        (cl-fltk:tile-size-range main-tile right-column
                                 :min-width 280 :max-width 480)
        (cl-fltk:tile-size-range left-column filmstrip-pane :min-height 140)
        (cl-fltk:tile-size-range left-column gallery-pane :min-height 140)
        (cl-fltk:tile-size-range right-column inspector :min-height 220)
        (cl-fltk:tile-size-range right-column graph-pane :min-height 140)
        (cl-fltk:on-resize window #'layout-ui)
        (cl-fltk:on-resize filmstrip-pane #'layout-left-pane)
        (cl-fltk:on-resize gallery-pane #'layout-gallery-pane)
        (cl-fltk:on-resize center-pane #'layout-center-pane)
        (cl-fltk:on-resize inspector #'layout-inspector-pane)
        (cl-fltk:on-resize graph-pane #'layout-graph-pane)
        (gui-model-set-selected-indices
         model (if (project-photos project) '(0) '()))
        (sync-controls)
        (sync-node-tools)
        (layout-ui)
        (setf poll-id (cl-fltk:add-timeout 0.08d0 #'poll :repeat t))
        ;; Best-effort housekeeping of the persistent preview cache.
        (enqueue-gui-task background-queue :evict
                          (lambda ()
                            (ignore-errors
                              (evict-stale-previews preview-directory))))
        (when (selected-job)
          (schedule-initial-preview))
        (unwind-protect
             (progn (cl-fltk:show window) (cl-fltk:run))
          (when poll-id (ignore-errors (cl-fltk:remove-timeout poll-id)))
          (when debounce-id (ignore-errors (cl-fltk:remove-timeout debounce-id)))
          (stop-gui-queue queue)
          (stop-gui-queue background-queue)
          (stop-gui-queue histogram-queue)
          (orfeus::clear-render-source-cache)
          (clear-preview-cache)
          (when window (ignore-errors (cl-fltk:destroy window))))))))

(defun main (&optional pathname)
  "Launch Orfeus with optional PHOTO or PROJECT PATHNAME."
  (let ((argument (or pathname (first (uiop:command-line-arguments)))))
    (run-gui argument)))
