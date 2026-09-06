(in-package #:orfeus/gui)

;;;; The photograph picker: Orfeus's own file dialog.
;;;;
;;;; The desktop's chooser shows a RAW file as a name in a list, one directory
;;;; at a time, with a preview if it feels like it and never of a RAW, and it
;;;; closes the moment anything is chosen. A card of five hundred frames is
;;;; picked by number, and a batch drawn from three folders takes three trips.
;;;;
;;;; This one has two panes. On the left a folder is browsed as a grid of the
;;;; camera's own previews, lifted from each file's first two megabytes in the
;;;; background and cached. On the right is the stash: the batch being built,
;;;; step by step, from as many folders as it takes. Frames go across by the
;;;; Add button, a double click, or a drag; back by Remove, a double click, or
;;;; Delete. Both grids select with the mouse, a rubber band, Control and
;;;; Shift, and the keyboard the way the filmstrip is culled. Add Photographs
;;;; hands over the stash — or, when nothing was stashed, what is selected, so
;;;; a single folder is still one step.
;;;;
;;;; Everything that decides something — what a directory lists, how a click
;;;; changes a selection, where a thumbnail is cached, which settings are
;;;; kept — is a function over plain data, so the tests can ask without a
;;;; window.

(defparameter *picker-raw-types* '("orf" "dng")
  "Extensions of the files Orfeus develops.")

(defparameter *picker-picture-types* '("orf" "dng" "jpg" "jpeg" "png" "tif" "tiff")
  "Extensions of anything that is a picture, for seeing what else is on a card.")

(defparameter *picker-thumbnail-edge* 240
  "Longer side of a picker thumbnail, in pixels. A cell shows about 144 by 108,
so this leaves a little to spare without decoding more than a cell can show.")

(defparameter *picker-cell-width* 160
  "Width of one grid cell, in pixels.")

(defparameter *picker-cell-height* 152
  "Height of one grid cell: the picture, the name, and a line of facts.")

(defparameter *picker-picture-height* 108
  "Height of the picture inside a cell.")

(defparameter *picker-recent-limit* 8
  "How many recently visited folders the places menu remembers.")

(defparameter *picker-thumbnail-workers* 2
  "Threads lifting previews out of files for the picker. Two keep the grid
filling while one file is being read; more just fight over the card reader.")

(defparameter *picker-drag-threshold* 6
  "How far a press has to travel, in pixels, before it is a drag of the
selection rather than a click on it.")

;;; Entries

(defstruct (picker-entry (:constructor %make-picker-entry))
  "One thing a directory lists: a folder or a file."
  (name "" :type string)
  (pathname nil)
  (directory-p nil)
  (size 0)
  (mtime 0))

(defun make-picker-entry (name pathname &key directory-p (size 0) (mtime 0))
  (%make-picker-entry :name name :pathname pathname :directory-p directory-p
                      :size size :mtime mtime))

(defun natural-string< (left right)
  "Compare like a person reading names with numbers in them: 9 before 10.

Runs of digits are compared by value, everything else by character without
regard to case, so P8294003 and P8294010 order by frame number and _7181470
sorts with the rest of its card."
  (let ((left-at 0) (right-at 0)
        (left-length (length left)) (right-length (length right)))
    (loop
      (when (>= left-at left-length) (return (< right-at right-length)))
      (when (>= right-at right-length) (return nil))
      (let ((a (char left left-at)) (b (char right right-at)))
        (cond
          ((and (digit-char-p a) (digit-char-p b))
           (let* ((left-end (or (position-if-not #'digit-char-p left :start left-at)
                                left-length))
                  (right-end (or (position-if-not #'digit-char-p right :start right-at)
                                 right-length))
                  (left-number (parse-integer left :start left-at :end left-end))
                  (right-number (parse-integer right :start right-at :end right-end)))
             (cond ((< left-number right-number) (return t))
                   ((> left-number right-number) (return nil))
                   (t (setf left-at left-end right-at right-end)))))
          ((char-equal a b) (incf left-at) (incf right-at))
          (t (return (char-lessp a b))))))))

(defun picker-file-shown-p (pathname filter)
  "Whether PATHNAME passes FILTER: :RAW, :PICTURES or :ALL."
  (let ((type (pathname-type pathname)))
    (ecase filter
      (:all t)
      (:raw (and type (member type *picker-raw-types* :test #'string-equal) t))
      (:pictures (and type (member type *picker-picture-types* :test #'string-equal) t)))))

(defun picker-entry-from-file (pathname)
  "An entry for the file PATHNAME, sized and dated by a stat, or NIL."
  (let ((stat (ignore-errors (sb-posix:stat (namestring pathname)))))
    (when stat
      (make-picker-entry (file-namestring pathname) pathname
                         :size (sb-posix:stat-size stat)
                         :mtime (sb-posix:stat-mtime stat)))))

(defun picker-hidden-name-p (name)
  (and (plusp (length name)) (char= #\. (char name 0))))

(defun picker-sort-entries (entries sort)
  "ENTRIES in SORT order: :NAME reads like a card, :NEWEST puts the latest
modification first and names decide ties."
  (let ((entries (copy-list entries)))
    (ecase sort
      (:name (sort entries #'natural-string< :key #'picker-entry-name))
      (:newest (sort entries (lambda (a b)
                               (or (> (picker-entry-mtime a) (picker-entry-mtime b))
                                   (and (= (picker-entry-mtime a) (picker-entry-mtime b))
                                        (natural-string< (picker-entry-name a)
                                                         (picker-entry-name b))))))))))

(defun picker-list-directory (directory &key (filter :raw) (sort :name))
  "What DIRECTORY holds: two values, its folders and the files FILTER admits,
each a list of entries in SORT order, hidden names left out.

Signals a file error the interface turns into a status line: a card that was
pulled or a folder that is not ours to read is an answer, not a crash."
  (let* ((directory (uiop:ensure-directory-pathname directory))
         (folders (loop for sub in (uiop:subdirectories directory)
                        for name = (car (last (pathname-directory sub)))
                        unless (or (not (stringp name)) (picker-hidden-name-p name))
                          collect (make-picker-entry name sub :directory-p t)))
         (files (loop for file in (uiop:directory-files directory)
                      for name = (file-namestring file)
                      when (and (not (picker-hidden-name-p name))
                                (picker-file-shown-p file filter))
                        collect (or (picker-entry-from-file file)
                                    (make-picker-entry name file)))))
    (values (picker-sort-entries folders :name)
            (picker-sort-entries files sort))))

(defun picker-size-text (bytes)
  "BYTES as a person reads a file size."
  (cond ((or (null bytes) (not (integerp bytes))) "")
        ((< bytes 1000) (format nil "~D B" bytes))
        ((< bytes 1000000) (format nil "~D kB" (round bytes 1000)))
        ((< bytes 10000000) (format nil "~,1F MB" (/ bytes 1000000.0)))
        ((< bytes 1000000000) (format nil "~D MB" (round bytes 1000000)))
        (t (format nil "~,2F GB" (/ bytes 1000000000.0)))))

(defun picker-time-text (unix-seconds)
  "UNIX-SECONDS as a local date and time, or nothing when unknown."
  (if (and (integerp unix-seconds) (plusp unix-seconds))
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time (+ unix-seconds
                                    (encode-universal-time 0 0 0 1 1 1970 0)))
        (declare (ignore second))
        (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D" year month day hour minute))
      ""))

;;; Places

(defun picker-unescape-mount (text)
  "Undo /proc/mounts' octal escapes for space, tab, newline and backslash."
  (with-output-to-string (out)
    (loop with index = 0
          while (< index (length text))
          do (let ((char (char text index)))
               (if (and (char= char #\\) (<= (+ index 4) (length text))
                        (every #'digit-char-p (subseq text (1+ index) (+ index 4))))
                   (progn
                     (write-char (code-char (parse-integer text :start (1+ index)
                                                                :end (+ index 4)
                                                                :radix 8))
                                 out)
                     (incf index 4))
                   (progn (write-char char out) (incf index)))))))

(defun picker-mounted-volumes ()
  "Where removable media turn up: mount points under the usual media roots."
  (let ((mounts '()))
    (with-open-file (stream "/proc/mounts" :if-does-not-exist nil)
      (when stream
        (loop for line = (read-line stream nil)
              while line
              do (let ((fields (uiop:split-string line :separator '(#\Space))))
                   (when (>= (length fields) 2)
                     (let ((point (second fields)))
                       (when (some (lambda (root)
                                     (and (> (length point) (length root))
                                          (string= root point :end2 (length root))))
                                   '("/run/media/" "/media/" "/mnt/"))
                         (push (uiop:ensure-directory-pathname
                                (picker-unescape-mount point))
                               mounts))))))))
    (nreverse mounts)))

(defun picker-directory-label (directory)
  "A directory as the menu shows it: its own name, and its parent's when
that alone would be ambiguous, like DCIM/100OMSYS."
  (let ((parts (remove-if-not #'stringp (pathname-directory directory))))
    (cond ((null parts) "/")
          ((and (>= (length parts) 2)
                (member (car (last parts)) '("100OMSYS" "DCIM") :test #'string-equal))
           (format nil "~A/~A" (car (last parts 2)) (car (last parts))))
          (t (car (last parts))))))

(defun picker-places (recent &key (volumes (picker-mounted-volumes)))
  "The places menu: home, the mounted VOLUMES, the root, then RECENT folders.
Each is (LABEL . DIRECTORY-PATHNAME); a label is never repeated."
  (let ((places (list (cons "Home" (user-homedir-pathname)))))
    (flet ((add (label directory)
             (unless (or (find directory places :key #'cdr :test #'equal)
                         (find label places :key #'car :test #'string=))
               (push (cons label directory) places))))
      (dolist (volume volumes)
        (add (or (car (last (pathname-directory volume))) (namestring volume)) volume))
      (add "File system" #P"/")
      (dolist (directory recent)
        (let ((directory (uiop:ensure-directory-pathname directory)))
          (add (picker-directory-label directory) directory))))
    (nreverse places)))

;;; Remembered settings

(defun picker-settings-pathname ()
  "Where the picker keeps the folder it was last in and how it was showing it."
  (uiop:xdg-config-home "orfeus/picker.sexp"))

(defun picker-valid-settings (plist)
  "PLIST with every value that is not what it should be dropped."
  (let ((directory (getf plist :directory))
        (recent (getf plist :recent))
        (filter (getf plist :filter))
        (sort (getf plist :sort)))
    (append (when (and (stringp directory) (plusp (length directory)))
              (list :directory directory))
            (when (and (listp recent) (every #'stringp recent))
              (list :recent (subseq recent 0 (min (length recent) *picker-recent-limit*))))
            (when (member filter '(:raw :pictures :all))
              (list :filter filter))
            (when (member sort '(:name :newest))
              (list :sort sort)))))

(defun picker-read-settings (&optional (pathname (picker-settings-pathname)))
  "The remembered settings, or nothing when there are none or they are unreadable."
  (handler-case
      (with-open-file (stream pathname :direction :input :if-does-not-exist nil)
        (when stream
          (let* ((*read-eval* nil)
                 (form (read stream nil nil)))
            (and (listp form) (picker-valid-settings form)))))
    (error () nil)))

(defun picker-write-settings (plist &optional (pathname (picker-settings-pathname)))
  "Remember PLIST for the next time. Failing to is not worth interrupting a pick."
  (handler-case
      (progn
        (ensure-directories-exist pathname)
        (with-open-file (stream pathname :direction :output :if-exists :supersede)
          (with-standard-io-syntax
            (let ((*print-readably* nil))
              (prin1 (picker-valid-settings plist) stream)
              (terpri stream))))
        t)
    (error () nil)))

(defun picker-remember-directory (recent directory)
  "RECENT with DIRECTORY moved to the front, capped at the remembered limit."
  (let* ((name (namestring (uiop:ensure-directory-pathname directory)))
         (others (remove name recent :test #'string=)))
    (subseq (cons name others) 0 (min (1+ (length others)) *picker-recent-limit*))))

;;; Thumbnail cache

(defun picker-cache-directory ()
  (uiop:xdg-cache-home "orfeus/picker/"))

(defun picker-thumbnail-pathname (entry &optional (directory (picker-cache-directory)))
  "Where ENTRY's thumbnail is cached: named by path, size and modification
time, so a re-shot card or an edited file gets a fresh one and an unchanged
card is never read twice."
  (let ((identity (format nil "~A|~D|~D|~D"
                          (namestring (picker-entry-pathname entry))
                          (picker-entry-size entry)
                          (picker-entry-mtime entry)
                          *picker-thumbnail-edge*)))
    (merge-pathnames
     (make-pathname
      :name (subseq (ironclad:byte-array-to-hex-string
                     (ironclad:digest-sequence
                      :sha256 (sb-ext:string-to-octets identity :external-format :utf-8)))
                    0 32)
      :type "jpg")
     directory)))

;;; Selection

(defstruct (picker-selection (:constructor make-picker-selection ()))
  "Which entries are picked, where the cursor is, and where a range began."
  (indices (make-hash-table))
  (cursor nil)
  (anchor nil))

(defun picker-selection-list (selection)
  "The picked indices, ascending."
  (sort (loop for index being the hash-keys of (picker-selection-indices selection)
              collect index)
        #'<))

(defun picker-selection-count (selection)
  (hash-table-count (picker-selection-indices selection)))

(defun picker-selected-p (selection index)
  (and index (gethash index (picker-selection-indices selection)) t))

(defun picker-selection-clear (selection)
  (clrhash (picker-selection-indices selection))
  selection)

(defun picker-selection-set-range (selection from to)
  (loop for index from (min from to) to (max from to)
        do (setf (gethash index (picker-selection-indices selection)) t)))

(defun picker-selection-click (selection index &key control shift)
  "A click on INDEX. Plain: this one alone. CONTROL: this one toggled, the
rest kept. SHIFT: everything from the anchor to here, replacing the rest —
and the anchor stays where it was, so a second shift-click resizes the range.
The cursor lands on INDEX either way."
  (cond
    (control
     (if (gethash index (picker-selection-indices selection))
         (remhash index (picker-selection-indices selection))
         (setf (gethash index (picker-selection-indices selection)) t))
     (setf (picker-selection-anchor selection) index))
    (shift
     (let ((anchor (or (picker-selection-anchor selection) index)))
       (picker-selection-clear selection)
       (picker-selection-set-range selection anchor index)
       (setf (picker-selection-anchor selection) anchor)))
    (t
     (picker-selection-clear selection)
     (setf (gethash index (picker-selection-indices selection)) t
           (picker-selection-anchor selection) index)))
  (setf (picker-selection-cursor selection) index)
  selection)

(defun picker-selection-move (selection index &key shift)
  "The cursor moves to INDEX by keyboard. Plain: the selection follows it.
SHIFT: the range from the anchor grows or shrinks to it."
  (if shift
      (let ((anchor (or (picker-selection-anchor selection)
                        (picker-selection-cursor selection)
                        index)))
        (picker-selection-clear selection)
        (picker-selection-set-range selection anchor index)
        (setf (picker-selection-anchor selection) anchor))
      (progn
        (picker-selection-clear selection)
        (setf (gethash index (picker-selection-indices selection)) t
              (picker-selection-anchor selection) index)))
  (setf (picker-selection-cursor selection) index)
  selection)

(defun picker-selection-toggle-cursor (selection)
  "Space: the cursor's entry goes in or comes out, the rest untouched."
  (let ((index (picker-selection-cursor selection)))
    (when index
      (if (gethash index (picker-selection-indices selection))
          (remhash index (picker-selection-indices selection))
          (setf (gethash index (picker-selection-indices selection)) t))
      (setf (picker-selection-anchor selection) index))
    selection))

(defun picker-selection-all (selection count)
  (picker-selection-clear selection)
  (when (plusp count)
    (picker-selection-set-range selection 0 (1- count)))
  (unless (picker-selection-cursor selection)
    (setf (picker-selection-cursor selection) (and (plusp count) 0)))
  selection)

(defun picker-selection-band (selection indices base &key control)
  "A rubber band over INDICES: they are the selection, added to BASE — the
selection as it was when the band began — when CONTROL is held."
  (picker-selection-clear selection)
  (when control
    (dolist (index base)
      (setf (gethash index (picker-selection-indices selection)) t)))
  (dolist (index indices)
    (setf (gethash index (picker-selection-indices selection)) t))
  selection)

(defun picker-selection-after-removal (selection removed count)
  "SELECTION renumbered after the ascending indices REMOVED left a list of
COUNT entries: what remains keeps its place, the cursor lands where the first
removed one was, clamped to what is left."
  (let ((kept (loop for index in (picker-selection-list selection)
                    unless (member index removed)
                      collect (- index (count-if (lambda (gone) (< gone index)) removed))))
        (first-gone (first removed)))
    (picker-selection-clear selection)
    (dolist (index kept)
      (setf (gethash index (picker-selection-indices selection)) t))
    (setf (picker-selection-anchor selection) nil
          (picker-selection-cursor selection)
          (cond ((zerop count) nil)
                (first-gone (min first-gone (1- count)))
                (t (picker-selection-cursor selection))))
    selection))

;;; Grid geometry

(defun picker-grid-columns (width)
  "How many cells fit across WIDTH pixels of grid."
  (max 1 (floor (- width 8) *picker-cell-width*)))

(defun picker-grid-height (count columns)
  "How tall COUNT cells stand in COLUMNS, in pixels."
  (* (ceiling (max count 0) columns) *picker-cell-height*))

(defun picker-cell-at (x y scroll columns count)
  "The index of the cell under grid point X, Y with the grid scrolled by
SCROLL pixels, or NIL between cells or past the last one."
  (let ((column (floor (- x 4) *picker-cell-width*))
        (row (floor (+ y scroll) *picker-cell-height*)))
    (when (and (<= 0 column (1- columns)) (>= row 0) (>= x 4))
      (let ((index (+ (* row columns) column)))
        (and (< index count) index)))))

(defun picker-cell-rect (index scroll columns)
  "Where cell INDEX draws in grid coordinates: X, Y, width, height."
  (values (+ 4 (* (mod index columns) *picker-cell-width*))
          (- (* (floor index columns) *picker-cell-height*) scroll)
          *picker-cell-width*
          *picker-cell-height*))

(defun picker-cells-in-rectangle (left top right bottom scroll columns count)
  "The indices of every cell the grid rectangle touches."
  (let ((left (min left right)) (right (max left right))
        (top (min top bottom)) (bottom (max top bottom)))
    (loop for index below count
          when (multiple-value-bind (x y width height)
                   (picker-cell-rect index scroll columns)
                 ;; The picture area, not the cell's full extent, so a band
                 ;; through the gutters picks nothing by accident.
                 (let ((cell-left (+ x 6)) (cell-top (+ y 4))
                       (cell-right (+ x width -6)) (cell-bottom (+ y height -6)))
                   (and (< cell-left right) (> cell-right left)
                        (< cell-top bottom) (> cell-bottom top))))
            collect index)))

(defun picker-scroll-to-show (scroll index columns view-height)
  "SCROLL adjusted so that cell INDEX is wholly on a grid VIEW-HEIGHT tall."
  (multiple-value-bind (x y width height) (picker-cell-rect index 0 columns)
    (declare (ignore x width))
    (cond ((< y scroll) y)
          ((> (+ y height) (+ scroll view-height)) (max 0 (- (+ y height) view-height)))
          (t scroll))))

(defun picker-type-ahead-match (names typed from)
  "The first name at or after FROM, wrapping round, that starts with TYPED
without regard to case, or NIL."
  (let ((count (length names)))
    (when (plusp count)
      (loop for offset below count
            for index = (mod (+ from offset) count)
            for name = (elt names index)
            when (and (>= (length name) (length typed))
                      (string-equal typed name :end2 (length typed)))
              return index))))

(defun picker-stash-add (stash entries)
  "STASH, a vector of entries, with ENTRIES appended that are not in it yet.
Returns the new vector and how many were new."
  (let ((added 0)
        (result (coerce stash 'list)))
    (dolist (entry entries)
      (unless (find (picker-entry-pathname entry) result
                    :key #'picker-entry-pathname :test #'equal)
        (setf result (append result (list entry)))
        (incf added)))
    (values (coerce result 'vector) added)))

(defun picker-stash-remove (stash indices)
  "STASH without the entries at INDICES."
  (coerce (loop for entry across stash
                for index from 0
                unless (member index indices)
                  collect entry)
          'vector))

;;; A grid: one pane of cells with its own selection, scroll and band

(defstruct (picker-grid (:constructor %make-picker-grid))
  "One pane of the picker: the browsed folder, or the stash."
  (kind :browse)
  canvas scrollbar
  (entries #())
  (selection (make-picker-selection))
  (scroll 0)
  ;; A rubber band in progress: (x0 y0 x1 y1) in grid coordinates, and the
  ;; selection as it was when the band began.
  (band nil) (band-base '())
  ;; A press on an already selected cell that may become a drag: the cell,
  ;; where it was pressed, and whether it has travelled far enough.
  (press nil) (dragging nil)
  (type-ahead "") (type-ahead-time 0))

(defun picker-grid-count (grid)
  (length (picker-grid-entries grid)))

(defun picker-grid-size (grid)
  (let ((canvas (picker-grid-canvas grid)))
    (values (lightfast:widget-width canvas) (lightfast:widget-height canvas))))

(defun picker-grid-column-count (grid)
  (picker-grid-columns (nth-value 0 (picker-grid-size grid))))

(defun picker-grid-redraw (grid)
  (when (picker-grid-canvas grid)
    (lightfast:redraw (picker-grid-canvas grid))))

(defun picker-grid-clamp-scroll (grid)
  (multiple-value-bind (width height) (picker-grid-size grid)
    (let* ((total (picker-grid-height (picker-grid-count grid) (picker-grid-columns width)))
           (limit (max 0 (- total height))))
      (setf (picker-grid-scroll grid) (max 0 (min limit (picker-grid-scroll grid))))
      (let ((bar (picker-grid-scrollbar grid)))
        (when bar
          (lightfast:set-range bar 0 limit)
          (setf (lightfast:value bar) (princ-to-string (picker-grid-scroll grid))))))))

(defun picker-grid-set-entries (grid entries)
  "GRID shows ENTRIES from the top, nothing selected, the cursor on the first."
  (setf (picker-grid-entries grid) (coerce entries 'vector)
        (picker-grid-scroll grid) 0
        (picker-grid-band grid) nil
        (picker-grid-press grid) nil
        (picker-grid-dragging grid) nil)
  (let ((selection (picker-grid-selection grid)))
    (picker-selection-clear selection)
    (setf (picker-selection-cursor selection) (and (plusp (length entries)) 0)
          (picker-selection-anchor selection) nil))
  (picker-grid-clamp-scroll grid))

(defun picker-grid-selected-entries (grid)
  "The selected entries in grid order, or the cursor's when none is selected."
  (let* ((selection (picker-grid-selection grid))
         (indices (or (picker-selection-list selection)
                      (and (picker-selection-cursor selection)
                           (list (picker-selection-cursor selection))))))
    (loop for index in indices
          when (< index (picker-grid-count grid))
            collect (aref (picker-grid-entries grid) index))))

(defun picker-grid-visible-range (grid)
  "The first and last index a full redraw draws, or NIL for none."
  (multiple-value-bind (width height) (picker-grid-size grid)
    (let* ((columns (picker-grid-columns width))
           (scroll (picker-grid-scroll grid))
           (count (picker-grid-count grid))
           (first-row (floor scroll *picker-cell-height*))
           (last-row (floor (+ scroll height -1) *picker-cell-height*))
           (first (* first-row columns))
           (last (min (1- count) (+ (* (1+ last-row) columns) -1))))
      (when (and (plusp count) (<= first last))
        (values first last)))))

;;; The dialog

(defstruct (photo-picker (:constructor %make-photo-picker))
  ;; Widgets.
  window path-input places-choice folder-browser
  filter-choice sort-choice browse-label stash-label
  stash-button unstash-button stash-all-button clear-stash-button
  select-all-button cancel-button accept-button
  back-button forward-button up-button home-button hint-label
  ;; The two panes.
  (browse (%make-picker-grid :kind :browse))
  (stash (%make-picker-grid :kind :stash))
  ;; Collaborators: the interface's event queue, the owning window, and the
  ;; threads lifting previews.
  queue owner (workers nil)
  ;; State.
  (directory nil) (folders '()) (filter :raw) (sort :name) (recent '())
  ;; Pathname -> cached thumbnail pathname, or :NONE for a file without one;
  ;; and the pathnames already asked for.
  (thumbnails (make-hash-table :test #'equal))
  (requested (make-hash-table :test #'equal))
  (history '()) (forward '())
  (on-pick nil) (accept-label "Add"))

(defun picker-status (picker text)
  (when (photo-picker-browse-label picker)
    (setf (lightfast:label (photo-picker-browse-label picker)) text)))

(defun picker-redraw (picker)
  (picker-grid-redraw (photo-picker-browse picker))
  (picker-grid-redraw (photo-picker-stash picker)))

(defun picker-entries-size (entries)
  (loop for entry in entries sum (or (picker-entry-size entry) 0)))

(defun picker-update-status (picker)
  "The two captions and the accept button say what is where."
  (let* ((browse (photo-picker-browse picker))
         (stash (photo-picker-stash picker))
         (count (picker-grid-count browse))
         (picked (picker-selection-count (picker-grid-selection browse)))
         (stashed (picker-grid-count stash))
         (stash-picked (picker-selection-count (picker-grid-selection stash))))
    (picker-status picker
                   (cond ((zerop count) "No photographs here")
                         ((zerop picked) (format nil "~D photograph~:P" count))
                         (t (format nil "~D photograph~:P · ~D selected · ~A"
                                    count picked
                                    (picker-size-text
                                     (picker-entries-size
                                      (picker-grid-selected-entries browse)))))))
    (when (photo-picker-stash-label picker)
      (setf (lightfast:label (photo-picker-stash-label picker))
            (cond ((zerop stashed) "Stash · nothing yet")
                  ((zerop stash-picked)
                   (format nil "Stash · ~D photograph~:P · ~A" stashed
                           (picker-size-text
                            (picker-entries-size (coerce (picker-grid-entries stash) 'list)))))
                  (t (format nil "Stash · ~D photograph~:P · ~D selected" stashed stash-picked)))))
    (when (photo-picker-accept-button picker)
      (let ((going (if (plusp stashed) stashed picked)))
        (setf (lightfast:label (photo-picker-accept-button picker))
              (if (plusp going)
                  (format nil "~A ~D Photograph~:P" (photo-picker-accept-label picker) going)
                  (format nil "~A Photographs" (photo-picker-accept-label picker))))))))

;;; Thumbnails

(defun picker-request-thumbnail (picker entry)
  "See to it that ENTRY's thumbnail exists: from the cache at once, otherwise
lifted out of the file on a worker thread, most recently asked first so what
is on screen fills before what was scrolled past."
  (let* ((key (picker-entry-pathname entry))
         (requested (photo-picker-requested picker)))
    (unless (gethash key requested)
      (setf (gethash key requested) t)
      (let ((cache (picker-thumbnail-pathname entry))
            (queue (photo-picker-queue picker)))
        (if (probe-file cache)
            (setf (gethash key (photo-picker-thumbnails picker)) cache)
            (enqueue-gui-task
             (photo-picker-workers picker) :picker-thumbnail
             (lambda ()
               (let ((result
                       (handler-case
                           (if (probe-file cache)
                               cache
                               (let ((temporary
                                       (make-pathname
                                        :name (format nil "~A.~D" (pathname-name cache)
                                                      (random 1000000))
                                        :type "tmp" :defaults cache)))
                                 (ensure-directories-exist cache)
                                 (orfeus:photo-embedded-preview
                                  (picker-entry-pathname entry) temporary
                                  :max-edge *picker-thumbnail-edge*)
                                 (rename-file temporary cache)
                                 cache))
                         (error () :none))))
                 (queue-event
                  queue
                  (list :call
                        (lambda ()
                          (setf (gethash key (photo-picker-thumbnails picker)) result)
                          (picker-redraw picker))))))
             :front-p t))))))

(defun picker-forget-pending-requests (picker)
  "Drop the previews still waiting to be lifted — the folder they were for is
gone from view — so they can be asked for again if it comes back."
  (discard-gui-tasks (photo-picker-workers picker) :picker-thumbnail)
  (let ((requested (photo-picker-requested picker))
        (done (photo-picker-thumbnails picker)))
    (loop for key being the hash-keys of requested
          unless (gethash key done)
            collect key into stale
          finally (dolist (key stale) (remhash key requested)))))

;;; Drawing

(defun picker-truncate (text budget)
  (if (> (length text) budget)
      (concatenate 'string (subseq text 0 (max 1 (- budget 1))) "…")
      text))

(defun picker-draw-grid (picker grid widget)
  "Paint one pane: a dark field, each cell its picture or a waiting mark,
the name and the facts beneath, selection in blue, the cursor framed in
amber, a band or a drag badge over the top."
  (let* ((wx (lightfast:widget-x widget))
         (wy (lightfast:widget-y widget))
         (width (lightfast:widget-width widget))
         (height (lightfast:widget-height widget))
         (columns (picker-grid-columns width))
         (scroll (picker-grid-scroll grid))
         (entries (picker-grid-entries grid))
         (selection (picker-grid-selection grid))
         (thumbnails (photo-picker-thumbnails picker)))
    (lightfast:with-clip (wx wy width height)
      (lightfast:draw-color-rgb :red 52 :green 54 :blue 58)
      (lightfast:draw-filled-rect wx wy width height)
      (multiple-value-bind (first last) (picker-grid-visible-range grid)
        (when first
          ;; Most recently visible first: requests go to the front of the
          ;; queue, so the row just scrolled to is lifted before the rest.
          (loop for index from last downto first
                for entry = (aref entries index)
                unless (gethash (picker-entry-pathname entry) thumbnails)
                  do (picker-request-thumbnail picker entry))
          (loop for index from first to last
                do (multiple-value-bind (cx cy cw ch) (picker-cell-rect index scroll columns)
                     (let* ((entry (aref entries index))
                            (x (+ wx cx)) (y (+ wy cy))
                            (selected (picker-selected-p selection index))
                            (cursor (eql index (picker-selection-cursor selection)))
                            (thumb (gethash (picker-entry-pathname entry) thumbnails))
                            (picture-x (+ x 8)) (picture-y (+ y 6))
                            (picture-w (- cw 16)) (picture-h *picker-picture-height*))
                       (when selected
                         (lightfast:draw-color-rgb :red 0 :green 0 :blue 128)
                         (lightfast:draw-filled-rect (+ x 2) (+ y 1) (- cw 4) (- ch 2)))
                       (cond
                         ((pathnamep thumb)
                          (draw-thumbnail-file widget thumb picture-x picture-y
                                               picture-w picture-h))
                         (t
                          (lightfast:draw-color-rgb :red 40 :green 42 :blue 45)
                          (lightfast:draw-filled-rect (+ picture-x 12) picture-y
                                                      (- picture-w 24) picture-h)
                          (lightfast:draw-stock-icon
                           (if (eq thumb :none) :photo :reload)
                           (+ picture-x (floor picture-w 2) -8)
                           (+ picture-y (floor picture-h 2) -8))))
                       (when cursor
                         (lightfast:draw-color-rgb :red 240 :green 190 :blue 90)
                         (lightfast:draw-rect (+ x 2) (+ y 1) (- cw 4) (- ch 2))
                         (lightfast:draw-rect (+ x 3) (+ y 2) (- cw 6) (- ch 4)))
                       (lightfast:draw-font :font lightfast:+font-helvetica-bold+ :size 12)
                       (lightfast:draw-color-rgb :red 236 :green 236 :blue 236)
                       (lightfast:draw-text (picker-truncate (picker-entry-name entry) 20)
                                            picture-x (+ picture-y picture-h 2)
                                            :width picture-w :height 16)
                       (lightfast:draw-font :font lightfast:+font-helvetica+ :size 10)
                       (lightfast:draw-color-rgb :red 170 :green 172 :blue 176)
                       (lightfast:draw-text (format nil "~A~@[ · ~A~]"
                                                    (picker-size-text (picker-entry-size entry))
                                                    (let ((time (picker-time-text
                                                                 (picker-entry-mtime entry))))
                                                      (and (plusp (length time)) time)))
                                            picture-x (+ picture-y picture-h 18)
                                            :width picture-w :height 14))))))
      (let ((band (picker-grid-band grid)))
        (when band
          (destructuring-bind (x0 y0 x1 y1) band
            (lightfast:draw-color-rgb :red 240 :green 190 :blue 90)
            (lightfast:draw-rect (+ wx (min x0 x1)) (+ wy (min y0 y1))
                                 (abs (- x1 x0)) (abs (- y1 y0))))))
      (let ((press (picker-grid-press grid)))
        (when (and press (picker-grid-dragging grid))
          ;; A badge with the count rides beside the pointer while a
          ;; selection is being carried to the other pane.
          (destructuring-bind (index x y) press
            (declare (ignore index))
            (let ((text (format nil "~D" (max 1 (picker-selection-count selection)))))
              (lightfast:draw-color-rgb :red 240 :green 190 :blue 90)
              (lightfast:draw-filled-rect (+ wx x 12) (+ wy y 12) 28 20)
              (lightfast:draw-color-rgb :red 30 :green 30 :blue 30)
              (lightfast:draw-font :font lightfast:+font-helvetica-bold+ :size 12)
              (lightfast:draw-text text (+ wx x 12) (+ wy y 12) :width 28 :height 20)))))
      (when (zerop (length entries))
        (lightfast:draw-font :font lightfast:+font-helvetica+ :size 12)
        (lightfast:draw-color-rgb :red 170 :green 172 :blue 176)
        (let ((lines (if (eq (picker-grid-kind grid) :stash)
                         '("Nothing stashed yet."
                           "Add frames from the folder on the left,"
                           "from as many folders as you like.")
                         (list (ecase (photo-picker-filter picker)
                                 (:raw "No RAW photographs in this folder")
                                 (:pictures "No pictures in this folder")
                                 (:all "Nothing in this folder"))))))
          (loop for line in lines
                for y from (+ wy 16) by 18
                do (lightfast:draw-text line (+ wx 16) y :width (- width 32) :height 18)))))))

;;; Navigation

(defun picker-parent-directory (directory)
  (let ((parent (uiop:pathname-parent-directory-pathname
                 (uiop:ensure-directory-pathname directory))))
    (and (not (equal parent (uiop:ensure-directory-pathname directory))) parent)))

(defun picker-show-directory (picker directory &key (remember t))
  "Make DIRECTORY the one browsed: list it, put the cursor on the first file,
and note where we came from for Back. The stash is untouched."
  (let ((directory (uiop:ensure-directory-pathname directory)))
    (handler-case
        (multiple-value-bind (folders files)
            (picker-list-directory directory
                                   :filter (photo-picker-filter picker)
                                   :sort (photo-picker-sort picker))
          (when (and remember (photo-picker-directory picker)
                     (not (equal directory (photo-picker-directory picker))))
            (push (photo-picker-directory picker) (photo-picker-history picker))
            (setf (photo-picker-forward picker) '()))
          (setf (photo-picker-directory picker) directory
                (photo-picker-folders picker) folders)
          (picker-forget-pending-requests picker)
          (picker-grid-set-entries (photo-picker-browse picker) files)
          (setf (lightfast:value (photo-picker-path-input picker)) (namestring directory))
          (picker-fill-folder-browser picker)
          (picker-update-status picker)
          (picker-redraw picker)
          (setf (photo-picker-recent picker)
                (picker-remember-directory (photo-picker-recent picker) directory))
          (picker-fill-places picker)
          (picker-save-settings picker)
          t)
      (error (condition)
        (picker-status picker (format nil "Cannot read ~A: ~A" (namestring directory)
                                      condition))
        nil))))

(defun picker-fill-folder-browser (picker)
  (let ((browser (photo-picker-folder-browser picker)))
    (lightfast:clear browser)
    (when (picker-parent-directory (photo-picker-directory picker))
      (lightfast:add-item browser ".."))
    (dolist (folder (photo-picker-folders picker))
      (lightfast:add-item browser (picker-entry-name folder)))))

(defun picker-folder-chosen (picker index)
  "Folder row INDEX was clicked: enter it."
  (let* ((parent (picker-parent-directory (photo-picker-directory picker)))
         (folder-index (if parent (1- index) index)))
    (cond ((and parent (zerop index))
           (picker-show-directory picker parent))
          ((and (>= folder-index 0) (< folder-index (length (photo-picker-folders picker))))
           (picker-show-directory
            picker (picker-entry-pathname (nth folder-index (photo-picker-folders picker))))))))

(defun picker-fill-places (picker)
  (let ((choice (photo-picker-places-choice picker)))
    (when choice
      (lightfast:clear choice)
      (lightfast:add-item choice "Places")
      (dolist (place (picker-places (photo-picker-recent picker)))
        (lightfast:add-item choice (car place)))
      (setf (lightfast:value choice) "Places"))))

(defun picker-place-chosen (picker label)
  (let ((place (find label (picker-places (photo-picker-recent picker))
                     :key #'car :test #'string=)))
    (when place
      (picker-show-directory picker (cdr place)))))

(defun picker-go-back (picker)
  (let ((previous (pop (photo-picker-history picker))))
    (when previous
      (push (photo-picker-directory picker) (photo-picker-forward picker))
      (picker-show-directory picker previous :remember nil))))

(defun picker-go-forward (picker)
  (let ((next (pop (photo-picker-forward picker))))
    (when next
      (push (photo-picker-directory picker) (photo-picker-history picker))
      (picker-show-directory picker next :remember nil))))

(defun picker-go-up (picker)
  (let ((parent (picker-parent-directory (photo-picker-directory picker))))
    (when parent (picker-show-directory picker parent))))

(defun picker-go-typed (picker)
  "The path field was entered: a folder is shown, a file is shown selected."
  (let* ((text (string-trim " " (lightfast:value (photo-picker-path-input picker))))
         (path (ignore-errors (probe-file (uiop:parse-native-namestring text)))))
    (cond ((null path)
           (picker-status picker (format nil "No such folder: ~A" text)))
          ((uiop:directory-pathname-p path)
           (picker-show-directory picker path))
          (t
           (when (picker-show-directory picker (uiop:pathname-directory-pathname path))
             (let* ((browse (photo-picker-browse picker))
                    (index (position (file-namestring path) (picker-grid-entries browse)
                                     :key #'picker-entry-name :test #'string=)))
               (when index
                 (picker-selection-click (picker-grid-selection browse) index)
                 (picker-update-status picker)
                 (picker-redraw picker))))))))

(defun picker-save-settings (picker)
  (picker-write-settings (list :directory (namestring (photo-picker-directory picker))
                               :recent (photo-picker-recent picker)
                               :filter (photo-picker-filter picker)
                               :sort (photo-picker-sort picker))))

;;; Moving frames between the panes

(defun picker-stash-entries (picker entries)
  "Put ENTRIES in the stash, once each, and say so."
  (let ((stash (photo-picker-stash picker)))
    (multiple-value-bind (entries-after added)
        (picker-stash-add (picker-grid-entries stash) entries)
      (setf (picker-grid-entries stash) entries-after)
      (let ((selection (picker-grid-selection stash)))
        (unless (picker-selection-cursor selection)
          (setf (picker-selection-cursor selection) (and (plusp (length entries-after)) 0))))
      (picker-grid-clamp-scroll stash)
      (picker-update-status picker)
      (picker-redraw picker)
      added)))

(defun picker-stash-selected (picker)
  "The browse pane's selection goes into the stash."
  (let ((entries (picker-grid-selected-entries (photo-picker-browse picker))))
    (if entries
        (picker-stash-entries picker entries)
        (picker-status picker "Select frames to stash first"))))

(defun picker-stash-all (picker)
  (picker-stash-entries picker (coerce (picker-grid-entries (photo-picker-browse picker)) 'list)))

(defun picker-unstash-indices (picker indices)
  "Take the stash entries at INDICES out again."
  (let* ((stash (photo-picker-stash picker))
         (indices (sort (copy-list indices) #'<)))
    (when indices
      (setf (picker-grid-entries stash) (picker-stash-remove (picker-grid-entries stash) indices))
      (picker-selection-after-removal (picker-grid-selection stash) indices
                                      (picker-grid-count stash))
      (picker-grid-clamp-scroll stash)
      (picker-update-status picker)
      (picker-redraw picker))))

(defun picker-unstash-selected (picker)
  (let* ((stash (photo-picker-stash picker))
         (selection (picker-grid-selection stash))
         (indices (or (picker-selection-list selection)
                      (and (picker-selection-cursor selection)
                           (list (picker-selection-cursor selection))))))
    (if indices
        (picker-unstash-indices picker indices)
        (picker-status picker "Select stashed frames to remove first"))))

(defun picker-clear-stash (picker)
  (picker-grid-set-entries (photo-picker-stash picker) '())
  (picker-update-status picker)
  (picker-redraw picker))

;;; Input

(defun picker-parse-mouse (value)
  "X Y BUTTON DX DY STATE out of a mouse event's value, or NIL."
  (let ((parts (remove "" (uiop:split-string (or value "") :separator '(#\Space))
                       :test #'string=)))
    (when (>= (length parts) 5)
      (handler-case (values-list (mapcar #'parse-integer
                                         (subseq parts 0 (min 6 (length parts)))))
        (error () nil)))))

(defun picker-parse-key (value)
  "KEY STATE TEXT out of a key event's value."
  (let* ((value (or value ""))
         (first-space (position #\Space value))
         (second-space (and first-space (position #\Space value :start (1+ first-space)))))
    (when second-space
      (values (parse-integer value :end first-space)
              (parse-integer value :start (1+ first-space) :end second-space)
              (subseq value (1+ second-space))))))

(defconstant +picker-shift+ #x00010000)
(defconstant +picker-control+ #x00040000)

(defun picker-other-grid (picker grid)
  (if (eq grid (photo-picker-browse picker))
      (photo-picker-stash picker)
      (photo-picker-browse picker)))

(defun picker-point-over-grid-p (grid window-x window-y)
  "Whether the window point lies on GRID's canvas."
  (let ((canvas (picker-grid-canvas grid)))
    (and canvas
         (<= (lightfast:widget-x canvas) window-x
             (+ (lightfast:widget-x canvas) (lightfast:widget-width canvas) -1))
         (<= (lightfast:widget-y canvas) window-y
             (+ (lightfast:widget-y canvas) (lightfast:widget-height canvas) -1)))))

(defun picker-grid-activate (picker grid)
  "A double click or Enter on GRID's selection: frames cross to the other pane."
  (if (eq (picker-grid-kind grid) :browse)
      (picker-stash-selected picker)
      (picker-unstash-selected picker)))

(defun picker-drop-selection (picker grid)
  "The selection carried from GRID was let go over the other pane."
  (if (eq (picker-grid-kind grid) :browse)
      (picker-stash-selected picker)
      (picker-unstash-selected picker)))

(defun picker-mouse (picker grid widget event value)
  (multiple-value-bind (x y button dx dy state) (picker-parse-mouse value)
    (declare (ignore dx))
    (when x
      (let* ((selection (picker-grid-selection grid))
             (columns (picker-grid-column-count grid))
             (count (picker-grid-count grid))
             (scroll (picker-grid-scroll grid))
             (control (logtest (or state 0) +picker-control+))
             (shift (logtest (or state 0) +picker-shift+)))
        (cond
          ((= event lightfast:+event-wheel+)
           (incf (picker-grid-scroll grid) (* dy 60))
           (picker-grid-clamp-scroll grid)
           (picker-grid-redraw grid))
          ((and (= event lightfast:+event-push+) (= button 1))
           (let ((index (picker-cell-at x y scroll columns count)))
             (cond
               ((and index (plusp (lightfast:event-clicks)) (not control) (not shift))
                ;; A double click: the frame — and whatever else is
                ;; selected with it — crosses over.
                (unless (picker-selected-p selection index)
                  (picker-selection-click selection index))
                (setf (picker-grid-press grid) nil)
                (picker-grid-activate picker grid))
               ((and index (picker-selected-p selection index) (not control) (not shift))
                ;; Pressing on a selected frame may be the start of a drag
                ;; of the whole selection; the click waits for the release.
                (setf (picker-grid-press grid) (list index x y)
                      (picker-grid-dragging grid) nil))
               (index
                (picker-selection-click selection index :control control :shift shift)
                (setf (picker-grid-press grid) (list index x y)
                      (picker-grid-dragging grid) nil))
               (t
                ;; A band starts on empty ground; plain, it replaces
                ;; whatever was picked, with Control or Shift it adds.
                (setf (picker-grid-band grid) (list x y x y)
                      (picker-grid-band-base grid)
                      (if (or control shift) (picker-selection-list selection) '())
                      (picker-grid-press grid) nil)
                (unless (or control shift) (picker-selection-clear selection))))
             (picker-update-status picker)
             (picker-grid-redraw grid)))
          ((and (= event lightfast:+event-drag+) (picker-grid-band grid))
           (destructuring-bind (x0 y0 x1 y1) (picker-grid-band grid)
             (declare (ignore x1 y1))
             (setf (picker-grid-band grid) (list x0 y0 x y))
             (picker-selection-band
              selection
              (picker-cells-in-rectangle x0 y0 x y scroll columns count)
              (picker-grid-band-base grid)
              :control (or control shift))
             (picker-update-status picker)
             (picker-grid-redraw grid)))
          ((and (= event lightfast:+event-drag+) (picker-grid-press grid))
           (destructuring-bind (index px py) (picker-grid-press grid)
             (when (or (picker-grid-dragging grid)
                       (> (max (abs (- x px)) (abs (- y py))) *picker-drag-threshold*))
               (setf (picker-grid-dragging grid) t
                     (picker-grid-press grid) (list index x y))
               (picker-grid-redraw grid))))
          ((and (= event lightfast:+event-release+) (picker-grid-band grid))
           (setf (picker-grid-band grid) nil)
           (picker-grid-redraw grid))
          ((and (= event lightfast:+event-release+) (picker-grid-press grid))
           (destructuring-bind (index px py) (picker-grid-press grid)
             (declare (ignore px py))
             (cond
               ((picker-grid-dragging grid)
                ;; Let go: over the other pane the selection crosses, else
                ;; nothing happened. FLTK reports the release to the widget
                ;; that was pressed, in its coordinates, so the point is
                ;; carried to the window to see what lies under it.
                (let ((window-x (+ x (lightfast:widget-x widget)))
                      (window-y (+ y (lightfast:widget-y widget))))
                  (setf (picker-grid-press grid) nil
                        (picker-grid-dragging grid) nil)
                  (if (picker-point-over-grid-p (picker-other-grid picker grid)
                                                window-x window-y)
                      (picker-drop-selection picker grid)
                      (picker-grid-redraw grid))))
               (t
                ;; A press on a selected frame that never became a drag is a
                ;; plain click after all.
                (setf (picker-grid-press grid) nil)
                (when (and (not control) (not shift)
                           (> (picker-selection-count selection) 1))
                  (picker-selection-click selection index))
                (picker-update-status picker)
                (picker-grid-redraw grid)))))
          ((and (= event lightfast:+event-push+) (= button 3))
           (let ((choice (lightfast:popup-menu
                          (if (eq (picker-grid-kind grid) :browse)
                              '("Add Selected to Stash" "Add All to Stash" "-"
                                "Select All" "Select None")
                              '("Remove Selected from Stash" "Clear Stash" "-"
                                "Select All" "Select None")))))
             (case choice
               (0 (picker-grid-activate picker grid))
               (1 (if (eq (picker-grid-kind grid) :browse)
                      (picker-stash-all picker)
                      (picker-clear-stash picker)))
               (3 (picker-selection-all selection count))
               (4 (picker-selection-clear selection)))
             (picker-update-status picker)
             (picker-redraw picker))))))))

(defun picker-key (picker grid value)
  (multiple-value-bind (key state text) (picker-parse-key value)
    (when key
      (let* ((selection (picker-grid-selection grid))
             (columns (picker-grid-column-count grid))
             (count (picker-grid-count grid))
             (cursor (or (picker-selection-cursor selection) 0))
             (shift (logtest state +picker-shift+))
             (control (logtest state +picker-control+))
             (view-height (nth-value 1 (picker-grid-size grid)))
             (rows-per-page (max 1 (floor view-height *picker-cell-height*))))
        (flet ((move-to (index &key (extend shift))
                 (when (plusp count)
                   (let ((index (max 0 (min (1- count) index))))
                     (picker-selection-move selection index :shift extend)
                     (setf (picker-grid-scroll grid)
                           (picker-scroll-to-show (picker-grid-scroll grid) index
                                                  columns view-height))
                     (picker-grid-clamp-scroll grid)))
                 (picker-update-status picker)
                 (picker-grid-redraw grid)))
          (case key
            (#xff53 (move-to (1+ cursor)))                  ; Right
            (#xff51 (move-to (1- cursor)))                  ; Left
            (#xff54 (move-to (+ cursor columns)))           ; Down
            (#xff52 (move-to (- cursor columns)))           ; Up
            (#xff56 (move-to (+ cursor (* rows-per-page columns)))) ; Page Down
            (#xff55 (move-to (- cursor (* rows-per-page columns)))) ; Page Up
            (#xff50 (move-to 0))                            ; Home
            (#xff57 (move-to (1- count)))                   ; End
            (32                                             ; Space
             (picker-selection-toggle-cursor selection)
             (picker-update-status picker)
             (picker-grid-redraw grid))
            ((#xff0d #xff8d)                                ; Return, keypad Enter
             (if control
                 (picker-accept picker)
                 (picker-grid-activate picker grid)))
            ((#xffff #xff08)                                ; Delete, Backspace
             (if (eq (picker-grid-kind grid) :stash)
                 (picker-unstash-selected picker)
                 (when (= key #xff08) (picker-go-up picker))))
            (t
             (cond
               ((and control (= key 97))                   ; Ctrl+A
                (picker-selection-all selection count)
                (picker-update-status picker)
                (picker-grid-redraw grid))
               ((and (not control) (= (length text) 1) (graphic-char-p (char text 0)))
                ;; Type-ahead: letters typed within a second join up.
                (let ((now (get-internal-real-time)))
                  (setf (picker-grid-type-ahead grid)
                        (if (< (- now (picker-grid-type-ahead-time grid))
                               internal-time-units-per-second)
                            (concatenate 'string (picker-grid-type-ahead grid) text)
                            text)
                        (picker-grid-type-ahead-time grid) now))
                (let ((match (picker-type-ahead-match
                              (map 'vector #'picker-entry-name (picker-grid-entries grid))
                              (picker-grid-type-ahead grid)
                              (if (> (length (picker-grid-type-ahead grid)) 1)
                                  cursor
                                  (1+ cursor)))))
                  (when match
                    (move-to match :extend nil))))))))))))

(defun picker-accept (picker)
  "Hand over the stash — or the browse selection when nothing was stashed —
and put the dialog away."
  (let* ((stash (photo-picker-stash picker))
         (entries (if (plusp (picker-grid-count stash))
                      (coerce (picker-grid-entries stash) 'list)
                      (picker-grid-selected-entries (photo-picker-browse picker))))
         (paths (mapcar #'picker-entry-pathname entries)))
    (cond ((null paths)
           (picker-status picker "Select or stash a photograph first"))
          (t
           (picker-save-settings picker)
           (lightfast:hide (photo-picker-window picker))
           (picker-clear-stash picker)
           (when (photo-picker-on-pick picker)
             (funcall (photo-picker-on-pick picker) paths))))))

(defun picker-cancel (picker)
  (lightfast:hide (photo-picker-window picker)))

(defun picker-refilter (picker)
  "The filter or sort changed: list the same folder again, keeping the
selection on the files still shown."
  (let* ((browse (photo-picker-browse picker))
         (picked (mapcar #'picker-entry-pathname (picker-grid-selected-entries browse)))
         (directory (photo-picker-directory picker)))
    (when (and directory (picker-show-directory picker directory :remember nil))
      (let ((selection (picker-grid-selection browse)))
        (loop for entry across (picker-grid-entries browse)
              for index from 0
              when (member (picker-entry-pathname entry) picked :test #'equal)
                do (setf (gethash index (picker-selection-indices selection)) t))
        (picker-update-status picker)
        (picker-redraw picker)))))

;;; Building

(defun picker-layout (picker)
  "Place every widget for the window's current size: toolbar, folders,
the browse grid, the crossing buttons, the stash, two rows of controls."
  (let* ((window (photo-picker-window picker))
         (width (lightfast:widget-width window))
         (height (lightfast:widget-height window))
         (gap 8) (row 26) (toolbar-y 8)
         (caption-y (+ toolbar-y row 8)) (caption-height 18)
         (grid-y (+ caption-y caption-height 4))
         (folders-width 200) (crossing-width 88) (places-width 170) (bar 16)
         (bottom (+ gap row gap row gap))
         (grid-height (max 120 (- height grid-y bottom)))
         (stash-width (+ 8 (* 2 *picker-cell-width*)))
         (stash-x (- width gap bar stash-width))
         (crossing-x (- stash-x gap crossing-width))
         (browse-x (+ gap folders-width gap))
         (browse-width (max 160 (- crossing-x gap bar browse-x))))
    (flet ((place (widget x y w h)
             (when widget (lightfast:resize-widget widget :x x :y y :width w :height h))))
      (place (photo-picker-back-button picker) gap toolbar-y 32 row)
      (place (photo-picker-forward-button picker) (+ gap 36) toolbar-y 32 row)
      (place (photo-picker-up-button picker) (+ gap 72) toolbar-y 32 row)
      (place (photo-picker-home-button picker) (+ gap 108) toolbar-y 32 row)
      (place (photo-picker-path-input picker) (+ gap 148) toolbar-y
             (max 120 (- width gap 148 gap places-width gap)) row)
      (place (photo-picker-places-choice picker) (- width gap places-width) toolbar-y
             places-width row)
      (place (photo-picker-browse-label picker) browse-x caption-y browse-width caption-height)
      (place (photo-picker-stash-label picker) stash-x caption-y (+ stash-width bar) caption-height)
      (place (photo-picker-folder-browser picker) gap grid-y folders-width grid-height)
      (let ((browse (photo-picker-browse picker)))
        (place (picker-grid-canvas browse) browse-x grid-y browse-width grid-height)
        (place (picker-grid-scrollbar browse) (+ browse-x browse-width) grid-y bar grid-height))
      (let ((y (+ grid-y (floor grid-height 2) -60)))
        (place (photo-picker-stash-button picker) crossing-x y crossing-width row)
        (place (photo-picker-stash-all-button picker) crossing-x (+ y 34) crossing-width row)
        (place (photo-picker-unstash-button picker) crossing-x (+ y 80) crossing-width row))
      (let ((stash (photo-picker-stash picker)))
        (place (picker-grid-canvas stash) stash-x grid-y stash-width grid-height)
        (place (picker-grid-scrollbar stash) (+ stash-x stash-width) grid-y bar grid-height))
      (let ((y (+ grid-y grid-height gap)))
        (place (photo-picker-filter-choice picker) gap y 170 row)
        (place (photo-picker-sort-choice picker) (+ gap 178) y 130 row)
        (place (photo-picker-select-all-button picker) (+ gap 316) y 100 row)
        (place (photo-picker-hint-label picker) (+ gap 424) y
               (max 100 (- stash-x gap gap 424)) row)
        (place (photo-picker-clear-stash-button picker) stash-x y 110 row))
      (let ((y (- height gap row)))
        (place (photo-picker-accept-button picker) (- width gap 200) y 200 row)
        (place (photo-picker-cancel-button picker) (- width gap 200 gap 96) y 96 row)))
    (picker-grid-clamp-scroll (photo-picker-browse picker))
    (picker-grid-clamp-scroll (photo-picker-stash picker))))

(defun picker-build-grid (picker grid window)
  "The canvas and scrollbar of one pane, wired to GRID."
  (let ((canvas (lightfast:make-canvas
                 :parent window :x 0 :y 0 :width 300 :height 300
                 :callback (lambda (widget event value)
                             (declare (ignore event value))
                             (picker-draw-grid picker grid widget)))))
    (setf (picker-grid-canvas grid) canvas)
    (lightfast:set-box canvas lightfast:+box-flat-box+)
    (dolist (event (list lightfast:+event-push+ lightfast:+event-drag+
                         lightfast:+event-release+ lightfast:+event-wheel+))
      (lightfast:on canvas (lambda (widget event value)
                             (picker-mouse picker grid widget event value))
                    :event event))
    (lightfast:on canvas (lambda (widget event value)
                           (declare (ignore widget event))
                           (picker-key picker grid value))
                  :event lightfast:+event-key+)
    (lightfast:set-tooltip
     canvas
     (if (eq (picker-grid-kind grid) :browse)
         "Click, drag a band, Ctrl and Shift to select; double-click, Enter or drag right to stash; arrows and Space cull"
         "The batch being built. Double-click, Delete or drag left to take a frame out; Ctrl+Enter adds the lot"))
    (setf (picker-grid-scrollbar grid)
          (lightfast:make-scrollbar
           :parent window :x 0 :y 0 :width 16 :height 300 :value "0"
           :callback (lambda (widget event value)
                       (declare (ignore event))
                       (let ((position (ignore-errors
                                         (round (parse-number
                                                 (if (plusp (length (or value "")))
                                                     value
                                                     (lightfast:value widget)))))))
                         (when position
                           (setf (picker-grid-scroll grid) (max 0 position))
                           (picker-grid-redraw grid))))))
    (lightfast:scrollbar-set-orientation (picker-grid-scrollbar grid) :vertical)
    grid))

(defun make-photo-picker (&key owner queue)
  "Build the picker dialog over OWNER, reporting through QUEUE. Built once and
shown as often as asked."
  (let* ((picker (%make-photo-picker
                  :owner owner :queue queue
                  :workers (make-gui-queue :workers *picker-thumbnail-workers*
                                           :name "Orfeus picker preview worker")))
         (window (lightfast:make-window :width 1240 :height 700 :label "Add Photographs")))
    (setf (photo-picker-window picker) window)
    (lightfast:window-set-modal window t)
    (lightfast:window-set-icon window :orfeus-32)
    (lightfast:set-size-range window :min-width 900 :min-height 480)
    (flet ((button (label icon action &key (width 32) tooltip return-p)
             (let ((widget (funcall (if return-p
                                        #'lightfast:make-return-button
                                        #'lightfast:make-button)
                                    :parent window :x 0 :y 0 :width width :height 26
                                    :label label
                                    :callback (lambda (&rest ignored)
                                                (declare (ignore ignored))
                                                (funcall action)))))
               (when icon (lightfast:set-stock-icon widget icon))
               (when tooltip (lightfast:set-tooltip widget tooltip))
               widget))
           (choice (items action &key width tooltip)
             (let ((widget (lightfast:make-choice
                            :parent window :x 0 :y 0 :width width :height 26 :items items
                            :callback (lambda (widget event value)
                                        (declare (ignore event value))
                                        (funcall action (lightfast:value widget))))))
               (when tooltip (lightfast:set-tooltip widget tooltip))
               widget)))
      (setf (photo-picker-back-button picker)
            (button "" :back (lambda () (picker-go-back picker)) :tooltip "Back")
            (photo-picker-forward-button picker)
            (button "" :forward (lambda () (picker-go-forward picker)) :tooltip "Forward")
            (photo-picker-up-button picker)
            (button "" :folder-open (lambda () (picker-go-up picker)) :tooltip "Up one folder")
            (photo-picker-home-button picker)
            (button "" :home (lambda () (picker-show-directory picker (user-homedir-pathname)))
                    :tooltip "Home folder"))
      (setf (photo-picker-path-input picker)
            (lightfast:make-input
             :parent window :x 0 :y 0 :width 400 :height 26 :value ""
             :callback (lambda (widget event value)
                         (declare (ignore widget event value))
                         (when (= (lightfast:event-key) #xff0d)
                           (picker-go-typed picker)))))
      (lightfast:set-when (photo-picker-path-input picker) :enter-key)
      (lightfast:set-tooltip (photo-picker-path-input picker)
                             "Type a folder and press Enter")
      (setf (photo-picker-places-choice picker)
            (choice '("Places") (lambda (label)
                                  (unless (string= label "Places")
                                    (picker-place-chosen picker label)))
                    :width 170
                    :tooltip "Home, mounted cards and drives, and folders visited before"))
      (setf (photo-picker-browse-label picker)
            (lightfast:make-label :parent window :x 0 :y 0 :width 300 :height 18 :label "")
            (photo-picker-stash-label picker)
            (lightfast:make-label :parent window :x 0 :y 0 :width 300 :height 18
                                  :label "Stash · nothing yet"))
      (lightfast:set-label-font (photo-picker-stash-label picker) lightfast:+font-helvetica-bold+)
      (setf (photo-picker-folder-browser picker)
            (lightfast:make-browser
             :parent window :x 0 :y 0 :width 200 :height 300
             :callback (lambda (widget event value)
                         (declare (ignore event value))
                         (let ((indices (lightfast:browser-selected-indices widget)))
                           (when indices
                             ;; Off the browser's own callback, which is still
                             ;; walking the rows the navigation replaces.
                             (let ((index (first indices)))
                               (lightfast:add-timeout
                                0.0 (lambda (&rest ignored)
                                      (declare (ignore ignored))
                                      (picker-folder-chosen picker index)))))))))
      (lightfast:browser-set-selection-mode (photo-picker-folder-browser picker) :hold)
      (lightfast:set-tooltip (photo-picker-folder-browser picker) "Click a folder to open it")
      (picker-build-grid picker (photo-picker-browse picker) window)
      (picker-build-grid picker (photo-picker-stash picker) window)
      (setf (photo-picker-stash-button picker)
            (button "Add ►" nil (lambda () (picker-stash-selected picker)) :width 88
                    :tooltip "Put the selected frames in the stash")
            (photo-picker-stash-all-button picker)
            (button "Add All ►" nil (lambda () (picker-stash-all picker)) :width 88
                    :tooltip "Put every frame of this folder in the stash")
            (photo-picker-unstash-button picker)
            (button "◄ Remove" nil (lambda () (picker-unstash-selected picker)) :width 88
                    :tooltip "Take the selected frames out of the stash"))
      (setf (photo-picker-filter-choice picker)
            (choice '("RAW photographs" "All pictures" "Everything")
                    (lambda (label)
                      (setf (photo-picker-filter picker)
                            (cond ((string= label "All pictures") :pictures)
                                  ((string= label "Everything") :all)
                                  (t :raw)))
                      (picker-refilter picker))
                    :width 170 :tooltip "Which files the folder shows")
            (photo-picker-sort-choice picker)
            (choice '("By name" "Newest first")
                    (lambda (label)
                      (setf (photo-picker-sort picker)
                            (if (string= label "Newest first") :newest :name))
                      (picker-refilter picker))
                    :width 130 :tooltip "The order of the frames"))
      (setf (photo-picker-select-all-button picker)
            (button "Select All" nil
                    (lambda ()
                      (let ((browse (photo-picker-browse picker)))
                        (picker-selection-all (picker-grid-selection browse)
                                              (picker-grid-count browse)))
                      (picker-update-status picker)
                      (picker-redraw picker))
                    :width 100))
      (setf (photo-picker-hint-label picker)
            (lightfast:make-label
             :parent window :x 0 :y 0 :width 300 :height 26
             :label "Stash frames from as many folders as you like, then add them all at once"))
      (setf (photo-picker-clear-stash-button picker)
            (button "Clear Stash" nil (lambda () (picker-clear-stash picker)) :width 110))
      (setf (photo-picker-cancel-button picker)
            (button "Cancel" nil (lambda () (picker-cancel picker)) :width 96))
      (setf (photo-picker-accept-button picker)
            (button "Add Photographs" :import (lambda () (picker-accept picker))
                    :width 200 :return-p t
                    :tooltip "Add the stash to the project — or the selection, when nothing is stashed")))
    (lightfast:on-resize window (lambda (widget)
                                  (declare (ignore widget))
                                  (picker-layout picker)))
    (picker-layout picker)
    picker))

(defun photo-picker-open (picker &key title directory on-pick (accept-label "Add"))
  "Show PICKER over its owner, in DIRECTORY or where it was last, and call
ON-PICK with the chosen pathnames when the photographer accepts."
  (let ((settings (picker-read-settings)))
    (setf (photo-picker-on-pick picker) on-pick
          (photo-picker-accept-label picker) accept-label
          (photo-picker-recent picker) (or (getf settings :recent) '()))
    (when (getf settings :filter)
      (setf (photo-picker-filter picker) (getf settings :filter)
            (lightfast:value (photo-picker-filter-choice picker))
            (ecase (getf settings :filter)
              (:raw "RAW photographs") (:pictures "All pictures") (:all "Everything"))))
    (when (getf settings :sort)
      (setf (photo-picker-sort picker) (getf settings :sort)
            (lightfast:value (photo-picker-sort-choice picker))
            (ecase (getf settings :sort) (:name "By name") (:newest "Newest first"))))
    (when title
      (setf (lightfast:label (photo-picker-window picker)) title))
    ;; Where it was last, first: the picker remembers across sessions
    ;; where the application only remembers within one.
    (let ((start (or (let ((remembered (getf settings :directory)))
                       (and remembered (probe-file remembered)))
                     (and directory (probe-file directory) directory)
                     (user-homedir-pathname))))
      (unless (picker-show-directory picker start :remember nil)
        (picker-show-directory picker (user-homedir-pathname) :remember nil)))
    (picker-update-status picker)
    (when (photo-picker-owner picker)
      (place-dialog-over (photo-picker-window picker) (photo-picker-owner picker)))
    (lightfast:show (photo-picker-window picker))
    (lightfast:take-focus (picker-grid-canvas (photo-picker-browse picker)))
    picker))
