(in-package #:orfeus/gui)

;;;; What one session leaves for the next: the folder the project dialogs were
;;;; last in, and the projects most recently opened or saved. Kept in one small
;;;; file under the user's configuration directory, read once at startup and
;;;; written whenever either changes. As with the picker's settings, every
;;;; value is checked on the way in, so a hand-edited or damaged file costs
;;;; nothing but what it remembered.

(defparameter *recent-project-limit* 8
  "How many projects the File menu lists under Recent Projects.")

(defun session-settings-pathname ()
  "Where the session keeps what it remembers."
  (uiop:xdg-config-home "orfeus/session.sexp"))

(defun session-valid-settings (plist)
  "PLIST with every value that is not what it should be dropped."
  (let ((directory (getf plist :project-directory))
        (recent (getf plist :recent-projects)))
    (append (when (and (stringp directory) (plusp (length directory)))
              (list :project-directory directory))
            (when (and (listp recent) (every #'stringp recent))
              (list :recent-projects
                    (subseq recent 0 (min (length recent)
                                          *recent-project-limit*)))))))

(defun session-read-settings (&optional (pathname (session-settings-pathname)))
  "The remembered settings, or nothing when there are none or they are unreadable."
  (handler-case
      (with-open-file (stream pathname :direction :input :if-does-not-exist nil)
        (when stream
          (let* ((*read-eval* nil)
                 (form (read stream nil nil)))
            (and (listp form) (session-valid-settings form)))))
    (error () nil)))

(defun session-write-settings (plist &optional (pathname (session-settings-pathname)))
  "Remember PLIST for the next session. Failing to is not worth interrupting anything."
  (handler-case
      (progn
        (ensure-directories-exist pathname)
        (with-open-file (stream pathname :direction :output :if-exists :supersede)
          (with-standard-io-syntax
            (let ((*print-readably* nil))
              (prin1 (session-valid-settings plist) stream)
              (terpri stream))))
        t)
    (error () nil)))

(defun session-remember-project (recent path)
  "RECENT with PATH moved to the front, capped at the listed limit."
  (let* ((name (namestring (pathname path)))
         (others (remove name recent :test #'string=)))
    (subseq (cons name others)
            0 (min (1+ (length others)) *recent-project-limit*))))

(defun session-existing-projects (recent)
  "RECENT without the projects whose files are gone."
  (remove-if-not (lambda (name) (ignore-errors (probe-file name))) recent))

(defun recent-project-label (number path)
  "The menu line for PATH as the NUMBERth recent project: its folder and its
file, so two projects both called project.sexp can be told apart, the number
as the mnemonic, and FLTK's own mnemonic marker doubled out of the name."
  (let* ((pathname (pathname path))
         (folder (car (last (pathname-directory pathname))))
         (name (file-namestring pathname))
         (text (if (stringp folder) (format nil "~A/~A" folder name) name)))
    (format nil "&~D ~A" number
            (with-output-to-string (out)
              (loop for char across text
                    do (write-char char out)
                       (when (char= char #\&) (write-char char out)))))))
