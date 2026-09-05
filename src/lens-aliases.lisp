(in-package #:orfeus)

(defparameter *lens-profile-aliases*
  '(("Ultron 0.7x"
     :lensfun-model "Voigtlander Ultron 40mm f/2 SLII Aspherical"
     :focal-reducer 0.71
     :crop-factor 2.0)
    ("Zeiss Planar"
     :lensfun-model "Carl Zeiss Makro-Planar T* 2/50 ZF.2"
     :focal-reducer 1.0
     :crop-factor 2.0)
    ("Zeiss Planar 0.7x"
     :lensfun-model "Carl Zeiss Makro-Planar T* 2/50 ZF.2"
     :focal-reducer 0.71
     :crop-factor 2.0))
  "Built-in Olympus nickname to Lensfun profile association list.

Each entry is (NICKNAME :LENSFUN-MODEL MODEL :FOCAL-REDUCER RATIO
:CROP-FACTOR FACTOR). User entries loaded from lenses.sexp take precedence.
A focal reducer ratio converts effective EXIF focal length back to the lens
profile focal length; crop factor is the camera sensor crop before reduction.
The bundled Lensfun database lacks the Makro-Planar 50/2 profile, so its aliases
fail explicitly rather than silently selecting a different Planar 50mm.")

(defun lens-profile-aliases-pathname ()
  "Return the portable per-user adapted-lens association-list pathname."
  (let ((configured (uiop:getenv "ORFEUS_LENS_ALIASES"))
        (xdg-home (uiop:getenv "XDG_CONFIG_HOME")))
    (pathname
     (or configured
         (if xdg-home
             (merge-pathnames "orfeus/lenses.sexp"
                              (uiop:ensure-directory-pathname xdg-home))
             (merge-pathnames ".config/orfeus/lenses.sexp"
                              (user-homedir-pathname)))))))

(defun lens-profile-alias-entry-validate (entry)
  (unless (and (consp entry) (stringp (first entry)) (listp (rest entry)))
    (project-invalid entry "expected (nickname :lensfun-model ... )"))
  (let ((properties (rest entry)))
    (unless (plist-known-keys-p
             properties '(:lensfun-model :focal-reducer :crop-factor))
      (project-invalid entry "unknown adapted-lens mapping key"))
    (let ((model (getf properties :lensfun-model))
          (reducer (getf properties :focal-reducer 1.0))
          (crop-factor (getf properties :crop-factor)))
      (unless (and (stringp model) (plusp (length model)))
        (project-invalid entry ":lensfun-model must be a nonempty string"))
      (unless (and (realp reducer) (<= 0.1 reducer 2.0))
        (project-invalid entry ":focal-reducer must be between 0.1 and 2.0"))
      (unless (and (realp crop-factor) (<= 0.1 crop-factor 10.0))
        (project-invalid entry ":crop-factor must be between 0.1 and 10.0"))))
  entry)

(defun lens-profile-aliases-read (pathname)
  "Read and validate a nickname association list with *READ-EVAL* disabled."
  (with-open-file (stream pathname :direction :input)
    (let* ((*read-eval* nil)
           (aliases (read stream nil :eof)))
      (unless (listp aliases)
        (project-invalid aliases "expected an adapted-lens association list"))
      (dolist (entry aliases)
        (lens-profile-alias-entry-validate entry))
      (unless (eq :eof (read stream nil :eof))
        (project-invalid pathname "adapted-lens file contains trailing data"))
      aliases)))

(defun effective-lens-profile-aliases ()
  (let ((pathname (lens-profile-aliases-pathname)))
    (if (probe-file pathname)
        (append (lens-profile-aliases-read pathname) *lens-profile-aliases*)
        *lens-profile-aliases*)))

(defun resolve-lens-profile-alias (nickname)
  "Resolve NICKNAME to MODEL, FOCAL-REDUCER, and CROP-FACTOR values."
  (when nickname
    (let ((entry (assoc nickname (effective-lens-profile-aliases)
                        :test #'string-equal)))
      (when entry
        (values (getf (rest entry) :lensfun-model)
                (getf (rest entry) :focal-reducer 1.0)
                (getf (rest entry) :crop-factor))))))

(defun lens-profile-alias-save (nickname model &key (focal-reducer 1.0)
                                                    crop-factor)
  "Remember that lenses described as NICKNAME use the profile named MODEL.

Written to the per-user adapted-lens file, replacing any earlier entry for the
same nickname; the built-in list is never edited. The file is what the picker
in the optics panel writes when asked to remember a choice, so that the next
photograph on the same lens gets its profile without being asked."
  (check-type nickname string)
  (check-type model string)
  (let* ((pathname (lens-profile-aliases-pathname))
         (existing (if (probe-file pathname)
                       (lens-profile-aliases-read pathname)
                       '()))
         (entry (lens-profile-alias-entry-validate
                 (list* nickname
                        :lensfun-model model
                        :focal-reducer (float focal-reducer 1.0)
                        (when crop-factor
                          (list :crop-factor (float crop-factor 1.0))))))
         (aliases (cons entry
                        (remove nickname existing
                                :key #'first :test #'string-equal))))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
      (with-standard-io-syntax
        (let ((*print-readably* nil)
              (*print-pretty* t)
              (*print-case* :downcase))
          (format stream ";;; Adapted lenses: (nickname :lensfun-model ...)~%")
          (format stream ";;; Written by Orfeus; edit freely.~%")
          (prin1 aliases stream)
          (terpri stream))))
    entry))
