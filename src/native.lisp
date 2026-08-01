(in-package #:orfeus)

(defvar *native-library* nil
  "The loaded CFFI library handle for the Orfeus Rust bridge.")

(defcfun ("orfeus_bridge_abi_version" %native-bridge-abi-version) :uint32)

(defun native-library-candidates ()
  (remove nil
          (list (uiop:getenv "ORFEUS_NATIVE_LIBRARY")
                (namestring
                 (asdf:system-relative-pathname
                  '#:orfeus "native/target/release/liborfeus_native.so"))
                (namestring
                 (asdf:system-relative-pathname
                  '#:orfeus "native/target/debug/liborfeus_native.so")))
))

(defun native-library-load ()
  (or *native-library*
      (let ((paths (native-library-candidates))
            (last-cause "no candidate exists"))
        (dolist (path paths)
          (when (probe-file path)
            (handler-case
                (return-from native-library-load
                  (setf *native-library* (load-foreign-library path)))
              (error (condition)
                (setf last-cause condition)))))
        (error 'native-library-unavailable
               :paths paths
               :cause last-cause))))

(defun native-bridge-available-p ()
  "Return true when the Rust native bridge can be loaded."
  (handler-case
      (progn
        (native-library-load)
        t)
    (native-library-unavailable () nil)))

(defun native-bridge-version ()
  "Return the loaded Rust bridge ABI version."
  (native-library-load)
  (%native-bridge-abi-version))
