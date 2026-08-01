(defpackage #:orfeus
  (:use #:cl)
  (:import-from #:cffi
                #:defcfun
                #:load-foreign-library)
  (:export
   #:main
   #:native-bridge-available-p
   #:native-bridge-version
   #:native-library-unavailable
   #:orfeus-error
   #:orfeus-version))

(in-package #:orfeus)
