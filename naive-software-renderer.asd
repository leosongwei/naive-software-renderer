(in-package :cl-user)

(defpackage :naive-software-renderer-asdf (:use :cl :cl-user :asdf))
(in-package :naive-software-renderer-asdf)

(defsystem naive-software-renderer
  :version "1.0"
  :author "Song Wei <leo_songwei@outlook.com>"
  :licence "GPL v3"
  :components
  ((:static-file "libcutils.so" :pathname "cutils/")
   (:file "package")
   (:module "algorithm"
            :pathname "./"
            :depends-on ("package")
            :components ((:file "utils")
                         (:file "math")
                         (:file "clip")
                         (:file "wavefront-obj")
                         (:file "transform")
                         (:file "model")))
   (:module "rasterize"
            :pathname "./"
            :depends-on ("package" "algorithm")
            :components ((:file "cffi")
                         (:file "rasterization"))))
  :depends-on (:cffi
                :sdl2
                :cl-autowrap
                :parse-number))
