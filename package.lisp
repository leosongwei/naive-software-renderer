;; (ql:quickload 'parse-number)
;; (ql:quickload "sdl2")
;; (ql:quickload "cl-autowrap")
;; (ql:quickload "cffi")

;; (load "utils.lisp")
;; (load "matrix.lisp")
;; (load "clip.lisp")
;; (load "wavefront-obj.lisp")
;; (load "transform.lisp")
;; (load "model.lisp")

(in-package :cl-user)

(defpackage :naive-software-renderer
  (:use :cl :cl-user)
  (:nicknames :naive)
  (:export :init-window
           :destroy-window
           :update-win
           :clear
           :get-cpu-count
           :make-z-map
           :make-vec2
           :make-vec3
           :make-vec4
           :frustum-mat
           :make-vertex
           :vertex
           :vertex-coord
           :vertex-normal
           :vertex-ndc
           :vertex-tex-coord
           :vertices-ndc
           :texture
           :sample-texture
           :wavefront-file-to-modelmesh
           :read-image-to-texture
           :*w*
           :*h*
           :*SDL2-PIXEL-BUFFER*
           :*sdl2-renderer*
           :make-triangle
           :build-triangle
           :triangle
           :triangle-vertices
           :triangle-normal3
           :modelmesh
           :modelmesh-vertices
           :modelmesh-tex-coords
           :modelmesh-normals
           :modelmesh-faces
           :3d-trans-mat
           :3d-scale
           :3d-rotate-x
           :3d-rotate-y
           :3d-rotate-z
           :mul-44-44
           :apply-transform
           :build-triangle-from-face
           :vec4->vec3
           :vec2+ :vec2- :vec2* :vec2/
           :vec3+ :vec3- :vec3* :vec3/
           :vec4+ :vec4- :vec4* :vec4/ :vec4-ndc
           :vec3-int-color :vec3-dot :vec3-clamp :vec3-normalize :vec3-reflection
           :map-color
           :clip-triangle
           :clip-line
           :draw-triangle-wire-ndc
           :draw-triangle-flat
           :draw-triangle-gouraud
           :draw-triangle-phong))

(in-package :naive-software-renderer)

;;;; window coordinate (x y):
;; (0, 0) (1, 0)
;; (0, 1) (1, 1)
(defparameter *pixel-buffer* nil)
(defparameter *sdl2-surface* nil)
(defparameter *sdl2-pixel-buffer* nil)
(defparameter *sdl2-window* nil)
(defparameter *sdl2-renderer* nil)

;; this declaim actually slow down the program???
(declaim (type (unsigned-byte 16) *w* *h*))

(defparameter *w* 640)
(defparameter *h* 480)

;; (load "init.lisp")
;; (load "rasterization.lisp")



