(ql:quickload 'cffi)
(ql:quickload 'parse-number)

(load "utils.lisp")
(load "matrix.lisp")
(load "clip.lisp")
(load "wavefront-obj.lisp")
(load "transform.lisp")
(load "model.lisp")


;;;; window coordinate (x y):
;; (0, 0) (1, 0)
;; (0, 1) (1, 1)
(defparameter *pixel-buffer* nil)
(defparameter *sdl2-surface* nil)
(defparameter *sdl2-pixel-buffer* nil)
(defparameter *sdl2-window* nil)
(defparameter *sdl2-renderer* nil)

(defparameter *w* 640)
(defparameter *h* 480)
(load "init.lisp")

(defparameter *eye* (make-array 3 :element-type 'single-float))

(defparameter *project-mat*
  (frustum-mat 45 (/ 4 3) 0.3 10))

;; todo:
;; establish screen buffer
;; load triangles frome obj file
;; traverse in triangles:
;;   1. do modelview transform
;;   2. project transform
;;   3. convert to NDC
;;   4. draw wire frame
;;      1. get coordinate and color
;;      2. do depth test
;;      3. draw
;;   5. swap buffer

(defstruct (gobject (:copier copy-gobject))
  triangles
  (position (make-array 3 :element-type 'single-float))
  (rotation (make-array 3 :element-type 'single-float)) ;; on x, y, z, in degree
  (scale (3d-scale 1.0))
  (trans-mat (make-array '(4 4) :element-type 'single-float)))


(defun copy-gobject (gobj)
  ;; the only thing need to be copied is the triangles list
  (make-gobject :triangles (mapcar #'copy-triangle (gobject-triangles gobj))
                :position (gobject-position gobj)
                :rotation (gobject-rotation gobj)
                :scale (gobject-scale gobj)
                :trans-mat (gobject-trans-mat gobj)))

(init-window)
(c-draw-line 10 10 100 200 #xff0000ff *sdl2-pixel-buffer* *w*)
(update-win)

;; (defun draw-triangle-list (triangle-list)
;;   )

