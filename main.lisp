(ql:quickload 'cffi)
(ql:quickload 'parse-number)

(load "utils.lisp")
(load "matrix.lisp")
(load "clip.lisp")
(load "wavefront-obj.lisp")
;;(load "model.lisp")

(defparameter *w* 640)
(defparameter *h* 480)

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
  (rotation (make-array 3 :element-type 'single-float))
  (trans-mat (make-array '(4 4) :element-type 'single-float)))

(defun copy-gobject (gobj)
  (make-gobject :triangles (mapcar #'copy-

(defun gobject-transmat-f (gobject)
  (let* ((pos (gobject-position gobject))
         (transition (3d-trans-mat (aref pos 0)
                                   (aref pos 1)
                                   (aref pos 2)))
         (rot (gobject-rotation gobject))
         (rot-x (3d-rotate-x (aref rot 0)))
         (rot-y (3d-rotate-y (aref rot 1)))
         (rot-z (3d-rotate-z (aref rot 2))))
    (

(defun gobject-transform-f (triangle transform-mat)
  )

;; (defun triangle-project-f (triangle projection-mat)
;;   )

;; (defun triangle-ndc-f (triangle)
;;   ())

;; (defun draw-triangle-list (triangle-list)
  
