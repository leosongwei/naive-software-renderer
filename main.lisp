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

(defun triangle-transform-f (triangle transform-mat)
  )

(let* ((v1 (make-vertex :coord (make-array 4 :element-type 'single-float
                                           :initial-contents '(1.0 1.0 1.0 1.0))))
       (v2 (copy-vertex v1))
       (v1-coord (vertex-coord v1)))
  (setf (aref v1-coord 0) 6.6)
  v2)

(defun triangle-project-f (triangle projection-mat)
  )

(defun triangle-ndc-f (triangle)
  ())

(defun draw-triangle-list (triangle-list)
  
