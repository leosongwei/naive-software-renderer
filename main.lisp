(load "matrix.lisp")

(defparameter *w* 640)
(defparameter *h* 480)

;; vertex
;;   3f: model space coord
;;   3f: normal
;;   3f: color
;;   2f: texture coord

(defstruct vertex
  (coord (make-array 4 :element-type 'single-float))
  (normal (make-array 3 :element-type 'single-float))
  (color (make-array 3 :element-type 'single-float))
  (tex-coorde (make-array 2 :element-type 'single-float)))


