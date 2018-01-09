(load "matrix.lisp")
(load "clip.lisp")

(defparameter *w* 640)
(defparameter *h* 480)

(defparameter *eye* (make-array 3 :element-type 'single-float))

(defparameter *project-mat*
  (frustum-mat 45 (/ 4 3) 0.3 10))

(defun vec4-ndc (vec4)
  "transform a vec4 to NDC(Normalized Device Coordinate)
   x,y [-1.0, +1.0]
   z   [0.0, 1.0]"
  (let* ((x (aref vec4 0)) (y (aref vec4 1)) (z (aref vec4 2))
         (w (aref vec4 3))
         (nx (/ x w)) (ny (/ y w)) (nz (/ z w)))
    (make-array 4 :element-type 'single-float
                :initial-contents
                `(,nx ,ny ,nz 1.0))))

