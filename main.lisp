(load "matrix.lisp")
;(load "clip.lisp")

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

(defun interpolate-vertex (v1 v2 pt)
  (make-vertex :coord (let ((c1 (vertex-coord v1))
                            (c2 (vertex-coord v2)))
                        (vec4+ c1 (vec4* (vec4- c2 c1) pt)))
               :normal (let ((n1 (vertex-normal v1))
                             (n2 (vertex-normal v2)))
                         (vec3+ (vec3* n1 (- 1 pt))
                                (vec3* n2 pt)))
               :color (let ((color1 (vertex-color v1))
                            (color2 (vertex-color v2)))
                        (vec3+ (vec3* color1 (- 1 pt))
                               (vec3* color2 pt)))
               :tex-coorde (let ((tc1 (vertex-tex-coorde v1))
                                 (tc2 (vertex-tex-coorde v2)))
                             (vec2+ tc1 (vec2* (vec2- tc2 tc1) pt)))))



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

