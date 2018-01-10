;; vertex
;;   3f: model space coord
;;   3f: normal
;;   3f: color
;;   2f: texture coord

(defstruct vertex
  (coord (make-array 4 :element-type 'single-float))
  (ndc (make-array 4 :element-type 'single-float))
  (normal (make-array 3 :element-type 'single-float))
  (color (make-array 3 :element-type 'single-float))
  (tex-coorde (make-array 2 :element-type 'single-float)))

(defstruct triangle
  (vertices (make-array 3 :element-type 'vertex
                        :initial-contents `(,(make-vertex)
                                             ,(make-vertex)
                                             ,(make-vertex)))))

(defun build-triangle (v1 v2 v3)
  (make-triangle :vertices (make-array 3 :element-type 'vertex
                                       :initial-contents `(,v1 ,v2 ,v3))))

;; ---------------------------------

(defun multiply-mat (a b)
  (let* ((row (array-dimension a 0))
         (col (array-dimension b 1))
         (col-row (if (= (array-dimension a 1) (array-dimension b 0))
                      (array-dimension a 1)
                      (progn
                        (error "multiply-mat: arrays not match")
                        -233)))
         (m (make-array `(,row ,col) :element-type 'single-float)))
    (dotimes (r row)
      (dotimes (c col)
        (setf (aref m r c)
              (let ((sum 0.0))
                (dotimes (j col-row)
                  (incf sum (* (aref a r j) (aref b j c))))
                sum))))
    m))

(defun mul-33-33 (a b)
  (let ((m (make-array '(3 3) :element-type 'single-float)))
    (dotimes (r 3)
      (dotimes (c 3)
        (setf (aref m r c)
              (+ (* (aref a r 0) (aref b 0 c))
                 (* (aref a r 1) (aref b 1 c))
                 (* (aref a r 2) (aref b 2 c))))))
    m))

(defun mul-44-44 (a b)
  (let ((m (make-array '(4 4) :element-type 'single-float)))
    (dotimes (r 4)
      (dotimes (c 4)
        (let ((sum 0.0))
          (dotimes (j 4)
            (incf sum (* (aref a r j) (aref b j c))))
          (setf (aref m r c) sum))))
    m))

(defun load-i-33 ()
  (make-array '(3 3) :element-type 'single-float
              :initial-contents '((1.0 0.0 0.0)
                                  (0.0 1.0 0.0)
                                  (0.0 0.0 1.0))))

(defun load-i-44 ()
  (make-array '(4 4) :element-type 'single-float
              :initial-contents '((1.0 0.0 0.0 0.0)
                                  (0.0 1.0 0.0 0.0)
                                  (0.0 0.0 1.0 0.0)
                                  (0.0 0.0 0.0 1.0))))

(defun mul-33-v3 (a v)
  (let ((vec (make-array '(3) :element-type 'single-float)))
    (dotimes (r 3)
      (setf (aref vec r)
            (let ((sum 0.0))
              (dotimes (j 3)
                (incf sum (* (aref a r j) (aref v j))))
              sum)))
    vec))

(defun mul-44-v4 (a v)
  (let ((vec (make-array '(4) :element-type 'single-float)))
    (dotimes (r 4)
      (setf (aref vec r)
            (let ((sum 0.0))
              (dotimes (j 4)
                (incf sum (* (aref a r j) (aref v j))))
              sum)))
    vec))

#|
(let ((m33 #2a((1.0 2.0 3.0)
               (4.0 5.0 6.0)
               (7.0 8.0 9.0)))
      (v3  #1a(1.0 2.0 3.0))
      (m44 #2a((1.0 2.0 3.0 4.0)
               (4.0 3.0 2.0 1.0)
               (1.0 2.0 3.0 4.0)
               (5.0 6.0 7.0 8.0)))
      (v4 #1a(1.0 2.0 3.0 4.0)))
  (mul-33-v3 m33 v3)
  (mul-44-44 m44 m44)
  (mul-44-v4 m44 v4)
  (multiply-mat m44 m44))
|#


#|
M : matrix([1, 2, 3],
        [4, 5, 6],
        [7, 8, 9]);
V : transpose(matrix([1,2,3]));
M4 : matrix(
    [1, 2, 3, 4],
    [4, 3, 2, 1],
    [1, 2, 3, 4],
    [5, 6, 7, 8]);
V4 : transpose(matrix([1.0, 2.0, 3.0, 4.0]));
|#

(defconstant M_PI 3.14159265358979323846)

(defun 3d-trans-mat (x y z)
  (make-array '(4 4) :element-type 'single-float
              :initial-contents `((1.0 0.0 0.0 ,x)
                                  (0.0 1.0 0.0 ,y)
                                  (0.0 0.0 1.0 ,z)
                                  (0.0 0.0 0.0 1.0))))

(defun 3d-rotate-x (degree)
  (let* ((theta (* (/ degree 180) M_PI))
         (sin (sin theta))
         (cos (cos theta)))
    (make-array '(4 4) :element-type 'single-float
                :initial-contents
                `((1.0  0.0   0.0      0.0)
                  (0.0  ,cos  ,(- sin) 0.0)
                  (0.0  ,sin  ,cos     0.0)
                  (0.0  0.0   0.0      1.0)))))

(defun 3d-rotate-y (degree)
  (let* ((theta (* (/ degree 180) M_PI))
         (sin (sin theta))
         (cos (cos theta)))
    (make-array '(4 4) :element-type 'single-float
                :initial-contents
                `((,cos      0.0  ,sin  0.0)
                  (0.0       1.0  0.0   0.0)
                  (,(- sin)  0.0  ,cos  0.0)
                  (0.0       0.0  0.0   1.0)))))

(defun 3d-rotate-z (degree)
  (let* ((theta (* (/ degree 180) M_PI))
         (sin (sin theta))
         (cos (cos theta)))
    (make-array '(4 4) :element-type 'single-float
                :initial-contents
                `((,cos  ,(- sin)  0.0  0.0)
                  (,sin  ,cos      0.0  0.0)
                  (0.0   0.0       1.0  0.0)
                  (0.0   0.0       0.0  1.0)))))

(defun 3d-scale (s)
  (make-array '(4 4) :element-type 'single-float
              :initial-contents `((,s  0.0 0.0 0.0)
                                  (0.0 ,s  0.0 0.0)
                                  (0.0 0.0 ,s  0.0)
                                  (0.0 0.0 0.0 1.0))))

(tan (/ pi 6))

(defun frustum-mat (degree-y aspect znear zfar)
  ;; https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluPerspective.xml
  ;; fov-y: angle in degree
  ;; aspect: x/y
  ;; output: project vertices into view space, with w.
  ;; **Z will be resulted in positive**
  (let* ((fov-y (* (/ degree-y 180) M_PI))
         (f (/ 1 (tan (/ fov-y 2))))
         (a (/ (+ zfar znear) (- znear zfar)))
         (b (/ (* 2 zfar znear) (- znear zfar))))
    (make-array '(4 4) :element-type 'single-float
                :initial-contents
                `((,(/ f aspect)  0.0   0.0  0.0)
                  (0.0            ,f    0.0  0.0)
                  (0.0            0.0   ,a   ,b)
                  (0.0            0.0  -1.0  0.0)))))

;; ---------------------------------------------------------
;; Vector Calculation
;; ---------------------------------------------------------
;; vec2
(defun vec4+ (vec1 vec2)
  (make-array 4 :element-type 'single-float
              :initial-contents
              `(,(+ (aref vec1 0) (aref vec2 0))
                 ,(+ (aref vec1 1) (aref vec2 1))
                 ,(+ (aref vec1 2) (aref vec2 2))
                 ,(+ (aref vec1 3) (aref vec2 3)))))

(defun vec4- (vec1 vec2)
  (make-array 4 :element-type 'single-float
              :initial-contents
              `(,(- (aref vec1 0) (aref vec2 0))
                 ,(- (aref vec1 1) (aref vec2 1))
                 ,(- (aref vec1 2) (aref vec2 2))
                 ,(- (aref vec1 3) (aref vec2 3)))))

(defun vec4* (vec n)
  (make-array 4 :element-type 'single-float
              :initial-contents
              `(,(* (aref vec 0) n)
                 ,(* (aref vec 1) n)
                 ,(* (aref vec 2) n)
                 ,(* (aref vec 3) n))))

(defun vec4/ (vec n)
  (make-array 4 :element-type 'single-float
              :initial-contents
              `(,(/ (aref vec 0) n)
                 ,(/ (aref vec 1) n)
                 ,(/ (aref vec 2) n)
                 ,(/ (aref vec 3) n))))
;; ---------------------------------------------------------
;; vec3
(defun vec3+ (vec1 vec2)
  (make-array 3 :element-type 'single-float
              :initial-contents
              `(,(+ (aref vec1 0) (aref vec2 0))
                 ,(+ (aref vec1 1) (aref vec2 1))
                 ,(+ (aref vec1 2) (aref vec2 2)))))

(defun vec3- (vec1 vec2)
  (make-array 3 :element-type 'single-float
              :initial-contents
              `(,(- (aref vec1 0) (aref vec2 0))
                 ,(- (aref vec1 1) (aref vec2 1))
                 ,(- (aref vec1 2) (aref vec2 2)))))

(defun vec3* (vec n)
  (make-array 3 :element-type 'single-float
              :initial-contents
              `(,(* (aref vec 0) n)
                 ,(* (aref vec 1) n)
                 ,(* (aref vec 2) n))))

(defun vec3/ (vec n)
  (make-array 3 :element-type 'single-float
              :initial-contents
              `(,(/ (aref vec 0) n)
                 ,(/ (aref vec 1) n)
                 ,(/ (aref vec 2) n))))

(defun vec3-normalize (vec)
  (let* ((length (sqrt (+ (expt (aref vec 0) 2)
                          (expt (aref vec 1) 2)
                          (expt (aref vec 2) 2)))))
    (make-array 3 :element-type 'single-float
                :initial-contents
                `(,(/ (aref vec 0) length)
                   ,(/ (aref vec 1) length)
                   ,(/ (aref vec 2) length)))))
;; ---------------------------------------------------------
;; vec2
(defun vec2+ (vec1 vec2)
  (make-array 2 :element-type 'single-float
              :initial-contents
              `(,(+ (aref vec1 0) (aref vec2 0))
                 ,(+ (aref vec1 1) (aref vec2 1)))))

(defun vec2- (vec1 vec2)
  (make-array 2 :element-type 'single-float
              :initial-contents
              `(,(- (aref vec1 0) (aref vec2 0))
                 ,(- (aref vec1 1) (aref vec2 1)))))

(defun vec2* (vec n)
  (make-array 2 :element-type 'single-float
              :initial-contents
              `(,(* (aref vec 0) n)
                 ,(* (aref vec 1) n))))

(defun vec2/ (vec n)
  (make-array 2 :element-type 'single-float
              :initial-contents
              `(,(/ (aref vec 0) n)
                 ,(/ (aref vec 1) n))))

;; -------------------------------------------

(defun interpolate-vertex (v1 v2 pt)
  (make-vertex :coord (let ((c1 (vertex-coord v1))
                            (c2 (vertex-coord v2)))
                        (vec4+ c1 (vec4* (vec4- c2 c1) pt)))
               :ndc (let ((ndc1 (vertex-ndc v1))
                          (ndc2 (vertex-ndc v2)))
                      (vec4+ ndc1 (vec4* (vec4- ndc2 ndc1) pt)))
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
