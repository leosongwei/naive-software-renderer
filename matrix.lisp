;; (aref array row col)

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
