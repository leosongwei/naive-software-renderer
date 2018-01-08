(< -0.000 0.0)

(block :test
  (return-from :test (values 1 2 3)))

(defun clip-line-x+ (x1 y1 z1 x2 y2 z2)
  "clip line with plane x=+1.0, leave part which x<=+1.0"
  (block :clip
    (cond ((and (> x1 1.0) (> x2 1.0))
           (return-from :clip (values nil
                                      0.0 0.0 0.0
                                      0.0 0.0 0.0)))
          ((and (<= x1 1.0) (<= x2 1.0))
           (return-from :clip (values t
                                      x1 y1 z1
                                      x2 y2 z2))))
    (let* ((x 1.0)
           (val (/ (- x x1)
                   (- x2 x1)))
           (y (+ y1 (* val (- y2 y1))))
           (z (+ z1 (* val (- z2 z1)))))
      (cond ((and (<= x1 x) (< x x2))
             (return-from :clip
               (values t x1 y1 z1 x y z)))
            ((and (<= x2 x) (< x x1))
             (return-from :clip
               (values t x y z x2 y2 z2)))))))

(defun clip-line-x- (x1 y1 z1 x2 y2 z2)
  "clip line with plane x=-1.0, leave part which x>=-1.0"
  (block :clip
    (cond ((and (< x1 -1.0) (< x2 -1.0))
           (return-from :clip (values nil
                                      0.0 0.0 0.0
                                      0.0 0.0 0.0)))
          ((and (>= x1 1.0) (>= x2 1.0))
           (return-from :clip (values t
                                      x1 y1 z1
                                      x2 y2 z2))))
    (let* ((x -1.0)
           (val (/ (- x x1)
                   (- x2 x1)))
           (y (+ y1 (* val (- y2 y1))))
           (z (+ z1 (* val (- z2 z1)))))
      (cond ((and (< x1 x) (<= x x2))
             (return-from :clip
               (values t x y z x2 y2 z2)))
            ((and (< x2 x) (<= x x1))
             (return-from :clip
               (values x1 y1 y2 x y z)))))))

(defun clip-line-y+ (x1 y1 z1 x2 y2 z2)
  "keep seg that y<=1.0"
  (block :clip
    (cond ((and (> y1 1.0) (> y2 1.0))
           (return-from :clip
             (values nil 0.0 0.0 0.0  0.0 0.0 0.0)))
          ((and (<= y1 1.0) (<= y2 1.0))
           (return-from :clip
             (values t x1 y1 z1 x2 y2 z2))))
    (let* ((y 1.0)
           (val (/ (- y y1)
                   (- y2 y1)))
           (x (+ x1 (* val (- x2 x1))))
           (z (+ z1 (* val (- z2 z1)))))
      (cond ((and (<= y2 y) (< y y1))
             (return-from :clip
               (values t x y z x2 y2 z2)))
            ((and (<= y1 y) (< y y2))
             (return-from :clip
               (values t x1 y1 z1 x y z)))))))

(defun clip-line-y- (x1 y1 z1 x2 y2 z2)
  "keep seg that y>=-1.0"
  (block :clip
    (cond ((and (< y1 -1.0) (< y2 -1.0))
           (return-from :clip
             (values nil 0.0 0.0 0.0  0.0 0.0 0.0)))
          ((and (<= -1.0 y1) (<= -1.0 y2))
           (return-from :clip
             (values t x1 y1 z1 x2 y2 z2))))
    (let* ((y -1.0)
           (val (/ (- y y1)
                   (- y2 y1)))
           (x (+ x1 (* val (- x2 x1))))
           (z (+ z1 (* val (- z2 z1)))))
      (cond ((and (< y1 y) (<= y y2))
             (return-from :clip
               (values t x y z x2 y2 z2)))
            ((and (< y2 y) (<= y y1))
             (return-from :clip
               (values t x1 y1 z1 x y z)))))))

(defun clip-line-z1 (x1 y1 z1 x2 y2 z2)
  "keep seg that z<=1.0"
  (block :clip
    (cond ((and (< 1.0 z1) (< 1.0 z2))
           (return-from :clip
             (values nil 0.0 0.0 0.0  0.0 0.0 0.0)))
          ((and (<= 1.0 z1) (<= 1.0 z2))
           (return-from :clip
             (values t x1 y1 z1 x2 y2 z2))))
    (let* ((z 1.0)
           (val (/ (- z z1)
                   (- z2 z1)))
           (y (+ y1 (* val (- y2 y1))))
           (x (+ x1 (* val (- x2 x1)))))
      (cond ((and (<= z2 z) (< z z1))
             (return-from :clip
               (values t x y z x2 y2 z2)))
            ((and (<= x1 z) (< z z2))
             (return-from :clip
               (values t x1 y1 z1 x y z)))))))

(defun clip-line-z0 (x1 y1 z1 x2 y2 z2)
  "keep seg that z>=0"
  (block :clip
    (cond ((and (< z1 0.0) (< z2 0.0))
           (return-from :clip
             (values nil 0.0 0.0 0.0  0.0 0.0 0.0)))
          ((and (<= 0.0 z1) (<= 0.0 z2))
           (return-from :clip
             (values t x1 y1 z1 x2 y2 z2))))
    (let* ((z 0.0)
           (val (/ (- z z1)
                   (- z2 z1)))
           (y (+ y1 (* val (- y2 y1))))
           (x (+ x1 (* val (- x2 x1)))))
      (cond ((and (< z1 z) (<= z z2))
             (return-from :clip
               (values t x y z x2 y2 z2)))
            ((and (< z2 z) (<= z z1))
             (return-from :clip
               (values t x1 y1 z1 x y z)))))))

(defun clip-triangle-x+ (x1 y1 z1 x2 y2 z2 x3 y3 z3)
  "keep part that x<=+1.0"
  )
