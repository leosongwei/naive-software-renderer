
(defmacro dot-offset (x y w)
  `(+ (* ,y ,w) ,x))

(defun draw-top-even-triangle-flat (x0 y0 z0 x1 y1 z1 x2 y2 z2 color z-map pixels)
  "  p0 -------- p2
       \        /
        .p0'   .p2'
         \    /
          \  /
           p1        "
  (declare (optimize (speed 3) (safety 0)))
  (let* ((fp0x (if (= x0 x1)
                   (lambda (y) (declare (ignore y)) x0)
                   (lambda (y)
                     (+ x0 (* (- x1 x0)
                              (/ (- y y0)
                                 (- y1 y0)))))))
         (fp1x (if (= x1 x2)
                   (lambda (y) (declare (ignore y)) x2)
                   (lambda (y)
                     (+ x1 (* (- x2 x1)
                              (/ (- y y1)
                                 (- y2 y1)))))))
         (height (- y1 y0))
         (zi0 (float (/ 1 z0))) ;; Linear interpolaton can apply to inverse Z
         (zi1 (float (/ 1 z1)))
         (zi2 (float (/ 1 z2))))
    (if (<= height 1)
        nil
        (dotimes (i height)
          (let* ((y (+ y0 i))
                 (pt (float (/ i height))) ;; t, interpolate
                 (x0p (floor (funcall fp0x y)))
                 (zi0p (+ (* (- 1.0 pt) zi0) (* pt zi1)))
                 (x1p (floor (funcall fp1x y)))
                 (zi2p (+ (* (- 1.0 pt) zi2) (* pt zi1)))
                 (length (- x1p x0p)))
            (if (= 0 length)
                nil
                (let* ((dzi (float (/ (- zi2p zi0p) length)))
                       (zi zi0p)
                       (x x0p))
                  (dotimes (i length)
                    (let ((z-map-depth (aref z-map x y))
                          (z (float (/ 1 zi))))
                      (if (< z z-map-depth)
                          (progn (setf (aref z-map x y) z)
                                 (setf (autowrap:c-aref pixels (dot-offset x y *w*)
                                                        :unsigned-int)
                                       color)))
                      (incf zi dzi)
                      (incf x))))))))))

;; (destroy-window)
;; (init-window :w 200 :h 200)
;; (update-win)
;; (let ((red (map-color 255 0 0))
;;       (blue (map-color 0 0 255))
;;       (z-map (make-array `(,*w* ,*h*)
;;                          :element-type 'single-float
;;                          :initial-element 1.0))
;;       (pixels *sdl2-pixel-buffer*))
;;   (clear 0 0 0)
;;   (draw-top-even-triangle-flat
;;    50 10 0.2
;;    40 150 0.2
;;    90 10 0.2
;;    red z-map pixels)
;;   (draw-top-even-triangle-flat
;;    90 10 0.2
;;    40 150 0.2
;;    180 10 0.2
;;    blue z-map pixels)
;;   (update-win)
;;   (aref z-map 88 10))

;; (let ((blue (map-color 0 0 255))
;;       (z-map (make-array `(,*w* ,*h*)
;;                          :element-type 'single-float
;;                          :initial-element 1.0))
;;       (pixels *sdl2-pixel-buffer*))
;;   (clear 0 0 0)
;;   (draw-top-even-triangle-flat
;;    0 10 0.2
;;    0 150 0.2
;;    180 10 0.2
;;    blue z-map pixels)
;;   (update-win)
;;   (aref z-map 20 26))

(defun draw-bottom-even-triangle-flat (x0 y0 z0 x1 y1 z1 x2 y2 z2 color z-map pixels)
  "        p0
          /  \
         /    \
        .p1'   .p2'
       /        \
      p1--------p2   "
  (declare (optimize (speed 3) (safety 0)))
  (let* ((fp1x (if (= x0 x1)
                   (lambda (y) (declare (ignore y)) x0)
                   (lambda (y)
                     (+ x0 (* (- x1 x0)
                              (/ (- y y0)
                                 (- y1 y0)))))))
         (fp2x (if (= x0 x2)
                   (lambda (y) (declare (ignore y)) x0)
                   (lambda (y)
                     (+ x0 (* (- x2 x0)
                              (/ (- y y0)
                                 (- y2 y0)))))))
         (height (- y1 y0))
         (zi0 (float (/ 1 z0))) ;; Linear interpolaton can apply to inverse Z
         (zi1 (float (/ 1 z1)))
         (zi2 (float (/ 1 z2))))
    (if (<= height 1)
        nil
        (dotimes (i height)
          (let* ((y (+ y0 i))
                 (pt (float (/ i height))) ;; t, interpolate
                 (x1p (floor (funcall fp1x y)))
                 (zi1p (+ (* (- 1.0 pt) zi0) (* pt zi1)))
                 (x2p (floor (funcall fp2x y)))
                 (zi2p (+ (* (- 1.0 pt) zi0) (* pt zi2)))
                 (length (- x2p x1p)))
            (if (= 0 length)
                nil
                (let* ((dzi (float (/ (- zi2p zi1p) length)))
                       (zi zi1p)
                       (x x1p))
                  (dotimes (i length)
                    (let ((z-map-depth (aref z-map x y))
                          (z (float (/ 1 zi))))
                      (if (< z z-map-depth)
                          (progn (setf (aref z-map x y) z)
                                 (setf (autowrap:c-aref pixels (dot-offset x y *w*)
                                                        :unsigned-int)
                                       color)))
                      (incf zi dzi)
                      (incf x))))))))))

;; (let ((z-map (make-array `(,*w* ,*h*)
;;                          :element-type 'single-float
;;                          :initial-element 1.0))
;;       (pixels *sdl2-pixel-buffer*))
;;   (clear 0 0 0)
;;   (let ((blue (map-color 0 0 255)))
;;     (draw-bottom-even-triangle-flat
;;      130 20 0.2
;;      100 180 0.2
;;      180 180 0.2
;;      blue z-map pixels))
;;   (let ((red (map-color 255 0 0)))
;;     (draw-bottom-even-triangle-flat
;;      130 20 0.2
;;      20 180 0.2
;;      100 180 0.2
;;      red z-map pixels))
;;   (update-win)
;;   (aref z-map 20 26))
;; (let ((z-map (make-array `(,*w* ,*h*)
;;                          :element-type 'single-float
;;                          :initial-element 1.0))
;;       (pixels *sdl2-pixel-buffer*))
;;   (clear 0 0 0)
;;   (let ((blue (map-color 0 0 255)))
;;     (draw-bottom-even-triangle-flat
;;      100 20 0.2
;;      10 180 0.2
;;      100 180 0.2
;;      blue z-map pixels))
;;   (let ((red (map-color 255 0 0)))
;;     (draw-bottom-even-triangle-flat
;;      150 20 0.1
;;      50 180 0.5
;;      150 180 0.1
;;      red z-map pixels))
;;   (update-win)
;;   (aref z-map 20 26))

(defmacro swap (a b)
  (let ((c (gensym)))
    `(let ((,c ,a))
       (setf ,a ,b)
       (setf ,b ,c))))

(defun ndc-xyz (ndc)
  (let* ((x (aref ndc 0))
         (y (aref ndc 1))
         (z (aref ndc 2)))
    (values (floor (* (* 0.5 (1- *w*)) (- x -1.0)))
            (floor (* (* 0.5 (1- *h*)) (- (- y 1.0))))
            z)))

(defmacro ndc-x (ndc)
  `(aref ,ndc 0))

(defmacro ndc-y (ndc)
  `(aref ,ndc 0))

(defmacro ndc-z (ndc)
  `(aref ,ndc 0))

(defun draw-triangle-flat (triangle color z-map pixels)
  (block :draw
    (let* ((vertices (triangle-vertices triangle))
           (v0-ndc (vertex-ndc (aref vertices 0)))
           (v1-ndc (vertex-ndc (aref vertices 1)))
           (v2-ndc (vertex-ndc (aref vertices 2))))
      ;; (if (> (ndc-y v0-ndc) (ndc-y v1-ndc)) (swap v0-ndc v1-ndc))
      ;; (if (> (ndc-y v0-ndc) (ndc-y v2-ndc)) (swap v0-ndc v2-ndc))
      ;; (if (> (ndc-y v1-ndc) (ndc-y v2-ndc)) (swap v1-ndc v2-ndc))
      (mvb-let* ((x0 y0 z0 (ndc-xyz v0-ndc))
                 (x1 y1 z1 (ndc-xyz v1-ndc))
                 (x2 y2 z2 (ndc-xyz v2-ndc)))
        (if (> y0 y1) (progn (swap x0 x1) (swap y0 y1) (swap z0 z1)))
        (if (> y0 y2) (progn (swap x0 x2) (swap y0 y2) (swap z0 z2)))
        (if (> y1 y2) (progn (swap x1 x2) (swap y1 y2) (swap z1 z2)))
        (cond ((= y0 y1)
               (progn (if (> x0 x1)
                          (progn (swap x0 x1) (swap y0 y1) (swap z0 z1)))
                      (draw-top-even-triangle-flat
                       x0 y0 z0 x2 y2 z2 x1 y1 z1 color z-map pixels)))
              ((= y1 y2)
               (progn (if (> x1 x2)
                          (progn (swap x1 x2) (swap y1 y2) (swap z1 z2)))
                      (draw-bottom-even-triangle-flat
                       x0 y0 z0 x1 y1 z1 x2 y2 z2 color z-map pixels)))
              (t
               (progn
                 ;(format t "y0:~A, y1:~A, y2:~A~%" y0 y1 y2)
                 (let* ((new-x (if (= x0 x2)
                                   x0
                                   (floor (+ x0 (* (- x2 x0)
                                                   (/ (- y1 y0)
                                                      (- y2 y0)))))))
                        (new-y y1)
                        (pt (float (/ (- y1 y0) (- y2 y0))))
                        (new-z (float (/ 1 (+ (* (- 1.0 pt) (/ 1 z0))
                                              (* pt (/ 1 z2)))))))
                   (if (< x1 new-x)
                       (progn (swap new-x x1) (swap new-y y1) (swap new-z z1)))
                   (draw-bottom-even-triangle-flat
                    x0 y0 z0 new-x new-y new-z x1 y1 z1 color z-map pixels)
                   (draw-top-even-triangle-flat
                    new-x new-y new-z x2 y2 z2 x1 y1 z1 color z-map pixels)))))))))



;; (init-window)
;; (destroy-window)
;; (init-window :w 200 :h 200)
;; (progn
;;   (clear)
;;   (let* ((v1 (make-vertex :ndc #(0.5 0.5 0.5 1.0)))
;;          (v3 (make-vertex :ndc #(0.7 -0.5 0.5 1.0)))
;;          (v2 (make-vertex :ndc #(0.1 -0.7 0.5 1.0)))
;;          (tri (build-triangle v1 v2 v3))
;;          (z-map (make-z-map))
;;          (pixels *sdl2-pixel-buffer*))
;;     (draw-triangle-flat tri (map-color 255 255 0) z-map pixels))
;;   (update-win))

;; (progn
;;   (clear)
;;   (let* ((v1 (make-vertex :ndc #(-0.0 0.5 0.5 1.0)))
;;          (v2 (make-vertex :ndc #(-0.7 -0.7 0.5 1.0)))
;;          (v3 (make-vertex :ndc #(0.5 0.2 0.5 1.0)))
;;          (tri (build-triangle v1 v2 v3))
;;          (z-map (make-z-map))
;;          (pixels *sdl2-pixel-buffer*))
;;     (draw-triangle-flat tri (map-color 255 255 0) z-map pixels))
;;   (update-win))

;; (progn
;;   (clear)
;;   (let* ((v1 (make-vertex :ndc #(0.5 0.1 0.5 1.0)))
;;          (v2 (make-vertex :ndc #(-0.5 0.1 0.5 1.0)))
;;          (v3 (make-vertex :ndc #(0.0 -0.4 0.5 1.0)))
;;          (tri (build-triangle v1 v2 v3))
;;          (z-map (make-z-map))
;;          (pixels *sdl2-pixel-buffer*))
;;     (draw-triangle-flat tri (map-color 255 255 0) z-map pixels))
;;   (update-win))

;; (progn
;;   (clear)
;;   (let* ((o (make-vertex :ndc #(0.0 0.0 0.5 1.0)))
;;          (a (make-vertex :ndc #(0.0 0.5 0.5 1.0)))
;;          (b (make-vertex :ndc #(-0.5 0.0 0.5 1.0)))
;;          (c (make-vertex :ndc #(0.0 -0.5 0.5 1.0)))
;;          (d (make-vertex :ndc #(0.5 0.0 0.5 1.0)))
;;          (t1 (build-triangle a o d))
;;          (t2 (build-triangle a b o))
;;          (t3 (build-triangle b c o))
;;          (t4 (build-triangle o c d))
;;          (z-map (make-z-map))
;;          (pixels *sdl2-pixel-buffer*))
;;     (draw-triangle-flat t1 (map-color 255 0 0) z-map pixels)
;;     (draw-triangle-flat t2 (map-color 0 255 0) z-map pixels)
;;     (draw-triangle-flat t3 (map-color 0 0 255) z-map pixels)
;;     (draw-triangle-flat t4 (map-color 255 255 0) z-map pixels)
;;     (update-win)))

;; (let* ((o (make-vertex :ndc #(0.0 0.0 0.5 1.0)))
;;        (a (make-vertex :ndc #(0.0 0.5 0.5 1.0)))
;;        (d (make-vertex :ndc #(0.5 0.0 0.5 1.0)))
;;        (tri (build-triangle a o d))
;;        (vertices (triangle-vertices tri))
;;        (center-ndc (center-ndc vertices)))
;;   (sort vertices
;;         (lambda (a b)
;;           (less-ccw-ndc
;;            center-ndc
;;            (vertex-ndc a)
;;            (vertex-ndc b)))))
