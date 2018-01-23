
(defmacro dot-offset (x y w)
  `(+ (* ,y ,w) ,x))

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
  (declare (optimize (speed 3) (safety 0)))
  (block :draw
    (let* ((vertices (triangle-vertices triangle))
           (v0-ndc (vertex-ndc (aref vertices 0)))
           (v1-ndc (vertex-ndc (aref vertices 1)))
           (v2-ndc (vertex-ndc (aref vertices 2))))
      (mvb-let* ((x0 y0 z0 (ndc-xyz v0-ndc))
                 (x1 y1 z1 (ndc-xyz v1-ndc))
                 (x2 y2 z2 (ndc-xyz v2-ndc)))
        (if (> y0 y1) (progn (swap x0 x1) (swap y0 y1) (swap z0 z1)))
        (if (> y0 y2) (progn (swap x0 x2) (swap y0 y2) (swap z0 z2)))
        (if (> y1 y2) (progn (swap x1 x2) (swap y1 y2) (swap z1 z2)))
        (if (or (= y0 y2) (= x0 x1 x2)) (return-from :draw))
        (let* ((height-total (- y2 y0))
               (zi0 (float (/ 1.0 z0)))
               (zi1 (float (/ 1.0 z1)))
               (zi2 (float (/ 1.0 z2))))
          ;; draw top
          (let* ((height (- y1 y0)))
            (dotimes (i height)
              (let* ((y (+ y0 i))
                     (t1 (float (/ i height)))
                     (zi1p (float (+ (* zi0 (- 1.0 t1))
                                     (* zi1 t1))))
                     (t2 (float (/ i height-total)))
                     (zi2p (float (+ (* zi0 (- 1.0 t2))
                                     (* zi2 t2))))
                     (x1p (if (= x0 x1) x0 (floor (+ x0 (* (- x1 x0)
                                                           (/ (- y y0)
                                                              (- y1 y0)))))))
                     (x2p (if (= x0 x2) x0 (floor (+ x0 (* (- x2 x0)
                                                           (/ (- y y0)
                                                              (- y2 y0))))))))
                (if (= 0 (- x1p x2p))
                    nil
                    (progn
                      (if (> x1p x2p) (progn (swap x1p x2p)
                                             (swap zi1p zi2p)))
                      (let* ((length (- x2p x1p))
                             (x x1p)
                             (zi zi1p)
                             (dzi (float (/ (- zi2p zi1p) length))))
                        (dotimes (j length)
                          (let ((z (float (/ 1.0 zi)))
                                (z-map-depth (aref z-map x y)))
                            (if (< z z-map-depth)
                                (progn
                                  (setf (aref z-map x y) z)
                                  (setf (autowrap:c-aref pixels (dot-offset x y *w*)
                                                         :unsigned-int)
                                        color)))
                            (incf zi dzi)
                            (incf x)))))))))
          ;; draw bottom
          (let* ((height (- y2 y1))
                 (dy0y1 (- y1 y0)))
            (dotimes (i height)
              (let* ((y (+ y1 i))
                     (t1 (float (/ i height)))
                     (zi1p (float (+ (* zi1 (- 1.0 t1))
                                     (* zi2 t1))))
                     (t2 (float (/ (+ i dy0y1) height-total)))
                     (zi2p (float (+ (* zi0 (- 1.0 t2))
                                     (* zi2 t2))))
                     (x1p (if (= x1 x2) x1 (floor (+ x1 (* (- x2 x1)
                                                           (/ (- y y1)
                                                              (- y2 y1)))))))
                     (x2p (if (= x0 x2) x0 (floor (+ x0 (* (- x2 x0)
                                                           (/ (- y y0)
                                                              (- y2 y0))))))))
                (if (= 0 (- x1p x2p))
                    nil
                    (progn
                      (if (> x1p x2p) (progn (swap x1p x2p)
                                             (swap zi1p zi2p)))
                      (let* ((length (- x2p x1p))
                             (x x1p)
                             (zi zi1p)
                             (dzi (float (/ (- zi2p zi1p) length))))
                        (dotimes (j length)
                          (let ((z (float (/ 1.0 zi)))
                                (z-map-depth (aref z-map x y)))
                            (if (< z z-map-depth)
                                (progn
                                  (setf (aref z-map x y) z)
                                  (setf (autowrap:c-aref pixels (dot-offset x y *w*)
                                                         :unsigned-int)
                                        color)))
                            (incf zi dzi)
                            (incf x))))))))))))))

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
