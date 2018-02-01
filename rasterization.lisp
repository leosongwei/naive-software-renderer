(in-package :naive-software-renderer)

(defun ndc-xy (ndc)
  (let* ((x (aref ndc 0))
         (y (aref ndc 1)))
    (values (floor (* (* 0.5 (1- *w*)) (- x -1.0)))
            (floor (* (* 0.5 (1- *h*)) (- (- y 1.0)))))))

(defun draw-line (v1 v2)
  (multiple-value-bind (x1 y1) (ndc-xy (vertex-ndc v1))
    (multiple-value-bind (x2 y2) (ndc-xy (vertex-ndc v2))
      (c-draw-line x1 y1 x2 y2 #xffff00ff *sdl2-pixel-buffer* *w*))))

(defun draw-triangle-wire-ndc (triangle)
  (let* ((vertices (triangle-vertices triangle))
         (v1 (aref vertices 0))
         (v2 (aref vertices 1))
         (v3 (aref vertices 2)))
    (draw-line v1 v2)
    (draw-line v2 v3)
    (draw-line v3 v1)))
;; (clear)
;; (let* ((v1 (make-vertex :ndc #(-0.5 0.5 0.5 1.0)))
;;        (v2 (make-vertex :ndc #(-0.2 -0.5 0.5 1.0)))
;;        (v3 (make-vertex :ndc #(0.5 0.5 0.5 1.0)))
;;        (tri (build-triangle v1 v2 v3)))
;;   (draw-triangle-wire-ndc tri))
;; (update-win)

(defmacro dot-offset (x y w)
  `(+ (* ,y ,w) ,x))

(defun make-z-map (&optional (w *w*) (h *h*))
  (make-array `(,w ,h)
              :element-type 'single-float
              :initial-element 1.0))

(defun vec3-int-color (vec3)
  (let ((clamped (vec3-clamp vec3)))
    (map-color (floor (* 255 (aref clamped 0)))
               (floor (* 255 (aref clamped 1)))
               (floor (* 255 (aref clamped 2))))))

(defmacro swap (a b)
  (let ((c (gensym)))
    `(let ((,c ,a))
       (setf ,a ,b)
       (setf ,b ,c))))

(defun ndc-xyz (ndc w h)
  (let* ((x (aref ndc 0))
         (y (aref ndc 1))
         (z (aref ndc 2)))
    (values (floor (* (* 0.5 (1- w)) (- x -1.0)))
            (floor (* (* 0.5 (1- h)) (- (- y 1.0))))
            z)))

(defun draw-triangle-flat (triangle color z-map pixels &optional (w *w*) (h *h*))
  (declare ;;(optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array single-float (* *)) z-map)
           (type (unsigned-byte 32) color))
  (block :draw
    (let* ((vertices (triangle-vertices triangle))
           (v0-ndc (vertex-ndc (aref vertices 0)))
           (v1-ndc (vertex-ndc (aref vertices 1)))
           (v2-ndc (vertex-ndc (aref vertices 2))))
      (mvb-let* ((x0 y0 z0 (ndc-xyz v0-ndc w h))
                 (x1 y1 z1 (ndc-xyz v1-ndc w h))
                 (x2 y2 z2 (ndc-xyz v2-ndc w h)))
                (declare (type (signed-byte 16) x0 y0 x1 y1 x2 y2)
                         (type single-float z0 z1 z2))
        (if (> y0 y1) (progn (swap x0 x1) (swap y0 y1) (swap z0 z1)))
        (if (> y0 y2) (progn (swap x0 x2) (swap y0 y2) (swap z0 z2)))
        (if (> y1 y2) (progn (swap x1 x2) (swap y1 y2) (swap z1 z2)))
        (if (or (= y0 y2)
                (= x0 x1 x2)) (return-from :draw))
        (let* ((height-total (- y2 y0))
               (zi0 (float (/ 1.0 z0)))
               (zi1 (float (/ 1.0 z1)))
               (zi2 (float (/ 1.0 z2))))
          ;; draw top
          (let* ((height (- y1 y0)))
            (declare (type (unsigned-byte 32) height))
            (dotimes (i height)
              (let* ((y (+ y0 i))
                     (t1 (float (/ (float i) height)))
                     (zi1p (float (+ (* zi0 (- 1.0 t1))
                                     (* zi1 t1))))
                     (t2 (float (/ (float i) height-total)))
                     (zi2p (float (+ (* zi0 (- 1.0 t2))
                                     (* zi2 t2))))
                     (x1p (floor (itplt-num (float x0) (float x1) t1)))
                     (x2p (floor (itplt-num (float x0) (float x2) t2))))
                (declare (type (unsigned-byte 16) y x1p x2p)
                         (type single-float t1 zi1p t2 zi2p))
                (if (= 0 (- x1p x2p))
                    nil
                    (progn
                      (if (> x1p x2p) (progn (swap x1p x2p)
                                             (swap zi1p zi2p)))
                      (let* ((length (- x2p x1p))
                             (x x1p)
                             (zi zi1p)
                             (dzi (float (/ (- zi2p zi1p) length))))
                        (declare (type (unsigned-byte 16) length x)
                                 (type single-float zi dzi))
                        (dotimes (j length)
                          (let ((z (float (/ 1.0 zi)))
                                (z-map-depth (aref z-map x y)))
                            (if (< z z-map-depth)
                                (progn
                                  (setf (aref z-map x y) z)
                                  (setf (cffi:mem-aref pixels :unsigned-int (dot-offset x y w))
                                        color)))
                            (incf zi dzi)
                            (incf x)))))))))
          ;; draw bottom
          (let* ((height (- y2 y1))
                 (dy0y1 (- y1 y0)))
            (declare (type (unsigned-byte 16) height dy0y1))
            (dotimes (i height)
              (let* ((y (+ y1 i))
                     (t1 (float (/ (float i) height)))
                     (zi1p (float (+ (* zi1 (- 1.0 t1))
                                     (* zi2 t1))))
                     (t2 (float (/ (float (+ i dy0y1)) height-total)))
                     (zi2p (float (+ (* zi0 (- 1.0 t2))
                                     (* zi2 t2))))
                     (x1p (floor (itplt-num x1 x2 t1)))
                     (x2p (floor (itplt-num x0 x2 t2))))
                (declare (type (unsigned-byte 16) y x1p x2p)
                         (type single-float t1 zi1p t2 zi2p))
                (if (= 0 (- x1p x2p))
                    nil
                    (progn
                      (if (> x1p x2p) (progn (swap x1p x2p)
                                             (swap zi1p zi2p)))
                      (let* ((length (- x2p x1p))
                             (x x1p)
                             (zi zi1p)
                             (dzi (float (/ (- zi2p zi1p) length))))
                        (declare (type (unsigned-byte 16) length x)
                                 (type single-float zi dzi))
                        (dotimes (j length)
                          (let ((z (float (/ 1.0 zi)))
                                (z-map-depth (aref z-map x y)))
                            (if (< z z-map-depth)
                                (progn
                                  (setf (aref z-map x y) z)
                                  (setf (cffi:mem-aref pixels :unsigned-int (dot-offset x y w))
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

(defun draw-triangle-phong (triangle shader z-map pixels &optional (w *w*) (h *h*))
  "draw-triangle-phong
   same as draw-triangle-flat
   shader: (shader triangle vertex-at-pixel) => color(int32, rgba)
           the shader function should not alter the triangle and vertex"
  (declare (type (simple-array single-float (* *)) z-map)
           (type (function (triangle vertex) (unsigned-byte 32)) shader))
  (block :draw
    (let* ((vertices (triangle-vertices triangle))
           (v0 (aref vertices 0))
           (v1 (aref vertices 1))
           (v2 (aref vertices 2))
           (v0-ndc (vertex-ndc (aref vertices 0)))
           (v1-ndc (vertex-ndc (aref vertices 1)))
           (v2-ndc (vertex-ndc (aref vertices 2))))
      (mvb-let* ((x0 y0 z0 (ndc-xyz v0-ndc w h))
                 (x1 y1 z1 (ndc-xyz v1-ndc w h))
                 (x2 y2 z2 (ndc-xyz v2-ndc w h)))
        (declare (type (signed-byte 16) x0 y0 x1 y1 x2 y2)
                 (type single-float z0 z1 z2))
        (if (> y0 y1) (progn (swap x0 x1) (swap y0 y1) (swap z0 z1) (swap v0 v1)))
        (if (> y0 y2) (progn (swap x0 x2) (swap y0 y2) (swap z0 z2) (swap v0 v2)))
        (if (> y1 y2) (progn (swap x1 x2) (swap y1 y2) (swap z1 z2) (swap v1 v2)))
        (if (or (= y0 y2) (= x0 x1 x2)) (return-from :draw))
        (let* ((height-total (- y2 y0))
               (zi0 (float (/ 1.0 z0)))
               (zi1 (float (/ 1.0 z1)))
               (zi2 (float (/ 1.0 z2))))
          ;; draw top
          (let* ((height (- y1 y0)))
            (declare (type (unsigned-byte 16) height))
            (dotimes (i height)
              (let* ((y (+ y0 i))
                     (t1 (float (/ (float i) height)))
                     (zi1p (float (+ (* zi0 (- 1.0 t1))
                                     (* zi1 t1))))
                     (t2 (float (/ (float i) height-total)))
                     (zi2p (float (+ (* zi0 (- 1.0 t2))
                                     (* zi2 t2))))
                     (x1p (floor (itplt-num x0 x1 t1)))
                     (x2p (floor (itplt-num x0 x2 t2))))
                (declare (type (unsigned-byte 16) y x1p x2p)
                         (type single-float t1 zi1p t2 zi2p))
                (if (= 0 (- x1p x2p))
                    nil
                    (let ((v1p (interpolate-vertex v0 v1 t1))
                          (v2p (interpolate-vertex v0 v2 t2)))
                      (if (> x1p x2p) (progn (swap x1p x2p)
                                             (swap zi1p zi2p)
                                             (swap v1p v2p)))
                      (let* ((length (- x2p x1p))
                             (vertex v1p)
                             (delta-v (dvertex v1p v2p length))
                             (x x1p)
                             (zi zi1p)
                             (dzi (float (/ (- zi2p zi1p) length))))
                        (declare (type (unsigned-byte 16) length x)
                                 (type single-float zi dzi))
                        (dotimes (j length)
                          (let ((z (float (/ 1.0 zi)))
                                (z-map-depth (aref z-map x y)))
                            (if (< z z-map-depth)
                                (progn
                                  (setf (aref z-map x y) z)
                                  (setf (cffi:mem-aref pixels :unsigned-int (dot-offset x y w))
                                        (funcall shader triangle vertex))))
                            (vertex+f vertex delta-v)
                            (incf zi dzi)
                            (incf x)))))))))
          ;; draw bottom
          (let* ((height (- y2 y1))
                 (dy0y1 (- y1 y0)))
            (declare (type (unsigned-byte 16) height dy0y1))
            (dotimes (i height)
              (let* ((y (+ y1 i))
                     (t1 (float (/ (float i) height)))
                     (zi1p (float (+ (* zi1 (- 1.0 t1))
                                     (* zi2 t1))))
                     (t2 (float (/ (float (+ i dy0y1)) height-total)))
                     (zi2p (float (+ (* zi0 (- 1.0 t2))
                                     (* zi2 t2))))
                     (x1p (floor (itplt-num x1 x2 t1)))
                     (x2p (floor (itplt-num x0 x2 t2))))
                (declare (type (unsigned-byte 16) y x1p x2p)
                         (type single-float t1 zi1p t2 zi2p))
                (if (= 0 (- x1p x2p))
                    nil
                    (let ((v1p (interpolate-vertex v1 v2 t1))
                          (v2p (interpolate-vertex v0 v2 t2)))
                      (if (> x1p x2p) (progn (swap x1p x2p)
                                             (swap zi1p zi2p)
                                             (swap v1p v2p)))
                      (let* ((length (- x2p x1p))
                             (vertex v1p)
                             (delta-v (dvertex v1p v2p length))
                             (x x1p)
                             (zi zi1p)
                             (dzi (float (/ (- zi2p zi1p) length))))
                        (declare (type (unsigned-byte 16) length x)
                                 (type single-float zi dzi))
                        (dotimes (j length)
                          (let ((z (float (/ 1.0 zi)))
                                (z-map-depth (aref z-map x y)))
                            (if (< z z-map-depth)
                                (progn
                                  (setf (aref z-map x y) z)
                                  (setf (cffi:mem-aref pixels :unsigned-int (dot-offset x y w))
                                        (funcall shader triangle vertex))))
                            (vertex+f vertex delta-v)
                            (incf zi dzi)
                            (incf x))))))))))))))

(defun sample-texture (vec2 texture)
  (declare (type (simple-array single-float (2)) vec2))
  (let* ((w (texture-w texture))
         (h (texture-h texture))
         (x (floor (* (1- w) (mod (aref vec2 0) 1.0))))
         (y (floor (* (1- h) (mod (aref vec2 1) 1.0)))))
    (make-vec3 (aref (texture-r texture) x y)
               (aref (texture-g texture) x y)
               (aref (texture-b texture) x y))))

