
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
    (dotimes (i height)
      (let* ((y (+ y0 i))
             (pt (float (/ i height))) ;; t, interpolate
             (x0p (funcall fp0x y))
             (zi0p (+ (* (- 1.0 pt) zi0) (* pt zi1)))
             (x1p (funcall fp1x y))
             (zi2p (+ (* (- 1.0 pt) zi2) (* pt zi1)))
             (length (- x1p x0p))
             ;; -------------------
             (dzi (/ (- zi2p zi0p) length))
             (zi zi0p)
             (x (ceiling x0p)))
        (dotimes (i (floor length))
          (let ((z-map-depth (aref z-map x y))
                (z (float (/ 1 zi))))
            (if (< z z-map-depth)
                (progn (setf (aref z-map x y) z)
                       (setf (autowrap:c-aref pixels (dot-offset x y *w*)
                                              :unsigned-int)
                             color)))
            (incf zi dzi)
            (incf x)))))))

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
;;    10 10 0.2
;;    90 150 0.2
;;    90 10 0.2
;;    red z-map pixels)
;;   (draw-top-even-triangle-flat
;;    90 10 0.2
;;    90 150 0.2
;;    180 10 0.2
;;    blue z-map pixels)
;;   (update-win)
;;   (aref z-map 20 26))

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

