(progn
  (defparameter *eye* (make-array 3 :element-type 'single-float))
  (defparameter *project-mat*
    (frustum-mat 20 (/ 4 3) 0.3 15))
  (defparameter *bunny-mesh* (wavefront-file-to-modelmesh #p"bunny.obj")))
;; (list :vertices ;; v
;;       (length (modelmesh-vertices *bunny-mesh*))
;;       :tex-coords ;; vt
;;       (length (modelmesh-tex-coords *bunny-mesh*))
;;       :normal ;; vn
;;       (length (modelmesh-normals *bunny-mesh*)))
(destroy-window)
(init-window :w 640 :h 480)

;; normal projections are not calculated yet!
(time
 (let* ((vertices (modelmesh-vertices *bunny-mesh*))
        (tex-coords (modelmesh-tex-coords *bunny-mesh*))
        (normals (modelmesh-normals *bunny-mesh*))
        (faces (modelmesh-faces *bunny-mesh*))
        ;; -------------------------
        (trans-mat (3d-trans-mat 0.0 -1.0 -10.0))
        (scale-mat (3d-scale 5.0))
        (view-vec #(0.0 0.0 -1.0)))
   (dotimes (i 1000)
     (clear)
     (let* ((rot-mat (3d-rotate-y (mod i 360)))
            (trans-world (mul-44-44 trans-mat
                                    (mul-44-44 rot-mat scale-mat)))
            (world-norms (apply-transform normals trans-world))
            (world-coords (apply-transform vertices trans-world))
            (proj-coords (apply-transform world-coords *project-mat*))
            (ndc-coords (vertices-ndc proj-coords)))
       (dotimes (i (length faces))
         (let* ((face (aref faces i))
                (triangle (build-triangle-from-face
                           face world-coords ndc-coords world-norms tex-coords))
                (a-normal (vec4->vec3
                           (vertex-normal
                            (aref (triangle-vertices triangle) 0)))))
           (if (< (vec3-dot a-normal view-vec) 0)
               (let ((cliped-triangles (clip-triangle triangle)))
                 (mapcar #'draw-triangle-wire-ndc cliped-triangles))))))
     (update-win))))

;; wire
(time
 (let* ((vertices (modelmesh-vertices *bunny-mesh*))
        (tex-coords (modelmesh-tex-coords *bunny-mesh*))
        (normals (modelmesh-normals *bunny-mesh*))
        (faces (modelmesh-faces *bunny-mesh*))
        ;; -------------------------
        (trans-mat (3d-trans-mat 0.0 -1.0 -10.0))
        (scale-mat (3d-scale 5.0))
                                        ;(view-vec #(0.0 0.0 -1.0))
        (eye-pos (make-array 4 :element-type 'single-float
                             :initial-contents '(0.0 0.0 0.0 1.0))))
   (dotimes (i 360)
     (clear)
     (let* ((rot-mat (3d-rotate-y (mod i 360)))
            (trans-world (mul-44-44 trans-mat
                                    (mul-44-44 rot-mat scale-mat)))
            (world-norms (apply-transform normals trans-world))
            (world-coords (apply-transform vertices trans-world))
            (proj-coords (apply-transform world-coords *project-mat*))
            (ndc-coords (vertices-ndc proj-coords)))
       (dotimes (i (length faces))
         (let* ((face (aref faces i))
                (triangle (build-triangle-from-face
                           face world-coords ndc-coords world-norms tex-coords))
                (view-vec (vec4->vec3
                           (vec4- (vertex-coord (aref (triangle-vertices triangle) 0))
                                  eye-pos)))
                (a-normal (vec4->vec3
                           (vertex-normal
                            (aref (triangle-vertices triangle) 0)))))
           (if (< (vec3-dot a-normal view-vec) 0)
               (let ((cliped-triangles (clip-triangle triangle)))
                 (mapcar #'draw-triangle-wire-ndc cliped-triangles))))))
     (update-win))))

;; full
(time
 (let* ((vertices (modelmesh-vertices *bunny-mesh*))
        (tex-coords (modelmesh-tex-coords *bunny-mesh*))
        (normals (modelmesh-normals *bunny-mesh*))
        (faces (modelmesh-faces *bunny-mesh*))
        ;; -------------------------
        (trans-mat (3d-trans-mat 0.0 -1.0 -10.0))
        (scale-mat (3d-scale 5.0))
                                        ;(view-vec #(0.0 0.0 -1.0))
        (eye-pos (make-array 4 :element-type 'single-float
                             :initial-contents '(0.0 0.0 0.0 1.0))))
   (dotimes (i 360)
     (clear)
     (let* ((rot-mat (3d-rotate-y (mod i 360)))
            (trans-world (mul-44-44 trans-mat
                                    (mul-44-44 rot-mat scale-mat)))
            (world-norms (apply-transform normals trans-world))
            (world-coords (apply-transform vertices trans-world))
            (proj-coords (apply-transform world-coords *project-mat*))
            (ndc-coords (vertices-ndc proj-coords))
            (z-map (make-z-map)))
       (dotimes (i (length faces))
         (let* ((face (aref faces i))
                (triangle (build-triangle-from-face
                           face world-coords ndc-coords world-norms tex-coords))
                (view-vec (vec4->vec3
                           (vec4- (vertex-coord (aref (triangle-vertices triangle) 0))
                                  eye-pos)))
                (a-normal (vec4->vec3
                           (vertex-normal
                            (aref (triangle-vertices triangle) 0))))
                (draw-func (lambda (tri)
                             (draw-triangle-flat tri (map-color 255 255 0) z-map
                                                 *sdl2-pixel-buffer*))))
           (if (< (vec3-dot a-normal view-vec) 0)
               (let ((cliped-triangles (clip-triangle triangle)))
                 (mapcar draw-func cliped-triangles))))))
     (update-win))))

;; flat
(time
(let* ((vertices (modelmesh-vertices *bunny-mesh*))
       (tex-coords (modelmesh-tex-coords *bunny-mesh*))
       (normals (modelmesh-normals *bunny-mesh*))
       (faces (modelmesh-faces *bunny-mesh*))
       ;; -------------------------
       (trans-mat (3d-trans-mat 0.0 -1.0 -10.0))
       (scale-mat (3d-scale 5.0))
       ;;(view-vec #(0.0 0.0 -1.0))
       ;; camera at 0,0,0, no need to transform light-pos
       (color-vec (make-vec3 0.55 0.17 0.17))
       (light-pos (make-array 4 :element-type 'single-float
                              :initial-contents '(10.0 10.0 0.0 1.0)))
       (eye-pos (make-array 4 :element-type 'single-float
                            :initial-contents '(0.0 0.0 0.0 1.0))))
  (dotimes (i 360)
    (clear 0 0 0)
    (let* ((rot-mat (3d-rotate-y (mod i 360)))
           (trans-world (mul-44-44 trans-mat
                                   (mul-44-44 rot-mat scale-mat)))
           (world-norms (apply-transform normals trans-world))
           (world-coords (apply-transform vertices trans-world))
           (proj-coords (apply-transform world-coords *project-mat*))
           (ndc-coords (vertices-ndc proj-coords))
           (z-map (make-z-map)))
      (dotimes (i (length faces))
        (let* ((face (aref faces i))
               (triangle (build-triangle-from-face
                          face world-coords ndc-coords world-norms tex-coords))
               (vertex-coord (vertex-coord (aref (triangle-vertices triangle) 0)))
               (view-vec (vec4->vec3 ;; vertex -> eye
                          (vec4- eye-pos vertex-coord)))
               (a-normal (vec3-normalize (vec4->vec3
                                          (vertex-normal
                                           (aref (triangle-vertices triangle) 0)))))
               (light-direction (vec3-normalize (vec4->vec3 ;; light-pos -> vertex
                                                 (vec4- vertex-coord light-pos))))
               ;; specular
               (reflection (vec3-reflection light-direction a-normal))
               (spec-factor (expt (max (vec3-dot view-vec reflection) 0.0) 0.6))
               (specular (vec3* color-vec (* 0.3 spec-factor)))
               ;; diffuse
               (light-direction-neg (vec3- #.(make-vec3 0.0 0.0 0.0)
                                           light-direction))
               (diffuse-factor (max (vec3-dot a-normal light-direction-neg) 0.0))
               (diffuse (vec3* color-vec (* 0.3 diffuse-factor)))
               ;; ambient
               (ambient (vec3* color-vec 0.8))
               ;; color blending
               (color-int (vec3-int-color (vec3+ specular (vec3+ diffuse ambient))))
               (draw-func (lambda (tri)
                            (draw-triangle-flat tri color-int z-map
                                                *sdl2-pixel-buffer*))))
          (if (> (vec3-dot a-normal view-vec) 0) ;; 
              (let ((cliped-triangles (clip-triangle triangle)))
                (mapcar draw-func cliped-triangles))))))
    (update-win))))

;; (destroy-window)

;; (require :sb-sprof)
;; (sb-sprof:with-profiling (:max-samples 20000
;;                                        :mode :alloc
;;                                        :report :flat)
;;   (let* ((vertices (modelmesh-vertices *bunny-mesh*))
;;          (tex-coords (modelmesh-tex-coords *bunny-mesh*))
;;          (normals (modelmesh-normals *bunny-mesh*))
;;          (faces (modelmesh-faces *bunny-mesh*))
;;          ;; -------------------------
;;          (trans-mat (3d-trans-mat 0.0 -1.0 -10.0))
;;          (scale-mat (3d-scale 5.0)))
;;     (dotimes (i 1000)
;;       (clear)
;;       (let* ((rot-mat (3d-rotate-y (mod i 360)))
;;              (trans-world (mul-44-44 trans-mat
;;                                      (mul-44-44 rot-mat scale-mat)))
;;              (world-coords (apply-transform vertices trans-world))
;;              (proj-coords (apply-transform world-coords *project-mat*))
;;              (ndc-coords (vertices-ndc proj-coords)))
;;         (dotimes (i (length faces))
;;           (let* ((face (aref faces i))
;;                  (triangle (build-triangle-from-face
;;                             face world-coords ndc-coords normals tex-coords))
;;                  (cliped-triangles (clip-triangle triangle)))
;;             (mapcar #'draw-triangle-wire-ndc cliped-triangles))))
;;       (update-win))))

;; (time
;;  (let* ((vertices (modelmesh-vertices *bunny-mesh*))
;;         (tex-coords (modelmesh-tex-coords *bunny-mesh*))
;;         (normals (modelmesh-normals *bunny-mesh*))
;;         (faces (modelmesh-faces *bunny-mesh*))
;;         ;; -------------------------
;;         (trans-mat (3d-trans-mat 0.0 -1.0 -10.0))
;;         (scale-mat (3d-scale 5.0)))
;;    (dotimes (i 1000)
;;      ;;(clear)
;;      (let* ((rot-mat (3d-rotate-y (mod i 360)))
;;             (trans-world (mul-44-44 trans-mat
;;                                     (mul-44-44 rot-mat scale-mat)))
;;             (world-coords (apply-transform vertices trans-world))
;;             (proj-coords (apply-transform world-coords *project-mat*))
;;             (ndc-coords (vertices-ndc proj-coords)))
;;        (dotimes (i (length faces))
;;          (let* ((face (aref faces i))
;;                 (triangle (build-triangle-from-face
;;                            face world-coords ndc-coords normals tex-coords))
;;                 (cliped-triangles (clip-triangle triangle)))
;;            cliped-triangles))))))


;; (progn
;; (defparameter *black-hole* nil)
;; (gc :full t)

;; (time
;;  (let* ((vertices (modelmesh-vertices *bunny-mesh*))
;;         (tex-coords (modelmesh-tex-coords *bunny-mesh*))
;;         (normals (modelmesh-normals *bunny-mesh*))
;;         (faces (modelmesh-faces *bunny-mesh*))
;;         ;; -------------------------
;;         (trans-mat (3d-trans-mat 0.0 -1.0 -10.0))
;;         (scale-mat (3d-scale 5.0)))
;;    (dotimes (i 1000)
;;      ;;(clear)
;;      (let* ((rot-mat (3d-rotate-y (mod i 360)))
;;             (trans-world (mul-44-44 trans-mat
;;                                     (mul-44-44 rot-mat scale-mat)))
;;             (world-coords (apply-transform vertices trans-world))
;;             (proj-coords (apply-transform world-coords *project-mat*))
;;             (ndc-coords (vertices-ndc proj-coords)))
;;        (dotimes (i (length faces))
;;          (let* ((face (aref faces i))
;;                 (triangle (build-triangle-from-face
;;                            face world-coords ndc-coords normals tex-coords)))
;;            (push triangle *black-hole*))))))))

;; Clip testing
(let* ((vertices (modelmesh-vertices *bunny-mesh*))
       (tex-coords (modelmesh-tex-coords *bunny-mesh*))
       (normals (modelmesh-normals *bunny-mesh*))
       (faces (modelmesh-faces *bunny-mesh*))
       ;; -------------------------
       (scale-mat (3d-scale 5.0))
       (view-vec #(0.0 0.0 -1.0)))
  (clear)
  (let* ((trans-mat (3d-trans-mat 0.0 -1.1 -10.0))
         (rot-mat (3d-rotate-y 30))
         (trans-world (mul-44-44 trans-mat
                                 (mul-44-44 rot-mat scale-mat)))
         (world-norms (apply-transform normals trans-world))
         (world-coords (apply-transform vertices trans-world))
         (proj-coords (apply-transform world-coords *project-mat*))
         (ndc-coords (vertices-ndc proj-coords)))
    (dotimes (i (length faces))
      (let* ((face (aref faces i))
             (triangle (build-triangle-from-face
                        face world-coords ndc-coords world-norms tex-coords))
             (a-normal (vec4->vec3
                        (vertex-normal
                         (aref (triangle-vertices triangle) 0)))))
        (if (< (vec3-dot a-normal view-vec) 0)
            (let ((cliped-triangles (clip-triangle triangle)))
              (mapcar #'draw-triangle-wire-ndc cliped-triangles))))))
  (update-win))
