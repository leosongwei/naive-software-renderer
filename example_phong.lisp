(progn
  (load "main.lisp")
  (defparameter *eye* (make-array 3 :element-type 'single-float))
  (defparameter *project-mat*
    (frustum-mat 20 (/ 4 3) 0.1 15))
  (defparameter *bunny-mesh* (wavefront-file-to-modelmesh #p"bunny_high.obj"))
  (defparameter *texture0* (read-image-to-texture "./true.jpeg"))
  (init-window :w 1024 :h 768))

(defun phong-frag (triangle v eye-pos light-pos)
  (declare (ignore triangle))
  (let* ((view-vec (vec3-normalize (vec4->vec3
                                    (vec4- eye-pos (vertex-coord v)))))
         (normal (vec3-normalize
                  (vec4->vec3 (vertex-normal v))))
         (light-direction (vec3-normalize
                           (vec4->vec3
                            (vec4- (vertex-coord v) light-pos))))
         ;; specular
         (reflection (vec3-reflection light-direction normal))
         (spec-factor (expt (max (vec3-dot view-vec reflection) 0.0)
                            80))
         ;; ambient
         (ambient-factor 0.25)
         ;; diffuse
         (light-direction-neg (vec3- #.(make-vec3 0.0 0.0 0.0)
                                     light-direction))
         (diffuse-factor (max (vec3-dot normal light-direction-neg)
                              0.0))
         (color-vec (sample-texture (vertex-tex-coord v) *texture0*)))
    ;; color blending
    (vec3-int-color (vec3+
                      (vec3* #.(make-vec3 1.0 1.0 1.0) spec-factor)
                      (vec3* color-vec (+ ambient-factor (* 0.6 diffuse-factor)))))))

;;(defparameter *bunny-mesh* (wavefront-file-to-modelmesh #p"bunny_high.obj"))

;; bunny

(time
(let* ((vertices (modelmesh-vertices *bunny-mesh*))
       (tex-coords (modelmesh-tex-coords *bunny-mesh*))
       (normals (modelmesh-normals *bunny-mesh*))
       (faces (modelmesh-faces *bunny-mesh*))
       ;; -------------------------
       (trans-mat (3d-trans-mat 0.05 -0.35 -10.0))
       ;;(trans-mat (3d-trans-mat 1.8 -2.3 -10.0))
       (scale-mat (3d-scale 5.0))
       ;;(view-vec #(0.0 0.0 -1.0))
       ;; camera at 0,0,0, no need to transform light-pos
       (light-pos (make-vec4 5.0 5.0 -5.0 1.0))
       (eye-pos (make-array 4 :element-type 'single-float
                            :initial-contents '(0.0 0.0 0.0 1.0)))
       (*project-mat* (frustum-mat 5 (/ 4 3) 0.1 15)))
  (progn
    (clear 0 0 0)
    (let* ((rot-mat (3d-rotate-y 0))
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
               (shader (lambda (triangle v)
                         (phong-frag triangle v eye-pos light-pos))))
          shader
          (if (> (vec3-dot a-normal view-vec) 0)
              (let ((cliped-triangles (clip-triangle triangle)))
                (mapcar (lambda (triangle)
                          (draw-triangle-phong triangle
                                               shader
                                               z-map
                                               *sdl2-pixel-buffer*))
                        cliped-triangles))))))
    (update-win))))

(defun split-work (num pieces)
  ;; return: (cons start, num)
  (let ((lst nil)
        (every-piece (floor (/ num pieces)))
        (remain num)
        (index 0))
    (if (= every-piece 0)
        (push (cons 0 remain) lst)
        (progn
          (dotimes (i (1- pieces))
            (push (cons index every-piece) lst)
            (decf remain every-piece)
            (incf index every-piece))
          (push (cons index remain) lst)))
    lst))

(time
 (let* ((vertices (modelmesh-vertices *bunny-mesh*))
        (tex-coords (modelmesh-tex-coords *bunny-mesh*))
        (normals (modelmesh-normals *bunny-mesh*))
        (faces (modelmesh-faces *bunny-mesh*))
        ;; -------------------------
        (trans-mat (3d-trans-mat 0.05 -0.35 -10.0))
        ;;(trans-mat (3d-trans-mat 1.8 -2.3 -10.0))
        (scale-mat (3d-scale 5.0))
        ;;(view-vec #(0.0 0.0 -1.0))
        ;; camera at 0,0,0, no need to transform light-pos
        (light-pos (make-vec4 5.0 5.0 -5.0 1.0))
        (eye-pos (make-array 4 :element-type 'single-float
                             :initial-contents '(0.0 0.0 0.0 1.0)))
        (*project-mat* (frustum-mat 5 (/ 4 3) 0.1 15)))
   (progn
     (clear 0 0 0)
     (let* ((rot-mat (3d-rotate-y 0))
            (trans-world (mul-44-44 trans-mat
                                    (mul-44-44 rot-mat scale-mat)))
            (world-norms (apply-transform normals trans-world))
            (world-coords (apply-transform vertices trans-world))
            (proj-coords (apply-transform world-coords *project-mat*))
            (ndc-coords (vertices-ndc proj-coords))
            (z-map (make-z-map)))
       (let* ((renderer-func
               (lambda (start amount)
                 (let ((draw-count 0))
                   (dotimes (ia amount)
                     (let* ((i (+ start ia))
                            (face (aref faces i))
                            (triangle (build-triangle-from-face
                                       face
                                       world-coords
                                       ndc-coords
                                       world-norms
                                       tex-coords))
                            (vertex-coord
                             (vertex-coord
                              (aref (triangle-vertices triangle) 0)))
                            (view-vec (vec4->vec3 ;; vertex -> eye
                                       (vec4- eye-pos vertex-coord)))
                            (a-normal
                             (vec3-normalize
                              (vec4->vec3
                               (vertex-normal
                                (aref (triangle-vertices triangle) 0)))))
                            (shader (lambda (triangle v)
                                      (phong-frag triangle v eye-pos light-pos))))
                       shader
                       (if (> (vec3-dot a-normal view-vec) 0)
                           (let ((cliped-triangles (clip-triangle triangle)))
                             (incf draw-count)
                             (mapcar (lambda (triangle)
                                       (draw-triangle-phong triangle
                                                            shader
                                                            z-map
                                                            *sdl2-pixel-buffer*))
                                     cliped-triangles)))))
                   draw-count)))
              (thread-count 4)
              (threads (mapcar (lambda (work)
                                 (sb-thread:make-thread
                                  (lambda ()
                                    (funcall renderer-func (car work) (cdr work)))))
                               (split-work (length faces) thread-count))))
         (princ (mapcar (lambda (thread)
                          (sb-thread:join-thread
                           thread
                           :default 'x)) threads)))))
   (update-win)))
