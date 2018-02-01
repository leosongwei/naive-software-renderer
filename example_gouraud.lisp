;; flat shading

;;(load "main.lisp")
(in-package :cl-user)
(push #p"./" asdf:*central-registry*)

(require :naive-software-renderer)
(defpackage :naive-software-renderer-example
  (:use :cl :cl-user :naive-software-renderer))
(in-package :naive-software-renderer-example)

(defparameter *bunny-mesh* (wavefront-file-to-modelmesh #p"bunny.obj"))

(defparameter *eye* (make-array 3 :element-type 'single-float))
(defparameter *project-mat*
  (frustum-mat 20 (/ 4 3) 0.8 15))

(init-window :w 640 :h 480)

(defun vertex-shader (vertex-coord light-pos eye-pos normal color-vec)
  (declare (optimize (speed 3))
           (type (simple-array single-float (4)) vertex-coord light-pos eye-pos)
           (type (simple-array single-float (3)) normal color-vec))
  (let* ((light-direction (vec3-normalize (vec4->vec3 ;; light-pos -> vertex
                                           (vec4- vertex-coord light-pos))))
         (view-vec (vec3-normalize (vec4->vec3 ;; vertex -> eye
                                    (vec4- eye-pos vertex-coord))))
         ;; specular
         (reflection (vec3-reflection light-direction normal))
         (spec-factor (expt (max (vec3-dot view-vec reflection) 0.0) 0.6))
         ;; diffuse
         (light-direction-neg (vec3- #.(make-vec3 0.0 0.0 0.0)
                                     light-direction))
         (diffuse-factor (max (vec3-dot normal light-direction-neg) 0.0))
         ;; ambient
         (ambient-factor 0.8))
    ;; color
    (vec3* color-vec (+ spec-factor diffuse-factor ambient-factor))))

(time
(let* ((vertices (modelmesh-vertices *bunny-mesh*))
       (tex-coords (modelmesh-tex-coords *bunny-mesh*))
       (normals (modelmesh-normals *bunny-mesh*))
       (faces (modelmesh-faces *bunny-mesh*))
       ;; -------------------------
       (trans-mat (3d-trans-mat 0.0 -1.0 -10.0))
       ;;(trans-mat (3d-trans-mat 1.8 -2.3 -10.0))
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
               (a-normal (triangle-normal3 triangle))
               (view-vec (vec4->vec3 ;; vertex -> eye
                          (vec4- eye-pos vertex-coord))))
          (if (> (vec3-dot a-normal view-vec) 0)
              (let ((clipped-triangles (clip-triangle triangle)))
                (dolist (triangle clipped-triangles)
                  (let* ((vc0 (vertex-coord (aref (triangle-vertices triangle) 0)))
                         (vc1 (vertex-coord (aref (triangle-vertices triangle) 1)))
                         (vc2 (vertex-coord (aref (triangle-vertices triangle) 2)))
                         (vn0 (vec3-normalize
                               (vec4->vec3 (vertex-normal
                                            (aref (triangle-vertices triangle) 0)))))
                         (vn1 (vec3-normalize
                               (vec4->vec3 (vertex-normal
                                            (aref (triangle-vertices triangle) 1)))))
                         (vn2 (vec3-normalize
                               (vec4->vec3 (vertex-normal
                                            (aref (triangle-vertices triangle) 2)))))
                         (c0 (vertex-shader vc0 light-pos eye-pos vn0 color-vec))
                         (c1 (vertex-shader vc1 light-pos eye-pos vn1 color-vec))
                         (c2 (vertex-shader vc2 light-pos eye-pos vn2 color-vec)))
                    (draw-triangle-gouraud triangle c0 c1 c2 z-map *sdl2-pixel-buffer*))))))))
    (update-win))))

(destroy-window)

(sb-ext:exit)
