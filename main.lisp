(ql:quickload 'cffi)
(ql:quickload 'parse-number)

(load "utils.lisp")
(load "matrix.lisp")
(load "clip.lisp")
(load "wavefront-obj.lisp")
(load "transform.lisp")
(load "model.lisp")


;;;; window coordinate (x y):
;; (0, 0) (1, 0)
;; (0, 1) (1, 1)
(defparameter *pixel-buffer* nil)
(defparameter *sdl2-surface* nil)
(defparameter *sdl2-pixel-buffer* nil)
(defparameter *sdl2-window* nil)
(defparameter *sdl2-renderer* nil)

(defparameter *w* 640)
(defparameter *h* 480)
(load "init.lisp")

;; todo:
;; establish screen buffer
;; load triangles frome obj file
;; traverse in triangles:
;;   1. do modelview transform
;;   2. project transform
;;   3. convert to NDC
;;   4. draw wire frame
;;      1. get coordinate and color
;;      2. do depth test
;;      3. draw
;;   5. swap buffer

;; (init-window)
;; (clear)
;; (c-draw-line 10 10 100 200 #xffff00ff *sdl2-pixel-buffer* *w*)
;; (update-win)

(defun build-vertex-from-indexes (attrib-index world-coords ndc-coords normals tex-coords)
  (let ((coord-index (aref attrib-index 0))
        (tex-coord-index (aref attrib-index 1))
        (normal-index (aref attrib-index 2)))
    (make-vertex :coord (aref world-coords coord-index)
                 :ndc (aref ndc-coords coord-index)
                 :normal (aref normals normal-index)
                 :tex-coord (aref tex-coords tex-coord-index))))

(defun build-triangle-from-face (face world-coords ndc-coords normals tex-coords)
  (let ((v1 (build-vertex-from-indexes (aref face 0) world-coords ndc-coords normals tex-coords))
        (v2 (build-vertex-from-indexes (aref face 1) world-coords ndc-coords normals tex-coords))
        (v3 (build-vertex-from-indexes (aref face 2) world-coords ndc-coords normals tex-coords)))
    (build-triangle v1 v2 v3)))

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

(defparameter *eye* (make-array 3 :element-type 'single-float))

(defparameter *project-mat*
  (frustum-mat 20 (/ 4 3) 0.3 15))

(defparameter *bunny-mesh* (wavefront-file-to-modelmesh #p"bunny.obj"))

(init-window :w 640 :h 480)

(list :vertices ;; v
      (length (modelmesh-vertices *bunny-mesh*))
      :tex-coords ;; vt
      (length (modelmesh-tex-coords *bunny-mesh*))
      :normal ;; vn
      (length (modelmesh-normals *bunny-mesh*)))

;; normal projections are not calculated yet!
(time
(let* ((vertices (modelmesh-vertices *bunny-mesh*))
       (tex-coords (modelmesh-tex-coords *bunny-mesh*))
       (normals (modelmesh-normals *bunny-mesh*))
       (faces (modelmesh-faces *bunny-mesh*))
       ;; -------------------------
       (trans-mat (3d-trans-mat 0.0 -1.0 -10.0))
       (scale-mat (3d-scale 5.0)))
  (dotimes (i 1000)
    (clear)
    (let* ((rot-mat (3d-rotate-y (mod i 360)))
           (trans-world (mul-44-44 trans-mat
                                   (mul-44-44 rot-mat scale-mat)))
           (world-coords (apply-transform vertices trans-world))
           (proj-coords (apply-transform world-coords *project-mat*))
           (ndc-coords (vertices-ndc proj-coords)))
      (dotimes (i (length faces))
        (let* ((face (aref faces i))
               (triangle (build-triangle-from-face
                          face world-coords ndc-coords normals tex-coords))
               (cliped-triangles (clip-triangle triangle)))
          (mapcar #'draw-triangle-wire-ndc cliped-triangles))))
    (update-win))))

;; (destroy-window)
