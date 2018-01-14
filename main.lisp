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
  (let ((v1 (build-vertex-from-indexes
             (aref face 0) world-coords ndc-coords normals tex-coords))
        (v2 (build-vertex-from-indexes
             (aref face 1) world-coords ndc-coords normals tex-coords))
        (v3 (build-vertex-from-indexes
             (aref face 2) world-coords ndc-coords normals tex-coords)))
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

