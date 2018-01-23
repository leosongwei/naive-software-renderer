(ql:quickload 'parse-number)
(ql:quickload "sdl2")
(ql:quickload "cl-autowrap")
(ql:quickload "cffi")

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
(load "rasterization.lisp")
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

