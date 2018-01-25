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

(load "init.lisp")
(load "rasterization.lisp")

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

