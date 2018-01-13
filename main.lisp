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

(defun display-gobject-wire (gobj)
  (gobject-transmat-update-f gobj)
  (dolist (triangle (gobject-triangles gobj))
    (let* ((tri (copy-triangle triangle)))
      (triangle-transform-f tri (gobject-trans-mat gobj))
      (triangle-ndc-f tri *project-mat*)
      (let ((cliped-triangles (clip-triangle tri)))
        (mapcar #'draw-triangle-wire-ndc cliped-triangles)))))

(defparameter *bunny-triangles* (wave-front-file-to-triangles #p"bunny.obj"))

(defparameter *bunny-obj*
  (make-gobject :triangles *bunny-triangles*
                :position #(0.0 -1.0 -10.0)
                :scale (3d-scale 5.0)))


;; (init-window :w 640 :h 480)

;; (progn
;;   (setf (aref (gobject-rotation *bunny-obj*) 1) (float (mod 30 360)))
;;   (clear)
;;   (display-gobject-wire *bunny-obj*)
;;   (update-win))

;; (time
;;  (dotimes (i (* 1 360))
;;    (progn
;;      (setf (aref (gobject-rotation *bunny-obj*) 1) (float (mod i 360)))
;;      (clear)
;;      (display-gobject-wire *bunny-obj*)
;;      (update-win))))

;; (destroy-window)
