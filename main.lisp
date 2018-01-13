(ql:quickload 'cffi)
(ql:quickload 'parse-number)

(load "utils.lisp")
(load "matrix.lisp")
(load "clip.lisp")
(load "wavefront-obj.lisp")
(load "model.lisp")

(defparameter *w* 640)
(defparameter *h* 480)

(defparameter *eye* (make-array 3 :element-type 'single-float))

(defparameter *project-mat*
  (frustum-mat 45 (/ 4 3) 0.3 10))

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

(defstruct (gobject (:copier copy-gobject))
  triangles
  (position (make-array 3 :element-type 'single-float))
  (rotation (make-array 3 :element-type 'single-float)) ;; on x, y, z, in degree
  (scale (3d-scale 1.0))
  (trans-mat (make-array '(4 4) :element-type 'single-float)))


(defun copy-gobject (gobj)
  ;; the only thing need to be copied is the triangles list
  (make-gobject :triangles (mapcar #'copy-triangle (gobject-triangles gobj))
                :position (gobject-position gobj)
                :rotation (gobject-rotation gobj)
                :scale (gobject-scale gobj)
                :trans-mat (gobject-trans-mat gobj)))

(defun gobject-transmat-update-f (gobject)
  (let* ((pos (gobject-position gobject))
         (transition (3d-trans-mat (aref pos 0)
                                   (aref pos 1)
                                   (aref pos 2)))
         (rot (gobject-rotation gobject))
         (rot-x (3d-rotate-x (aref rot 0)))
         (rot-y (3d-rotate-y (aref rot 1)))
         (rot-z (3d-rotate-z (aref rot 2)))
         (rot-mat (mul-44-44 rot-x (mul-44-44 rot-y rot-z)))
         (scale (gobject-scale gobject)))
    (setf (gobject-trans-mat gobject)
          (mul-44-44 transition
                     (mul-44-44 rot-mat scale)))))

(defun triangle-transform-f (triangle trans-mat)
  (let* ((vertices (triangle-vertices triangle))
         (v1 (aref vertices 0))
         (v2 (aref vertices 1))
         (v3 (aref vertices 2)))
    (setf (vertex-coord v1) (mul-44-v4 trans-mat (vertex-coord v1)))
    (setf (vertex-coord v2) (mul-44-v4 trans-mat (vertex-coord v2)))
    (setf (vertex-coord v3) (mul-44-v4 trans-mat (vertex-coord v3)))
    triangle))
;; (let* ((v1 (make-vertex :coord #(1.0 1.0 1.0 1.0)))
;;        (v2 (make-vertex :coord #(1.0 0.0 0.0 1.0)))
;;        (v3 (make-vertex :coord #(0.0 1.0 0.0 1.0)))
;;        (tri (build-triangle v1 v2 v3))
;;        (trans-mat (3d-trans-mat 1.0 1.0 1.0)))
;;   (triangle-transform-f tri trans-mat)
;;   tri)

(defun gobject-transform-f (gobj)
  (setf (gobject-triangles gobj)
        (mapcar (lambda (triangle)
                  (triangle-transform-f triangle (gobject-trans-mat gobj)))
                (gobject-triangles gobj))))

(defun triangle-ndc-f (triangle project-mat)
  ;; apply project matrix to the triangle, and convert to NDC space
  (let* ((vertices (triangle-vertices triangle)))
    (dotimes (i 3)
      (let* ((v (aref vertices i))
             (coord (vertex-coord v))
             (vp (mul-44-v4 project-mat coord))
             (ndc (vec4-ndc vp)))
        (setf (vertex-ndc v) ndc)))
    triangle))

(defun draw-triangle-list (triangle-list)
  )

