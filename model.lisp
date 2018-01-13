;; model: an arry of triangles

(defun list-2-array (list)
  (let* ((length (length list))
         (array (make-array length))
         (index 0))
    (dolist (e list)
      (setf (aref array index) e)
      (incf index))
    array))

(defun make-array-list (length list)
  (make-array length
              :element-type 'single-float
              :initial-contents list))
;;(make-array-list 4 '(1.0 2.0 3.0 4.0))

(defun make-model-from-wave-front (vertices tex-coords normals faces)
  (let ((vertices-array (list-2-array vertices))
        (tex-coords-array (list-2-array tex-coords))
        (normals-array (list-2-array normals)))
    (mapcar (lambda (face)
              (apply #'build-triangle
                     (mapcar (lambda (vertex-index)
                               (let ((coord-index (nth 0 vertex-index))
                                     (tex-coord-index (nth 1 vertex-index))
                                     (normal-index (nth 2 vertex-index)))
                                 (make-vertex :coord
                                              (make-array-list
                                               4 (aref vertices-array (1- coord-index)))
                                              :tex-coord
                                              (make-array-list
                                               2 (aref tex-coords-array (1- tex-coord-index)))
                                              :normal
                                              (make-array-list
                                               3 (aref normals-array (1- normal-index))))))
                             face)))
            faces)))

;; (multiple-value-bind (vertices tex-coords normals faces)
;;     (read-wavefront-obj-file #p"test.obj")
;;   ;;(list :vertices vertices :tex-coords tex-coords :normals normals :faces faces)
;;   (make-model-from-wave-front vertices tex-coords normals faces))

(defun wave-front-file-to-triangles (file-path)
  (multiple-value-bind (vertices tex-coords normals faces)
      (read-wavefront-obj-file file-path)
    (make-model-from-wave-front vertices tex-coords normals faces)))
