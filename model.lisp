(in-package :naive-software-renderer)
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

(defstruct modelmesh
  vertices  ;; #(float-vec4 ...)
  tex-coords ;; #(float-vec4 ...)
  normals ;; #(float-vec4 ...)
  faces) ;; #(face), face:#(int-vec3 int-vec3 int-vec3)
;; vertex attribute index, 0-indexed, not like wavefront!!

(defun make-model-from-wave-front (vertices tex-coords normals faces)
  (make-modelmesh :vertices
                  (let* ((vectors
                          (mapcar (lambda (list)
                                    (make-array
                                     4 :element-type 'single-float
                                     :initial-contents list))
                                  vertices)))
                    (make-array (length vectors)
                                :initial-contents vectors))
                  :tex-coords
                  (let* ((vectors
                          (mapcar (lambda (list)
                                    (make-array
                                     2 :element-type 'single-float
                                     :initial-contents list))
                                  tex-coords)))
                    (make-array (length vectors)
                                :initial-contents vectors))
                  :normals
                  (let* ((vectors
                          (mapcar (lambda (list)
                                    (make-array
                                     4 :element-type 'single-float
                                     :initial-contents list))
                                  normals)))
                    (make-array (length vectors)
                                :initial-contents vectors))
                  :faces
                  (let* ((vectors
                          (mapcar (lambda (list)
                                    (make-array ;; vertices
                                     3 :initial-contents
                                     (mapcar (lambda (attrib-index)
                                               (make-array ;; vertex attri index
                                                ;; 1-indexed -> 0-indexed
                                                3 :element-type 'integer
                                                :initial-contents
                                                (mapcar (lambda (x) (1- x)) attrib-index)))
                                             list)))
                                  faces)))
                    (make-array (length vectors)
                                :initial-contents vectors))))

;;(multiple-value-bind (vertices tex-coords normals faces)
;;    (read-wavefront-obj-file #p"test.obj")
;;  (list :vertices vertices :tex-coords tex-coords :normals normals :faces faces))
;;   ;;(make-model-from-wave-front vertices tex-coords normals faces))

(defun wavefront-file-to-modelmesh (file-path)
  (multiple-value-bind (vertices tex-coords normals faces)
      (read-wavefront-obj-file file-path)
    (make-model-from-wave-front vertices tex-coords normals faces)))
;;(wavefront-file-to-modelmesh #p"test.obj")
;; #S(MODELMESH
;;    :VERTICES #(#(0.123 0.234 0.345 1.0) #(0.123 0.234 0.345 1.0)
;;                #(0.123 0.234 0.345 1.0) #(0.123 0.234 0.345 1.0)
;;                #(0.123 0.234 0.345 1.0) #(0.123 0.234 0.345 1.0))
;;    :TEX-COORDS #(#(0.5 1.0) #(0.5 1.0) #(0.5 1.0) #(0.5 1.0) #(0.5 1.0)
;;                  #(0.5 1.0))
;;    :NORMALS #(#(0.707 0.0 0.707 0.0) #(0.707 0.0 0.707 0.0)
;;               #(0.707 0.0 0.707 0.0) #(0.707 0.0 0.707 0.0)
;;               #(0.707 0.0 0.707 0.0) #(0.707 0.0 0.707 0.0))
;;    :FACES #(#(#(0 0 0) #(1 1 1) #(2 2 2)) #(#(3 3 3) #(4 4 4) #(5 5 5))))

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
