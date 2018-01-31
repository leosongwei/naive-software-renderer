(in-package :naive-software-renderer)

(defun apply-transform (fv4-array trans-mat)
  (let* ((length (length fv4-array))
         (a (make-array length
                        :element-type '(SIMPLE-ARRAY SINGLE-FLOAT (4))
                        :initial-element (make-array 4 :element-type 'single-float
                                                     :initial-element 0.0))))
    (dotimes (i length)
      (setf (aref a i)
            (mul-44-v4 trans-mat (aref fv4-array i))))
    a))
;; (defparameter *test-mesh* (wavefront-file-to-modelmesh #p"test.obj"))
;; (apply-transform (modelmesh-vertices *test-mesh*) *project-mat*)

(defun vertices-ndc (fv4-array)
  (let* ((length (length fv4-array))
         (a (make-array length
                        :element-type '(SIMPLE-ARRAY SINGLE-FLOAT (4))
                        :initial-element (make-array 4 :element-type 'single-float
                                                     :initial-element 0.0))))
    (dotimes (i length)
      (setf (aref a i) (vec4-ndc (aref fv4-array i))))
    a))

