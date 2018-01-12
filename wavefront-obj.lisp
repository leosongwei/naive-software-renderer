;; (defstruct module
;;   vertex
;;   tex-coord
;;   normals
;;   faces
;;   texture)

(defun read-text-into-line-list (file-path)
  (with-open-file (stream file-path :direction :input
                          :if-does-not-exist :error)
    (let ((list nil))
      (do ((line (read-line stream nil nil) (read-line stream nil nil)))
          ((null line) nil)
        (push line list))
      (reverse list))))

(defun split-string-with-char (string &optional (char #\Space))
  (let ((list nil)
        (collect (make-dynamic-string)))
    (dotimes (i (length string))
      (let ((c (aref string i)))
        (if (not (char= c char))
            (vector-push-extend c collect)
            (progn (push collect list)
                   (setf collect (make-dynamic-string))))))
    (if (> (length collect) 0) (push collect list))
    (reverse list)))
;;(split-string-with-char "sadfsad/sdfasdfa/asfd" #\/)

(defun read-wavefront-obj-file (file-path)
  (let ((line-list (read-text-into-line-list file-path))
        (vertex-list nil)
        (tex-coord-list nil)
        (normal-list nil)
        (face-list nil))
    (dolist (string line-list)
      (let ((list (split-string-with-char string)))
        (cond ((string= "v" (car list))
               ;; v 0.123 0.234 0.345 1.0
               (if (= 4 (length (cdr list)))
                   ;; (0.123 0.234 0.345 1.0)
                   (push (mapcar #'float
                                 (mapcar
                                  #'parse-number:parse-number
                                  (cdr list)))
                         vertex-list)
                   (error (format nil
                                  "read-wavefront-obj-file: unknown vertex format: ~A"
                                  string))))
              ((string= "vt" (car list))
               ;; vt 0.500 1
               (if (= 2 (length (cdr list)))
                   ;; #(0.5 1.0)
                   (push (mapcar #'float
                                 (mapcar
                                  #'parse-number:parse-number
                                  (cdr list)))
                         tex-coord-list)
                   (error (format nil
                                  "read-wavefront-obj-file: unknown tex coord format: ~A"
                                  string))))
              ((string= "vn" (car list))
               ;; vn 0.707 0.000 0.707
               (if (= 3 (length (cdr list)))
                   ;; #(0.707 0.000 0.707)
                   (push (mapcar #'float
                                 (mapcar
                                  #'parse-number:parse-number
                                  (cdr list)))
                         normal-list)
                   (error (format nil
                                  "read-wavefront-obj-file: unknown normal format: ~A"
                                  string))))
              ((string= "f" (car list))
               ;; f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3
               ;; f 6/4/1 3/5/3 7/6/5
               ;; 1-indexed
               (if (= 3 (length (cdr list)))
                   ;; ((6 4 1) (3 5 3) (7 6 5))
                   (push (mapcar (lambda (string)
                                   (mapcar #'parse-integer
                                           (split-string-with-char string #\/)))
                                 (cdr list))
                         face-list)
                   (error (format nil
                                  "read-wavefront-obj-file: unknown face format: ~A"
                                  string)))))))
    (values (reverse vertex-list)
            (reverse tex-coord-list)
            (reverse normal-list)
            (reverse face-list))))

;; (multiple-value-bind (vertices tex-coords normals faces)
;;     (read-wavefront-obj-file #p"test.obj")
;;   (list :vertices vertices :tex-coords tex-coords :normals normals :faces faces))
