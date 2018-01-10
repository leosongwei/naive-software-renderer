(defconstant +empty-vertex+ (make-vertex))

(defun clip-line (v1 v2 axis plane direction)
  "CLIP-LINE
   vertices: v1->v2
   axis: 'x 'y 'z
   plane: plane on the axis, eg x=3
   direction: '+, '-, keep segment on direction
   return: keep: 'both-drop
                 'cut-2nd
                 'cut-1st
                 'both-keep
           v1'
           v2'"
  (let* ((p1 (vertex-ndc v1))
         (p2 (vertex-ndc v2))
         (axis-nth (ccase axis
                     (x 0) (y 1) (z 2)))
         ;; assuming w is the same,
         ;; because clip-line appears after vertex normalizing
         (delta-on-axis (- (aref p2 axis-nth)
                           (aref p1 axis-nth)))
         (sign (if (> delta-on-axis 0) +1 -1)) ;;s
         (d (ccase direction
              (+ +1.0)
              (- -1.0)))
         (dp (* d sign))) ;; d'
    (if (< (abs delta-on-axis) #.(expt 10 -15))
        ;; handle very small delta
        (let ((location (aref p1 axis-nth)))
          (if (>= (* (- location plane) d) 0)
              (values 'both-keep v1 v2)
              (values 'both-drop +empty-vertex+ +empty-vertex+)))
        ;; normal case
        (let* ((pt (/ (- plane (aref p1 axis-nth)) ;; point t
                      delta-on-axis)))
          ;;(format t " t=~A~% s=~A~% delta=~A~% dp=~A~%" pt sign delta-on-axis dp)
          (cond ((> pt 1)
                 (if (< dp 0)
                     (values 'both-keep v1 v2)
                     (values 'both-drop +empty-vertex+ +empty-vertex+)))
                ((< pt 0)
                 (if (> dp 0)
                     (values 'both-keep v1 v2)
                     (values 'both-drop +empty-vertex+ +empty-vertex+)))
                ((> dp 0)
                 (values 'cut-1st (interpolate-vertex v1 v2 pt) v2))
                ((< dp 0)
                 (values 'cut-2nd v1 (interpolate-vertex v1 v2 pt))))))))
;; (let ((v1 (make-vertex :ndc #(2.0 1.0 -3.0 1.0)))
;;       (v2 (make-vertex :ndc #(0.0 3.0 2.0 1.0))))
;;   (clip-line v1 v2 'x 1.0 '-))
;; (let ((v1 (make-vertex :ndc #(2.0 0.0 -3.0 1.0)))
;;       (v2 (make-vertex :ndc #(0.0 2.0 2.0 1.0))))
;;   (clip-line v1 v2 'y 1.0 '-))
;; (let ((v1 (make-vertex :ndc #(2.0 -2.0 -3.0 1.0)))
;;       (v2 (make-vertex :ndc #(0.0 2.0 2.0 1.0))))
;;   (clip-line v1 v2 'y -1.0 '-))
;; (let ((v1 (make-vertex :ndc #(2.0 -2.0 -3.0 1.0)))
;;       (v2 (make-vertex :ndc #(0.0 -1.5 2.0 1.0))))
;;   (clip-line v1 v2 'y -1.0 '+))
;; (let ((v1 (make-vertex :ndc #(2.0 -2.0 -3.0 1.0)))
;;       (v2 (make-vertex :ndc #(0.0 -1.5 2.0 1.0))))
;;   (clip-line v1 v2 'y 1.0 '-))
;; ;; delta very small
;; (let ((v1 (make-vertex :ndc #(2.0 -2.0 -3.0 1.0)))
;;       (v2 (make-vertex :ndc #(2.0 -2.0 -3.0 1.0))))
;;   (clip-line v1 v2 'y 1.0 '-))

;; (let* ((tri (make-triangle))
;;        (vc0 (vertex-coord (aref (triangle-vertices tri) 0))))
;;   (setf (aref vc0 0) 1.0)
;;   tri)
;; (make-triangle)

(defmacro bind-clip-line (clips &body body)
  (let* ((clip (car clips))
         (clip-values-expression (cadr clip))
         (clip-name-string (symbol-name (car clip)))
         (clip-keep-symbol (intern (concatenate 'string clip-name-string "-KEEP")))
         (clip-v1-symbol (intern (concatenate 'string clip-name-string "-V1")))
         (clip-v2-symbol (intern (concatenate 'string clip-name-string "-V2"))))
    (if (null (cdr clips))
        `(multiple-value-bind (,clip-keep-symbol ,clip-v1-symbol ,clip-v2-symbol)
             ,clip-values-expression
           ,@body)
        `(multiple-value-bind (,clip-keep-symbol ,clip-v1-symbol ,clip-v2-symbol)
             ,clip-values-expression
           (bind-clip-line ,(cdr clips) ,@body)))))
;; (macroexpand-1
;;  '(bind-clip-line ((clip-v1 (clip-values))
;;                    (clip-v2 (clip-values))
;;                    (clip-v3 (clip-values)))
;;    (list clip-v1-keep clip1-v1p clip1-v2p)))

(let ((c 'c))
  (ccase c
    ((a b) 1)
    (c 2)))

;; (defun clip-triangle (triangle axis plane direction)
;;   "clip-triangle
;;    triangle: triangle
;;    axis: 'x 'y 'z
;;    plane: plane on the axis, e.g. x=3
;;    direction: '+, '-
;;    return: list of triangles / nil"
;;   (let* ((vertices (triangle-vertices triangle))
;;          (triangles (list vertices)))
;;     (block :loop-on-clip-planes
;;       (dolist (clip-func (list (lambda (v1 v2) (clip-line v1 v2 'x -1.0 '+))
;;                                (lambda (v1 v2) (clip-line v1 v2 'x +1.0 '-))
;;                                (lambda (v1 v2) (clip-line v1 v2 'y -1.0 '+))
;;                                (lambda (v1 v2) (clip-line v1 v2 'y +1.0 '-))
;;                                (lambda (v1 v2) (clip-line v1 v2 'z 0.0 '+))
;;                                (lambda (v1 v2) (clip-line v1 v2 'z 1.0 '-))))
;;         (if (null triangles)
;;             (return-from :loop-on-clip-planes nil))
;;         (let ((triangles-local (copy-seq triangles)))
;;           (setf triangles nil)
;;           (dolist (triangle-current triangles-local)
;;             (let ((vertex-array (triangle-vertices triangle-current))
;;                   (v1 (aref vertex-array 0))
;;                   (v2 (aref vertex-array 1))
;;                   (v3 (aref vertex-array 2)))
;;               (bind-clip-line ((c12 (funcall clip-func v1 v2))
;;                                (c13 (funcall clip-func v1 v3))
;;                                (c23 (funcall clip-func v2 v3)))
;;                 (let* ((vertex-cutted (+ (ccase c12-keep ;; v1
;;                                            ((both-drop cut-1st) 1)
;;                                            ((both-keep cut-2nd) 0))
;;                                          (ccase c12-keep ;; v2
;;                                            ((both-drop cut-2nd) 1)
;;                                            ((both-keep cut-1st) 0))
;;                                          (ccase c23-keep ;; v3
;;                                            ((both-drop cut-2nd) 1)
;;                                            ((both-keep cut-1st) 0)))))
;;                   (case vertex-cutted
;;                     (0 (push triangle-current triangles))
;;                     (1 (....))
;;                     (2 (....))
;;                     (3 nil)))))))))
;;     triangles))




