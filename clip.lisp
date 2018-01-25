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
    (if (< (abs delta-on-axis) #.(expt 10 -6))
        ;; handle very small delta
        (let ((location (aref p1 axis-nth)))
          (if (>= (* (- location plane) d) 0)
              (values 'both-keep v1 v2)
              (values 'both-drop +empty-vertex+ +empty-vertex+)))
        ;; normal case
        (let* ((pt (/ (- plane (aref p1 axis-nth)) ;; point t
                      delta-on-axis)))
          ;;(format t " t=~A~% s=~A~% delta=~A~% dp=~A~%" pt sign delta-on-axis dp)
          (cond ((> pt #.(- 1.0 (expt 10 -6)))
                 (if (< dp 0)
                     (values 'both-keep v1 v2)
                     (values 'both-drop +empty-vertex+ +empty-vertex+)))
                ((< pt #.(+ 0.0 (expt 10 -6)))
                 (if (> dp 0)
                     (values 'both-keep v1 v2)
                     (values 'both-drop +empty-vertex+ +empty-vertex+)))
                ((> dp 0)
                 (values 'cut-1st (interpolate-vertex v1 v2 pt) v2))
                ((< dp 0)
                 (values 'cut-2nd v1 (interpolate-vertex v1 v2 pt))))))))
;; (let ((v1 (make-vertex :ndc #(0.5 -2.0 -3.0 1.0)))
;;       (v2 (make-vertex :ndc #(1.0 -2.0 -3.0 1.0))))
;;   (clip-line v1 v2 'x 1.0 '-))
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

(defun array-2-list (array)
  (let ((list nil))
    (dotimes (i (length array))
      (setf list (cons (aref array (- (length array) 1 i)) list)))
    list))

(defun clip-triangle (triangle)
  "clip-triangle
   input: triangle
   return: list of triangles"
  (let* ((vertices (triangle-vertices triangle))
         ;; ndc
         (v0-ndc (vertex-ndc (aref vertices 0)))
         (x0 (ndc-x v0-ndc))
         (y0 (ndc-y v0-ndc))
         (z0 (ndc-z v0-ndc))
         (v1-ndc (vertex-ndc (aref vertices 1)))
         (x1 (ndc-x v1-ndc))
         (y1 (ndc-y v1-ndc))
         (z1 (ndc-z v1-ndc))
         (v2-ndc (vertex-ndc (aref vertices 2)))
         (x2 (ndc-x v2-ndc))
         (y2 (ndc-y v2-ndc))
         (z2 (ndc-z v2-ndc)))
    (if (and (<= -1.0 x0 1.0) (<= -1.0 y0 1.0) (<= 0.00001 z0 1.0)
             (<= -1.0 x1 1.0) (<= -1.0 y1 1.0) (<= 0.00001 z1 1.0)
             (<= -1.0 x2 1.0) (<= -1.0 y2 1.0) (<= 0.00001 z2 1.0))
        ;; no need to be clipped
        (list triangle)
        ;; need to be clipped
        (let ((vertices-list (array-2-list vertices)))
          (block :loop-on-clip-planes
            (dolist (clip-func (list (lambda (v1 v2) (clip-line v1 v2 'x -1.0 '+))
                                     (lambda (v1 v2) (clip-line v1 v2 'x +1.0 '-))
                                     (lambda (v1 v2) (clip-line v1 v2 'y -1.0 '+))
                                     (lambda (v1 v2) (clip-line v1 v2 'y +1.0 '-))
                                     (lambda (v1 v2) (clip-line v1 v2 'z 0.00001 '+))
                                     (lambda (v1 v2) (clip-line v1 v2 'z 1.0 '-))))
              (if (null vertices-list)
                  (return-from :loop-on-clip-planes))
              (let* ((new-list vertices-list)
                     (first (car new-list)))
                (setf vertices-list nil)
                (dotimes (i (1- (length new-list)))
                  (let ((v1 (nth 0 new-list))
                        (v2 (nth 1 new-list)))
                    (mvb-let* ((keep v1p v2p (funcall clip-func v1 v2)))
                      (ccase keep
                        (both-keep (push v1p vertices-list))
                        (cut-2nd (progn (push v1p vertices-list)
                                        (push v2p vertices-list)))
                        (cut-1st (push v1p vertices-list))
                        (both-drop nil)))
                    (setf new-list (cdr new-list))))
                (let ((v1 (car new-list))
                      (v2 first))
                  (mvb-let* ((keep v1p v2p (funcall clip-func v1 v2)))
                    (ccase keep
                      (both-keep (push v1p vertices-list))
                      (cut-2nd (progn (push v1p vertices-list)
                                      (push v2p vertices-list)))
                      (cut-1st (push v1p vertices-list))
                      (both-drop nil))))
                (setf vertices-list (reverse vertices-list)))))
          (let* ((head (car vertices-list))
                 (triangle-num (- (length vertices-list) 2))
                 (triangles-list nil))
            (setf vertices-list (cdr vertices-list))
            (dotimes (i triangle-num)
              (push (build-triangle head
                                    (nth 0 vertices-list)
                                    (nth 1 vertices-list))
                    triangles-list)
              (setf vertices-list (cdr vertices-list)))
            triangles-list)))))

(defun print-triangle-ndc (triangle)
  "generate Kig script, loaded by pykig.py"
  (let* ((vertices (triangle-vertices triangle)))
    (let* ((vertex (aref vertices 0))
           (ndc (vertex-ndc vertex)))
      (format t "a = Point(~A, ~A)~%" (aref ndc 0) (aref ndc 2)))
    (let* ((vertex (aref vertices 1))
           (ndc (vertex-ndc vertex)))
      (format t "b = Point(~A, ~A)~%" (aref ndc 0) (aref ndc 2)))
    (let* ((vertex (aref vertices 2))
           (ndc (vertex-ndc vertex)))
      (format t "c = Point(~A, ~A)~%" (aref ndc 0) (aref ndc 2)))
    (format t "s = Segment(a,b)~%")
    (format t "s = Segment(b,c)~%")
    (format t "s = Segment(c,a)~%")))

;; (let* ((v1 (make-vertex :ndc #(-1.17 0.0 0.15 1.0)))
;;        (v2 (make-vertex :ndc #(+1.32 0.0 -0.22 1.0)))
;;        (v3 (make-vertex :ndc #(+0.23 0.0 1.22 1.0))))
;;   (mapcar #'print-triangle-ndc (clip-triangle
;;                                 (build-triangle v1 v2 v3))))
;; (let* ((v1 (make-vertex :ndc #(-1.3 0.0 0.2 1.0)))
;;        (v2 (make-vertex :ndc #(+1.3 0.0 0.2 1.0)))
;;        (v3 (make-vertex :ndc #(+0.5 0.0 1.2 1.0))))
;;   (mapcar #'print-triangle-ndc (clip-triangle
;;                                 (build-triangle v1 v2 v3))))
;; (let* ((v1 (make-vertex :ndc #(-0.5 0.0 0.2 1.0)))
;;        (v2 (make-vertex :ndc #(+0.5 0.0 0.2 1.0)))
;;        (v3 (make-vertex :ndc #(0.0 0.0 1.0 1.0))))
;;       ;; this produce 1 unecesarry triangle
;;   (mapcar #'print-triangle-ndc (clip-triangle
;;                                 (build-triangle v1 v2 v3))))

