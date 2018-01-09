(defconstant +empty-vertex+ (make-vertex))

(defun clip-line (v1 v2 axis plane direction)
  "CLIP-LINE
   vertices: v1->v2
   axis: 'x 'y 'z
   plane: plane on the axis, eg x=3
   direction: '+, '-
   return: keep: 'both-drop
                 'cut-2nd
                 'cut-1st
                 'both-keep
           v1'
           v2'"
  (let* ((p1 (vertex-coord v1))
         (p2 (vertex-coord v2))
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
         (dp (* d sign)) ;; d'
         (pt (/ (- plane (aref p1 axis-nth)) ;; point t
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
           (values 'cut-2nd v1 (interpolate-vertex v1 v2 pt))))))

;; (let ((v1 (make-vertex :coord #(2.0 1.0 -3.0 1.0)))
;;       (v2 (make-vertex :coord #(0.0 3.0 2.0 1.0))))
;;   (clip-line v1 v2 'x 1.0 '-))
;; (let ((v1 (make-vertex :coord #(2.0 0.0 -3.0 1.0)))
;;       (v2 (make-vertex :coord #(0.0 2.0 2.0 1.0))))
;;   (clip-line v1 v2 'y 1.0 '-))
;; (let ((v1 (make-vertex :coord #(2.0 -2.0 -3.0 1.0)))
;;       (v2 (make-vertex :coord #(0.0 2.0 2.0 1.0))))
;;   (clip-line v1 v2 'y -1.0 '-))
;; (let ((v1 (make-vertex :coord #(2.0 -2.0 -3.0 1.0)))
;;       (v2 (make-vertex :coord #(0.0 -1.5 2.0 1.0))))
;;   (clip-line v1 v2 'y -1.0 '+))
;; (let ((v1 (make-vertex :coord #(2.0 -2.0 -3.0 1.0)))
;;       (v2 (make-vertex :coord #(0.0 -1.5 2.0 1.0))))
;;   (clip-line v1 v2 'y 1.0 '-))
