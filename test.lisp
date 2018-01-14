(defparameter *eye* (make-array 3 :element-type 'single-float))

(defparameter *project-mat*
  (frustum-mat 20 (/ 4 3) 0.3 15))

(defparameter *bunny-mesh* (wavefront-file-to-modelmesh #p"bunny.obj"))
;; (list :vertices ;; v
;;       (length (modelmesh-vertices *bunny-mesh*))
;;       :tex-coords ;; vt
;;       (length (modelmesh-tex-coords *bunny-mesh*))
;;       :normal ;; vn
;;       (length (modelmesh-normals *bunny-mesh*)))

(init-window :w 640 :h 480)

;; normal projections are not calculated yet!
(time
 (let* ((vertices (modelmesh-vertices *bunny-mesh*))
        (tex-coords (modelmesh-tex-coords *bunny-mesh*))
        (normals (modelmesh-normals *bunny-mesh*))
        (faces (modelmesh-faces *bunny-mesh*))
        ;; -------------------------
        (trans-mat (3d-trans-mat 0.0 -1.0 -10.0))
        (scale-mat (3d-scale 5.0)))
   (dotimes (i 1000)
     (clear)
     (let* ((rot-mat (3d-rotate-y (mod i 360)))
            (trans-world (mul-44-44 trans-mat
                                    (mul-44-44 rot-mat scale-mat)))
            (world-coords (apply-transform vertices trans-world))
            (proj-coords (apply-transform world-coords *project-mat*))
            (ndc-coords (vertices-ndc proj-coords)))
       (dotimes (i (length faces))
         (let* ((face (aref faces i))
                (triangle (build-triangle-from-face
                           face world-coords ndc-coords normals tex-coords))
                (cliped-triangles (clip-triangle triangle)))
           (mapcar #'draw-triangle-wire-ndc cliped-triangles))))
     (update-win))))

;; (destroy-window)

;; (require :sb-sprof)
;; (sb-sprof:with-profiling (:max-samples 20000
;;                                        :mode :alloc
;;                                        :report :flat)
;;   (let* ((vertices (modelmesh-vertices *bunny-mesh*))
;;          (tex-coords (modelmesh-tex-coords *bunny-mesh*))
;;          (normals (modelmesh-normals *bunny-mesh*))
;;          (faces (modelmesh-faces *bunny-mesh*))
;;          ;; -------------------------
;;          (trans-mat (3d-trans-mat 0.0 -1.0 -10.0))
;;          (scale-mat (3d-scale 5.0)))
;;     (dotimes (i 1000)
;;       (clear)
;;       (let* ((rot-mat (3d-rotate-y (mod i 360)))
;;              (trans-world (mul-44-44 trans-mat
;;                                      (mul-44-44 rot-mat scale-mat)))
;;              (world-coords (apply-transform vertices trans-world))
;;              (proj-coords (apply-transform world-coords *project-mat*))
;;              (ndc-coords (vertices-ndc proj-coords)))
;;         (dotimes (i (length faces))
;;           (let* ((face (aref faces i))
;;                  (triangle (build-triangle-from-face
;;                             face world-coords ndc-coords normals tex-coords))
;;                  (cliped-triangles (clip-triangle triangle)))
;;             (mapcar #'draw-triangle-wire-ndc cliped-triangles))))
;;       (update-win))))

;; (time
;;  (let* ((vertices (modelmesh-vertices *bunny-mesh*))
;;         (tex-coords (modelmesh-tex-coords *bunny-mesh*))
;;         (normals (modelmesh-normals *bunny-mesh*))
;;         (faces (modelmesh-faces *bunny-mesh*))
;;         ;; -------------------------
;;         (trans-mat (3d-trans-mat 0.0 -1.0 -10.0))
;;         (scale-mat (3d-scale 5.0)))
;;    (dotimes (i 1000)
;;      ;;(clear)
;;      (let* ((rot-mat (3d-rotate-y (mod i 360)))
;;             (trans-world (mul-44-44 trans-mat
;;                                     (mul-44-44 rot-mat scale-mat)))
;;             (world-coords (apply-transform vertices trans-world))
;;             (proj-coords (apply-transform world-coords *project-mat*))
;;             (ndc-coords (vertices-ndc proj-coords)))
;;        (dotimes (i (length faces))
;;          (let* ((face (aref faces i))
;;                 (triangle (build-triangle-from-face
;;                            face world-coords ndc-coords normals tex-coords))
;;                 (cliped-triangles (clip-triangle triangle)))
;;            cliped-triangles))))))


;; (progn
;; (defparameter *black-hole* nil)
;; (gc :full t)

;; (time
;;  (let* ((vertices (modelmesh-vertices *bunny-mesh*))
;;         (tex-coords (modelmesh-tex-coords *bunny-mesh*))
;;         (normals (modelmesh-normals *bunny-mesh*))
;;         (faces (modelmesh-faces *bunny-mesh*))
;;         ;; -------------------------
;;         (trans-mat (3d-trans-mat 0.0 -1.0 -10.0))
;;         (scale-mat (3d-scale 5.0)))
;;    (dotimes (i 1000)
;;      ;;(clear)
;;      (let* ((rot-mat (3d-rotate-y (mod i 360)))
;;             (trans-world (mul-44-44 trans-mat
;;                                     (mul-44-44 rot-mat scale-mat)))
;;             (world-coords (apply-transform vertices trans-world))
;;             (proj-coords (apply-transform world-coords *project-mat*))
;;             (ndc-coords (vertices-ndc proj-coords)))
;;        (dotimes (i (length faces))
;;          (let* ((face (aref faces i))
;;                 (triangle (build-triangle-from-face
;;                            face world-coords ndc-coords normals tex-coords)))
;;            (push triangle *black-hole*))))))))