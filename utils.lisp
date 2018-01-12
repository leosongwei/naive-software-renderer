(defun make-dynamic-string ()
  (make-array 0 :adjustable t
              :fill-pointer 0
              :element-type 'character))
;; (let ((s (make-dynamic-string)))
;;   (vector-push-extend #\a s)
;;   (vector-push-extend #\b s)
;;   s)
