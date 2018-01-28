(cffi:load-foreign-library #p"./cutils/libcutils.so")
(cffi:defcfun "SDL_Surface_pixels" :pointer
  (sdl2-surface-prt :pointer))

(cffi:defcfun "c_draw_line" :void
  (x0 :int) (y0 :int) (x1 :int) (y1 :int)
  (rgba :uint32) (pixels :pointer) (width :int))

(cffi:defcfun "read_image" :pointer
  (filepath :pointer) (x-ptr :pointer) (y-ptr :pointer))

(cffi:defcfun "free_img" :pointer
  (data-ptr :pointer))

(defstruct texture
  (h 0 :type integer)
  (w 0 :type integer)
  (r (make-array '(1 1) :element-type 'single-float :initial-element 0.0)
     :type (simple-array single-float (* *)))
  (g (make-array '(1 1) :element-type 'single-float :initial-element 0.0)
     :type (simple-array single-float (* *)))
  (b (make-array '(1 1) :element-type 'single-float :initial-element 0.0)
     :type (simple-array single-float (* *))))

(defun read-image-to-texture (image-path)
  (cffi:with-foreign-string (path image-path)
    (cffi:with-foreign-objects ((x-ptr :int 1)
                                (y-ptr :int 1))
      (let* ((data (read-image path x-ptr y-ptr))
             (w (cffi:mem-ref x-ptr :int))
             (h (cffi:mem-ref y-ptr :int))
             (r-a (make-array `(,w ,h)
                              :element-type 'single-float
                              :initial-element 0.0))
             (g-a (make-array `(,w ,h)
                              :element-type 'single-float
                              :initial-element 0.0))
             (b-a (make-array `(,w ,h)
                              :element-type 'single-float
                              :initial-element 0.0)))
        (dotimes (y h)
          (dotimes (x w)
            (let* ((pixel-offset (* 3 (+ x (* y w))))
                   (r (cffi:mem-aref data :unsigned-char pixel-offset))
                   (g (cffi:mem-aref data :unsigned-char (+ 1 pixel-offset)))
                   (b (cffi:mem-aref data :unsigned-char (+ 2 pixel-offset))))
              (setf (aref r-a x y) (float (/ r 255)))
              (setf (aref g-a x y) (float (/ g 255)))
              (setf (aref b-a x y) (float (/ b 255))))))
        (free-img data)
        (make-texture :h h :w w :r r-a :g g-a :b b-a)))))

(defun get-sdl2-surface-pixels (sdl2-surface)
  (sdl-surface-pixels (sdl2-ffi::sdl-surface-ptr sdl2-surface)))


(defun map-color (r g b &optional (a 255))
  (declare (type (unsigned-byte 8) r g b a))
  (let ((result (+ (ash r 24) (ash g 16) (ash b 8) a)))
    (declare (type (unsigned-byte 32) result))
    result))

(defun clear (&optional (r 150) (g 50) (b 50))
  (sdl2:fill-rect *sdl2-surface* nil (map-color r g b)))

(defun update-win ()
  (sdl2:blit-surface *sdl2-surface* nil (sdl2:get-window-surface *sdl2-window*) nil)
  (sdl2:render-present (sdl2:get-renderer *sdl2-window*)))

(defun init-window (&key (title "Naive Software Renderer!!!") (w 640) (h 480))
  (setf *w* w) (setf *h* h)
  (sdl2:init :everything)
  (setf *pixel-buffer* (make-array `(,h ,w) :element-type '(unsigned-byte 32)))
  (setf *sdl2-window* (sdl2:create-window :title title
                                          :w w :h h
                                          :flags '(:shown)))
  (setf *sdl2-renderer* (sdl2:create-renderer *sdl2-window* -1 '(:software)))
  (setf *sdl2-surface* (sdl2:create-rgb-surface *w* *h* 32
                                                :r-mask #xff000000
                                                :g-mask #x00ff0000
                                                :b-mask #x0000ff00
                                                :a-mask #x000000ff))
  (setf *sdl2-pixel-buffer* (get-sdl2-surface-pixels *sdl2-surface*))
  (clear)
  (update-win))

(defun destroy-window ()
  (setf *sdl2-pixel-buffer* nil)
  (sdl2:free-surface *sdl2-surface*)
  (sdl2:destroy-window *sdl2-window*)
  (sdl2:destroy-renderer *sdl2-renderer*)
  (sdl2:quit))
