(ql:quickload "sdl2")
(ql:quickload "cl-autowrap")
(ql:quickload "cffi")

(cffi:load-foreign-library #p"./cutils/libcutils.so")
(cffi:defcfun "SDL_Surface_pixels" :pointer
  (sdl2-surface-prt :pointer))

(cffi:defcfun "c_draw_line" :void
  (x0 :int) (y0 :int) (x1 :int) (y1 :int)
  (rgba :uint32) (pixels :pointer) (width :int))

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

(defun init-window (&key (title "New Game") (w 640) (h 480))
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
