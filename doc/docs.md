# SDL buffer format

The pixel buffer is come from a SDL structure called `SDL_Surface`. This is done in the `libcutils.so` in the `cutils` directory.

* `void* SDL_Surface_pixels(SDL_Surface* surface);`

This function is called by `(get-sdl2-surface-pixels sdl2-surface)` in the `init.lisp`.

The `SDL_Surface` is created in `(init-window ...)`, by:

```
(setf *sdl2-surface*
  (sdl2:create-rgb-surface *w* *h* 32
                           :r-mask #xff000000
                           :g-mask #x00ff0000
                           :b-mask #x0000ff00
                           :a-mask #x000000ff))
(setf *sdl2-pixel-buffer* (get-sdl2-surface-pixels *sdl2-surface*))
```

This `*sdl2-pixel-buffer*` is a raw pointer which will be passed to `(draw-line-c ...)` in the `(draw-fragments)`.
