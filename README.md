naive-software-renderer
=======================

Stay simple, stay naive. (Made with lisp)

<img src="bunny.gif" />

<img src="bunny_china.jpg" />

Require:

* SBCL, QuickLisp
* cmake, make, gcc
* SDL2

How to:

* `cd` into `cutils`, `$ cmake CMakeLists.txt`, `make`
* `cd` into source code directory
* Rotating flat shaded bunny: `$ sbcl --load example.lisp`
* Phong shading bunny: `$sbcl --load example_phong.lisp`
