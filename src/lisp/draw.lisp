(defpackage :coffee.umbrella.clarity.draw
  (:use :cl :alexandria :coffee.umbrella.utils :coffee.umbrella.clarity.font)
  (:local-nicknames
   (:display :coffee.umbrella.clarity.display))
  (:import-from :coffee.umbrella.clarity.display #:*display*)
  (:export
   #:moveby
   #:with-graphics-group
   #:box
   #:cls
   #:with-clip-shape
   #:with-font
   #:textat
   #:rgba-fill
   #:poly
   #:clear-bounds
   #:qpoly
   #:rotateby
   #:scaleby
   #:reserve-box
   #:lozenge
   #:rounded-rect
   #:text))
(in-package :coffee.umbrella.clarity.draw)

;; drawing commands for user code that write to the current display

(defmacro with-graphics-group (() &body body)
  `(display:with-group (*display*)
     ,@body))

(declaim (inline draw-buffer))
(defun draw-buffer (buffer)
  (display:write-buffer *display* buffer))

(defun moveby (x y)
  (display:translate *display* x y))
(defun rotateby (a) ;; radians
  (display:rotate *display* a))
(defun scaleby (sx sy)
  (display:scale *display* sx sy))

(declaim (inline rgba-fill))
(defun rgba-fill (r g b a)
  (display:set-fill *display* r g b a))

(defun poly (&rest xyxy)
  (apply 'display:poly *display* xyxy))
(define-compiler-macro poly (&rest xy)
  `(display:poly *display* ,@xy))
(defun qpoly (&rest xyxy)
  (apply 'display:qpoly *display* xyxy))
(define-compiler-macro qpoly (&rest xy)
  `(display:qpoly *display* ,@xy))

(defun box (x y w h)
  (let ((x2 (+ w x))
        (y2 (+ h y)))
    (display:poly *display* x y x2 y x2 y2 x y2)))

(defun reserve-box (x y w h)
  (let ((x2 (+ w x))
        (y2 (+ h y)))
    (display:reserve *display* x y x2 y x2 y2 x y2)))

(defun lozenge (x y w h)
  (let ((xm (+ x (* w 0.5)))
        (ym (+ y (* h 0.5)))
        (x2 (+ w x))
        (y2 (+ h y)))
    (display:qpoly
     *display*
     x ym   x y
     xm y   x2 y
     x2 ym  x2 y2
     xm y2  x y2)))

(declaim (inline rounded-rect))
(defun rounded-rect (x y w h inset)
  (declare (optimize (speed 3) (safety 1))
           (type single-float x y w h inset))
  (let* ((i inset)
         (xm (+ x (* w 0.5)))
         (ym (+ y (* h 0.5)))
         (x2 (+ w x))
         (y2 (+ h y))
         (xi (+ x i))
         (yi (+ y i))
         (y2-i (- y2 i))
         (x2-i (- x2 i)))
    (declare (type single-float xm ym x2 y2 xi yi x2-i y2-i))
    (display:qpoly
     *display*
     x    yi    x  y
     xi   y    xm  y
     x2-i y    x2  y
     x2   yi   x2  ym
     x2   y2-i x2  y2
     x2-i y2   xm  y2
     xi   y2   x   y2
     x    y2-i x   ym))
  (values))

(defun text (string)
  (declare (optimize (speed 3) (safety 1)))
  (display:text *display* 0.0 0.0 string))

(defun textat (x y string)
  (declare (optimize (speed 3) (safety 1)))
  (display:text *display* x y string))

(defun %use-font (font)
  (if (eq :monospace (font-family font))
      (display:set-monospace *display*)
      (error "font family not supported ~A" (font-family font)))
  (display:set-font-size *display* (font-size font)))

(defun cls ()
  (display:clear-screen *display*)
  (%use-font *font*))

(defmacro with-font ((font) &body body)
  (once-only (font)
    `(unwind-protect
          (let ((*font* ,font)) (%use-font ,font) ,@body)
       (%use-font *font*))))

(defmacro with-clip-shape (() &body body)
  `(display:with-clip-group (*display*) ,@body))

(defun clear-bounds ()
  (display:clear-screen *display*))
