(defpackage :coffee.umbrella.clarity.font (:use :cl :alexandria :coffee.umbrella.model)
            (:export
             #:*font*
             #:concrete-font
             #:font-variant
             ;; TODO: remove below exports in favor of dot-accessors
             #:font-family
             #:font-size
             #:font-measure-line
             #:font-mono-glyph-width
             #:font-mono-glyph-line-height
             #:font-mono-glyph-height))
(in-package :coffee.umbrella.clarity.font)

;; very basic font support.
;; at this point in the prototype, only monospace is supported,
;; and there is not (yet) support for custom fonts.
;; e.g. the glyph-width and glyph-height values below are hardcoded
;; for the default font in use in the current display implementations

;; eventually will send ttf fonts to the display,
;; use zpb-ttf to measure them on the lisp side,
;; and support variable-width type, bold, italic etc.
;; (there is a messy version of some of this elsewhere already)

(defgeneric font-family (thing))
(defgeneric font-size (thing))
(defgeneric font-mono-glyph-width  (thing))
(defgeneric font-mono-glyph-line-height (thing))

(define-model concrete-font ()
  ((family :monospace) ;; currently only monospace is supported
   (size 17.0)
   (glyph-width 0.618)
   (glyph-height 1.20)
   (weight :normal)))

(defvar *font* (make-instance 'concrete-font))

(defmethod font-family ((f concrete-font))
  (concrete-font-family f))
(defmethod font-size ((f concrete-font))
  (concrete-font-size f))
(defmethod (setf font-size) (value (f concrete-font))
  (setf (concrete-font-size f) value))
(defmethod font-mono-glyph-width ((f concrete-font))
  (concrete-font-glyph-width f))
(defmethod font-mono-glyph-height ((f concrete-font))
  (concrete-font-glyph-height f))

(define-model font-variant ()
  ((font *font*)
   (scale 1.0)))

(defmethod font-family ((f font-variant))
  (font-family (font-variant-font f)))
(defmethod font-size ((f font-variant))
  (* (font-variant-scale f) (font-size (font-variant-font f))))
(defmethod font-mono-glyph-width ((f font-variant))
  (* (font-variant-scale f) (font-mono-glyph-width (font-variant-font f))))
(defmethod font-mono-glyph-height ((f font-variant))
  (* (font-variant-scale f) (font-mono-glyph-height (font-variant-font f))))

(defmethod font-measure-line ((f concrete-font) string)
  (let ((s (concrete-font-size f)))
    (values (* s (length string) (concrete-font-glyph-width f))
            (* s (concrete-font-glyph-height f)))))

(defmethod font-measure-line ((f font-variant) string)
  (multiple-value-bind (w h) (font-measure-line (font-variant-font f) string)
    (let ((s (font-variant-scale f)))
      (values (* w s) (* h s)))))
