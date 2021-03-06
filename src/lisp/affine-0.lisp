(defpackage :coffee.umbrella.affine (:use :cl :alexandria) (:shadow #:rotate)
            (:export
             #:component-values
             #:make-affine
             #:load-standard-inverse-node-transform
             #:load-standard-node-transform
             #:get-translation-rotation-and-scale
             #:transform-xy
             #:four-points-contain-point?
             #:l-translate
             #:l-rotate
             #:l-scale
             #:translate
             #:rotate
             #:scale
             #:translation))
(in-package :coffee.umbrella.affine)

(deftype affine () '(simple-vector 9))

(defmacro fref (a y x) `(the single-float (svref (the affine ,a) ,(+ x (* y 3)))))

(defmacro backwards (&body body) `(progn ,@(reverse body)))
