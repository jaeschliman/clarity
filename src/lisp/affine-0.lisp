(defpackage :coffee.umbrella.affine (:use :cl :alexandria) (:shadow #:rotate))
(in-package :coffee.umbrella.affine)

(deftype affine () '(simple-vector 9))

(defmacro fref (a y x) `(the single-float (svref (the affine ,a) ,(+ x (* y 3)))))

(defmacro backwards (&body body) `(progn ,@(reverse body)))
