(defpackage :coffee.umbrella.dot-access (:use :cl :alexandria)
            (:export
             #:define-dot-accessor))
(in-package :coffee.umbrella.dot-access)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :coffee.umbrella.dot-access.symbols)
    (make-package :coffee.umbrella.dot-access.symbols :use nil)))

(defun dot-name (name)
  (let ((s (intern (concatenate 'string "." (string name)) :coffee.umbrella.dot-access.symbols)))
    (prog1 s
      (export s :coffee.umbrella.dot-access.symbols))))

(defmacro define-dot-accessor (class-name property-name accessor-name)
  (let ((dot (dot-name property-name)))
    `(progn
       (defmethod ,dot ((,class-name ,class-name)) (,accessor-name ,class-name))
       (defmethod (setf ,dot) (new-value (,class-name ,class-name))
         (setf (,accessor-name ,class-name) new-value)))))
