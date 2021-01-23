(defpackage :coffee.umbrella.utils (:use :cl :alexandria)
            (:export
             #:doseq
             #:rev-doseq))
(in-package :coffee.umbrella.utils)

(defmacro doseq ((var form &optional result) &body body)
  (with-gensyms (doit)
    `(labels ((,doit (,var) ,@body))
       (declare (dynamic-extent #',doit))
       (map nil #',doit ,form)
       ,result)))

(defmacro rev-doseq ((var form &optional result) &body body)
  (with-gensyms (acc)
    `(progn
       (reduce (lambda (,var ,acc) (declare (ignore ,acc))
                       (prog1 nil ,@body)) ,form :initial-value nil :from-end t)
       ,result)))
