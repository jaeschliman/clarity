(defpackage :coffee.umbrella.model (:use :cl :alexandria :trivial-garbage :coffee.umbrella.dot-access :coffee.umbrella.utils)
            (:export
             #:define-model
             #:have
             #:have-slot))
(in-package :coffee.umbrella.model)

;; package for defining classes with a shorthand, support for dot-access, and change detection

(defvar *model-macroexpanders* (make-weak-hash-table :weakness :key))
(defvar *model-macroexpand-before* (make-weak-hash-table :weakness :key))

(defun model-find-expander (class-name)
  (gethash class-name *model-macroexpanders*))
(defun model-find-before-expander (class-name)
  (gethash class-name *model-macroexpand-before*))

(defun model-find-super-class-names (direct-superclasses)
  (remove-duplicates
   (append
    (mapcar 'class-name
            (mappend (lambda (class) (cons class (closer-mop:class-precedence-list class)))
                     (mapcar 'find-class direct-superclasses)))
    '(t))))

(defun model-gather-symbol-property (name direct-superclasses)
  (remove-if 'null (mapcar (lambda (s) (getf (symbol-plist s) name))
                           (model-find-super-class-names direct-superclasses))))

(defun model-find-expanders (direct-superclasses)
  (let ((cpl (model-find-super-class-names direct-superclasses)))
    (remove-if 'null (append (mapcar 'model-find-before-expander cpl)
                             (mapcar 'model-find-expander cpl)))))

(defun model-definition-macroexpand-1 (preamble name superclasses slots meta postamble)
  (doseq (fn (model-find-expanders superclasses))
    (multiple-value-setq (preamble
                          name superclasses slots meta
                          postamble)
      (funcall fn preamble name superclasses slots meta postamble)))
  (values preamble name superclasses slots meta postamble))

(defmacro define-model-expander (class-name phase (preamble name superclasses slots meta postamble)
                                 &body body)
  (check-type phase (member t :before))
  (let ((lambda `(lambda (,preamble ,name ,superclasses ,slots ,meta ,postamble)
                   ,@body)))
    (case phase
      (:before `(setf (gethash ',class-name *model-macroexpand-before*) ,lambda))
      (otherwise `(setf (gethash ',class-name *model-macroexpanders*) ,lambda)))))
