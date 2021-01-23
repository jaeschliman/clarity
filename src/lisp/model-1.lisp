(in-package :coffee.umbrella.model)

;; TODO: ability to prefix accessors for a slot (e.g. with %)
;; TODO: ability to suppress dot access for a slot (mark :private)
(defun expand-slot-shorthands (slot)
  (if (symbolp slot)
      `(,slot :initarg ,(intern (string slot) :keyword))
      `(,(car slot) :initarg ,(intern (string (car slot)) :keyword) :initform ,(cadr slot))))

(defun create-slot-accessors (name slotspec)
  (let* ((slot (car slotspec))
         (accessor-name (symbolicate name '- slot)))
    `((declaim (inline ,accessor-name))
      (defun ,accessor-name (,name)
        (declare (optimize (speed 3) (safety 1))
                 (type ,name ,name))
        (slot-value ,name ',slot))
      (declaim (inline (setf ,accessor-name)))
      (defun (setf ,accessor-name) (new-value ,name)
        (let ((old-value (slot-value ,name ',slot)))
          (prog1
              (setf (slot-value ,name ',slot) new-value)
            (loop for hook in (slot-value ,name 'shared-update-hooks) do
              (funcall hook ,name ',slot new-value))
            (unless (eq old-value new-value)
              (loop for (hook object slot) in (slot-value ,name 'update-hooks) do
                (when (or (null slot) (eq slot ',slot))
                  (funcall hook ,name object ',slot old-value new-value)))))))
      (define-dot-accessor ,name ,slot ,accessor-name))))

(defun keep (item sequence &key (key 'identity) (test 'eq))
  (remove-if-not (lambda (it) (funcall test it item)) sequence :key key))

(define-model-expander t :before (preamble name superclasses slots meta postamble)
  (setq slots (mapcar 'expand-slot-shorthands slots))
  (let ((hook (cadr (find :direct-update-hook meta :key 'car))))
    (when hook
      (appendf postamble `((setf (getf (symbol-plist ',name) 'direct-update-hook)
                                 ',hook))))
    (appendf postamble `((closer-mop:ensure-finalized (find-class ',name))))
    (appendf meta
             (mapcar (lambda (s) (list :direct-update-hook s))
                     (model-gather-symbol-property 'direct-update-hook superclasses)))
    (values preamble name superclasses slots meta postamble)))

(define-model-expander t t (preamble name superclasses slots meta postamble)
  (let ((direct-update-hooks (map 'list 'cadr (keep :direct-update-hook meta :key 'car))))
    (setq meta (remove :direct-update-hook meta :key 'car))
    (doseq (hook direct-update-hooks) (check-type hook (or null symbol)))
    (appendf postamble
             (mappend (lambda (slot) (create-slot-accessors name slot)) slots))
    (push `(shared-update-hooks :allocation :class
                                :initform ',direct-update-hooks)
          slots)
    (when (null superclasses)
      (push `(update-hooks :initform nil) slots))
    (values preamble name superclasses slots meta postamble)))

(defmacro define-model (name (&rest superclasses) (&rest slots) &rest meta)
  (multiple-value-bind (preamble name superclasses slots meta postamble)
      (model-definition-macroexpand-1 nil name superclasses slots meta nil)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@preamble
       (defclass ,name ,superclasses ,slots ,@meta)
       ,@postamble)))

#+nil
(progn
  (defun foo-handle-change (foo slot new-value)
    (format t "foo ~A has a new ~A : ~A~%" foo slot new-value))
  (defun bar-handle-change (bar slot new-value)
    (format t "bar ~A has a new ~A : ~A~%" bar slot new-value))
  (define-model foo () ((baz 0) quux) (:direct-update-hook foo-handle-change))
  (define-model bar (foo) (zonk) (:direct-update-hook bar-handle-change))
  (defparameter *x (make-instance 'foo))
  (setf (foo-baz *x) (random 100))
  (defparameter *y (make-instance 'bar))
  (setf (foo-baz *y) (random 100)))


;; TODO: would be nice for both of these to use weak pointers to object, and a
;; facility for un-listening will be needed at some point

;; macros for per-instance change-detection and updates
;; e.g. (have model update view) => calls 'update when model reports a change
(defmacro have (subject verb object)
  (check-type verb symbol)
  (once-only (subject object)
    `(pushnew (list ',verb ,object nil) (slot-value ,subject 'update-hooks) :test 'equal)))

(defmacro have-slot (subject slot verb object)
  (once-only (subject object)
    `(pushnew (list ',verb ,object ',slot) (slot-value ,subject 'update-hooks) :test 'equal)))
