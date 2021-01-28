(defpackage :coffee.umbrella.utils (:use :cl :alexandria :trivia)
            (:export
             #:doseq
             #:rev-doseq
             #:&&
             #:&fn
             #:&
             #:aref2
             #:plist-set
             #:quantize))
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


;; ---------------------------------------------------------------
;; the '&' macro
;; provides a shorthand for writing straight-line code, rather than lots of
;; nesting.
;; it is a bit experimental, and parts of it may be phased out, but it
;; helps keep things terse, especially in the presence of multiple-valued
;; functions etc.

;; the branching construct for &
(defmacro && (value &body body)
  `(match ,value
     ,@(loop for (test . rest) in body
          collect `(,test (& ,rest)))))

;; TODO: match args at toplevel
(defmacro &fn (name lambda-list &body body)
  `(defun ,name ,lambda-list (& ,@body)))

(defun %process-fn-def (def)
  (match def
    ((list name lambda-list body)
     (let* ((syms (loop for _ in lambda-list collect (gensym)))
            (clss (mapcar 'list lambda-list syms))
            (final (last-elt body))
            (other (butlast body)))
       `(,name ,syms (& ,(append clss other) ,final))))
    (_ (error "whoops"))))

(defmacro & ((&rest all-clauses) &body body)
  (match all-clauses
    ((list* clause clauses)
     (match clause
       ((list* '& (list* inner-clauses) inner-body)
        `(progn (& ,inner-clauses ,@inner-body)
                (& ,clauses ,@body)))
       ((list* '&& value match-clauses)
        `(progn
           (&& ,value ,@match-clauses)
           (& ,clauses ,@body)))
       ((list* '&fn name lambda-list fn-body)
        (let ((found (list (list name lambda-list fn-body))))
          (loop do
               (match clauses
                 (nil (return `(labels ,(mapcar '%process-fn-def found) ,@body)))
                 ((list* (list* '&fn name lambda-list fn-body) _)
                  (push (list name lambda-list fn-body) found))
                 (_ (return `(labels ,(mapcar '%process-fn-def found) (& ,clauses ,@body)))))
               (pop clauses))))
       ((list* :in macro-name macro-args)
        `(,macro-name ,@macro-args (& ,clauses ,@body)))
       ((list* :loop loop-forms)
        (if-let (idx (position-if (lambda (s) (and (symbolp s) (string= s "FINALLY"))) loop-forms))
          (let ((start (subseq loop-forms 0 idx))
                (end (subseq loop-forms idx)))
            `(loop ,@start do (& ,clauses ,@body) ,@end))
          `(loop ,@loop-forms do (& ,clauses ,@body))))
       ((list* :flet name vars fn-body)
        `(flet ((,name ,vars ,@fn-body)) (& ,clauses ,@body)))
       ((list* :do forms)
        `(progn ,@forms (& ,clauses ,@body)))
       ((list :when condition)
        `(when ,condition (& ,clauses ,@body)))
       ((list :unless condition)
        `(unless ,condition (& ,clauses ,@body)))
       ((list :doseq subpattern form)
        (with-gensyms (x acc doit)
          `(labels ((,doit (,acc ,x)
                     (declare (ignore ,acc))
                     (prog1 nil
                       (& ((,subpattern ,x) ,@clauses) ,@body))))
            (declare (dynamic-extent (function ,doit)))
            (reduce #',doit ,form :initial-value nil))))
       ((list :v patterns form)
        (let ((binds (loop repeat (length patterns) collect (gensym))))
          `(multiple-value-bind ,binds ,form
             (& ,(append (mapcar 'list patterns binds) clauses) ,@body))))
       ((list* :decl decls)
        `(locally (declare ,@decls)
           (& ,clauses ,@body)))
       ((list pattern form)
        (cond ((and (symbolp pattern)
                    (not (string= "_" (string pattern))))
               `(let ((,pattern ,form))
                  (& ,clauses ,@body)))
              (t `(match ,form
                    (,pattern
                     ,@(if clauses
                           (list `(& ,clauses ,@body))
                           body))
                    (_ (error "failed match of ~A" ',form))))))
       (_ (error "failed match of ~A" clause))))
    (_ `(progn ,@body))))
