(defpackage :coffee.umbrella.list (:use :cl :alexandria :trivia)
            (:export
             #:remove-at
             #:replace-at
             #:insert-at))
(in-package :coffee.umbrella.list)

(defun insert-at (list item n)
  (append (subseq list 0 n) (list item) (subseq list n)))
(defun replace-at (list item n)
  (append (subseq list 0 n) (list item) (subseq list (1+ n))))
(defun remove-at (list n)
  (append (subseq list 0 n) (subseq list (1+ n))))
