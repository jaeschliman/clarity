(defpackage :coffee.umbrella.string (:use :cl :alexandria)
            (:export
             #:longest-line-length
             #:line-count
             #:char-dimensions))
(in-package :coffee.umbrella.string)

(defun longest-line-length (string)
  (let ((best 0) (end (length string)))
    (do* ((left 0 (1+ right))
          (right (or (position #\Newline string :start 0)
                     end)
                 (or (position #\Newline string :start (1+ right))
                     end)))
         ((= right end) (max best (- right left)))
      (maxf best (- right left)))))

(defun line-count (string)
  (1+ (count #\Newline string)))

(defun char-dimensions (string)
  (values (longest-line-length string)
          (line-count string)))
