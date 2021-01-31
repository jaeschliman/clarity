(defpackage :coffee.umbrella.string (:use :cl :alexandria)
            (:export
             #:longest-line-length
             #:line-count
             #:char-dimensions
             #:insert-at
             #:ensure-length
             #:line-insert-at
             #:line-delete-at
             #:lines))
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

(defun insert-at (string insertion-string n)
  (concatenate 'string (subseq string 0 n) insertion-string (subseq string n)))

(defun ensure-length (string length character)
  (if (>= (length string) length) string
      (let* ((diff (- length (length string)))
             (add (make-string diff :initial-element character)))
        (concatenate 'string string add))))

(defun line-insert-at (string insertion-string n)
  (insert-at (ensure-length string n #\Space) insertion-string n))

(defun line-delete-at (string count n)
  (let* ((left (subseq string 0 (min n (length string))))
         (rstart (+ n count)))
    (if (<= rstart (length string))
        (concatenate 'string left (subseq string rstart))
        left)))

(defun lines (string)
  (with-input-from-string (input string)
    (loop for line = (read-line input nil nil)
         while line collect line)))
