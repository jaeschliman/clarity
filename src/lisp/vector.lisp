(defpackage :coffee.umbrella.vector (:use :cl :alexandria)
            (:export
             #:make
             #:pop-index
             #:move
             #:push-index
             #:delete-1))
(in-package :coffee.umbrella.vector)

;; utils for manipulating extensible vectors of element-type t

(defun make (&optional (initial-capacity 16))
  (make-array initial-capacity :element-type t :adjustable t :fill-pointer 0))

(defun pop-index (idx evector)
  (loop for i from idx below (1- (length evector)) do
       (setf (aref evector i) (aref evector (1+ i))))
  (decf (fill-pointer evector)))

(defun move (oldidx newidx evector)
  (let ((item (aref evector oldidx)))
    (if (< oldidx newidx)
        (loop for i from oldidx below newidx do
             (setf (aref evector i) (aref evector (1+ i))))
        (loop for i from oldidx downto (1+ newidx) do
             (setf (aref evector i) (aref evector (1- i)))))
    (setf (aref evector newidx) item)))

(defun push-index (item idx evector)
  (vector-push-extend item evector)
  (move (1- (length evector)) idx evector))

(defun delete-1 (item evector)
  (when-let (p (position item evector))
    (pop-index p evector)))
