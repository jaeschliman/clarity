(in-package :coffee.umbrella.utils)

(declaim (inline aref2))
(defun aref2 (a i j)
  (values (aref a i) (aref a j)))

(defun plist-set (plist key value)
  (prog1 value
    (loop for sublist on plist by 'cddr do
      (when (eq (car sublist) key)
        (setf (cadr sublist) value)
        (return))
      (unless (cddr sublist)
        (setf (cdr (last sublist)) (list key value))))))

(defun quantize (float step &optional (centering 0.5))
  (declare (optimize (speed 3) (safety 1))
           (type single-float float step centering))
  (+ (* step centering) (* step (ffloor float step))))
