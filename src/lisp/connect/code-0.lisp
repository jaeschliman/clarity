(in-package :coffee.umbrella.clarity.connect)

;; very simple encoding for communicating with the display/input devices
;; sufficient for prototyping

(defparameter *header-registry (make-hash-table))

(defstruct tagged-header
  (1st-64 0 :type (unsigned-byte 64))
  (2nd-64 0 :type (unsigned-byte 64)))

(defstruct envelope tag content)

(defmacro define-header (name magic-number)
  `(progn
     (defstruct (,name (:include tagged-header)))
     (setf (gethash ,magic-number *header-registry) ',(symbolicate 'make- name))
     (defconstant ,(symbolicate 'hdr. name) ,magic-number)
     (defun ,name (2nd-64)
       (,(symbolicate 'make- name)
         :1st-64 ,magic-number
         :2nd-64 2nd-64))))
