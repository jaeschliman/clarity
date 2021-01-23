(defpackage :coffee.umbrella.clarity.datastream (:use :cl :alexandria
                                                      :coffee.umbrella.model :coffee.umbrella.utils)
            (:export
             #:transform-datastream
             #:send
             #:add-listener
             #:listen-for-close
             #:for-each
             #:receive
             #:signal-termination
             #:stop-listening))
(in-package :coffee.umbrella.clarity.datastream)

(define-model datastream ()
    ((parent-stream nil)
     (child-streams nil :type list)
     ;;TODO: should use a transducer like thing instead to avoid consing
     (input-filter #'list :type function)
     (output-filter #'list :type function)
     (listeners nil :type list)))

(defun make (&key parent-stream child-streams input-filter output-filter listeners)
  (make-instance 'datastream
                 :parent-stream parent-stream
                 :child-streams child-streams
                 :input-filter input-filter
                 :output-filter output-filter
                 :listeners listeners))

;; datastream -> datastream
(defmethod transform-datastream (ds &key (input #'list) (output #'list))
  (let ((result (make :parent-stream ds
                      :input-filter input
                      :output-filter output)))
    (push result (datastream-child-streams ds))
    result))

(defmethod send (object (ds datastream))
  (doseq (it (funcall (datastream-output-filter ds) object))
    (send it (datastream-parent-stream ds))))

(defmethod add-listener (fn state (ds datastream))
  (push (cons fn state) (datastream-listeners ds)))

(defmethod listen-for-close ((ds datastream) fn)
  "Takes the target datastream, and a callback to be executed
   with one argument when the stream closes.
   The argument is the datastream. "
  (add-listener (lambda (stream stream-state context event)
                  (declare (ignore context event))
                  (when (eq stream-state :closed)
                    (funcall fn stream)))
                nil ds))

(defmethod for-each ((ds datastream) context fn)
  "Takes the target datastream, and a callback to be executed
   for each event in the datastream. The callback receives two
   arguments, the user-provided context, and the event from the stream."
  (add-listener (lambda (stream stream-state context event)
                  (declare (ignore stream))
                  (unless (eq stream-state :closed)
                    (funcall fn context event)))
                context ds))

(defmethod receive (object (ds datastream))
  (doseq (it (funcall (datastream-input-filter ds) object))
    (let ((to-remove nil))
      (doseq (fn-state (datastream-listeners ds))
        (let ((ok nil))
          (catch '%remove
            (funcall (car fn-state) ds nil (cdr fn-state) it)
            (setf ok t))
          (unless ok (push fn-state to-remove))))
      (setf (datastream-listeners ds)
            (set-difference (datastream-listeners ds) to-remove))
      (doseq (ds2 (datastream-child-streams ds))
        (receive it ds2)))))

(defmethod signal-termination ((ds datastream))
  (doseq (pair (datastream-listeners ds))
    (funcall (car pair) ds :closed (cdr pair) nil))
  (setf (datastream-listeners ds) nil)
  (map nil 'signal-termination (datastream-child-streams ds)))

(defun stop-listening () (throw '%remove nil))
