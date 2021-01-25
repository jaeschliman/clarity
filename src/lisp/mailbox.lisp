(defpackage :coffee.umbrella.mailbox (:use
                                      :cl :alexandria
                                      :bordeaux-threads
                                      :coffee.umbrella.model
                                      :coffee.umbrella.dot-access.symbols)
            (:export
             #:mbox
             #:send
             #:receive))
(in-package :coffee.umbrella.mailbox)

(define-model mbox ()
  ((lock (make-lock "mailbox lock"))
   (variable (make-condition-variable :name "mailbox condition"))
   (thread nil)
   (queue nil)))

(defun send (mbox mail)
  (with-lock-held ((.lock mbox))
    (push mail (.queue mbox))
    (condition-notify (.variable mbox))))

(defun receive (mbox &key timeout)
  (let (mail)
    (with-lock-held ((.lock mbox))
      (condition-wait (.variable mbox) (.lock mbox) :timeout timeout)
      (setf mail (.queue mbox)
            (.queue mbox) nil))
    (reverse mail)))
