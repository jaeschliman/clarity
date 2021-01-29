(defpackage :coffee.umbrella.clarity.an-app
  (:use :cl :alexandria :trivia :bordeaux-threads
   :coffee.umbrella.utils
   :coffee.umbrella.model :coffee.umbrella.dot-access.symbols)
  (:local-nicknames
  (:mbox :coffee.umbrella.mailbox)
  (:conn :coffee.umbrella.clarity.connect)
  (:disp :coffee.umbrella.clarity.display)
  (:node :coffee.umbrella.clarity.simple-scene-graph-0)
  (:dstm :coffee.umbrella.clarity.datastream)))
(in-package :coffee.umbrella.clarity.an-app)

(defvar *app* nil)

(define-model world-stack ()
  ((index -1)
   (stack nil)))

(define-model app-stats ()
  ((total-times-drawn 0)
   (total-times-flushed 0)))

(define-model app ()
  ((stats (make-instance 'app-stats))
   (device-server (conn:ensure-global-device-server :ip "0.0.0.0" :port 1111))
   (host nil)
   (displays (vector nil nil nil nil nil nil nil nil nil nil))
   (keyboard nil)
   (worlds (make-instance 'world-stack))
   (package-list nil)
   (wants-display-refresh nil)
   (inbox nil)))

(defmacro with-step-app-bindings-and-restarts (() &body body)
  `(let ((*print-circle* t))
     (restart-case (progn ,@body)
       (exit-break-loop (&optional _)
         (declare (ignore _))
         (throw exit-tag :user-quit))
       (stop-work (&optional _)
         (declare (ignore _))
         (signal 'stop-work)))))

(defmacro app-do-active-displays (app (var) &body body)
  (once-only (app)
    `(let* ((flush-count 0)
            (doit (lambda (,var)
                    (let ((node:*current-display-device* ,var)
                          (node:*display* (.stream ,var)))
                      (node::with-active-world ((app-world ,app))
                        (progn ,@body))
                      (when (disp:flush (.stream ,var))
                        (incf flush-count))))))
       (loop for display across (app-displays ,app) do
            (when display (funcall doit display)))
       flush-count)))
