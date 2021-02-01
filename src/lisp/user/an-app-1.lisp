(in-package :coffee.umbrella.clarity.an-app)

(define-condition stop-work (error) ())

(defun send-work (mbox work)
  (with-lock-held ((.lock mbox))
    (push (cons (get-internal-real-time) work) (.queue mbox))
    (condition-notify (.variable mbox))))

(defun stop-work (mbox)
  (when-let (thread (.thread mbox))
    (multiple-value-bind (_ err?) (ignore-errors (interrupt-thread thread  (lambda () (signal 'stop-work))))
      (declare (ignore _))
      (when err?
        (format t "[APP] Error stopping work: ~A~%" err?)))))

(defmethod world-stack-current ((world-stack world-stack))
  (nth (world-stack-index world-stack)
       (world-stack-stack world-stack)))

(defmethod app-world ((app app))
  (world-stack-current (app-worlds app)))


(defun make-user-display-for-stream (stream id)
  (make-instance 'node:device.display
                 :id id
                 :stream stream
                 :bounds nil))

(defun make-user-keyboard-for-stream (stream id)
  (make-instance 'node:device.keyboard
                 :id id
                 :stream stream))

(defvar *outer-debugger-hook* nil)
(defvar *custom-debugger-hook* 'break-loop-debugger)
(defvar *use-custom-debugger* t)
(defvar *break-level* -1)
(defvar *sentinel* nil)

(defun app-debugger-hook ()
  (if *use-custom-debugger* *custom-debugger-hook* *outer-debugger-hook*))

(defun %draw-and-simulate-app (step-function app exit-tag)
  ;; simulate first, then draw
  (when (plusp (app-active-display-count app))
    (with-step-app-bindings-and-restarts ()
      (with-simple-restart (skip "Skip this frame")
        (funcall step-function app (cons (get-internal-real-time) :simulate))))
    (with-step-app-bindings-and-restarts ()
      (with-simple-restart (skip "Skip this frame")
        (funcall step-function app  (cons (get-internal-real-time) :frame))))))

(defun step-app (step-function app work exit-tag)
  (let ((*debugger-hook* *outer-debugger-hook*))
    ;; handle user/device input, if any
    (when work
      (rev-doseq (item work)
        (with-step-app-bindings-and-restarts ()
          (with-simple-restart (skip "Skip this input ~A" item)
            (let ((*debugger-hook* (app-debugger-hook))
                  #+nil(*standard-input* (app-input-stream)))
              (funcall step-function app item))))))
    ;; draw/step a frame
    (%draw-and-simulate-app step-function app exit-tag)))

;; er, uh actually it is in seconds
(defvar *app-frame-wait-ms* 0.0125)

(defun do-run-loop-inner (inbox app step-function)
  (let* ((sentinel (list 'sentinel 'for 'break 'level *break-level*))
         (*sentinel* sentinel))
    (catch sentinel
      (prog1 :normal-exit
        (loop
           with work = nil
           do
             (with-lock-held ((.lock inbox))
               (let ((world (app-world app)))
                 (condition-wait
                  (.variable inbox) (.lock inbox)
                  :timeout (when (plusp (node:world-active-simulation-count world))
                             *app-frame-wait-ms*)))
               (setf work (.queue inbox)
                     (.queue inbox) nil))
             (step-app step-function app work sentinel))))))

(defvar *step-function* nil)

(defun do-run-loop (inbox app step-function)
  (let* ((*break-level* (1+ *break-level*))
         (*app* app)
         (*step-function* step-function)
         (exit-status (do-run-loop-inner inbox app step-function)))
    (unless (eq exit-status :normal-exit)
      (when (eq *break-level* 0)
        (signal 'stop-work))
      (match exit-status
        ;; user quit is when the user gives up (dismisses the debugger)
        (:user-quit ;; should return control to containing run loop
         (format t "user quit this break loop!!~%")
         (invoke-restart 'abort))
        ((list* :restart restart args)
         (apply 'invoke-restart restart args))
        (_ (error "unknown break loop exit status: ~A" exit-status))))))

(defun break-loop-make-error (condition)
  `(:error ,condition
           ,(lambda () (invoke-restart 'exit-break-loop))
           ,(lambda (restart &rest args) (throw *sentinel* (list* :restart restart args)))
           ,(compute-restarts)))

(defun break-loop-make-input-request (condition)
  `(:input-request ,condition
                   ,(lambda (string) (invoke-restart 'give-input string))
                   ,(lambda (restart &rest args) (throw *sentinel* (list* :restart restart args)))
                   ,(compute-restarts)))

(defun break-loop-debugger (condition fn)
  (declare (ignorable fn))
  (let ((*debugger-hook* *outer-debugger-hook*))
    (if (< *break-level* 3)
        ;; just pile another one up on the stack
        ;; would prefer to start another thread.
        (let ((inbox (app-inbox *app*)))
          (if (typep condition 'want-input)
              (send-work inbox `(:system . ,(break-loop-make-input-request condition)))
              (send-work inbox `(:system . ,(break-loop-make-error condition))))
          (do-run-loop inbox *app* *step-function*))
        (funcall *outer-debugger-hook* condition fn))))

(defun start-work-thread (function app clean-up
                          &key initial-bindings
                            (name "anonymous"))
  (let ((box (make-instance 'mbox:mbox)))
    (prog1 box
      (make-thread
       (lambda ()
         (setf (.thread box) (current-thread))
         (format t "[APP] starting work thread. *debugger-hook* = ~A~%" *debugger-hook*)
         (let ((*outer-debugger-hook* *debugger-hook*))
           (block toplevel
             (handler-bind ((stop-work (lambda (c)
                                         (declare (ignore c))
                                         (format t "stopping work.~%")
                                         (setf (.thread box) nil)
                                         (funcall clean-up app)
                                         (return-from toplevel))))
               (do-run-loop box app function))))))
      :name (format nil "system 0.0 work loop (~A)" name)
      :initial-bindings initial-bindings)))


;;FIXME devices should maintain their own inboxes, but we're not there yet

(defparameter *user-package-prefix :an-app.page)

(defun make-user-package (app page-index)
  (let ((p (make-package (format nil "~A.~A"  *user-package-prefix (1+ page-index))
                       :use '(:cl :alexandria :trivia
                              ))))
    (prog1 p (push p (app-package-list app)))))

(defun make-port-handler (portname device)
  (lambda (app message)
    (if (eq (ensure-car message) :device-properties)
        ;; FIXME: this could lead to threading issues...
        ;; should probably use a system event instead of acting
        ;; directly here.
        (ignore-errors
          (appendf (.properties device) (cadr message))
          (%app-update-keyboard-state app))
        (send-work (.inbox app) (cons portname message)))))

(defun app-start-work (app)
  (unless (.inbox app)
    (prog1 t
      (let ((mbox
             (start-work-thread 'app-handle-input app 'app-stop-work
                                :initial-bindings
                                '((*standard-output* . *standard-output*)
                                  (*print-circle* . t)
                                  (*debugger-hook* . *debugger-hook*)
                                  (*custom-debugger-hook* . *custom-debugger-hook*)
                                  (*use-custom-debugger* . *use-custom-debugger*)))))
        (setf (.inbox app) mbox)))))

(defun app-display-refresh (app)
  (format t "refreshing display output~%")
  ;; better than this would be posting a refresh event to the inbox
  (setf (.wants-display-refresh app) t))

(defun app-draw-1 (app)
  (node:with-active-world ((app-world app))
    (if (.wants-display-refresh app)
        (app-force-redraw app)
        (app-draw-world app)))
  (setf (.wants-display-refresh app) nil))

(defun app-sim-1 (app)
  (node:with-active-world ((app-world app))
    (node:world-simulate)))

(defun app-handle-input (app event)
  (let* ((timestamp (car event))
         (input (cdr event))
         (now (get-internal-real-time))
         (elapsed (/ (float (- now timestamp)) internal-time-units-per-second))
         (skippable (> elapsed 0.25)))
    (cond
      ((eq input :frame) (unless skippable (app-draw-1 app)))
      ((eq input :simulate) (unless skippable (app-sim-1 app)))
      (t (let ((from  (car input))
               (message (cdr input)))
           (match from
             ((list :display index)
              (let ((display (svref (app-displays app) index)))
                (unless skippable
                  (handle-display-input app display message))))
             (:keyboard (handle-keyboard-input app message))
             (:system (let ((*debugger-hook* *outer-debugger-hook*))
                        (send-event-to-world app message)))
             (otherwise (format t "[APP] unknown sender: ~A, ~A~%" from message))))))))

(defun app-stop-work (app)
  (format t "[APP] shutting down app.~%")
  (stop! app))

(defmethod new-world ((app app) index)
  (let* ((world (node:make-world))
         (package (make-user-package app index)))
    ;; drawing errors are handled somewhat internally
    ;; by marking the offending node as 'bad' so we don't redraw it,
    (setf (.drawing-error-hook world)
          (lambda (condition node)
            ;; would be nice to make this more obvious on the screen too
            ;; the orange color isn't that obvious
            (format t "Error while drawing: ~A~%   Bad Node was: ~A~%" condition node)
            ;; TODO: should rename to 'skip-frame'
            (invoke-restart 'skip)))
    (when-let (hook (.new-world-hook app))
      (let ((*package* package))
        (funcall hook world)))
    world))

(defmethod initialize-instance :after ((app app) &key)
  (let ((world-stack (app-worlds app)))
    (setf (world-stack-index world-stack) 0
          (world-stack-stack world-stack)
          (list (new-world app 0)))))

(defmethod world-stack-flip-back ((app app))
  (let ((world-stack (app-worlds app)))
    (when (plusp (world-stack-index world-stack))
      (decf (world-stack-index world-stack))
      (app-display-refresh app))))

(defmethod world-stack-flip-forward ((app app))
  (let ((world-stack (app-worlds app)))
    (let* ((idx (world-stack-index world-stack))
           (cons (nthcdr idx (world-stack-stack world-stack))))
      (when (null (cdr cons))
        (setf (cdr cons)
              (list (new-world app (1+ idx)))))
      (incf (world-stack-index world-stack))
      (app-display-refresh app))))

;; it would be good to have option that all incoming events be delivered to a mailbox
;; so you can run your app in a single thread if you like.

;; it would be cool to have a 'timer' device that sends pulses every n milliseconds
;; you would probably want to 'ack' each pulse so that in the case of mailbox, if
;; for example, we get stuck in the debugger for a few minutes figuring something out,
;; we /don't/ want the mbox getting stuffed with timer pulses and then suddenly
;; consuming them all in rapid sequence when we exit the debugger :o
(defun create-demo-app (&key new-world-hook)
  (let* ((app (make-instance 'app :new-world-hook new-world-hook)))
    (when *app* (stop! *app*))
    (setf *app* app)
    (let* ((supported-display-count (length (.displays app)))
           (display-ports
            ;; I guess we could eventually make this dynamic too...
            (loop for id below supported-display-count collect
                 `((:display ,id) :display.instructions.v0))))

          (setf (app-host app) (conn:create-simple-device-host
                                :attach-to app
                                :port-descriptions `(,@display-ports
                                                     (:keyboard :physical-keyboard.v0))
                                :on-port-connect 'app-connect-port
                                :on-port-disconnect 'app-disconnect-port))
      (start! app))))

(defun %app-update-keyboard-state (app)
  ;; share the one keyboard for now
  (when-let* ((kb (app-keyboard app))
              (did (getf (.properties kb) :display-id)))
    (loop for d across (app-displays app) do
         (when (and d (equalp (getf (.properties d) :display-id) did))
           (format t "[APP] Connecting display and keyboard: ~A~%" did)
           (setf (.keyboard d) kb
                 (.display kb) d)))))

(defmethod app-connect-port ((app app) port stream)
  (format t "[APP] port ~S connected.~%" port)
  (match port
    ((list :display index)
     (let ((device (make-user-display-for-stream
                    (disp:make-rendercode-display :output stream)
                    index)))
       (setf (svref (app-displays app) index) device)
       (dstm:for-each stream app (make-port-handler port device))
       (app-display-refresh app)))
    (:keyboard
     (let ((device (make-user-keyboard-for-stream stream 0)))
       (setf (app-keyboard app) device)
       (dstm:for-each stream app (make-port-handler :keyboard device))))))

(defmethod app-disconnect-port ((app app) port stream)
  (declare (ignorable stream))
  (format t "[APP] port ~A disconnected.~%" port)
  (match port
    ((list :display index)
     (let ((display (svref (app-displays app) index)))
       (setf (.state display) :disconnected
             (svref (app-displays app) index) nil)))
    (:keyboard
     (node:modifier-state-reset-for-disconnect (.modifier-state (app-keyboard app)))
     (setf (app-keyboard app) nil))))

(defmethod start! ((app app))
  (format t "[APP] starting app~%")
  (let ((server (app-device-server app))
        (host (app-host app)))
    (format t "[APP] attaching host~%")
    (conn:attach-host host server)
    (format t "[APP] requesting autoconnect~%")
    (conn:request-autoconnect host server)
    (format t "[APP] starting work thread~%")
    (app-start-work app)
    t))

(defmethod stop! ((app app))
  (format t "[APP] stopping app~%")
  (let ((server (app-device-server app))
        (host (app-host app)))
    (format t "[APP] detaching host~%")
    (conn:detach-host host server)
    (setf (app-package-list app) nil) t))

(defun quit-app! ()
  (prog1 t
    (when *app*
      (format t "[APP] shutting down running instance...")
      (map nil 'delete-package (app-package-list *app*))
      (when-let (mbox (.inbox *app*))
        (stop-work mbox))
      (setf *app* nil))))

(defun restart-app! (&key new-world-hook)
  (prog1 t
    (format t "[APP] restarting...~%")
    (quit-app!)
    (format t "[APP] starting new session...~%")
    (create-demo-app :new-world-hook new-world-hook)))

(defun app-active-display-count (app)
  (count-if-not 'null (app-displays app)))

(defun app-note-draw-count (app count)
  (incf (.total-times-drawn (.stats app)) (app-active-display-count app))
  (incf (.total-times-flushed (.stats app)) count))

(defun app-draw-world (app)
  (node:world-start-frame)
  (let ((draw-count
         (app-do-active-displays app (display)
           (node:draw-world))))
    (app-note-draw-count app draw-count))
  (node:world-end-frame))

(defun app-force-redraw (app)
  (app-do-active-displays app (display)
    (node:force-redraw)))

(defmethod send-event-to-world ((app app) raw-event &key device)
  (let ((*current-input-device* device)) ;; we can probably just pass the device to w-d-e below...
    (node:with-active-world ((app-world app))
      (node:world-dispatch-event node:*world* raw-event))))

(defmethod maybe-send-keypress-to-world ((app app) keyname)
  (unless (member keyname '(:shift :control :command :option :function))
    (send-event-to-world app (list :keypress keyname)
                         :device (app-keyboard app))))

(defmethod handle-keyboard-input ((app app) event)
  (let* ((kb (app-keyboard app))
         (mod (.modifier-state kb)))
    (match event
      ((list :keydown key)
       (node:modifier-state-update-for-key mod key t)
       (match (list (.command mod) key)
         ;; this is great for dev, but we don't want to 'flip the world' for everyone...
         ;; should be per-user
         ((list t "[") (world-stack-flip-back app))
         ((list t "]") (world-stack-flip-forward app))
         (_
          (maybe-send-keypress-to-world app (cadr event))
          (send-event-to-world app event :device (app-keyboard app)))))
      ((list :keyup key)
       (node:modifier-state-update-for-key mod key nil)
       (send-event-to-world app event :device (app-keyboard app))))))

(defgeneric handle-display-input (app display raw-event))
(defmethod handle-display-input ((app app) display raw-event)
  (match raw-event
    ((list :display-bounds x y w h)
     (setf (.bounds display) (list x y w h)))
    (_ (send-event-to-world app raw-event :device display))))

(defun whoops! ()
  (setf (.bad (app-world *app*)) nil))

(defun busted ()
  (quit-app!)
  (reset-device-server)
  (restart! (ensure-global-device-server))
  (restart-app!))
