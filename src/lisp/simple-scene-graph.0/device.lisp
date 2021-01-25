(in-package :coffee.umbrella.clarity.simple-scene-graph-0)

;; should be bound iff a drawing flush is in progress
(defvar *current-display-device* nil)
 ;; should be bound iff an event is being processed -- currently only done for display input
(defvar *current-input-device* nil)

(define-model device () (id stream (state nil) (properties nil)))

(define-model device.display (device) (windows bounds (keyboard nil)))
(define-model device.keyboard (device)
  ((modifier-state (make-instance 'modifier-state))
   (focused-node nil)
   (display nil)))

(define-model device.output-window () (id layers bounds display))
(define-model device.output-layer () (id bounds window))

(define-model key-event ()
  (keyname keyboard type flags target))

(define-model modifier-state ()
  ((shift nil)
   (control nil)
   (option nil)
   (command nil)
   (function nil)))

(defmethod modifier-state-reset-for-disconnect ((m modifier-state))
  (setf
   (modifier-state-shift m) nil
   (modifier-state-control m) nil
   (modifier-state-option m) nil
   (modifier-state-command m) nil
   (modifier-state-function m) nil))

(defmethod modifier-state-update-for-key ((m modifier-state) key pressed)
  (case key
   (:shift    (setf (modifier-state-shift m)    pressed ))
   (:control  (setf (modifier-state-control m)  pressed ))
   (:option   (setf (modifier-state-option m)   pressed ))
   (:command  (setf (modifier-state-command m)  pressed ))
   (:function (setf (modifier-state-function m) pressed ))))
