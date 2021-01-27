(in-package :coffee.umbrella.clarity.simple-scene-graph-0)

;; -----------------------------------------------------------------------------
;; search

(defgeneric node-find (node x y))
(defgeneric node-find-if (node pred x y &key from-leaves))

(defgeneric node-search-child (node pred x y))

;; TODO binary search for vstack, etc
(defmethod node-search-child ((node node) pred x y)
  (rev-doseq (ch (.children node))
    (when-let (result (funcall pred ch x y))
      (return-from node-search-child result))))

(defmethod node-find-if ((node node) pred x y &key (from-leaves nil))
  (multiple-value-bind (x y) (node-transform-into-local-space node x y)
    (when (node-contains? node (node-parent node) x y)
      (if from-leaves
          (or (node-search-child node (lambda (ch x y) (node-find-if ch pred x y :from-leaves from-leaves))
                               x y)
              (and (funcall pred node x y) node))
          (or (and (funcall pred node x y) node)
              (node-search-child node (lambda (ch x y) (node-find-if ch pred x y :from-leaves from-leaves))
                               x y))))))

;; -----------------------------------------------------------------------------
;; gestures


(define-model gesture () (phase (trigger 0)))
(define-model targeted-gesture (gesture) (x y (target nil)))
(define-model gesture.click (targeted-gesture) ())
(define-model gesture.drag (targeted-gesture) ())
(define-model gesture.mouse-hover (targeted-gesture) ())
(define-model gesture.scroll (targeted-gesture) ()) ;; TODO: separate x y from dx dy in scroll
(define-model gesture.magnify (targeted-gesture) (magnification))

(defgeneric allow-gesture (node parent gesture))
(defgeneric wants-gesture (node parent gesture))
(defgeneric handle-gesture (node parent gesture))

(defmethod allow-gesture ((n node) (parent node) (g gesture))
  t)
(defmethod wants-gesture ((n node) (parent node) (g gesture))
  nil)
(defmethod handle-gesture ((n node) (parent node) (g gesture))
  (format t "unhandled gesture~%  target: ~A parent: ~A gesture: ~A~%" n parent g)
  nil)

(defun attach-gesture (node gesture)
  (have-slot gesture trigger update node)
  (setf (.target gesture) node)
  (incf (.trigger gesture)))

(defun detach-gesture (node gesture)
  #+nil(format t "disconnecting gesture ~A from ~A~%" gesture node))

(defmethod update ((g gesture) (n node) slot old-value new-value)
  (handle-gesture n (node-parent n) g))


(defgeneric dispatch-gesture (world gesture &optional target-x target-y))
(defmethod dispatch-gesture ((w world-node) (g targeted-gesture) &optional target-x target-y)
  ;; (format t "d g : ~A ~%" g)
  (dokids (k w)
    (when-let (target (node-find-if k (lambda (n x y)
                                        (declare (ignore x y))
                                        (wants-gesture n (node-parent n) g))
                                    (or target-x (.x g))
                                    (or target-y (.y g))
                                    :from-leaves t))
      (attach-gesture target g)
      (return-from dispatch-gesture)))
  (when (wants-gesture w w g)
    (attach-gesture w g)))

;; -----------------------------------------------------------------------------
;; events -> gestures

(defgeneric allow-event (node event))
(defgeneric handle-event (node event))

(defmethod allow-event ((node node) event)
  (declare (ignore event))
  t)

(defmethod handle-event ((node node) event)
  (declare (ignore event))
  nil)

(defun dispatch-walk (node event)
  (let ((parent (node-parent node)))
    (if (null parent)
        (error "a parent of focused node detached somewhere ~A" node)
        (if (eq parent node) ;; world node
            (allow-event node event)
            (when (dispatch-walk parent event)
              (allow-event node event))))))

(defun bubble-up (node event)
  (unless (handle-event node event)
    (if (null (node-parent node))
        (error "a parent of focused node detached somewhere ~A" node)
        (unless (eq (node-parent node) node)
          (bubble-up (node-parent node) event)))))

(defun map-raw-event-into-device-view (world raw-event)
  (declare (ignore world))
  (or
   (when-let* ((d *current-input-device*)
               (xf (device-property d :world-view))) ;; eventually this should always be present.
     (let ((tbl (device-info d))
           (event nil))
       (match raw-event
         ((list :mouse motion rawx rawy)
          (multiple-value-bind (x y)
              (node-transform-into-local-space xf rawx rawy)
            (setq event (list :mouse motion x y))))
         ((list* :magnify motion rawx rawy rest)
          (multiple-value-bind (x y)
              (node-transform-into-local-space xf rawx rawy)
            (setq event (list* :magnify motion x y rest))))
         ((list* :tprotate motion rawx rawy rest)
          (multiple-value-bind (x y)
              (node-transform-into-local-space xf rawx rawy)
            (setq event (list* :tprotate motion x y rest))))
         ((list :scroll motion rawx rawy rdx rdy)
          (multiple-value-bind (x y)
              (node-transform-into-local-space xf rawx rawy)
            (multiple-value-bind (dx dy)
                (node-transform-into-local-space xf rdx rdy)
              ;; ok, this is a bit much to have in here, but let's see!
              (multiple-value-bind (tx ty) (cu.affine::affine-translation (.affine xf))
                (let ((dx (- dx tx))
                      (dy (- dy ty)))
                  (setq event (list :scroll motion x y dx dy))))))))
       (match event
         ((list :mouse _ x y)
          (setf (gethash :mouse-x tbl) x
                (gethash :mouse-y tbl) y)))
       event))
   raw-event))


;; TODO: port implementation of gesture extraction
;; (current impl is too pre-alpha, needs cleaning up...)
(defun %world-extract-gestures-from-event-stream (w e)
  (declare (ignore w e)))


;; may consume the raw-event, in which case nil is returned
(defun maybe-extract-gestures-from-raw-event (world raw-event)
  (let ((event (map-raw-event-into-device-view world raw-event)))
    (%world-extract-gestures-from-event-stream world event)
    ;; we dont interpret touch events yet, but we are filtering them out for now
    ;; we may want them at some point,
    ;; but I think I'll rely on gesture reporting for now.
    (unless (member (car event) '(:mouse :magnify :scroll :touch))
      event)))

(defun map-keyboard-event (type key)
  (when-let* ((device *current-input-device*)
              (focus (device-property device :keyboard-focus))
              (mod (.modifier-state device)))
    (let (flags)
      (when (.command  mod) (push :command  flags))
      (when (.control  mod) (push :control  flags))
      (when (.option   mod) (push :option   flags))
      (when (.shift    mod) (push :shift    flags))
      (when (.function mod) (push :function flags))
      (make-instance 'key-event
                     :keyname key
                     :keyboard device
                     :type type
                     :flags flags
                     :target focus))))

(defun current-keyboard ()
  (when-let* ((device *current-input-device*))
    (if (typep device 'device.keyboard) device
        (.keyboard device))))

(defun set-keyboard-focus (node)
  (when-let (kb (current-keyboard))
    (when-let (prev (device-property kb :keyboard-focus))
      (setf (.focused prev) nil))
    (setf (device-property kb :keyboard-focus) node
          (.focused node) t)))

(defmethod world-dispatch-event ((world world-node) raw-event)
  (when-let (event (maybe-extract-gestures-from-raw-event world raw-event))
    (case (car event)
      ((:error)
       (if-let (handler *error-handler*)
         (funcall handler event)
         (progn
           (format t "missing error handler~%")
           (invoke-restart 'abort))))
      ((:input-request)
       (if-let (handler *stream-input-handler*)
         (funcall handler event)
         (progn
           (format t "missing input handler~%")
           (invoke-restart 'abort))))
      ((:keydown :keypress :keyup)
       (when-let ((evt (map-keyboard-event (car event) (cadr event))))
         (let* ((node (.target evt))
                (allowed (dispatch-walk node evt)))
           (when allowed
             (bubble-up node evt)))))
      (otherwise
       (format t "unrecognized event: ~A~%" event)))))

(defun ensure-world-view (world)
  (let ((*world* world))
    (when *current-input-device*
      (ensure-gethash :world-view (device-info *current-input-device*)
                      (make-instance 'world-view)))))

(defmethod wants-gesture ((w world-node) (ww world-node) (g gesture.scroll))
  t)

(defun %world-view-translate (node x y)
  (affine:translate x y (.iaffine node))
  (affine:l-translate (- x) (- y) (.affine node)))

(defun world-view-translate (node x y)
  (%world-view-translate node x y)
  (setf (node-wants-redraw node) t)))

;; radians, counter clockwise
(defun world-view-rotate (node x y angle)
  (let ((a (.affine node))
        (ia (.iaffine node)))
    (%world-view-translate node x y)
    (affine:l-rotate (- angle) a)
    (affine:rotate angle ia)
    (%world-view-translate node (- x) (- y))
    (setf (node-wants-redraw node) t)))

(defmethod handle-gesture ((w world-node) (ww world-node) (g gesture.scroll))
  (when-let (node (ensure-world-view w))
    (let ((x (.x g)) (y (.y g)))
      (world-view-translate node x y))))

(defmethod wants-gesture ((n world-node) (ww world-node) (g gesture.magnify))
  t)

(defmethod handle-gesture ((w world-node) (ww world-node) (g gesture.magnify))
  (when-let (node (ensure-world-view w))
    (let* ((sm (* 0.1 (.magnification g)))
           (m (+ 1.0 sm))
           (im (/ 1.0 m))
           (x (.x g)) (y (.y g))
           (a (.affine node))
           (ia (.iaffine node))
           (sx (* m x))
           (sy (* m y))
           (dx (* -1 (- sx x) im))
           (dy (* -1 (- sy y) im)))

      (affine:l-scale im im a)
      (affine:scale m m ia)

      (%world-view-translate node dx dy)

      (setf (node-wants-redraw node) t))))
