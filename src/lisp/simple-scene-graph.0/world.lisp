(in-package :coffee.umbrella.clarity.simple-scene-graph-0)

;; the current world
(defvar *world* nil)

;; hook to invoke the GUI debugger
(defvar *error-handler* nil) ;; may want a less generic name

;; hook to invoke GUI input on stream read
(defvar *stream-input-handler* nil) ;; not clear what file should have this yet

;; -------------------------------------------------------------
;; a root in the scene graph

(define-model world-node (node)
  ((background-color '(0.6 1.0 0.6 1.0))
   (drawing-error-hook nil)
   (simulations nil)
   (devices (make-weak-hash-table :weakness :key :test 'eq))
   (dirty-regions nil)
   (display-trackers (make-weak-hash-table :weakness :key :test 'eq))
   (child-map (make-hash-table :test 'equalp))))

(defmethod initialize-instance :after ((w world-node) &key)
  ;; by default, worlds contain themselves
  (setf (node-parent w) w))

(define-model world-view (node)
  ((affine (affine:make-affine))
   (iaffine (affine:make-affine))
   (lod 1.0)
   (previous-child-marks (make-weak-hash-table :test 'eq :weakness :key))))

;; ---------------------------------------------------------------
;; spatial hash for tracking world children

(declaim (ftype (function () single-float) world-index-bucket-size ))
(defun world-index-bucket-size () 1000.0)

;; TODO: we should also keep the child's position in the children array in the index,
;;       so that we have a z-ordering on query results.
;; TODO: this doesn't handle the case where a node is larger than a hash bucket
(defun world-make-index-for-child (child)
  (let (xa ya xb yb xc yc xd yd (s (world-index-bucket-size)))
    (setf (values xa ya xb yb xc yc xd yd)
          (node-four-corners child))
    (list (cons (floor xa s) (floor ya s))
          (cons (floor xb s) (floor yb s))
          (cons (floor xc s) (floor yc s))
          (cons (floor xd s) (floor yd s)))))

(declaim (inline world-index-key))
(defun world-index-key (point)
  (declare (optimize (speed 3) (safety 1))
           (type (cons fixnum fixnum) point))
  ;; this is the bucket count at which the spatial hash starts wrapping,
  ;; leading to hash collisions. world is still technically unbounded
  ;; (unless you count float max)
  (flet ((wrap (n s) (mod n s)))
    (let* ((world-size 4096) ;; ought to be enough for anybody
           (x (wrap (car point) world-size))
           (y (wrap (cdr point) world-size)))
      (+ (* 4096 x) y))))

(defun world-insert-child-at-index (map index child)
  ;; note that this won't cut it if child is very large
  (loop for point in index do
       (pushnew child (gethash (world-index-key point) map))))

(defun world-remove-child-at-index (map index child)
  (loop for point in index do
       (deletef (gethash (world-index-key point) map) child)))

(defun world-update-child-in-map (map child)
  (declare (optimize (speed 3) (safety 1)))
  (let ((index (.index child)))
    (world-remove-child-at-index map index child)
    (& ((s (world-index-bucket-size))
        (:decl (type single-float s))
        (:v (xa ya xb yb xc yc xd yd) (node-four-corners child))
        (:decl (type single-float xa ya xb yb xc yc xd yd)))
      (let ((ii index))
        (setf
         (car (first ii)) (floor xa s)
         (cdr (first ii)) (floor ya s))
        (pop ii)
        (setf
         (car (first ii)) (floor xb s)
         (cdr (first ii)) (floor yb s))
        (pop ii)
        (setf
         (car (first ii)) (floor xc s)
         (cdr (first ii)) (floor yc s))
        (pop ii)
        (setf
         (car (first ii)) (floor xd s)
         (cdr (first ii)) (floor yd s)))
      (world-insert-child-at-index map index child))))


(defun bounds-in-view-space (world-view x y w h)
  (declare (optimize (speed 3) (safety 1)))
  (& ((xf (.affine world-view))
      (:v (ax ay) (affine:transform-xy xf x       y))
      (:v (bx by) (affine:transform-xy xf (+ x w) y))
      (:v (cx cy) (affine:transform-xy xf (+ x w) (+ y h)))
      (:v (dx dy) (affine:transform-xy xf x       (+ y h))))
    (values ax ay bx by cx cy dx dy)))

;; xform region from world space -> view space
(defun region-in-view-space (world-view ax ay bx by cx cy dx dy)
  (declare (optimize (speed 3) (safety 1)))
  (& ((xf (.iaffine world-view))
      (:v (ax ay) (affine:transform-xy xf ax ay))
      (:v (bx by) (affine:transform-xy xf bx by))
      (:v (cx cy) (affine:transform-xy xf cx cy))
      (:v (dx dy) (affine:transform-xy xf dx dy)))
    (values ax ay bx by cx cy dx dy)))

(defun node-four-corners-in-view-space (node world-view &optional (outset 0.0))
  ;; TODO: should be 'four corners in world space'  not node-four-corners
  (& ((:v (a b c d e f g h) (node-four-corners node outset))
      (xf (.iaffine world-view))
      (:v (ax ay) (affine:transform-xy xf a b))
      (:v (bx by) (affine:transform-xy xf c d))
      (:v (cx cy) (affine:transform-xy xf e f))
      (:v (dx dy) (affine:transform-xy xf g h)))
    (values ax ay bx by cx cy dx dy)))

(defmethod node-call-with-drawing-context ((node world-view) thunk)
  (let ((*lod* (* (.lod node) *lod*)))
    (draw:with-graphics-group ()
      (display:affine-concat *display* (.iaffine node))
      (funcall thunk))))

(defmethod node-transform-into-local-space ((node world-view) px py)
  (affine:transform-xy (.affine node) px py))

(defmethod world-add-device ((world-node world-node) device)
  (ensure-gethash device (.devices world-node) (make-hash-table)))

(defun device-info (device)
  (world-add-device *world* device))

(defun device-property (device prop)
  (or (getf (.properties device) prop)
      (gethash prop (world-add-device *world* device))))

(defun (setf device-property) (value device prop)
  (setf (gethash prop (world-add-device *world* device)) value))

(defmethod world-rem-device ((world-node world-node) device)
  (remhash device (.devices world-node)))

(defun reset-all-world-views! ()
  (maphash-values (lambda (tbl) (remhash :world-view tbl))
                  (.devices *world*)))

(defun call-with-active-world (world function)
  (let ((*world* world))
    (funcall function)))

(defmacro with-active-world ((world) &body body)
  `(call-with-active-world ,world (lambda () ,@body)))

(defmethod node-transform-into-local-space ((w world-node) px py)
  (if (eq (node-parent w) w)
      (values px py)
      (call-next-method)))

(defmethod get-active-viewports ((w world-node))
  (let (vs)
    (maphash (lambda (k v)
                    (when (typep k 'device.display)
                      (push (list (.bounds k)
                                  (ensure-gethash :world-view v (make-instance 'world-view)))
                            vs)))
                  (.devices w))
    vs))

(defmacro map-active-viewports (((view bounds) world) &body body)
  (with-gensyms (k v)
    `(maphash (lambda (,k ,v)
                (when (typep ,k 'device.display)
                  (let ((,view (ensure-gethash :world-view ,v (make-instance 'world-view)))
                        (,bounds (.bounds ,k)))
                    ,@body)))
              (.devices ,world))))

(defun current-display ()
  (or *current-display-device*
      (and (typep *current-input-device* 'device.display)
           *current-input-device*)
      (.display *current-input-device*)))

(defun current-display-info ()
  (device-info (current-display)))

(defun current-world-view ()
  (ensure-gethash :world-view (current-display-info) (make-instance 'world-view)))

(defun current-display-width-height ()
  (let ((size (cddr (.bounds (current-display)))))
    (values (car size) (cadr size))))

(defun rotate-point (angle offs-x offs-y)
  (let ((c (cos angle))
        (s (sin angle)))
    (values
     (- (* offs-x c) (* offs-y s))
     (+ (* offs-x s) (* offs-y c)))))

;; parameters range in 0..1 relative to the current display width/height
;; return value is an angle of rotation, and x y w h in world space
;; ax ay are anchor point
;; apply results to a node
(defun get-display-relative-rotation-and-bounds (pctx pcty pctw pcth &optional (ax 0.5) (ay 0.5))
  (let ((v (current-world-view)))
    (multiple-value-bind (w h) (current-display-width-height)
      (multiple-value-bind (x y r sx sy)
          (affine:get-translation-rotation-and-scale (.iaffine v))
        (declare (ignore sy))
        (let* ((s  sx)
               (is (/ 1.0 s))
               (isw (* w is))
               (ish (* h is))
               (scx (* -1 x is))
               (scy (* -1 y is))
               (iw (* pctw isw))
               (ih (* pcth ish))
               (x0 (* isw pctx))
               (y0 (* ish pcty))
               ;; have to account for the anchor point
               ;; we rotate around it, so we target the anchor point here...
               (x1 (* isw (+ pctx (* ax pctw))))
               (y1 (* ish (+ pcty (* ay pcth))))
               (xdiff (- x1 x0))
               (ydiff (- y1 y0))
               (ang (atan ydiff xdiff))
               (a xdiff)
               (b ydiff)
               (dist (sqrt (+ (* a a) (* b b)))))
          (multiple-value-bind (dx dy)
              (let ((a (+ ang r)))
                (rotate-point a dist 0.0))
            (values (+ r)
                    (+ scx (- x1 dx))
                    (+ scy (- y1 dy))
                    iw ih)))))))

(defun snap-to-display-right (root-child)
  (multiple-value-bind (r x y w h) (get-display-relative-rotation-and-bounds 0.5 0.0 0.5 1.0)
    (format t "setting to: ~A ~A ~A ~A ~A ~%" r x y w h)
    (setf
     (.width root-child) w
     (.height root-child) h
     (node-pos root-child) (cons x y)
     (.rotation root-child) r)))

;; not accounting for very large/ very small viewports yet.
;; TODO: this should be a region-based query, not an 'every viewport' query
(defmethod map-active-children ((w world-node) (fn function))
  (declare (optimize (speed 3) (safety 0) (compilation-speed 0)))
  (let ((seen  (make-array 64 :initial-element nil :element-type t :fill-pointer 0))
        (found (make-array 16 :initial-element nil :element-type t :fill-pointer 0))
        (sample (cons 0.0 0.0)))
    (declare (dynamic-extent sample seen found))
    (& ((map (.child-map w))
        (:in map-active-viewports ((xform full-bounds) w))
        (:when full-bounds)
        (size (world-index-bucket-size))
        (:v (xa ya xb yb xc yc xd yd)
            (destructuring-bind (x y w h) full-bounds
              (node-four-corners-of-bounds xform x y w h)))
        (:decl (type single-float xa ya xb yb xc yc xd yd))
        (minx (min xa xb xc xd))
        (maxx (max xa xb xc xd))
        (miny (min ya yb yc yd))
        (maxy (max ya yb yc yd))
        (:do (setf (fill-pointer found) 0))
        (& (;; this is probably over-sampling, but that is ok for now.
            (:loop for y from (1- (floor miny size)) to (1+ (floor maxy size)))
            (:loop for x from (1- (floor minx size)) to (1+ (floor maxx size))))
          (setf (car sample) x (cdr sample) y)
          (let ((here (gethash (world-index-key sample) map)))
            (unless (find here found :test 'eq)
              (vector-push here found))))
        (:doseq children found)
        (:doseq ch children)
        (:unless (find ch seen :test 'eq))
        (:do
         (vector-push ch seen)
         (funcall fn ch))))))

(defmacro dokids ((var world) &body body)
  `(labels ((dokids-aux (,var) ,@body))
     (declare (dynamic-extent (function dokids-aux)))
     (map-active-children ,world #'dokids-aux)))

(defmethod node-begin-frame :before ((w world-node) (w2 world-node))
  (& ((:when (eq w w2))
      (:when (and (slot-boundp w 'children)
                  (slot-value w 'children)))
      (:in dokids (ch w))
      (:when (node-wants-layout ch))
      ;; root children must maintain their own layout size in this layout mode.
      ;; (perhaps the 'natural' layout query is more appropriate here)
      (:v (w h) (node-layout-size ch))
      (:do
       (node-prepare-for-layout ch)
       (node-layout-children ch w h)
       (node-finalize-layout ch)))))

;; TODO: break this up
;; apologies to the reader for the excessive macrology on this one
(defmethod draw-world-view ((v world-view) (w world-node))
  (declare (optimize (speed 3) (safety 1)))
  (& ((&fn bounds->region (bounds)
        (multiple-value-list (apply 'node-four-corners-of-bounds w bounds)))
      (display *current-display-device*)
      (display-bounds (.bounds display))
      (display-region (bounds->region display-bounds))
      (*redraw-policy* (if (slot-value v 'wants-redraw) :refresh *redraw-policy*))
      (scratch (.scratch-region v))
      (:v (a b c d e f g h) (apply 'bounds-in-view-space v display-bounds))
      (:do (region-load a b c d e f g h scratch))
      (*clipping-region* scratch)
      (&fn region-equal (a b) (every '= a b))
      (&fn region-intersects-aux? (a b)
        ((list ax ay bx by cx cy dx dy) b)
        (:loop for (px py) on a by #'cddr)
        ;; this doesn't handle the 'cross' case
        (:when (affine:four-points-contain-point? ax ay bx by cx cy dx dy px py))
        t)
      (&fn region-intersects? (a b)
        (or (region-intersects-aux? a b)
            (region-intersects-aux? b a)))
      (&fn redraw-region (node)
        (multiple-value-list (node-four-corners-in-view-space node v 2.0)))
      (&fn clear-region (r)
        (:in draw:with-clip-shape ())
        (progn
         (apply 'draw:poly r)
         (draw:clear-bounds)))
      (&fn transform-world-region (r)
        (result (multiple-value-list (apply 'region-in-view-space v r)))
        result)
      (&fn debug-draw-meter ()
        (:v (_ _ r sx _) (affine:get-translation-rotation-and-scale (.affine v)))
        (:in draw:with-graphics-group ())
        (progn
          (draw:moveby (* 0.5 (third display-bounds)) (* 0.5 (fourth display-bounds)))
          (draw:rotateby r)
          (draw:rgba-fill 1.0 1.0 0.2 1.0)
          (draw:box 0.0 0.0 (* (/ 1.0 sx) 50.0) 4.0)))
      (&fn debug-draw-level ()
        (:v (x y r sx sy) (affine:get-translation-rotation-and-scale (.iaffine v)))
        (:in draw:with-graphics-group ())
        (progn
          (draw:rotateby r)
          (draw:moveby (* -1.0 (/ 1.0 sx ) x) (* -1.0 (/ 1.0 sy ) y))
          (draw:moveby (* 0.5 (/ 1.0 sx) (third display-bounds))
                  (* 0.5 (/ 1.0 sy) (fourth display-bounds)))
          (draw:rgba-fill 0.0 0.3 1.0 1.0)
          (draw:box 0.0 0.0 (* (/ 1.0 sx) 50.0) 4.0)))
      (&fn debug-draw-regions ()
        (:in draw:with-graphics-group ())
        (:do
         (draw:moveby 10.0 10.0)
         (draw:scaleby 0.2 0.2)
         (draw:rgba-fill 1.0 0.0 0.0 0.2)
         (apply 'draw:poly display-region)
         (draw:scaleby 0.5 0.5)
         (draw:moveby (* 0.5 (third display-bounds)) (* 0.5 (fourth display-bounds)))
         (draw:rgba-fill 0.0 0.0 1.0 0.2)
         (apply 'draw:poly display-region)
         (draw:rgba-fill 1.0 1.0 1.0 0.4))
        (:in dokids (k w))
        (apply 'draw:poly (redraw-region k)))
      (draw-levels nil)
      (redraw nil)
      (all-redraws nil)
      (map (.previous-child-marks v))
      (&fn determine-redraws ()
         (clean nil)
         (untested nil)
         (& ((:in dokids (k w)) ;:doseq k ks)
             (&& (node-wants-redraw k)
               (t
                (:do (push k redraw))
                (found (gethash k map))
                (:when found)
                (bounds (redraw-region k))
                (:do (push bounds untested))
                (:unless (region-equal bounds found))
                (:do (push found untested)))
               (_
                (:do (push k clean))))))
        (:do (appendf untested (map 'list #'transform-world-region (.dirty-regions w))))
         (& ((:loop while untested)
             (:loop with new-untested = nil
                    for r in untested
                    finally (progn
                              (appendf all-redraws untested)
                              (setf untested new-untested)))
             (:loop with new-clean = nil
                    for k in clean
                    finally (setf clean new-clean))
             (bounds (redraw-region k))
             (&& (region-intersects? bounds r)
               (t
                (:do (push k redraw))
                (:do (push bounds new-untested)))
               (_
                (:do (push k new-clean))))))
        t)
      (&& (eq *redraw-policy* :refresh)
        (t
         (:do (setf (slot-value v 'wants-redraw) nil))
         (map (.previous-child-marks v))
         (dc 0)
         (:in draw:with-graphics-group ())
         (:in draw:with-font (*font*))
         (:do (clear-region display-region)
              (with-node-drawing-context (v)
                (dokids (k w)
                  (draw-child-node w k)
                  (incf dc)
                  (setf (gethash k map) (redraw-region k)))
                (when draw-levels (debug-draw-level)))
              (when draw-levels (debug-draw-meter))
              (draw:rgba-fill 0.2 0.2 0.0 1.0)
              (draw:textat 0.0 0.0 (format nil "drew: ~A" dc))
              (debug-draw-regions)))
        (_
         (:do (determine-redraws))
         (& ((&fn draw-contents ()
               (:in draw:with-font (*font*))
               (:in with-node-drawing-context (v))
               (:doseq k redraw)
               (:do (draw-child-node w k))
               (:do (setf (gethash k map) (redraw-region k)))
               nil)
             ;; TODO: if the areas of all-redraws exceeds a percentage of the viewport,
             ;;       just do a full redraw.
             (&fn clear-regions ()
               (:do (map nil #'clear-region all-redraws))
               nil)
             (:do
              (cond ((and all-redraws redraw)
                     (draw:with-graphics-group ()
                       (clear-regions)
                       (draw-contents)))
                    (all-redraws
                     (draw:with-graphics-group ()
                       (clear-regions)))
                    (redraw
                     (draw:with-graphics-group ()
                       (draw-contents)))))
             (:do (when draw-levels (debug-draw-level)))))
         (:do (when draw-levels (debug-draw-meter))))))))

(defun world-has-drawable-kid (w)
  (dokids (k w)
    (when (node-wants-redraw k)
      (return-from world-has-drawable-kid t)))
  nil)

(defmethod draw-node ((w world-node) (ww world-node))
  (if (not (eq w ww))
      (call-next-method)
      (when (or (node-wants-redraw w)
                (world-has-drawable-kid w))
        (draw-children w ww))))

(defmethod draw-node :after ((w world-node) (ww world-node))
  (when-let ((transformer (device-property *current-display-device* :world-view)))
    (when (slot-value transformer 'wants-redraw)
      (setf (slot-value transformer 'wants-redraw) nil))))

(defmethod draw-children ((w world-node) (ww world-node))
  (if (eq w ww)
      (draw-world-view (current-world-view) w)
      (call-next-method)))

(defmethod clear-dirty-regions ((world world-node))
  (setf (.dirty-regions world) nil))

(defmethod node-end-frame :after ((world world-node) (w2 world-node))
  (when (eq world w2)
    (clear-dirty-regions world)))

(defmethod node-should-redraw ((child node) (w world-node))
  (or (eq *redraw-policy* :refresh)
      (slot-value child 'wants-redraw)))

(defmethod node-should-redraw ((w world-node) (pw world-node))
  ;; when we are the root world, redraw
  (eq w pw))

(defmethod node-contains? ((w world-node) (ww world-node) x y)
  (declare (ignore x y))
  t)

;; TODO: dokids is a temp thing here, we want a general spatial query
(defun node-at-x-y (x y &optional target-node)
  (labels ((check (n p x y)
             (multiple-value-bind (x y) (node-transform-into-local-space n x y)
               (when (node-contains? n p x y)
                 (if (eq n target-node) n
                     (or (find-if (lambda (ch) (check ch n x y)) (node-children n) :from-end t)
                         n))))))
    (dokids (k *world*)
      (check k *world* x y))))

(defmethod node-contains-descendant?  ((n node) (d node))
  (or (find d (node-children n))
      (some (lambda (c) (node-contains-descendant? c d)) (node-children n))))

;; -----------------------------------------------------------------------------


(defmethod draw-background ((w world-node) (ww world-node))
  ;; (rgba-fill (random 1.0) (random 1.0) (random 1.0) 1.0)
  ;; (box (random 600.0) (random 600.0) 50.0 50.0)
  (if (eq w ww)
      (if-let ((transformer (device-property *current-display-device* :world-view)))
        (let* ((dirty-xform (slot-value transformer 'wants-redraw))
               ;; we need to wipe the screen if our transform has changed
               (*redraw-policy* (if dirty-xform :refresh *redraw-policy*)))
          ;; clear device bounds on refresh
          (when (eq *redraw-policy* :refresh)
            (draw:with-clip-shape ()
              (apply 'draw:box (.bounds *current-display-device*))
              (draw:cls)))))))

(defun draw-my-mouse-location ()
  (draw:rgba-fill 0.0 0.5 1.0 1.0)
  (when-let ((x (device-property *current-display-device* :mouse-x))
             (y (device-property *current-display-device* :mouse-y)))
    (draw:lozenge (- x 2.5) (- y 2.5) 15.0 15.0)))

(defun draw-mouse-locations (world)
  (draw:rgba-fill 1.0 0.5 0.0 1.0)
  (maphash (lambda (k info)
             (when-let ((x (gethash :mouse-x info))
                        (y (gethash :mouse-y info)))
               (when (not (eq *current-display-device* k))
                 (draw:lozenge (- x 2.5) (- y 2.5) 15.0 15.0))))
           (world-node-devices world))
  (draw-my-mouse-location))

;; uncommment to view mouse positions
;; (helpful for working out tranforms etc)
#+nil
(defmethod draw-foreground ((w world-node) (ww world-node))
  (rgba-fill 1.0 0.5 0.0 1.0)
  (if-let ((transformer (device-property *current-display-device* :world-view)))
    (with-node-drawing-context (transformer)
      (draw-mouse-locations w))
    (draw-mouse-locations w)))


(defun make-world (&key (background-color '(0.6 1.0 0.6 1.0)))
  ;; TODO:
  ;; create 2 custom classes in the custom user package
  ;;   1 named 'world that should be homed in the package
  ;;   1 (name doesn't matter) that inherits from 'world and 'world-node
  ;;   then we make an instance of the latter.
  ;;   this allows us to inherit world-node functionality,
  ;;   while allowing user to redefine the world class as they see fit.
  (make-instance 'world-node :background-color background-color))

(defun world-start-frame ()
  (node-begin-frame *world* (node-parent *world*)))
(defun world-end-frame ()
  (node-end-frame *world* (node-parent *world*)))

;; should be 'draw on display'
(defun draw-world ()
  ;; TODO: move the error handling out of the per-node call
  ;;       and into this call so less work is done on a per-draw-per-node basis
  (when-let (b (.bounds (current-display)))
    (draw-world-view (current-world-view) *world*)))

(defun force-redraw ()
  (let ((*redraw-policy* :refresh))
    (draw-world)
    (display:flush *display*)))

(defmethod add-to-world ((node node) (w world-node))
  (node-add-child w node))

;; this will change when we nest worlds
(defmethod node-remove-from-world ((w world-node) (w2 world-node))
  (cerror "cowardly refusing to remove the world ~A from the world ~A" w w2))

(defmethod node-remove-from-world ((node node) (world world-node))
  (prog1 t
    ;; TODO: proper per-keyboard/remote focus handling
    #+nil
    (when (node-contains-focus-in-world? node world)
      (world-lose-focus-for-node world node))
    (when (node-parent node)
      (node-rem-child (node-parent node) node))))

(defun remove-from-world (node)
  (node-remove-from-world node *world*))

(defun ensure-in-world (node)
  (prog1 t
    (unless (node-parent node)
      (add-to-world node *world*))))

(defun region-from-bounds (x y w h)
  (values x y (+ x w) y (+ x w) (+ y h) x (+ y h)))

(defmethod node-add-child :after ((w world-node) (ch node))
  ;; have children do a layout when they attach to the world
  (setf (node-wants-layout ch) t)
  (setf (.index ch) (world-make-index-for-child ch))
  (setf (.index-context ch) w)
  (world-insert-child-at-index (.child-map w) (.index ch) ch))

(defmethod node-rem-child :after ((world world-node) (child node))
  (world-remove-child-at-index (.child-map world) (.index child) child)
  (setf (.index child) nil)
  (setf (.index-context child) nil)
  (multiple-value-bind (w h) (node-layout-size child)
    (let* ((bounds (list (.x child) (.y child) w h))
           (region (multiple-value-list (apply 'region-from-bounds bounds))))
      (push region (.dirty-regions world)))))

(defmethod node-update-in-index-context ((ch node) (ctx world-node))
  (world-update-child-in-map (.child-map ctx) ch))

;; ---------------------------------------------------------------
;; 'simulation' is step similar to drawing and event handling
;; currently used for animation
;; will probably become associated with individual nodes, rather than
;; just a list on *world*

(defun world-simulate ()
  (let ((bad)
        (good (.simulations *world*)))
    (doseq (fn good)
      (multiple-value-bind (_ error) (ignore-errors (funcall fn))
        (declare (ignore _))
        (when error (push fn bad))))
    (setf (.simulations *world*) (set-difference good bad))
    (values)))

(defun world-active-simulation-count (world)
  (length (.simulations world)))

(define-condition exit-simulation (error) ())
;; It'd be nice if we could have a watchdog that checks if we're
;; stuck in the simulation phase for too long and interrupts us
(defun exit-simulation () (error 'exit-simulation))
(defun enter-simulation (thunk) (pushnew thunk (.simulations *world*)))
(defun stop-all-simulations () (setf (.simulations *world*) nil))
