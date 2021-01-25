(in-package :coffee.umbrella.clarity.simple-scene-graph-0)

(deftype region () '(simple-vector 8))

 ;; current level of detail
(defvar *lod* 1.0)

;; 'nil' indicates an unbounded clipping region
;; otherwise a region (array 8) of single-float
(defvar *clipping-region* nil)

;; generally nil, but can have other values depending on
;; whether a full screen refresh is wanted and so on.
(defvar *redraw-policy* nil)

(defgeneric node-enable-per-instance-hooks (node)
  (:method-combination progn))

;; -------------------------------------------------------------
;; a node in the scene graph

(define-model node ()
  ((bad nil)
   ;; TODO: combine pos, anchor, scale, rotation into one vector
   (pos (cons 0.0 0.0))
   (anchor (cons 0.5 0.5))
   (scale (cons 1.0 1.0))
   (rotation 0.0)
   (children (vector:make))
   (parent nil)
   (visible t)
   (wants-redraw nil)
   (wants-layout nil)
   (simple-transform t)
   (index nil) ;; opaque index maintained by the parent container
   (index-context nil) ;; an object the parent may proved to update against
   ;; node transform
   (xform (affine:make-affine)) ;; should not be accessed directly, may be invalid
   ;; inverse node transform
   (ixform (affine:make-affine)) ;; should not be accessed directly, may be invalid
   (xform-dirty t) ;; private, slot value only
   ;; a region 'owned' by this node, to reduce allocations
   (scratch-region (make-array 8 :element-type t :initial-element 0.0))
   (child-drawing-strategy nil)
   (focused nil)
   (discard-clipping-region nil)
   ;; list of tags (metadata)
   (tags nil))
  (:direct-update-hook node-changed-slot-value))

;; no methods by default -- only called when index-context is non-null
(defgeneric node-update-in-index-context (node context))

(defmethod initialize-instance :after ((node node) &key)
  (node-enable-per-instance-hooks node))

(defmethod node-enable-per-instance-hooks progn ((node node)))

;; handler for 'have' and  'have slot'
(defmethod update-node (model (node node) slot-name old-value new-value)
  (declare (ignorable slot-name old-value new-value model))
  (setf (node-wants-redraw node) t))

(declaim (inline node-x))
(defun node-x (node) (car (node-pos node)))
(declaim (inline node-y))
(defun node-y (node) (cdr (node-pos node)))
(defun (setf node-x) (x node)
  (setf (node-pos node) (cons x (node-y node))))
(defun (setf node-y) (y node)
  (setf (node-pos node) (cons (node-x node) y)))

(define-dot-accessor node x node-x)
(define-dot-accessor node y node-y)

(declaim (inline %node-ensure-transforms))
(defun %node-ensure-transforms (n)
  (declare (optimize (speed 3) (safety 1)))
  (when (slot-value n 'xform-dirty)
    (setf (slot-value n 'xform-dirty) nil)
    (let* ((p (.pos n))
           (nx (car p))
           (ny (cdr p))
           (sc (.scale n))
           (sx (car sc))
           (sy (cdr sc))
           (a (.anchor n))
           (r (.rotation n)))
      (declare (type single-float nx ny sx sy r)
               (type (cons single-float single-float) p a))
      (multiple-value-bind (w h) (node-layout-size n)
        (declare (type single-float w h))
        (let ((ax (* (car a) w))
              (ay (* (cdr a) h)))
          (affine:load-standard-node-transform
           nx ny ax ay sx sy r (slot-value n 'xform))
          (affine:load-standard-inverse-node-transform
           nx ny ax ay sx sy r (slot-value n 'ixform)))))))

(defun %node-get-xform (n)
  (prog1 (slot-value n 'xform)
    (%node-ensure-transforms n)))

(defun %node-get-ixform (n)
  (prog1 (slot-value n 'ixform)
    (%node-ensure-transforms n)))

(define-model bounded ()
  ((width 0.0) (height 0.0)))

(define-model bounded-node (bounded node) ())

;; -------------------------------------------------------------
;; common node type
;;
;; has defined bounds, and can participate in layouts


(define-model common-node (bounded-node)
    ;; the layout stuff is still getting worked out
  ((layout-style nil) (layout-force-x nil) (layout-force-y nil)
   (container-style nil) (container-force-x nil) (container-force-y nil)))

;; for layouts to determine if they require node to update in the next layout pass
(defgeneric maybe-invalidate-layout-on-change (common-node layout slot-name old-value new-value))

(defmethod node-enable-per-instance-hooks progn ((node common-node))
  (when-let (layout (.layout-style node))
    (have node maybe-invalidate-layout-on-change layout)))

;; -------------------------------------------------------------
;; generic function declarations

(defgeneric node-child-wants-redraw (node child))
(defgeneric node-transform-into-local-space (node px py))
(defgeneric node-transform-into-parent-space (node px py))
(defgeneric node-transform-from-world-space (node wx wy))
(defgeneric node-add-child (node child))
(defgeneric node-rem-child (node child))
(defgeneric node-reparent (node new-parent))

(defgeneric node-call-with-drawing-context (node thunk))

;; a vague idea when writing these was that a node may present itself
;; differently in different contexts, thus the parent argument to many methods.
(defgeneric node-begin-frame (node parent))
(defgeneric draw-node (node parent))
(defgeneric node-should-redraw (child parent))
(defgeneric node-should-redraw-with-policy (policy child parent))
(defgeneric draw-background (node parent))
(defgeneric draw-child-node (parent child))
(defgeneric draw-children (parent child))
(defgeneric draw-foreground (node parent))
(defgeneric node-end-frame (node parent))

(defgeneric node-active-region (node))
(defgeneric node-contains? (node parent x y))

;; this is used for culling, and as such does not need to be 100% precise.
;; if the nodes _do_ intersect then this method _must_ return nil.
;; otherwise, if they do not intersect, then either t or nil is ok,
;; allowing the node to use a performant rejection method if it so chooses.
(defgeneric node-outside-parent-region (node parent-region))

(defgeneric node-transform-region-from-parent-space (node parent-region output-region))
(defgeneric node-transform-region-to-parent-space (node child-region output-region))

(defgeneric node-layout-size (node))
(defgeneric node-child-wants-layout (node child))

;; -------------------------------------------------------------
;; node update hook


(defmethod simple-node-transform? ((node node))
  (declare (optimize (speed 3) (safety 1)))
  (and (zerop (node-rotation node))
       (let ((s (node-scale node)))
         (= 1.0 (car s) (cdr s)))))

(defun maybe-mark-simple-transform (node)
  (setf (slot-value node 'simple-transform) (simple-node-transform? node)))

(defun node-changed-slot-value (node slot value)
  (declare (optimize (speed 3) (safety 1)))
  (cond ((eq slot 'wants-redraw)
         (when value
           (when-let ((p (node-parent node)))
             (unless (eq p node)
               (node-child-wants-redraw p node)))))
        ((eq slot 'wants-layout)
         (when value
           (when-let ((p (node-parent node)))
             (unless (eq p node)
               (node-child-wants-layout p node)))))
        (t
         (setf (node-wants-redraw node) t)))
  (when (find slot #(pos anchor scale rotation width height))
    (when-let (context (slot-value node 'index-context))
      (node-update-in-index-context node (slot-value node 'index-context)))
    (unless (maybe-mark-simple-transform node)
      (unless (slot-value node 'xform-dirty)
        (setf (slot-value node 'xform-dirty) t)))))

(defun node-is-axis-aligned? (n)
  (slot-value n 'simple-transform))

;; -------------------------------------------------------------
;; node dirty query


(defun node-should-be-drawn (node parent)
  (declare (optimize (speed 3) (safety 1)))
  (when (node-visible node)
    (if-let (policy *redraw-policy*)
      (node-should-redraw-with-policy policy node parent)
      (node-should-redraw node parent))))

(defmethod node-call-with-drawing-context ((n node) thunk)
  (declare (optimize (speed 3) (safety 1)))
  (draw:with-graphics-group ()
    (if (and (zerop (node-rotation n))
             (= 1.0 (car (node-scale n)))
             (= 1.0 (cdr (node-scale n))))
        (let ((p (node-pos n)))
          (draw:moveby (car p) (cdr p))
          (funcall thunk))
        ;; choosing scale-x as lod
        (let ((*lod* (* (car (.scale n)) *lod*)))
          (display:affine-concat *display* (%node-get-xform n))
          (funcall thunk)))))

(defmacro with-node-drawing-context ((node) &body commands)
  (with-gensyms (doit)
    `(labels ((,doit () ,@commands))
      (declare (dynamic-extent #',doit))
       (node-call-with-drawing-context ,node #',doit))))

;; -------------------------------------------------------------
;; basic methods on node

;; adding a child to n makes n layout
(defmethod node-add-child :after ((n node) (ch node))
  (setf (node-wants-layout n) t))

;; reming a child from n makes n layout
(defmethod node-rem-child :after ((n node) (ch node))
  (setf (node-wants-layout n) t))

(defmethod node-child-wants-redraw ((n node) (ch node))
  (unless (eq n ch)
    #+nil
    (format t "child node => parent node ~A ~A~%" ch n)
    (unless (node-wants-redraw n)
      (setf (node-wants-redraw n) t)
      (when-let (p (node-parent n))
        (node-child-wants-redraw p n)))))

;; FIXME: we shouldn't need this method in here, but, I make mistakes sometimes...
(defmethod node-child-wants-redraw ((n null) (ch node)))

(defmethod node-child-wants-layout ((n node) (ch node))
  (& ((:unless (eq n ch))
      #+nil (:do (format t "child node => parent node ~A ~A~%" ch n))
      (:unless (node-wants-layout n))
      (:do (setf (node-wants-layout n) t))
      (p (node-parent n))
      (:when p))
    (node-child-wants-layout p n)))

;; FIXME: we shouldn't need this method in here, but, I make mistakes sometimes...
(defmethod node-child-wants-layout ((n null) (ch node)))

(defun node-four-corners-of-bounds (n x y w h)
  (declare (optimize (speed 3) (safety 1))
           (type node n) (type single-float x y w h))
  (if (node-is-axis-aligned? n)
      (& ((p (node-pos n))
          (:decl (type (cons single-float single-float) p))
          (x1 (- x (car p)))
          (y1 (- y (cdr p)))
          (x2 (+ x1 w))
          (y2 (+ y1 h)))
        (values x1 y1 x2 y1 x2 y2 x1 y2))
      (& ((x2 (+ x w))
          (y2 (+ y h))
          (:v (xa ya) (node-transform-into-local-space n x  y))
          (:v (xb yb) (node-transform-into-local-space n x2 y))
          (:v (xc yc) (node-transform-into-local-space n x2 y2))
          (:v (xd yd) (node-transform-into-local-space n x y2)))
        (values xa ya xb yb xc yc xd yd))))

(declaim (inline node-four-corners-of-bounds-in-parent-space ))
(defun node-four-corners-of-bounds-in-parent-space (n x y w h)
  (declare (optimize (speed 3) (safety 0))
           (type node n) (type single-float x y w h))
  (if (node-is-axis-aligned? n)
      (& ((p (node-pos n))
          (:decl (type (cons single-float single-float) p))
          (x1 (+ x (car p)))
          (y1 (+ y (cdr p)))
          (x2 (+ x1 w))
          (y2 (+ y1 h)))
        (values x1 y1 x2 y1 x2 y2 x1 y2))
      (& ((x2 (+ x w))
          (y2 (+ y h))
          (:v (xa ya) (node-transform-into-parent-space n x  y))
          (:v (xb yb) (node-transform-into-parent-space n x2 y))
          (:v (xc yc) (node-transform-into-parent-space n x2 y2))
          (:v (xd yd) (node-transform-into-parent-space n x  y2)))
        (values xa ya xb yb xc yc xd yd))))

(defgeneric node-four-corners (node &optional outset))

(defmethod node-four-corners ((n node) &optional (outset 0.0))
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-bind (w h) (node-layout-size n)
    (node-four-corners-of-bounds-in-parent-space n
                                                 (- outset)
                                                 (- outset)
                                                 (+ outset outset w)
                                                 (+ outset outset h))))

(defun node-moveto (node x y)
  (setf (node-pos node) (cons x y)))
