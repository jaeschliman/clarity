(in-package :coffee.umbrella.clarity.simple-scene-graph-0)

;; -------------------------------------------------------------
;; transformations/intersections

;; incomplete -- there are cases where a point of the region is contained in the
;; other or vice versa, but there is also the case of the 'cross shape', where
;; no points are contained in either, but there is an intersection
(defun region-intersects? (a b)
  (& ((&fn region-intersects-aux? (a b)
        ((array :rank 1 :contents (ax ay bx by cx cy dx dy)) b)
        (:loop for i below 8 by 2)
        (px (aref a i))
        (py (aref a (1+ i)))
        (:when (affine:four-points-contain-point? ax ay bx by cx cy dx dy px py))
        ;; return needed for the enclosing loop -- yuck
        (return t)))
    (cond ((null a) t)
          ((null b) t)
          ((region-intersects-aux? a b) t)
          ((region-intersects-aux? b a) t)
          (t nil))))

;; AABB rejection
(defun region-does-not-intersect-approx (a b)
  (declare (type (simple-vector 8) a b)
           (optimize (speed 3) (safety 1)))
  (let ((aminx (aref a 0)) (aminy (aref a 1)) (amaxx (aref a 0)) (amaxy (aref a 1))
        (bminx (aref b 0)) (bminy (aref b 1)) (bmaxx (aref b 0)) (bmaxy (aref b 1)))
    (declare (type single-float
                   aminx aminy amaxx amaxy
                   bminx bminy bmaxx bmaxy))
    (& ((:loop for i below 8 by 2)
        (:v (ax ay) (aref2 a i (1+ i)))
        (:decl (type single-float ax ay))
        (:v (bx by) (aref2 b i (1+ i)))
        (:decl (type single-float bx by))
        (:do (minf aminx (the single-float ax)) (minf aminy (the single-float ay))
             (maxf amaxx (the single-float ax)) (maxf amaxy (the single-float ay)))
        (:do (minf bminx (the single-float bx)) (minf bminy (the single-float by))
             (maxf bmaxx (the single-float bx)) (maxf bmaxy (the single-float by)))))
    (or (< amaxx bminx)
        (< amaxy bminy)
        (> aminx bmaxx)
        (> aminy bmaxy))))

(defun region-edges (a)
  (declare (type (simple-vector 8) a)
           (optimize (speed 3) (safety 1)))
  (loop for i below 8 by 2
        for x = (aref a i)
        for y = (aref a (1+ i))
        minimize x into minx minimize y into miny
        maximize x into maxx maximize y into maxy
        finally (return (values minx miny maxx maxy))))

;; poorly named -- should 'from parent' not 'into local'
(defmethod node-transform-into-local-space ((n node) px py)
  (declare (optimize (speed 3) (safety 1)))
  (if (node-is-axis-aligned? n)
      (let ((p (node-pos n)))
        (values (- px (car p)) (- py (cdr p))))
      (affine:transform-xy (%node-get-ixform n) px py)))

(defmethod node-transform-into-parent-space ((n node) nx ny)
  (declare (optimize (speed 3) (safety 1)))
  (if (node-is-axis-aligned? n)
      (let ((p (node-pos n)))
        (values (+ nx (car p)) (+ ny (cdr p))))
      (affine:transform-xy (%node-get-xform n) nx ny)))

(defmethod node-transform-from-world-space ((n node) wx wy)
  (if (eq (.parent n) n) (values wx wy)
      (multiple-value-bind (px py) (node-transform-from-world-space (.parent n) wx wy)
        (node-transform-into-local-space n px py))))

(defmethod node-layout-size ((n node))
  (values 0.0 0.0))

(defmethod node-outside-parent-region ((node node) parent-region)
  (declare (optimize (speed 3) (safety 1)))
  (if (node-is-axis-aligned? node)
      (& ((:when parent-region)
          (p (node-pos node))
          (:decl (type (cons single-float single-float) p))
          (x1 (car p))
          (y1 (cdr p))
          (:v (w h) (node-layout-size node))
          (:decl (type single-float w h))
          (x2 (+ x1 w))
          (y2 (+ y1 h))
          (:v (a b c d) (region-edges parent-region))
          (:decl (type single-float a b c d)))
        (or (< x2 a) (< y2 b)
            (> x1 c) (> y1 d)))
      (& ((:when parent-region)
          (scratch (.scratch-region node))
          (:v (w h) (node-layout-size node))
          (:v (ax ay bx by cx cy dx dy) (node-four-corners-of-bounds-in-parent-space node 0.0 0.0 w h))
          (:do (region-load ax ay bx by cx cy dx dy scratch)))
        (region-does-not-intersect-approx scratch parent-region))))

(defmethod node-transform-region-from-parent-space ((node node) parent-region output-region)
  (declare (optimize (speed 3) (safety 1))
           (type (simple-vector 8) parent-region output-region))
  (if (node-is-axis-aligned? node)
      (& ((pos (node-pos node))
          (:decl (type (cons single-float single-float) pos))
          (nx (car pos))
          (ny (cdr pos))
          (:loop for i below 8 by 2)
          (:v (a b) (aref2 parent-region i (1+ i)))
          (:decl (type single-float a b))
          (:v (x y) (values (- a nx) (- b ny)))
          (:do (setf (aref output-region i) x  (aref output-region (1+ i)) y))))
      (& ((:loop for i below 8 by 2)
          (:v (a b) (aref2 parent-region i (1+ i)))
          (:decl (type single-float a b))
          (:v (x y) (node-transform-into-local-space node a b))
          (:do (setf (aref output-region i) x  (aref output-region (1+ i)) y))))))

(defgeneric node-transform-region-to-parent-space (node child-region output-region))

;; -------------------------------------------------------------
;; drawing

(defmethod draw-background ((node node) (parent node)))
(defmethod draw-foreground ((node node) (parent node)))

(defmethod node-should-redraw-with-policy ((policy (eql :refresh)) child parent)
  t)

(defmethod node-should-redraw ((child node) (parent node))
  (slot-value child 'wants-redraw))

(defun draw-bad-node (n)
  (multiple-value-bind (w h) (node-layout-size n)
    ;; should be more careful -- w-n-d-c could also produce an error if we borked an xform
    ;; would be nice to show an error text too
    ;; maybe could set the bad slot to the exception object instead of t
    (with-node-drawing-context (n)
      (draw:rgba-fill 1.0 0.5 0.0 1.0)
      (draw:box 0.0 0.0 w h))))

(defmethod draw-node :around ((n node) (p node))
  (let ((should-draw? (node-should-be-drawn n p))
        (is-bad? (slot-value n 'bad)))
    (cond
       ;; should report this better, but for now let's ensure
       ;; that errors in bad node drawing don't bring the whole
       ;; thing down
      (is-bad?
       (ignore-errors (draw-bad-node n)))
      (should-draw?
       ;; TODO: move this error handling up to `draw-world'
       ;;       it might be a lot to do this per-draw-per-node.
       (handler-case (call-next-method)
         (t (condition)
           ;; mark as bad, transfer control
           (setf (slot-value n 'bad) t)
           ;; inform someone there was a problem with this node
           (funcall (world-node-drawing-error-hook *world*) condition n)))))))

(defmethod draw-child-node ((parent node) (child node))
  #+nil
  (multiple-value-bind (x y) (node-transform-into-parent-space child 0.0 0.0)
    (rgba-fill 1.0 1.0 0.0 1.0)
    (box x y 5.0 5.0)
    (with-node-drawing-context (child)
      (multiple-value-bind (x2 y2) (node-transform-into-local-space child (.x child) (.y child))
        (rgba-fill 1.0 0.0 1.0 1.0)
        (box x2 y2 20.0 3.0)
        (rgba-fill 1.0 1.0 1.0 0.8))))
  (draw-node child parent))

(defmethod draw-children ((node node) (parent node))
  (declare (optimize (speed 3) (safety 1)))
  (if (and *clipping-region* (not (slot-value node 'discard-clipping-region)))
      (& ((scratch (.scratch-region node))
          (:do (node-transform-region-from-parent-space node *clipping-region* scratch))
          (*clipping-region* scratch)
          (:doseq child (node-children node))
          (:unless (node-outside-parent-region child scratch))
          (:do (draw-child-node node child))))
      (& ((*clipping-region* nil)
          (:doseq child (node-children node))
          (:do (draw-child-node node child)))))
  (values nil))

(defmethod node-active-region ((node node))
  (& (((cons x y) (node-pos node))
      (:v (w h) (node-layout-size node)))
    (values (1- x ) (1- y) (+ x w 2) (+ y h 2))))

(defmethod node-begin-frame ((n node) (p node)))

(defmethod draw-node ((node node) (parent node))
  (with-node-drawing-context (node)
    (draw-background node parent)
    (draw-children node parent)
    (draw-foreground node parent))
  #+nil
  (multiple-value-bind (x y) (node-transform-into-parent-space node 0.0 0.0)
    (rgba-fill 0.0 1.0 0.0 1.0)
    (box x y 12.0 2.0)))

(defmethod node-end-frame ((n node) (p node))
  (when (slot-value n 'wants-redraw)
    (doseq (ch (node-children n))
      (node-end-frame ch n))
    (setf (slot-value n 'wants-redraw) nil)))

(defmethod node-end-frame ((n world-node) (p world-node))
  (when (slot-value n 'wants-redraw)
    (dokids (ch n)
      (node-end-frame ch n))
    (setf (slot-value n 'wants-redraw) nil)))

(defmethod node-add-child ((node node) (child node))
  (prog1 node
    (when-let (p (node-parent child))
      (unless (eq p node)
        (node-reparent child node))
      (return-from node-add-child))
    (let ((wanted-draw? (slot-value child 'wants-redraw)))
      (setf (node-parent child) node)
      (vector-push-extend child (node-children node))
      (setf (node-wants-redraw node) t)
      (when wanted-draw?
        (node-child-wants-redraw node child)))))

(defmethod node-rem-child ((node node) (child node))
  (prog1 t
    (vector:delete-1 child (node-children node))
    (setf (node-wants-redraw node) t)
    (setf (node-parent child) nil)))

(defmethod node-reparent ((node node) (new-parent node))
  (node-rem-child (node-parent node) node)
  (node-add-child new-parent node))

(defmethod node-contains? ((node node) (parent node) x y)
  (declare (ignore x y))
  nil)

(defmethod node-natural-size-in-layout ((n common-node) layout w h)
  (values (.width n) (.height n) (.layout-force-x n) (.layout-force-y n)))

(defmethod node-layout-size ((b bounded))
  (values (bounded-width b) (bounded-height b)))

(defmethod node-contains? ((node bounded) (parent node) x y)
  (let* ((nw (bounded-width node))
         (nh (bounded-height node)))
    (not (or (< x 0.0) (< y 0.0)
             (> x nw) (> y nh)))))

(defmethod node-find-parent ((node node) test)
  (when-let (p (node-parent node))
    (unless (eq node p)
      (if (funcall test p) p
          (node-find-parent p test)))))
