(in-package :coffee.umbrella.affine)

(declaim (inline is-left?))
(defun is-left? (ax ay bx by px py)
  (declare (type single-float ax ay bx by px py))
  (- (* (- bx ax) (- py ay))
     (* (- px ax) (- by ay))))

(declaim (inline four-points-contain-point?))
(defun four-points-contain-point? (ax ay bx by cx cy dx dy px py)
  (declare (type single-float ax ay bx by cx cy dx dy px py))
  (and (plusp (is-left? ax ay bx by px py))
       (plusp (is-left? bx by cx cy px py))
       (plusp (is-left? cx cy dx dy px py))
       (plusp (is-left? dx dy ax ay px py))))

(declaim (inline make-affine))
(defun make-affine ()
  (make-array 9 :element-type t
                :initial-contents '(1.0 0.0 0.0
                                    0.0 1.0 0.0
                                    0.0 0.0 1.0)))

(declaim (inline copy-affine))
(defun copy-affine (a)
  (copy-array a))

(declaim (inline load-identity))
(defun load-identity (a)
  (setf (fref a 0 0) 1.0 (fref a 0 1) 0.0 (fref a 0 2) 0.0
        (fref a 1 0) 0.0 (fref a 1 1) 1.0 (fref a 1 2) 0.0))

(defun component-values (a)
  ;; a b c d tx ty (matching expectations of core graphics)
  (values (fref a 0 0) (fref a 1 0)
          (fref a 0 1) (fref a 1 1)
          (fref a 0 2) (fref a 1 2)))

(defun set-translation (a x y)
  (incf (fref a 0 2) x)
  (incf (fref a 1 2) y)
  (values))

(defun translation (a)
  (values (fref a 0 2) (fref a 1 2)))
;;
;; counter-clockwise, origin at top-left
(defun set-rotation (a rads)
  (let ((s (coerce (sin rads) 'single-float))
        (c (coerce (cos rads) 'single-float)))
    (setf (fref a 0 0) c     (fref a 0 1) s
          (fref a 1 0) (- s) (fref a 1 1) c)
    (values)))

(defun set-scale (a sx sy)
  (setf (fref a 0 0) sx
        (fref a 1 1) sy)
  (values))

;; ---------------------------------------------------------------
;; [a b c] [j k l]
;; [d e f] [m n o]
;; [g h i] [p q r]
;;
;; 0,0 = aj + bm + cp
;; 1,0 = ak + bn + cq
;; 2,0 = al + bo + cr
;; x,y = row(y, A) *** col(x, B)
;;
(declaim (inline mmul))
(defun mmul (aa bb out)
  (macrolet
      ((row (r idx) `(fref aa ,r ,idx))
       (col (c idx) `(fref bb ,idx ,c))
       (*** (x y)
         `(+ (* (row ,y 0) (col ,x 0))
             (* (row ,y 1) (col ,x 1))
             (* (row ,y 2) (col ,x 2))))
       (! (x y) `(setf (fref out ,y ,x) (*** ,x ,y)))
       (doit ()
         `(progn ,@(loop for x below 3 append (loop for y below 3 collect `(! ,x ,y))))))
    (doit)))

(declaim (inline load-other))
(defun load-other (input output)
  (macrolet
      ((! (x y) `(setf (fref output ,x ,y) (fref input ,x ,y)))
       (doit () `(progn ,@(loop for x below 3 append (loop for y below 3 collect `(! ,x ,y))))))
    (doit)
    (values)))

(declaim (inline rotate))
(defun rotate (radians in-place)
  (declare (type single-float radians)
           (type affine in-place))
  (let ((a (make-affine))
        (b (make-affine)))
    (declare (type affine a b)
             (dynamic-extent a b))
    (load-other in-place a)
    (set-rotation b radians)
    (mmul a b in-place)
    (values)))

(declaim (inline l-rotate))
(defun l-rotate (radians in-place)
  (declare (type single-float radians)
           (type affine in-place))
  (let ((a (make-affine))
        (b (make-affine)))
    (declare (type affine a b)
             (dynamic-extent a b))
    (load-other in-place a)
    (set-rotation b radians)
    (mmul b a in-place)
    (values)))

(declaim (inline translate))
(defun translate (x y in-place)
  (declare (type single-float x y)
           (type affine in-place))
  (let ((a (make-affine))
        (b (make-affine)))
    (declare (type affine a b)
             (dynamic-extent a b))
    (load-other in-place a)
    (set-translation b x y)
    (mmul a b in-place)
    (values)))

(declaim (inline l-translate))
(defun l-translate (x y in-place)
  (declare (type single-float x y)
           (type affine in-place))
  (let ((a (make-affine))
        (b (make-affine)))
    (declare (type affine a b)
             (dynamic-extent a b))
    (load-other in-place a)
    (set-translation b x y)
    (mmul b a in-place)
    (values)))

(declaim (inline scale))
(defun scale (sx sy in-place)
  (declare (type single-float sx sy)
           (type affine in-place))
  (let ((a (make-affine))
        (b (make-affine)))
    (declare (type affine a b)
             (dynamic-extent a b))
    (load-other in-place a)
    (set-scale b sx sy)
    (mmul a b in-place)
    (values)))

(declaim (inline l-scale))
(defun l-scale (sx sy in-place)
  (declare (type single-float sx sy)
           (type affine in-place))
  (let ((a (make-affine))
        (b (make-affine)))
    (declare (type affine a b)
             (dynamic-extent a b))
    (load-other in-place a)
    (set-scale b sx sy)
    (mmul b a in-place)
    (values)))

;; from child node up to parent node space
(defun standard-node-transform-out-xy (nx ny ax ay sx sy r x y)
  (declare (type single-float nx ny ax ay sx sy r x y))
  (let ((m (make-affine)))
    (declare (type affine m)
             (dynamic-extent m))
    (translate ax ay m)
    (rotate r m)
    (scale sx sy m)
    (translate (- ax) (- ay) m)
    (translate nx ny m)
    (transform-xy m x y)))

;; from parent node into child node space
(defun standard-node-transform-in-xy (nx ny ax ay sx sy r x y)
  (declare (type single-float nx ny ax ay sx sy r x y))
  (let ((m (make-affine)))
    (declare (type affine m)
             (dynamic-extent m))
    (backwards
      (translate (- nx) (- ny) m)
      (translate ax ay m)
      (rotate (- r) m)
      (scale (/ 1.0 sx) (/ 1.0 sy) m)
      (translate (- ax) (- ay) m))
    (multiple-value-bind (ansx ansy)
        (transform-xy m x y)
      (values ansx ansy))))

;; for drawing
(defun call-with-standard-node-transform (nx ny ax ay sx sy r fn)
  (declare (optimize (speed 3) (safety 1))
           (type single-float nx ny ax ay sx sy r))
  (let ((m (make-affine)))
    (declare (type affine m)
             (dynamic-extent m))
    (backwards
      (translate nx ny m)
      (translate (- ax) (- ay) m)
      (rotate r m)
      (scale sx sy m)
      (translate ax ay m))
    (funcall fn m)
    (values)))

(defun load-standard-node-transform (nx ny ax ay sx sy r m)
  (declare (optimize (speed 3) (safety 1))
           (type single-float nx ny ax ay sx sy r)
           (type affine m))
  (load-identity m)
  (backwards
    (translate nx ny m)
    (translate (- ax) (- ay) m)
    (rotate r m)
    (scale sx sy m)
    (translate ax ay m))
  (values))

(defun load-standard-inverse-node-transform (nx ny ax ay sx sy r m)
  (declare (optimize (speed 3) (safety 1))
           (type single-float nx ny ax ay sx sy r)
           (type affine m))
  (load-identity m)
  (backwards
    (l-translate (- nx) (- ny) m)
    (l-translate ax ay m)
    (l-rotate (- r) m)
    (l-scale (/ 1.0 sx) (/ 1.0 sy) m)
    (l-translate (- ax) (- ay) m))
  (values))

(declaim (inline transform-xy))
(defun transform-xy (a x y)
  (declare (type affine a)
           (type single-float x y)
           (optimize (speed 3) (safety 1)))
  (let ((a (fref a 0 0)) (b (fref a 0 1)) (c (fref a 0 2))
        (d (fref a 1 0)) (e (fref a 1 1)) (f (fref a 1 2)))
    (values (+ (* a x) (* b y) c)
            (+ (* d x) (* e y) f))))

(defun scale-xy (a x y)
  (let ((sx (fref a 0 0))
        (sy (fref a 1 1)))
    (let ((b (make-affine)))
      (declare (dynamic-extent b))
      ;; this is silly, but I'm too bleary to work out the mmul for this RN
      (set-scale b sx sy)
      (transform-xy b x y))))

(defun inverse-scale-xy (a x y)
  (let ((sx (fref a 0 0))
        (sy (fref a 1 1)))
    (let ((b (make-affine)))
      (declare (dynamic-extent b))
      ;; this is silly, but I'm too bleary to work out the mmul for this RN
      (set-scale b (/ 1.0 sx ) (/ 1.0 sy))
      (transform-xy b x y))))

(defun scale-around-origin (sx sy a)
  (let ((asx (fref a 0 0))
        (asy (fref a 1 1)))
    (setf (fref a 0 0) (* asx sx)
          (fref a 1 1) (* asy sy))
    (values)))

(defun get-translation-rotation-and-scale (a)
  (multiple-value-bind (ax ay) (transform-xy a 0.0 0.0)
    (multiple-value-bind (bx by) (transform-xy a 1.0 0.0)
      (let* ((dx (- bx ax))
             (dy (- by ay))
             ;; TODO: get sy as well
             (sx (sqrt (+ (* dx dx) (* dy dy))))
             (angle (atan dy dx)))
        (values ax ay angle sx sx)))))
