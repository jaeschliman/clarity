(in-package :coffee.umbrella.clarity.simple-scene-graph-0)

;; -------------------------------------------------------------
;; start of a layout protocol
;;   really just sketched in at this point, not yet well-defined

;; align may be :leading, :center, or :trailing
(define-model layout () ((container-bounds-changed? nil)))
(define-model layout.vstack (layout) ((align :center)))
(define-model layout.hstack (layout) ((align :center)))
(define-model layout.zstack (layout) ())
(define-model layout.wrapping-hstack (layout) ((align :center)))
(define-model layout.center-contents (layout) ())
(define-model layout.stretch-contents (layout.center-contents) ())
(define-model layout.scale-contents (layout) ((x-align :center) (y-align :center)))

(defvar *currently-laying-out* nil)

;; where force is not neccessarily a number -- could be an object
(defgeneric node-natural-size-in-layout (node layout w h)) ; -> w, h, force, force
(defmethod node-natural-size-in-layout ((node node) layout w h)
  (declare (ignore layout))
  (values 0.0 0.0 nil nil))

(defgeneric node-children-for-layout (node))
(defgeneric layout-begin-layout (layout))
(defgeneric layout-arrange-nodes (layout container children w h)) ;; void
(defgeneric layout-container-layout-area (layout container parent-width parent-height))  ;; x y w h
(defgeneric layout-adjust-nodes-after-layout (layout container children))
(defgeneric layout-end-layout (layout))
(defgeneric node-layout-in-bounds (node x y w h))
(defgeneric node-prepare-for-layout (node))
(defgeneric node-layout-children (node w h))
;; what to do with this one...
;; I guess we need it for scaling still? so we can properly set the anchor point?
(defgeneric node-finalize-layout (node))

(defmethod maybe-invalidate-layout-on-change ((n common-node) (l layout) slot-name old-value new-value)
  (declare (ignore old-value new-value))
  (unless (eq n *currently-laying-out*)
    ;; by default we invalidate a layout on any change to width or height
    (case slot-name
      ((width height)
       (unless (node-wants-layout n) (setf (node-wants-layout n) t))
       (setf (.container-bounds-changed? l) t)))))

(defmethod layout-begin-layout ((layout layout)))
(defmethod layout-end-layout ((layout layout))
  (setf (.container-bounds-changed? layout) nil))

(defmethod node-layout-in-bounds :around ((node node) x y w h)
  (let ((*currently-laying-out* node))
    (call-next-method)))

(defmethod node-layout-in-bounds ((node node) x y w h)
  (node-moveto node x y)
  (node-layout-children node w h))

(defmethod node-children-for-layout ((n node))
  (node-children n))

(defmethod layout-adjust-nodes-after-layout (layout container children)
  (declare (ignore layout container children)))

(defmethod node-prepare-for-layout (node)
  (declare (ignore node))
  #+nil
  (format t "preparing to layout node:: ~A~%" node))

(defmethod node-finalize-layout (node)
  (declare (ignore node)))

(defmethod node-finalize-layout :after ((node node))
  (setf (slot-value node 'wants-layout) nil))

(defmethod node-layout-children :before ((node node) w h)
  (map nil 'node-prepare-for-layout (node-children-for-layout node)))

(defmethod node-layout-children ((n node) w h)
  ;; just propagate by default
  (doseq (ch (node-children-for-layout n))
    (node-layout-in-bounds ch 0.0 0.0 w h)))

(defmethod node-layout-children :after ((node node) w h)
  (map nil 'node-finalize-layout (node-children-for-layout node)))

(defmethod node-layout-children ((n common-node) w h)
  ;; layout "should" not require iteration,
  ;; but this can help when working things out
  (prog ((iterations 0) (max-iterations 5))
     :loop
     (when (> iterations max-iterations)
       #+nil
       (format t "giving up on layout out node: ~A~%" n)
       (return))
     (setf (slot-value n 'wants-layout) nil)
      #+nil
     (format t "laying out children for node: ~A iteration:~A ~%" n iterations)
     (let ((children (node-children-for-layout n))
           (layout   (.layout-style n)))
       (when layout (layout-begin-layout layout))
       (layout-arrange-nodes layout n children w h)
       (layout-adjust-nodes-after-layout layout n children)
       (when layout (layout-end-layout layout)))
     (when (slot-value n 'wants-layout)
       (incf iterations)
       (go :loop))))

(defmethod layout-container-layout-area (layout (container bounded-node) w h)
  (values 0.0 0.0 (max w (.width container)) (max h (.height container))))

(defmethod layout-arrange-nodes ((layout null) (container node) children pw ph)
  (doseq (ch children)
    (when (node-wants-layout ch)
      (multiple-value-bind (w h fx fy) (node-natural-size-in-layout ch layout pw ph)
        (declare (ignore fx fy))
        (node-layout-in-bounds ch (.x ch) (.y ch) w h)))))

(defmethod layout-arrange-nodes ((layout layout.center-contents) (container node) children pw ph)
  (multiple-value-bind (lx ly lw lh) (layout-container-layout-area layout container pw ph)
    (doseq (ch children)
      (multiple-value-bind (chw chh fx fy) (node-natural-size-in-layout ch layout lw lh)
        (let* ((w (if (eq fx :expand) lw chw))
               (h (if (eq fy :expand) lh chh))
               (cx (+ lx (* lw 0.5)))
               (cy (+ ly (* lh 0.5))))
          (node-layout-in-bounds ch (- cx (* w 0.5)) (- cy (* h 0.5)) w h))))))

(defmethod layout-adjust-nodes-after-layout ((layout layout.stretch-contents) (container node) children)
  (multiple-value-bind (lx ly lw lh) (layout-container-layout-area layout container
                                                                   (.width container)
                                                                   (.height container))
    (declare (ignore lx ly))
    (doseq (ch children)
      (multiple-value-bind (nw nh) (node-layout-size ch)
        (unless (or (zerop nw) (zerop nh))
          (let ((sx (/ lw nw))
                (sy (/ lh nh)))
            (setf (.scale ch) (cons sx sy))))))))

(defmethod layout-arrange-nodes ((layout layout.scale-contents) (container node) children pw ph)
  (multiple-value-bind (lx ly lw lh) (layout-container-layout-area layout container pw ph)
    (doseq (ch children)
      (multiple-value-bind (chw chh fx fy) (node-natural-size-in-layout ch layout lw lh)
        (let* ((w (if (eq fx :expand) lw chw))
               (h (if (eq fy :expand) lh chh))
               (child-aspect (if (zerop h) 1.0 (/ w h)))
               (bound-aspect (if (zerop lh) 1.0 (/ lw lh)))
               (direction (if (< bound-aspect child-aspect) :vertical :horizontal))
               (hw (* w 0.5))
               (hh (* h 0.5))
               (cx (ecase direction
                     (:vertical (+ lx (* lw 0.5)))
                     (:horizontal
                      (ecase (.x-align layout)
                        (:leading hw)
                        (:trailing (- lw hw))
                        (:center (+ lx (* lw 0.5)))))))
               (cy (ecase direction
                     (:horizontal (+ ly (* lh 0.5)))
                     (:vertical
                      (ecase (.y-align layout)
                        (:leading hh)
                        (:trailing (- lh hh))
                        (:center (+ ly (* lh 0.5)))))))
               (ax (ecase direction
                     (:vertical 0.5)
                     (:horizontal
                      (ecase (.x-align layout)
                        (:leading 0.0)
                        (:trailing 1.0)
                        (:center 0.5)))))
               (ay (ecase direction
                     (:horizontal 0.5)
                     (:vertical
                      (ecase (.y-align layout)
                        (:leading 0.0)
                        (:trailing 1.0)
                        (:center 0.4))))))
          (setf (.anchor ch) (cons ax ay))
          (node-layout-in-bounds ch (- cx hw) (- cy hh) w h))))))

(defmethod layout-adjust-nodes-after-layout ((layout layout.scale-contents) (container node) children)
  (let ((pw (.width container))
        (ph (.height container)))
    (multiple-value-bind (lx ly lw lh) (layout-container-layout-area layout container pw ph)
      (declare (ignore lx ly))
      (doseq (ch children)
        (multiple-value-bind (nw nh) (node-layout-size ch)
          (unless (or (zerop nw) (zerop nh))
            (let* ((sx (/ lw nw))
                   (sy (/ lh nh))
                   (s (min sx sy)))
              (setf (.scale ch) (cons s s)))))))))

(defmethod layout-arrange-nodes ((layout layout.vstack) (container node) children pw ph)
  (multiple-value-bind (lx ly lw lh) (layout-container-layout-area layout container pw ph)
    (let ((cstyle (.container-style container)))
      (let ((bot ly) (right lx))
        (map nil (lambda (ch)
                   (multiple-value-bind (chw chh fx fy) (node-natural-size-in-layout ch layout lw lh)
                     (declare (ignore fy))
                     (let* ((w (if (eq fx :expand) (max lw chw) chw))
                            (h chh))
                       (node-layout-in-bounds ch lx bot w h)
                       (multiple-value-bind (actualw actualh) (node-layout-size ch)
                         (maxf right (+ lx actualw))
                         (incf bot actualh)))))
             children)
        (case cstyle
          (:stack-trailing
           (setf (.height container) bot)
           (setf (.width container) (max right (.width container))))
          (:fit-children
           (setf (.height container) bot)
           (setf (.width container) (max right (.width container)))))))))

(defmethod layout-arrange-nodes ((layout layout.hstack) (container node) children pw ph)
  (multiple-value-bind (lx ly lw lh) (layout-container-layout-area layout container pw ph)
    (let ((cstyle (.container-style container)))
      (let ((bot ly) (right lx))
        (map nil (lambda (ch)
                   (multiple-value-bind (chw chh fx fy) (node-natural-size-in-layout ch layout lw lh)
                     (declare (ignore fx))
                     (let* ((w chw)
                            (h (if (eq fy :expand) (max lh chh) chh)))
                       (node-layout-in-bounds ch right ly w h)
                       (multiple-value-bind (actualw actualh) (node-layout-size ch)
                         (maxf bot (+ ly actualh))
                         (incf right actualw)))))
             children)
        (case cstyle
          (:fit-children
           (setf (.height container) (max bot (.height container)))
           (setf (.width container) right)))))))

(defmethod layout-arrange-nodes ((layout layout.wrapping-hstack) (container node) children pw ph)
  (multiple-value-bind (lx ly lw lh) (layout-container-layout-area layout container pw ph)
    (let ((cstyle (.container-style container)))
      (let ((bot ly) (right lx) (currtop ly) (currright lx) (currbot ly))
        (map nil (lambda (ch)
                   (multiple-value-bind (chw chh fx fy) (node-natural-size-in-layout ch layout lw lh)
                     (declare (ignore fx fy))
                     (let* ((w chw)
                            (h chh))
                       (when (> (+ w currright) pw)
                         (setf currright lx
                               currtop currbot))
                       (node-layout-in-bounds ch currright currtop w h)
                       (multiple-value-bind (actualw actualh) (node-layout-size ch)
                         (incf currright actualw)
                         (maxf currbot (+ currtop actualh))
                         (maxf right currright)
                         (maxf bot currbot)))))
             children)
        (case cstyle
          (:fit-children
           (setf (.height container) (max bot (.height container)))
           (setf (.width container) right)))))))
