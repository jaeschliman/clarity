(defpackage :coffee.umbrella.clarity.editors.text
  (:use :cl :alexandria :trivia
   :coffee.umbrella.utils :coffee.umbrella.model :coffee.umbrella.dot-access.symbols)
  (:local-nicknames
   (:node :coffee.umbrella.clarity.simple-scene-graph-0)
   (:list :coffee.umbrella.list)
   (:string :coffee.umbrella.string)
   (:draw :coffee.umbrella.clarity.draw)
   (:font :coffee.umbrella.clarity.font)))
(in-package :coffee.umbrella.clarity.editors.text)

;; stripped-down WIP text editor.

(define-model editor-node (node:common-node)
  ((editor nil)
   (scroll-offset 0.0))
  (:default-initargs :width 600.0 :height 800.0))

(define-model editor ()
  (
   ;; to be replaced by a cursor model
   ;; would be nice to have more than one cursor too.
   ;; and perhaps they should "stick to" the line they are on unless moved,
   ;; so that both cursors can edit without losing place.
   cursor-x
   cursor-y
   cursor-style
   cursor-color
   cursor-blink

   (mouse-cursor-x 0)
   (mouse-cursor-y 0)
   (mouse-cursor-active nil)

   ;; the package to evaluate in
   package

   ;; model of the text we are editing
   text

   (poplist nil)
   (evaluator 'lisp-compile-free-string)))

(define-model line ()
  (string))

(define-model line-buffer ()
  (lines))

(define-model poplist ()
  ((prefix "") (contents nil) (selection-index 0)  (row 0) (col 0) (visible nil)))

(defmethod poplist-insertion-text ((p poplist))
  (elt (.contents p) (.selection-index p)))

(define-model poplist-node (node:node)
  (model editor))

(defmethod initialize-instance :after ((self poplist-node) &key)
  (have (.model self) node:update self)
  (node:node-add-child (.node (.editor self)) self))

(defun xof (node) (car (.pos node)))
(defun yof (node) (cdr (.pos node)))

(defmethod node:update ((m poplist) (n poplist-node) slot old-value new-value)
  (declare (ignorable old-value))
  (when (eq slot 'visible) (setf (.visible n) new-value))
  (let ((chw (font:character-width #\Space))
        (chh (font:character-height #\Space)))
    (when (eq slot 'selection-index)
      (let ((y (editor-y-position-for-row (.editor n) (.row m)))
            (x (* chw (.col m)))
            (offs (* chh new-value)))
        (setf (.pos n) (cons x (- y offs)))))
    (when (eq slot 'row)
      (let ((y (editor-y-position-for-row (.editor n) new-value))
            (offs (* chh (.selection-index m))))
        (setf (.pos n) (cons (xof n) (- y offs)))))
    (when (eq slot 'col)
      (let ((x (* chw new-value)))
        (setf (.pos n) (cons x (yof n)))))))
  ;; catchall
  (setf (.wants-redraw n) t))

(defmethod ensure-poplist ((editor editor))
  (or (.poplist editor)
      (let ((poplist (make-instance 'poplist)))
        (prog1 poplist
          (setf (.poplist editor) poplist)
          (node-add-child
           (.node editor)
           (make-instance 'poplist-node :editor editor :model poplist))))))

(defmethod node:draw-background ((n poplist-node) (p node))
(let ((m (.model n))
      (w (font:character-width #\Space)))
    (multiple-value-bind (w h) (node:node-layout-size n)
      ;; TODO: adjust the actual position of the node, not just draw offset (once it looks right)
      (draw:moveby (* (length (.prefix m)) w -1) 0.0)
      (draw:reserve-box 0.0 0.0 w h)
      (draw:rgba-fill 1.0 1.0 1.0 0.5)
      (draw:box 0.0 0.0 w h)
      ;; TODO figure out what to do with the prefix/selection/etc
      (let ((h (font:character-height #\Space))
            (y 0.0) (idx 0) (sidx (.selection-index m)))
        (draw:rgba-fill 0.2 0.2 0.3 1.0)
        (draw:with-font (*font*)
          (doseq (item (.contents m))
            (if (= sidx idx)
                (progn
                  (let* ((pfx (.prefix m))
                         (pfxlen (length pfx)))
                    (if (starts-with-subseq pfx item)
                        (progn
                          (draw:textat 0.0 y pfx)
                          (draw:rgba-fill 0.2 0.7 0.3 1.0)
                          (draw:textat (* w pfxlen) y (subseq item pfxlen))
                          (draw:rgba-fill 0.2 0.2 0.3 1.0))
                        (progn
                          (draw:textat 0.0 y pfx)
                          (draw:rgba-fill 0.2 0.3 0.9 1.0)
                          (draw:textat (* w (1+ pfxlen)) y item)
                          (draw:rgba-fill 0.2 0.2 0.3 1.0)))))
                (draw:textat 0.0 y item))
            (incf y h)
            (incf idx)))))))
