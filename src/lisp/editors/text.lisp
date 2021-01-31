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

   ;; "stickers"
   ;; in an early implementation, the text editor supported
   ;; annotating text with 'stickers' of variable height that were collapsible
   ;; and so on. this was used to display compiler notes and warnings.
   ;; will probably bring them into this implementation at some point,
   ;; but for now the focus is on porting the basic text editor _somewhat_ cleanly,
   ;; and then porting over the WIP structure editor.
   (stickers nil)

   ;; model of the text we are editing
   text
   (transient-output nil)
   (poplist nil)
   (evaluator 'lisp-compile-free-string)))


(define-model line ()
  (string))

(define-model line-buffer ()
  (lines))

(defun make-editor (&key on-string
                      (initial-row 0)
                      (initial-col 0)
                      package
                      evaluator)
  (let* ((node (make-instance 'editor-node))
         (buffer (line-buffer-from-string (or on-string "")))
         (editor (make-instance 'editor
                                :cursor-x initial-col :cursor-y initial-row
                                :cursor-style :block
                                :cursor-color '(0.0 1.0 1.0 0.3)
                                :cursor-blink nil
                                :text buffer
                                :transient-output nil
                                :node node
                                :package package)))
    (when evaluator (setf (.evaluator editor) evaluator))
    (have editor node:update node)
    (have buffer node:update node)
    editor))

(defmethod node:update ((e editor) (n editor-node) slot old-value new-value)
  (declare (ignore slot old-value new-value))
  (setf (node-wants-redraw n) t))

(defmethod node:update ((e line-buffer) (n editor-node) slot old-value new-value)
  (declare (ignore slot old-value new-value))
  (setf (node-wants-redraw n) t))

;; ---------------------------------------------------------------
;; stub sticker implementation
;;
;; placeholder functions until sticker code is ported over

(defmethod stickers-for-row ((editor editor) (stickers null) row)
  (declare (ignore row)) nil)
(defmethod stickers-at-point ((editor editor) (stickers null) col row)
  (declare (ignore col row)) nil)
(defun stickers-combined-row-height (stickers)
  (declare (ignore stickers))
  0.0)

;; ---------------------------------------------------------------
;; autocomplete implementation
;;
;; 'poplist' being a list of completions that 'pop' up

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

;; very very bare bones stuff here..
(defun whitespace? (ch) (or (char-equal ch #\Space)
                            (char-equal ch #\Newline)))

(defun token-break? (ch)
  (or (whitespace? ch)
      (member ch '(#\[ #\] #\( #\) #\; #\' #\"))))

(defun string-lisp-token-under-col (string col)
  (when (< col (length string))
    ;; don't understand block comments yet, sorry!
    (unless (token-break? (aref string col))
      (let ((comment-start (position #\; string :from-end t :end col)))
        (unless comment-start
          (let ((start (1+ (or (position-if 'token-break? string :from-end t :end col) -1)))
                (end  (or (position-if 'token-break? string :start col)
                          (length string))))
            (values (subseq string start end) start end)))))))

(defmethod line-token-under-xy ((line-buffer line-buffer) col row)
  (let ((lines (line-buffer-lines line-buffer)))
    (when (<= row (length lines))
      (let* ((line (elt lines row)))
        (string-lisp-token-under-col line col)))))

(defmethod line-token-before-xy ((line-buffer line-buffer) col row)
  (unless (zerop col)
    (line-token-under-xy line-buffer (1- col) row)))

(defun get-swank-completions-for-token (token package-name)
  (swank:simple-completions token package-name)
  #+nil
  (swank:completions token package-name))

(defmethod editor-showing-completions? ((editor editor))
  (when-let (p (.poplist editor))
    (and (.visible p) p)))

(defmethod editor-refresh-completions ((editor editor))
  (when-let (poplist (editor-showing-completions? editor))
    (let ((col (.cursor-x editor))
          (row (.cursor-y editor)))
      (if-let (token (line-token-before-xy (.text editor) col row))
        (if-let (completions (get-swank-completions-for-token token (package-name (.package editor))))
          (let* ((prev-index (.selection-index poplist))
                 (prev-item  (ignore-errors  (elt (.contents poplist) prev-index))) ;; ignore-erros gross
                 (new-idx
                  (if (= 1 (length token))
                      ;; prefer the suggested completion. it doesn't appear to be that helpful though (yet?)
                      ;; perhaps I need to learn how to use swank fuzzy matching!
                      (or (position (second completions) (first completions) :test 'string-equal) 0)
                      (or (and prev-item (position prev-item (first completions) :test 'string-equal)) 0))))
            (if (and (= 1 (length (first completions)))
                     (string-equal (car (first completions)) token))
                (setf (.visible poplist) nil
                      (.transient-output editor)
                      (list (format nil ";; ~S is complete.~%" token)))
                (setf (.transient-output editor)
                      (list (format nil ";; ~A completions for ~S ~%" (length (first completions)) token))
                      (.contents poplist) (first completions)
                      (.col poplist) col
                      (.row poplist) row
                      (.prefix poplist) token
                      (.selection-index poplist) new-idx
                      (.visible poplist) t)))
          (setf (.visible poplist) nil
                (.transient-output editor)
                (list (format nil ";; No completions for: ~S~%" token))))
        (setf (.visible poplist) nil
              (.transient-output editor)
              (list (format nil ";; No completion token at point.~%")))))))

(defmethod editor-ensure-hide-completions ((editor editor))
  (when-let (p (editor-showing-completions? editor))
    (setf (.visible p) nil (.selection-index p) 0)))

(defmethod editor-ensure-show-completions ((editor editor))
  (let ((p (ensure-poplist editor)))
    (unless (editor-showing-completions? editor)
      (setf (.visible p) t)
      (editor-refresh-completions editor))))

(defvar *editor-auto-auto-complete* t)

(defmethod editor-try-activate-completion ((editor editor))
  (when *editor-auto-auto-complete*
    (editor-ensure-show-completions editor)))

;; toggle completions
(defmethod editor-toggle-completions ((editor editor))
  (setf *editor-auto-auto-complete* (not *editor-auto-auto-complete*))
  (if-let (p (editor-showing-completions? editor))
    (editor-ensure-hide-completions editor)
    (editor-ensure-show-completions editor))
    (setf (.transient-output editor)
          (list (format nil ";; Autocomplete ~A~%" (if *editor-auto-auto-complete* "active" "inactive")))))

(defmethod editor-next-completion ((editor editor))
  (when-let (p (editor-showing-completions? editor))
    (setf (.selection-index p) (min (1- (length (.contents p)))
                                    (1+ (.selection-index p))))))

(defmethod editor-prev-completion ((editor editor))
  (when-let (p (editor-showing-completions? editor))
    (setf (.selection-index p) (max 0 (1- (.selection-index p))))))


(defmethod editor-completions-append-character ((editor editor) char)
  (when-let (p (editor-showing-completions? editor))
    (let* ((selection (ignore-errors (elt (.contents p) (.selection-index p))))
           (pfx (concatenate 'string (.prefix p) (list char)))
           (contents (remove-if-not (lambda (match) (starts-with-subseq pfx match))
                                    (.contents p)))
           (new-idx (or (and selection (position selection contents :test 'string-equal)) 0)))
      (if contents
          (setf (.prefix p) pfx (.contents p) contents (.col p) (1+ (.col p)) (.selection-index p) new-idx)
          (editor-ensure-hide-completions editor)))))

(defmethod editor-completions-delete-character ((editor editor))
  (when-let (p (editor-showing-completions? editor))
    (editor-refresh-completions editor)))

(defmethod editor-completions-accept-current ((editor editor))
  (when-let (p (editor-showing-completions? editor))
    (let* ((text (poplist-insertion-text p))
           (pfx (.prefix p))
           (plen (length pfx)))
      (delete-text (.text editor) plen (- (.cursor-x editor) plen) (.cursor-y editor))
      (decf (.cursor-x editor) plen)
      (editor-ensure-hide-completions editor)
      (editor-insert editor text))))

(defmethod split-line ((line-buffer line-buffer) col row)
  (let ((lines (line-buffer-lines line-buffer)))
    (when (<= row (length lines))
      (let* ((line (elt lines row))
             (left (subseq line 0 (min col (length line))))
             (right (if (<= col (length line))
                        (subseq line col)
                        "")))
        (setf lines (list:replace-at lines left row))
        (setf lines (list:insert-at lines right (1+ row)))
        (setf (line-buffer-lines line-buffer) lines)))))

(defmethod join-line ((line-buffer line-buffer) row)
  (let ((lines (line-buffer-lines line-buffer)))
    (when (< row (length lines))
      (let* ((line (elt lines row))
             (next-line (elt lines (1+ row)))
             (new-line (concatenate 'string line next-line)))
        (setf lines (list:remove-at lines (1+ row)))
        (setf lines (list:replace-at lines new-line row))
        (setf (line-buffer-lines line-buffer) lines)))))

(defmethod line-length ((line-buffer line-buffer) row)
  (let ((lines (line-buffer-lines line-buffer)))
    (if (<= row (length lines))
        (length (elt lines row))
        0)))

(defmethod insert-text ((line-buffer line-buffer) (text string) col row)
  (let* ((lines (line-buffer-lines line-buffer))
         (line (elt lines row))
         (replacement-line (string:line-insert-at line text col))
         (replacement-lines (list:replace-at lines replacement-line row)))
    (setf (line-buffer-lines line-buffer) replacement-lines)))

(defmethod delete-text ((line-buffer line-buffer) character-count col row)
   (let* ((lines (line-buffer-lines line-buffer))
         (line (elt lines row))
         (replacement-line (string:line-delete-at line character-count col))
         (replacement-lines (list:replace-at lines replacement-line row)))
     (setf (line-buffer-lines line-buffer) replacement-lines)))

(defun line-buffer-from-string (string)
  (make-instance 'line-buffer :lines (string:lines string)))

(defmethod line-buffer-string-contents ((line-buffer line-buffer))
  (with-output-to-string (s)
    (doseq (line (line-buffer-lines line-buffer))
      (write-line line s))))

(defmethod editor-string-contents ((editor editor))
  (line-buffer-string-contents (editor-text editor)))

(defmethod editor-y-position-for-row ((editor editor) row)
  (let ((sticker-index (editor-stickers editor))
        (step (font:character-height #\Space)))
    (float
     (loop
        for curr-row below row
        for stickers = (stickers-for-row editor sticker-index curr-row)
        for sticker-line-height = (stickers-combined-row-height stickers)
        sum (+ step sticker-line-height)))))

(defmethod editor-character-position-for-col-row ((editor editor) col row)
  (let ((line-buffer (.text editor)))
    (+ (min col (1- (length (elt (.lines line-buffer) row))))
       (loop for curr-row below row
          for line in (.lines line-buffer)
          sum (1+ (length line))))))

(defmethod editor-col-row-for-x-y ((editor editor) x y)
  (let ((sticker-index (editor-stickers editor))
        (chw  (font:character-width #\Space))
        (step (font:character-height #\Space)))
    (values
     (float (floor x chw))
     (float
      (loop
         for curr-row upfrom 0
         for stickers = (stickers-for-row editor sticker-index curr-row)
         for sticker-line-height = (stickers-combined-row-height stickers)
         sum (+ step sticker-line-height) into bottom-edge
         until (> bottom-edge y)
         finally (return curr-row))))))

(defun draw-cursor (editor)
  (let* ((x (editor-cursor-x editor))
         (y (editor-cursor-y editor))
         (w (font:character-width #\Space))
         (h (font:character-height #\Space))
         (xl (* x w))
         (yt (editor-y-position-for-row editor y))
         (yb (+ yt h)))
    (draw:with-graphics-group ()
      (apply 'draw:rgba-fill (editor-cursor-color editor))
      (case (editor-cursor-style editor)
        (:block     (draw:box xl yt w h))
        (:bar       (draw:box (- xl 2.0) yt 2.0 h))
        (:underline (draw:box xl (- yb 5.0) w 5.0))))))

(defun draw-token-highlight (token start end)
  (declare (ignore token))
  (draw:with-graphics-group ()
    (draw:rgba-fill 0.6 1.0 0.7 1.0)
    (let ((w (font:character-width #\Space))
          (h (font:character-height #\Space)))
      (draw:box (* w start) 0.0 (* w (- end start)) h))))

(defmethod draw-line-buffer-line ((line string))
  (draw:textat 0.0 0.0 line)
  (draw:translate 0.0 (font:character-height #\Space)))

(defmethod editor-draw-lines ((editor editor) scroll-x-offset scroll-y-offset width height)
  (let ((buffer (editor-text editor))
        (sticker-index (editor-stickers editor))
        (mouse-active (.mouse-cursor-active editor))
        (mx (round (.mouse-cursor-x editor)))
        (my (round (.mouse-cursor-y editor))))
    (draw:with-clip-shape ()
      (draw:box 0.0 0.0 width height)
      (loop
        with endline = scroll-y-offset
        with startline = 0.0
        with drawing = nil
        with line-height = (font:character-height #\Space)
        for line in (line-buffer-lines buffer)
        for row upfrom 0
        for stickers = (stickers-for-row editor sticker-index row)
        for sticker-line-height = (stickers-combined-row-height stickers)
        do
           (when (> endline height) (return))
           (let ((step (+ line-height sticker-line-height)))
             (when (> endline 0.0)
               (unless drawing
                 (setf drawing t)
                 (draw:moveby scroll-x-offset (+ scroll-y-offset startline)))
               (when (and (= row my) mouse-active
                          (>= mx 0) (>= my 0))
                 (multiple-value-bind (token start end) (line-token-under-xy buffer mx my)
                   (when (and token start end)
                     (draw-token-highlight token start end))))
               (draw-line-buffer-line line)
               #+nil
               (doseq (s stickers)
                 (draw-sticker s))
               #+nil
               (draw:translate 0.0 sticker-line-height))
             (incf endline step)
             (incf startline step))))))

(defmethod draw-background ((node editor-node) (parent node))
  (let ((editor (editor-node-editor node)))
    (draw:with-graphics-group (d)
      (if (.focused node)
          (display-set-fill d 0.99 0.99 0.95 1.0)
          (display-set-fill d 0.89 0.89 0.85 1.0))
      (multiple-value-bind (w h) (node-layout-size node)
        (draw:box 0.0 0.0 w h)
        (draw-cursor editor d)
        (draw:rgba-fill 0.3 0.2 0.8 1.0)
        (draw:set-monospace)
        (draw:set-font-size d (font-size *font*))
        (editor-draw-lines editor 0.0 (.scroll-offset node) w h)))))
