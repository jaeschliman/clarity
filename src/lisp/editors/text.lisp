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

(defun make-editor-node (&key on-string
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
                                :package package)))
    (when evaluator (setf (.evaluator editor) evaluator))
    (setf (.editor node) editor)
    (have editor node:update node)
    (have buffer node:update node)
    node))

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
        (setf (.pos n) (cons x (yof n))))))
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

(defmethod node:draw-background ((n poplist-node) (p node:node))
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

(defmethod editor-insert ((editor editor) (text string))
  (insert-text (.text editor) text
               (.cursor-x editor)
               (.cursor-y editor))
  ;; there should be a method that accepts a string, rather than char
  (map nil (lambda (ch) (editor-completions-append-character editor ch)) text)
  (prog1 (incf (.cursor-x editor) (length text))
    (editor-try-activate-completion editor)))

(defun move-cursor (editor dir)
  (case dir
    (:left  (decf (.cursor-x editor)))
    (:right (incf (.cursor-x editor)))
    (:up    (decf (.cursor-y editor)))
    (:down  (incf (.cursor-y editor)))))

(defmethod editor-row-col-for-charactor-position ((editor editor) (position fixnum))
  (let ((buffer (.text editor)))
    (check-type buffer line-buffer)
    (let ((lines (.lines buffer)))
      (loop
         with col = position
         for row upfrom 0
         for line in lines
         ;; 1+ for the invisible newline
         for len = (1+ (length line)) do
           (if (< col len) (return (list row col))
               (decf col len))))))

;; ---------------------------------------------------------------
;;
;; gesture/event handling

(defvar *editor-click-token-hook*)

(defun handle-string-key (editor key flags)
  (cond ((member :control flags)
         (match key
           ("n" (move-cursor editor :down))
           ("p" (move-cursor editor :up))
           ("b" (move-cursor editor :left))
           ("f" (move-cursor editor :right))
           ("a" (setf (.cursor-x editor) 0))
           ("e" (setf (.cursor-x editor)
                      (line-length (.text editor) (.cursor-y editor))))))
        ((member :option flags)
         (match key
           ("/" (editor-toggle-completions editor))
           ("n" (editor-next-completion editor))
           ("p" (editor-prev-completion editor))))
        (t (editor-insert editor key))))

(defmethod handle-keydown ((editor editor) event)
  (let ((key (.keyname event))
        (flags (.flags event)))
    (if (stringp key) ;; should also confirm the text is not like a control character or smth
        (handle-string-key editor key flags)
        (if (member :command flags)
            ;; holding off on porting the evaluation code for the moment, it is rather messy
            nil #+nil
            (case key
              (:return
               (if (member :shift flags)
                   (editor-evaluate-buffer-contents editor (editor-string-contents editor) 0)
                   (multiple-value-bind (pos str) (find-toplevel-form editor)
                     (editor-evaluate-buffer-contents editor str pos)))))
            (let ((should-dismiss-completions t))
              (case key
                ((:left :right :up :down) (move-cursor editor key))
                #+nil
                (:tab
                 (if (editor-showing-completions? editor)
                     (editor-completions-accept-current editor)
                     (doseq (sticker (stickers-at-point editor (editor-stickers editor)
                                                        (editor-cursor-x editor)
                                                        (editor-cursor-y editor)))
                       (setf (sticker-visible sticker)
                             (not (sticker-visible sticker))))))
                (:return
                 (split-line (.text editor)
                             (.cursor-x editor)
                             (.cursor-y editor))
                 (setf (.cursor-x editor) 0)
                 (incf (.cursor-y editor)))
                (:delete
                 (let* ((col (.cursor-x editor))
                        (row (.cursor-y editor)))
                   (cond
                     ;; do nothing at position zero
                     ((and (zerop row) (zerop col)))
                     ;; backward join line when flush left
                     ((zerop col)
                      (let* ((text (.text editor))
                             (prevlen (line-length text (1- row))))
                        (join-line text (1- row))
                        (decf (.cursor-y editor))
                        (setf (.cursor-x editor) prevlen)))
                     ;; otherwise delete backwards 1 char
                     ((plusp col)
                      (delete-text (.text editor) 1 (1- col) row)
                      (decf (.cursor-x editor))
                      (editor-completions-delete-character editor)
                      (setf should-dismiss-completions nil)))))
                (t (setf should-dismiss-completions nil)))
              (when should-dismiss-completions
                (editor-ensure-hide-completions editor)))))))

(defmethod node:handle-event ((enode editor-node) event)
  (when (and (typep event 'node:key-event)
             (eq :keydown (.type event)))
    (handle-keydown (.editor enode) event)))

(defmethod node:wants-gesture ((n editor-node) (p node:node) (g node:gesture.click))
  t)

(defmethod node:handle-gesture ((n editor-node) (p node:node) (g node:gesture.click))
  (node:focus! n)
  (when-let (hook *editor-click-token-hook*)
    (when-let (token (line-token-under-xy (.text (.editor n))
                                          (floor (.mouse-cursor-x (.editor n)))
                                          (floor (.mouse-cursor-y (.editor n)))))
      (let ((*package* (.package (.editor n))))
        (funcall hook (.x g) (.y g) token)))))

(defmethod node:wants-gesture ((n editor-node) (p node:node) (g node:gesture.scroll))
  t)

(defmethod node:handle-gesture ((n editor-node) (p node:node) (g node:gesture.scroll))
  (incf (.scroll-offset n) (.y g)))

(defmethod node:wants-gesture ((n editor-node) (p node:node) (g node:gesture.mouse-hover))
  t)

(defmethod node:handle-gesture ((n editor-node) (p node:node) (g node:gesture.mouse-hover))
  (let ((e (.editor n)))
    (if (eq :end (.phase g))
        (setf (.mouse-cursor-active e) nil)
        (multiple-value-bind (x y) (node:node-transform-from-world-space n (.x g) (.y g))
          (multiple-value-bind  (col row) (editor-col-row-for-x-y e x (- y (.scroll-offset n)))
            (setf (.mouse-cursor-active e) t
                  (.mouse-cursor-x e) col
                  (.mouse-cursor-y e) row))))))

;; ---------------------------------------------------------------
;;
;; layout

(defmethod node:node-layout-in-bounds ((n editor-node) x y w h)
  (setf (.x n) x (.y n) y (.width n) w (.height n) h))

(defmethod node:node-natural-size-in-layout ((n editor-node) layout w h)
  (values (.width n) (.height n) :expand :expand))

;; ---------------------------------------------------------------
;;
;; display

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
  (draw:moveby 0.0 (font:character-height #\Space)))

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
               (draw:moveby 0.0 sticker-line-height))
             (incf endline step)
             (incf startline step))))))

(defmethod node:draw-background ((node editor-node) (parent node:node))
  (let ((editor (editor-node-editor node)))
    (draw:with-graphics-group ()
      (if (.focused node)
          (draw:rgba-fill 0.99 0.99 0.95 1.0)
          (draw:rgba-fill 0.89 0.89 0.85 1.0))
      (multiple-value-bind (w h) (node:node-layout-size node)
        (draw:box 0.0 0.0 w h)
        (draw-cursor editor)
        (draw:rgba-fill 0.3 0.2 0.8 1.0)
        (draw:with-font (font:*font*)
          (editor-draw-lines editor 0.0 (.scroll-offset node) w h))))))
