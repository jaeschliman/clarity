(defpackage :coffee.umbrella.clarity.display
  (:use :cl :alexandria
        :coffee.umbrella.model :coffee.umbrella.utils)
  (:shadow #:rotate)
  (:local-nicknames
   (:datastream :coffee.umbrella.clarity.datastream)
   (:affine :coffee.umbrella.affine))
  (:export
   #:*display*
   #:with-group
   #:with-clip-group
   #:translate
   #:rotate
   #:scale
   #:set-fill
   #:clear-screen
   #:set-font-size
   #:set-monospace
   #:set-variable-width
   #:poly
   #:qpoly
   #:reserve
   #:text
   #:write-buffer))
(in-package :coffee.umbrella.clarity.display)

;; the 'display' package
;;
;; used by the drawing package (not a user package)
;;
;; encodes drawing commands into a bytecode format, which is then interpreted
;; by visual display as it sees fit
;;
;; currently only a small number of drawing commands are supported,
;; (e.g. filling a poly, but not a poly-line)
;; adding more is easy, but not the current priority
;;
;; TODO: move models to define-model

(defvar *display* nil)

(defmethod connect-display (display output) t)

(defstruct (display (:constructor %make-display))
  (write-count 0 :type (unsigned-byte 64))
  (output nil)
  (pending (list nil))
  (flush-enqueued nil))

(defun make-display (&key output)
  (let ((display (%make-display :output output)))
    (prog1 display
      (connect-display display output))))

(defmethod print-object ((d display) stream)
  (format stream "[display]"))

;; TODO: should no longer need 32->8 stream, as we should be writing directly to
;; the in-memory stream
;;
;; TODO: look into replacing this use of gray streams with fast-io package.
;; ---------------------------------------------------------------
;; little endian 32 -> 8 stream
;;
(defclass u32->u8-stream (trivial-gray-streams:fundamental-binary-output-stream)
  ((stream :initform (flex:make-in-memory-output-stream :element-type '(unsigned-byte 8)))))

(defmethod trivial-gray-streams:stream-write-byte ((s u32->u8-stream) integer)
  (with-slots (stream) s
    (write-byte (ldb (byte 8 0) integer)  stream)
    (write-byte (ldb (byte 8 8) integer)  stream)
    (write-byte (ldb (byte 8 16) integer) stream)
    (write-byte (ldb (byte 8 24) integer) stream)))

;; ---------------------------------------------------------------
;; rendercode display

(defstruct (rendercode-display (:include display) (:constructor %make-rendercode-display))
  (codestream (make-instance 'u32->u8-stream))
  (clip-next nil))

(defmethod print-object ((rd rendercode-display) stream)
  (write-string "[rendercode display]" stream))

(defun make-rendercode-display (&key output)
  (let ((display (%make-rendercode-display :output output)))
    (prog1 display
      (connect-display display output))))

(defmethod send-output ((rd rendercode-display) output (ds datastream:datastream))
  (datastream:send output ds))

(defmethod flush ((rd rendercode-display))
  (with-slots (stream) (rendercode-display-codestream rd)
    (let ((output (slot-value stream 'vector)))
      (when (> (length output) 0)
        (send-output rd output (display-output rd))
        ;; would be nice to pool these (have send 'return' the buffer when done)
        (setf (slot-value stream 'vector)
              (make-array 0 :element-type '(unsigned-byte 8)
                            :initial-element 0
                            :fill-pointer 0
                            :adjustable t))
        (display-output rd)))))

;; append rendercode in buffer to this display
(defun write-buffer (rd buffer)
  (declare (optimize speed (debug 1) (space 0) (compilation-speed 0))
           (type (array (unsigned-byte 8)) buffer))
  (let* ((out (rendercode-display-codestream rd))
         (stream (slot-value out 'stream))
         (buffer2 (slot-value stream 'vector)))
    (declare (type (array (unsigned-byte 8)) buffer2))
    (let ((len (length buffer2))
          (sum (+ (length buffer) (length buffer2)) ))
      (if (< (array-dimension buffer2 0) sum)
          (adjust-array buffer2 (* 1 sum) :fill-pointer sum)
          (setf (fill-pointer buffer2) sum))
      (replace buffer2 buffer :start1 len))
    (values)))

(defstruct captured-output)

(defmacro with-group ((var) &body body)
  (once-only (var)
    `(unwind-protect
          (progn (start-group ,var)
                 ,@body)
       (end-group ,var))))

(defmacro with-clip-group ((var) &body body)
  (once-only (var)
    `(unwind-protect
          (progn (start-clip-group ,var)
                 ,@body)
       (end-group ,var))))

(defvar *captured-output-display (make-rendercode-display :output (make-captured-output)))

(defun %call-capturing-output (buffer thunk)
  (let* ((*display* *captured-output-display)
         (out (rendercode-display-codestream *display*))
         (stream (slot-value out 'stream)))
    (setf (slot-value stream 'vector) buffer)
    (unwind-protect (prog1 buffer (funcall thunk))
      (setf (slot-value stream 'vector) nil))))

(defmacro with-output-to-buffer ((var) &body body)
  (with-gensyms (doit)
    `(labels ((,doit () ,@body))
      (declare (dynamic-extent #',doit))
      (%call-capturing-output ,var #',doit))))

(declaim (inline rendercode-thing))
(defun rendercode-thing (it)
  (declare (optimize (speed 3) (safety 1)))
  (etypecase it
    (single-float (ieee-floats:encode-float32 it))
    (fixnum       (ldb (byte 32 0) it))))

(defmacro defrendercode (name code (&rest params) &body body)
    `(defun ,name (%display ,@params)
       (declare (optimize (speed 3) (safety 1)))
       ,@(when-let (n (member '&rest params))
           `((declare (dynamic-extent ,(cadr n)))))
       (let* ((%out (rendercode-display-codestream %display))
              (%out8 (slot-value %out 'stream))
              (clip-mode (rendercode-display-clip-next %display)))
         (declare (ignorable %out8))
         (macrolet ((<<8 (v) `(write-byte ,v %out8))
                    (<<32 (v) `(let ((b ,v))
                                (<<8 (ldb (byte 8 0)  b))
                                (<<8 (ldb (byte 8 8)  b))
                                (<<8 (ldb (byte 8 16) b))
                                (<<8 (ldb (byte 8 24) b))))
                    (<<1 (v) `(<<32 ,(if (numberp v) (rendercode-thing v) `(rendercode-thing ,v))))
                    (<< (&rest vs) `(progn ,@(loop for v2 in vs collect `(<<1 ,v2))))
                    (<<<< (vs) `(progn (<< (length ,vs)) (doseq (v1 ,vs) (<< v1)))))
           (if clip-mode
               (progn
                 (setf (rendercode-display-clip-next %display) nil)
                 (assert (or (eq ,code instr.fillpoly)
                             (eq ,code instr.fillqpoly))))
               (<< ,code))
           ,@body))))
