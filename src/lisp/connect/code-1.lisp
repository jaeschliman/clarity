(in-package :coffee.umbrella.clarity.connect)

(define-header fixnum-header     0)
;; 1 unused at the moment
(define-header ascii-header      2)
(define-header keyword-header    3)
(define-header list-header       4)
(define-header struct-header     5)
(define-header envelope-header   6)
(define-header float32-header    7)
(define-header rendercode-header 8)

(defgeneric buffer-write (thing stream))

;; ---------------------------------------------------------------

(defun write-integer (value byte-count stream)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (type (integer 0 32) byte-count)
           (type fixnum value))
  (let ((buff (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (declare (dynamic-extent buff))
    (loop for i below byte-count do (setf (aref buff i) (ldb (byte 8 (* 8 i)) value)))
    (trivial-gray-streams:stream-write-sequence stream buff 0 byte-count)))

(defun read-integer (byte-count stream)
  (declare (optimize (speed 3) (debug 0)))
  (loop for i below byte-count with num = 0 do
    (setf (ldb (byte 8 (* i 8)) num) (read-byte stream))
        finally (return num)))

(define-compiler-macro read-integer (&whole form byte-count stream)
  (if (numberp byte-count)
      `(let ((stream ,stream)
             (value 0))
         (declare (type (unsigned-byte ,(* 8 byte-count)) value))
         ,@(loop for i below byte-count collect
                 `(setf (ldb (byte 8 ,(* i 8)) value) (read-byte stream)))
         value)
      form))

(defun write-bytes (bytes stream)
  ;; need to ensure it is a specialized array
  ;; on CCL, writing a vector of bytes triggers a bug somewhere that
  ;; falls back to the 'character' stream expectation of underlying streams somewhere.
  (let ((byte-vector (make-array (length bytes) :element-type 'flex:octet)))
    (map-into byte-vector 'identity bytes)
    (lisp-binary:write-bytes byte-vector stream)))

(defun read-bytes (byte-count stream)
  (multiple-value-bind (value bytes-read)
      (lisp-binary:read-bytes byte-count stream)
    (assert (= byte-count bytes-read))
    value))

(declaim (notinline buffer-read))
(defun buffer-read (stream)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let ((buff (make-array 1024 :fill-pointer 0)) ;; TODO: this needs to handle larger sizes
        (1st-64 (read-integer 8 stream))
        (2nd-64 (read-integer 8 stream)))
    (declare (dynamic-extent buff))
    (ecase 1st-64
      (#.hdr.fixnum-header (hdr->int 2nd-64))
      (#.hdr.ascii-header
       (let* ((count 2nd-64)
              (str (make-string count)))
         (setf (fill-pointer buff) count)
         (read-sequence buff stream  :start 0 :end count)
         (map-into str 'code-char buff)
         str))
      (#.hdr.keyword-header (intern (buffer-read stream) :keyword))
      (#.hdr.list-header (loop repeat 2nd-64 collect (buffer-read stream)))
      (#.hdr.struct-header (assert nil))
      (#.hdr.envelope-header (make-envelope :tag (hdr->int 2nd-64) :content (buffer-read stream)))
      (#.hdr.float32-header (hdr->f32 2nd-64))
      (#.hdr.rendercode-header (assert nil)))))

(defmethod write-header ((hdr tagged-header) stream)
  (write-integer (tagged-header-1st-64 hdr) 8 stream)
  (write-integer (tagged-header-2nd-64 hdr) 8 stream))

(defun int->hdr (int) (ldb (byte 64 0) int))
(defun hdr->int (bytes)
  (if (logbitp 63 bytes)
      (dpb bytes (byte 63 0) most-negative-fixnum)
      (dpb bytes (byte 63 0) 0)))

(defmethod buffer-write ((it fixnum) stream)
  (write-header (fixnum-header (int->hdr it)) stream))

(defun force-ascii-char-code (it)
  (min 255 (char-code it)))

(defmethod buffer-write ((it string) stream)
  (write-header (ascii-header (length it)) stream)
  (let ((bytes (map 'vector 'force-ascii-char-code it)))
    (write-bytes bytes stream)))

(defmethod buffer-write ((it symbol) stream)
  (assert (keywordp it))
  (write-header (keyword-header 0) stream)
  (buffer-write (symbol-name it) stream))

(defmethod buffer-write ((it list) stream)
  (assert it);; we don't want nil to write as a list
  (write-header (list-header (length it)) stream)
  (doseq (item it) (buffer-write item stream)))

(defmethod buffer-write ((it array) stream)
  (assert (equal '(unsigned-byte 8) (array-element-type it)))
  (write-header (rendercode-header (int->hdr (floor (length it) 4))) stream)
  (trivial-gray-streams:stream-write-sequence stream it 0 (length it)))

(defmethod buffer-write ((it envelope) stream)
  (write-header (envelope-header (int->hdr (envelope-tag it))) stream)
  (buffer-write (envelope-content it) stream))

(defun f32->hdr (it) (int->hdr (ieee-floats:encode-float32 it)))
(declaim (inline hdr->f32))
(defun hdr->f32 (it) (ieee-floats:decode-float32 (hdr->int it)))

(defmethod buffer-write ((it single-float) stream)
  (write-header (float32-header (f32->hdr it)) stream))
