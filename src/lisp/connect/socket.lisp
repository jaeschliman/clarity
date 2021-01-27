(in-package :coffee.umbrella.clarity.connect)

(defgeneric stop (thing))
(defgeneric start (thing))
(defgeneric restart! (thing))

(define-model server ()
  ((ip "127.0.0.1" :type string)
   (port 1111 :type (unsigned-byte 16))
   (next-connection-id 0)
   (connections nil :type list)
   (state :initial :type keyword)
   (socket nil)
   (accept-thread nil)))

(define-model connection ()
  (format ;; eventually we'll support more than one wire format.
   (id -1 :type fixnum)
   (socket nil)
   (server nil)
   (stream nil)
   (write-stream nil)
   (state :initial)
   (read-thread nil)
   (write-thread nil)
   (next-datastream-tag 0)
   (datastreams nil)
   (mbox (make-instance 'mbox:mbox))))

;; one connection may have multiple 'datastreams',
;; each of which is identified by a tag used to wrap/unwrap
;; data sent in an 'envelope'
;; `root-datastream-proxy' isn't the best name, really...
(define-model root-datastream-proxy ()
  ((datastream nil)
   (identifier nil)
   (connection nil)))

(defmethod restart! ((self server))
  (format t "restarting server ~A, you're welcome.~%" self)
  (stop self)
  (start self))

(defun run-write-loop (self)
  (block write-loop
    (loop with stream = (connection-write-stream self)
          while (eq :open (connection-state self))
          with mbox = (.mbox self)
          for writes = (mbox:receive mbox)
          do (doseq (it writes)
               (buffer-write it stream))
             (force-output stream))))

(defun run-read-loop (self)
  (block read-loop
    (loop
       with stream = (connection-stream self)
       while (eq :open (connection-state self))
       do (receive (buffer-read stream) self))))

(defmethod start ((self connection))
  (assert (eq :ready (connection-state self)))
  (let ((output *standard-output*))
    (setf (connection-state self) :handshake
          (connection-read-thread self)
          (make-thread
           (lambda ()
             (let ((*standard-output* output)
                   (stream (connection-stream self))
                   (socket (connection-socket self))
                   (ended-well nil))
               (handler-bind ((error (lambda (e)
                                       (format t "unexpected error in socket loop (case -2): ~A, aborting~%" e)
                                       (abort))))
                 (unwind-protect
                      (progn
                        (write-line "exchanging pleasantries")
                        ;; send some test data at connection open,
                        ;; these values are expected on the other side.
                        (buffer-write '("hi!" 123.0 -77.8) stream)
                        (force-output stream)

                        (usocket:wait-for-input socket)

                        (write-line "client ready for input.")
                        (let ((greeting (buffer-read stream)))
                          (format t "read returned greeting: ~S~%" greeting))
                        (setf (connection-state self) :open)
                        (server-add-connection (connection-server self) self)
                        (write-line "starting write thread")
                        (setf (connection-write-thread self)
                              (make-thread
                               (lambda () (run-write-loop self))
                               :name "connection write"))
                        (write-line "entering read loop")
                        (run-read-loop self))
                   ;; unwind-protect end clause
                   (unless ended-well
                     (write-line "Something unexpected happened (close case 2)")
                     (stop self))
                   (unless (eq :closed (connection-state self))
                     (write-line "connection unexpectedly not yet closed on loop exit")
                     (stop self))))))
           :name "connection read"))))

(defmethod server-add-connection ((server server) (conn connection))
  (appendf (server-connections server) (list conn))
  (assert (eq conn (last-elt (server-connections server))))
  (format t "client connected ~A ~%" (connection-id conn))
  (format t "listing connection ids:~%")
  (doseq (conn (server-connections server))
    (format t "   ~A~%" (connection-id conn))))

(defmethod server-remove-connection ((server server) (conn connection))
  (deletef (server-connections server) conn)
  (assert (null (member conn (server-connections server)))))

(defmethod stop ((self connection))
  (setf (connection-state self) :closing)
  (when-let (socket (connection-socket self))
    (ignore-errors
     (usocket:socket-close socket))
    (setf (connection-socket self) nil))
  (when-let (thread (connection-read-thread self))
    (unless (eq thread (current-thread))
      (ignore-errors
       (destroy-thread thread))
      (setf (connection-read-thread self) nil)))
  (when-let (thread (connection-write-thread self))
    (unless (eq thread (current-thread))
      (ignore-errors
       (destroy-thread thread))
      (setf (connection-write-thread self) nil)))
  (map nil (lambda (pair) (stop (cdr pair))) (connection-datastreams self))
  (when (and (connection-server self)
             (member self (server-connections (connection-server self))))
    (server-remove-connection (connection-server self) self))
  (setf (connection-server self) nil)
  (setf (connection-state self) :closed))

(defmethod stop ((self root-datastream-proxy))
  (format t "stopping root datastream proxy.~%")
  (datastream-signal-termination (root-datastream-proxy-datastream self)))

(defmethod accept-client ((server server) client-socket)
  ;; make a connection, start its listen loop, add to server
  (let* ((ended-well nil))
    (unwind-protect
         (let* ((stream (flex:make-flexi-stream (usocket:socket-stream client-socket)
                                                :element-type 'flex:octet))
                (write-stream (flex:make-flexi-stream (usocket:socket-stream client-socket)
                                                      :element-type 'flex:octet))
                (id (server-next-connection-id server))
                (connection (make-instance 'connection
                             :id id
                             :state :initial
                             :server server
                             :stream stream
                             :write-stream write-stream
                             :socket client-socket)))
           (incf (server-next-connection-id server))
           (setf (connection-state connection) :ready)
           (start connection)
           (setf ended-well t))
      (unless ended-well
        (format t "some issue accepting a client, just restarting the whole thing~%")
        ;; for ease of development -- this will change
        (restart! server)))))

(defmethod start ((self server))
  (assert (eq :initial (server-state self)))
  (let ((ok nil))
    (unwind-protect
         (let ((ip (server-ip self))
               (port (server-port self)))
           (setf (server-socket self) (usocket:socket-listen
                                       ip port :reuse-address t :element-type 'flex-octet)
                 (server-state self) :listen)
           (let ((output *standard-output*))
             (setf (server-accept-thread self)
                   (make-thread
                    (lambda () (let ((*standard-output* output))
                                 (loop while (eq :listen (server-state self)) do
                                      (accept-client self
                                                     (usocket:socket-accept
                                                      (server-socket self)
                                                      :element-type 'flex:octet)))))
                    :name "server accept")))
           (setf ok t))
      (unless ok
        (stop self)))))


(defmethod send (object (proxy root-datastream-proxy))
  (send (make-envelope :tag (root-datastream-proxy-identifier proxy)
                       :content object)
        (root-datastream-proxy-connection proxy)))

(defmethod receive ((env envelope) (proxy root-datastream-proxy))
  (receive (envelope-content env) (root-datastream-proxy-datastream proxy)))

(defmethod request-datastream ((conn connection))
  (let* ((tag (connection-next-datastream-tag conn))
         (datastream (make-datastream))
         (proxy (make-instance 'root-datastream-proxy :datastream datastream
                                                      :identifier tag
                                                      :connection conn)))
    (format t "creating datastream with tag ~A for connection with id ~A~%"
            tag (connection-id conn))
    (incf (connection-next-datastream-tag conn))
    (setf (datastream-parent-stream datastream) proxy)
    (push (cons tag proxy) (connection-datastreams conn))
    (values datastream tag)))

(defmethod send (object (conn connection))
  (mbox:send (.mbox conn) object))

(defmethod receive (object (con connection))
  (typecase object
    (envelope
     (let* ((tag (envelope-tag object))
            (target? (find tag (connection-datastreams con) :key 'car)))
       (if-let (datastream (cdr target?))
         (recieve object datastream)
         (format t "wrongly addressed envelope: ~S~%" object))))
    (t
     (format t "unaddressed message: ~S~%" object))))
