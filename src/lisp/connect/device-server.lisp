(in-package :coffee.umbrella.clarity.connect)

;; TODO: move to define-model, clean up
(defstruct device
  id
  type
  description
  inuse?
  datastream
  connection ;; for debugging, do not write to this
  (state :initial)
  ;; if a device is in use, it should have a user
  user)

(defmethod print-object ((self device) stream)
  (format stream "[device id:~A type:~S state:~S]"
          (device-id self)
          (device-type self)
          (device-state self)))

(defstruct device-registry
  (available-devices nil)
  (inuse-devices nil) ;; not used yet
  (pending-requests (make-hash-table :test 'eq)))

(define-model device-server (server)
  ((device-registry nil)))

(defvar *device-server nil)

(defun ensure-global-device-server (&key (ip "127.0.0.1") (port 1234))
  (unless *device-server
    (setf *device-server (make-instance 'device-server
                          :ip ip :port port
                          :device-registry (make-device-registry))))
  (unless (eq (server-state *device-server) :listen)
    (restart! *device-server))
  *device-server)

(defmethod device-disconnected (requestor type datastream device)
  (declare (ignorable datastream device))
  (error "~A requested device of type ~A but has not defined a method on device-disconnected"
         requestor type))

(defmethod device-inform-disconnection ((device device))
  (format t "device has disconnected, informing user if any~%.")
  (when (device-inuse? device)
    (device-disconnected (device-user device)
                         (device-type device)
                         (device-datastream device)
                         device)))

(defmethod ensure-device-initialized ((device device))
  (when (eq :initial (device-state device))
    (setf (device-state device) :opening)
    (send `(:open-device ,(device-id device)) (device-datastream device))
    (add-listener (lambda (stream stream-state context event)
                    (declare (ignore stream context event))
                    (when (eq stream-state :closed)
                      (device-inform-disconnection device)))
                  nil (device-datastream device))))

(defmethod device-ready (requestor type datastream device)
  (declare (ignorable datastream device))
  (error "~A requested device of type ~A but has not defined a method on device-ready"
         requestor type))

(defmethod use-device ((device device) user)
  (assert (not (device-inuse? device)))
  (setf (device-inuse? device) t
        (device-user device) user))

(defmethod release-device ((device device))
  (assert (device-inuse? device))
  (setf (device-inuse? device) nil)
  (device-disconnected (device-user device)
                       (device-type device)
                       (device-datastream device)
                       device))

(defmethod device-server-dequeue-requestor ((server device-server) (device device))
  (pop (gethash
        (device-type device)
        (device-registry-pending-requests (device-server-device-registry server)))))

(defmethod device-server-try-assign-device ((server device-server) (device device))
  (unless (device-inuse? device)
    (if-let (requestor (device-server-dequeue-requestor server device))
      (progn
        (ensure-device-initialized device)
        (format t "found requestor for device: ~A~%" device)
        (add-listener (lambda (output state data event)
                        (declare (ignore output data state))
                        (match event
                          (:device-ready
                           (setf (device-state device) :open)
                           (loop do
                             (device-ready
                              requestor (device-type device) (device-datastream device) device)
                             (when (device-inuse? device) (stop-listening))
                             (format t "requestor did not use device, looking for another~%")
                             (setf requestor (device-server-dequeue-requestor server device))
                             (unless requestor
                               (format t "ran out of requestors for device.~%")
                               (stop-listening))))
                          (x (format t "unexpected device reply: ~S~%" x))))
                      nil (device-datastream device)))
      (format t "unable to find requestor for device: ~A~%" device))))

(defmethod device-server-add-device ((server device-server) (device device))
  (format t "new device available. ~A~%" device)
  (push device (device-registry-available-devices (device-server-device-registry server)))
  (send-notification server :device-added device)
  (device-server-try-assign-device server device))

(defmethod device-server-remove-all-devices-for-connection ((server device-server) (conn connection))
  (deletef (device-registry-available-devices (device-server-device-registry server))
           conn :key 'device-connection))

(defmethod parse-devices (device-list (server device-server) (conn connection))
  (let ((found nil))
    (doseq (desc device-list)
      (match desc
        ((list :id id :type type :description description)
         (push (make-device :id id :type type :description description
                            :datastream (request-datastream conn)
                            :connection conn)
               found))))
    (format t "found devices: ~A~%" (length found))
    (doseq (dev found) (device-server-add-device server dev))))

(defmethod server-add-connection :after ((server device-server) (conn connection))
  ;; query the connection for capabilities
  (let ((ds (request-datastream conn))) ;; this better have tag 0!
    (datastream:send :list-devices ds)
    (add-listener (lambda (output state data event)
                    (declare (ignore output state data))
                    (match event
                      ((list :devices device-list)
                       (parse-devices device-list server conn))))
                  nil ds)))

(defmethod server-remove-connection :after ((server device-server) (conn connection))
  (device-server-remove-all-devices-for-connection server conn))

;; FIXME: doesn't handle the case of device already connected.
(defmethod request-device ((server device-server) requestor device-type)
  (appendf
   (gethash device-type
            (device-registry-pending-requests (device-server-device-registry server)))
   (list requestor)))

;; FIXME: need a lock here
(defmethod rescind-all-device-requests ((server device-server) requestor)
  (let* ((reg (device-server-device-registry server))
         (pend (device-registry-pending-requests reg))
         (new-pend (make-weak-hash-table :test 'eq :weakness :key)))
    (maphash (lambda (k v)
               (setf (gethash k new-pend) (remove requestor v)))
             pend)
    (setf (device-registry-pending-requests reg) new-pend)
    t))
