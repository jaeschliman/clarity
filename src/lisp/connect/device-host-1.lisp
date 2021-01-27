(in-package :coffee.umbrella.clarity.connect)

(define-metadata device-host-extension server
  attached-hosts
  autoconnect-hosts
  requested-device-list
  (hosts-to-devices (make-hash-table :test 'eq))
  (devices-to-hosts (make-hash-table :test 'eq)))

(defun reset-device-server ()
  (clear-device-host-extension))

(defgeneric host-connect (host port datastream))
(defgeneric host-disconnect (host port datastream))
(defgeneric host-port-state (host))

(defmethod host-connect (host port datastream)
  (declare (ignorable datastream))
  (error "You must implement host-connect for host and port: ~A ~A" host port))
(defmethod host-disconnect (host port datastream)
  (declare (ignorable datastream))
  (error "You must implement host-disconnect for host and port: ~A ~A" host port))

;; need to be able to list ports along with their state (:open :inuse) and device type
;;
;; -> list of (port port-state device-type datastream)
;;    where port-state is :open | :inuse
;;    and datastream should be nil if port-state is :open
(defmethod host-port-state (host)
  (error "You must implement host-port-state for host ~A~%" host))

(defun %host-port-state-for-device (host device)
  (let ((stream (device-datastream device)))
    (find stream (host-port-state host) :key 'fourth)))

;; ensures that we have an active request for this device type if needed
(defun %ensure-autoconnect-listener (server device-type)
  (progn
    (format t "[Device Host] requesting device of type ~A~%" device-type)
    (request-device server (device-host-extension server) device-type)
    (pushnew device-type (server-requested-device-list server))))

(defun %host-available-port-for-device (server host device)
  (declare (ignore server))
  (loop for (port state device-type) in (host-port-state host) do
       (when (and (eq state :open)
                  (eq device-type device))
         (return port))))

(defun %host-wants-device? (server host device)
  (not (null (%host-available-port-for-device server host device))))

(defun %server-attach-available-device (server user device-type stream device)
  (when-let (host
           (find-if (lambda (host) (%host-wants-device? server host device-type))
                    (server-autoconnect-hosts server)))
    (prog1 t
      (host-connect host
                    (%host-available-port-for-device server host device-type)
                    stream)
      (push device (gethash host (server-hosts-to-devices server)))
      (setf (gethash device (server-devices-to-hosts server)) host)
      (use-device device user))))

(defun %server-wants-device? (server device-type)
  (loop for host in (server-autoconnect-hosts server) thereis
       (%host-wants-device? server host device-type)))

(defmethod request-autoconnect (host server)
  (unless (member host (server-autoconnect-hosts server))
    (push host (server-autoconnect-hosts server))
    (format t "[Device Host] setting up host for autoconnect~%")
    (loop for (nil state device-type nil) in (host-port-state host) do
         (when (eq state :open)
           (%ensure-autoconnect-listener server device-type)))))

(defmethod rescind-autoconnect (host server)
  (when (member host (server-autoconnect-hosts host))
    (deletef (server-autoconnect-hosts server) host)
    (format t "turning off autoconnect for host~%")))

(defmethod device-ready ((wrapper device-host-extension) device-type stream device)
  (let ((server (device-host-extension-value wrapper)))
    (deletef (server-requested-device-list server) device-type)
    ;; we keep all lifecycle target at the server wrapper, so the 'user' is not
    ;; the host in this case, but the server itself.
    (if (%server-attach-available-device server wrapper device-type stream device)
        (progn
          (format t "server found user for device ~A~%" device-type)
          (when (%server-wants-device? server device-type)
            (format t "other users awaiting device of type ~A, initiating request~%" device-type)
            (%ensure-autoconnect-listener server device-type)))
        (format t "server found no use for device ~A, FIXME, return it~%" device-type))))

(defmethod device-disconnected ((wrapper device-host-extension) device-type stream device)
  (let ((server (device-host-extension-value wrapper)))
    (if-let (host (gethash device (server-devices-to-hosts server)))
      (progn
        (format t "device of type ~S disconnected~%" device-type)
        (setf (gethash device (server-devices-to-hosts server)) nil)
        (deletef (gethash host (server-hosts-to-devices server)) device)
        ;; now we need to find what port this device was on and inform that it is disconnected
        (if-let (port (%host-port-state-for-device host device))
          (progn
            (format t "disconnecting device from port: ~S~%" (car port))
            (host-disconnect host (car port) stream))
          (format t "could not find active port for disconnecting device.~%"))
        (when (member host (server-autoconnect-hosts server))
          (format t "host is in autoconnect list, ensuring request to reconnect.~%")
          (%ensure-autoconnect-listener server device-type)))
      (format t "could not find host for disconnected device.~%"))))


(defmethod attach-host (host server)
  (prog1 t
    (pushnew host (server-attached-hosts server))))
(defmethod detach-host (host server)
  (when (find host (server-attached-hosts server))
    (doseq (device (gethash host (server-hosts-to-devices server)))
      (release-device device))
    (deletef (server-autoconnect-hosts server) host)
    (deletef (server-attached-hosts server) host)
    t))

;; ---------------------------------------------------------------

(defstruct %sdh
  port-states
  context
  on-connect
  on-disconnect)

(defun %host-port-state-for-port (host port)
  (find port (host-port-state host) :key 'first))

(defmethod host-connect ((host %sdh) port datastream)
  (funcall (%sdh-on-connect host)
           (%sdh-context host)
           port
           datastream)
  (setf (second (%host-port-state-for-port host port)) :inuse
        (fourth (%host-port-state-for-port host port)) datastream)
  t)

(defmethod host-disconnect ((host %sdh) port datastream)
  (funcall (%sdh-on-disconnect host)
           (%sdh-context host)
           port
           datastream)
  (setf (second (%host-port-state-for-port host port)) :open)
  t)

(defmethod host-port-state ((host %sdh))
  ;; for simplicity's sake we just keep a list and mutate it
  (%sdh-port-states host))


(defun create-simple-device-host (&key attach-to port-descriptions on-port-connect on-port-disconnect)
  "Creates a simple device host suitable for most uses, and does the
internal bookeeping required to manage port connection state and so
on. Note that you must still manage connecting the host to a device
server and request autoconnect if you want it, etc.

  attach-to = a context object of your choosing, provided in the callbacks
  port-descriptions = list of (port-name, device-type)
  on-port-connect = fn of (context-object port-name datastream) -> void
  on-port-disconnect = fn of (context-object port-name datastream) -> void

returns newly created device host. the type of the simple device host
is internal.
"
  (loop for (port-name device-type) in port-descriptions
     collect (list port-name :open device-type nil) into states
     finally (return
               (make-%sdh :port-states states :context attach-to
                          :on-connect on-port-connect :on-disconnect on-port-disconnect))))
