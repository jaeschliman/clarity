(defpackage :coffee.umbrella.clarity.connect
  (:use :cl :alexandria :trivia
        :bordeaux-threads
        :trivial-garbage
        :coffee.umbrella.utils
        :coffee.umbrella.model
        :coffee.umbrella.dot-access.symbols)
  (:local-nicknames
   (:mbox :coffee.umbrella.mailbox)
   (:datastream :coffee.umbrella.clarity.datastream))
  (:export
   #:ensure-global-device-server
   #:attach-host
   #:request-autoconnect
   #:detach-host
   #:create-simple-device-host))
