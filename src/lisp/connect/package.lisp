(defpackage :coffee.umbrella.clarity.connect
  (:use :cl :alexandria
        :bordeaux-threads
        :trivial-garbage
        :coffee.umbrella.utils
        :coffee.umbrella.model)
  (:local-nicknames
   (:mbox :coffee.umbrella.mailbox)))
