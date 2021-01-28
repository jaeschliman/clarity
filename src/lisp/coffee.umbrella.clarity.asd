(asdf:defsystem :coffee.umbrella.clarity
  :version "0.0.0"
  :author "Jason Aeschliman <j.aeschliman@gmail.com>"
  :licence "Public Domain"
  :depends-on ("alexandria"
               "bordeaux-threads"
               "flexi-streams"
               "lisp-binary"
               "closer-mop"
               "usocket"
               "ieee-floats"
               "trivia"
               "trivial-garbage"
               "trivial-package-local-nicknames"
               "swank")
  :serial t
  :components
  ((:file "dot-access")
   (:file "affine-0")
   (:file "affine-1")
   (:file "utils-0")
   (:file "utils-1")
   (:file "vector")
   (:file "string")
   (:file "model-0")
   (:file "model-1")
   (:file "datastream")
   (:file "mailbox")
   (:file "font")
   (:file "display-0")
   (:file "display-1")
   (:file "draw")
   (:module "simple-scene-graph.0"
    :serial t
    :components
    ((:file "package")
     (:file "node")
     (:file "layout")
     (:file "device")
     (:file "world")
     (:file "event-handling")
     (:file "misc")))
   (:module "connect"
    :serial t
    :components
    ((:file "package")
     (:file "code-0")
     (:file "code-1")
     (:file "socket")
     (:file "device-host-0")
     (:file "device-host-1")))))
