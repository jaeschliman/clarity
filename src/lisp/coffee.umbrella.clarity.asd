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
   (:file "vector")
   (:file "string")
   (:file "model-0")
   (:file "model-1")
   (:file "datastream")
   (:file "font")
   (:file "display-0")
   (:file "display-1")
   (:file "draw")
   (:module "simple-scene-graph.0"
    :serial t
    :components
    ((:file "package")
     (:file "node")
     (:file "layout")))))
