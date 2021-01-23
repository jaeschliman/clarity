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
  :components ((:file "dot-access")))
