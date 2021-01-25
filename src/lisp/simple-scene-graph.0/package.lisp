(defpackage :coffee.umbrella.clarity.simple-scene-graph-0
  (:use
   :cl :alexandria :trivia :trivial-garbage
   :coffee.umbrella.dot-access.symbols
   :coffee.umbrella.dot-access
   :coffee.umbrella.utils
   :coffee.umbrella.model
   :coffee.umbrella.clarity.font)
  (:import-from :coffee.umbrella.clarity.display #:*display*)
  (:local-nicknames
   (:affine :coffee.umbrella.affine)
   (:vector :coffee.umbrella.vector)
   (:display :coffee.umbrella.clarity.display)
   (:draw :coffee.umbrella.clarity.draw)
   ))
