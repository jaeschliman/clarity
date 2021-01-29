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
   )
  (:export
   #:*current-display-device*
   #:*display*
   #:with-active-world
   #:device.display
   #:device.keyboard
   #:world-active-simulation-count
   #:world-dispatch-event
   #:world-simulate
   #:*world*
   #:world-start-frame
   #:world-end-frame
   #:draw-world
   #:force-redraw
   #:world-make-index-for-child
   #:modifier-state-reset-for-disconnect
   #:common-node
   #:node
   #:update
   #:node-add-child))
