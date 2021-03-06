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
   #:node-add-child
   #:draw-background
   #:node-layout-size
   #:key-event
   #:gesture.click
   #:focus!
   #:gesture.scroll
   #:gesture.mouse-hover
   #:node-transform-from-world-space
   #:node-layout-in-bounds
   #:node-natural-size-in-layout
   #:handle-event
   #:wants-gesture
   #:handle-gesture
   #:make-world
   #:modifier-state-update-for-key))
