(defpackage :scratch.0
  (:use
   :cl :alexandria
   :coffee.umbrella.dot-access.symbols
   :coffee.umbrella.utils)
  (:local-nicknames
   (:app :coffee.umbrella.clarity.an-app)
   (:text-editor :coffee.umbrella.clarity.editors.text)
   (:node :coffee.umbrella.clarity.simple-scene-graph-0)))

(in-package :scratch.0)

(defun add-an-editor (world)
  (let ((n (text-editor::make-editor-node
             :on-string "Hello, world!"
             :package *package*)))
    (node:node-add-child world n)))

(app::restart-app! :new-world-hook 'add-an-editor)
