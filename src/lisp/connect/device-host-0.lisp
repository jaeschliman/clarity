(in-package :coffee.umbrella.clarity.connect)

(defmacro define-metadata (extension-name target-class &body structure-slots)
  (let* ((struct-name (symbolicate '%% target-class '--metadata-- extension-name))
         (constructor-name (symbolicate 'make- struct-name))
         (accessor-names (loop for slot in structure-slots
                            collect (list
                                     (symbolicate target-class '- (ensure-car slot))
                                     (symbolicate struct-name '- (ensure-car slot)))))
         (table-name (symbolicate '* struct-name '--table*))
         (relation-table-name (symbolicate '* struct-name '--relation-table*))
         (relation-constructor (symbolicate 'make- extension-name)))
    `(progn
       (defun ,(symbolicate 'clear- extension-name) ()
         (clrhash ,relation-table-name))
       (defstruct ,struct-name ,@structure-slots)
       ;; uses the extension name to create a struct that wraps the target class
       ;; so that methods may be defined on on the metadata extension directly.
       (defparameter ,relation-table-name (make-weak-hash-table :test 'eq :weakness :key))
       (defstruct ,extension-name value) ;; not intended to be constructed directly.
       (defun ,extension-name (target-class)
         (ensure-gethash target-class ,relation-table-name
                         (,relation-constructor :value target-class)))
       ;; wipes out existing state if we change the definition
       (defparameter ,table-name (make-weak-hash-table :test 'eq :weakness :key))
       ;; reuse the struct name as a function to access associated state
       (defun ,struct-name (target)
         (ensure-gethash target ,table-name (,constructor-name)))
       ,@(loop for (external-accessor internal-accessor) in accessor-names appending
              `(
                (defun ,external-accessor (,target-class)
                  (,internal-accessor (,struct-name ,target-class)))
                (defun (setf ,external-accessor) (new-value ,target-class)
                  (setf (,internal-accessor (,struct-name ,target-class)) new-value)))))))
