(cl:in-package #:pozzo)


(defvar *module-registry* (make-hash-table :test 'eq))


(defun register-module (name)
  (when (gethash name *module-registry*)
    (error "Module ~A already registered" name))
  (setf (gethash name *module-registry*) name)
  (values))


(defgeneric make-module (name &key &allow-other-keys)
  (:documentation "No thread guarantee, can be loaded parallel with other modules, engine startup"))

(defgeneric initialize-module (module-instance pozzo)
  (:documentation "Called in main thread"))

(defgeneric release-module (module-instance)
  (:documentation "Called in main thread"))

(defgeneric iterate-module (module-instance)
  (:documentation "Called in main thread each iteration"))
