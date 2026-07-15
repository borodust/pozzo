(cl:in-package #:pozzo)

;;;
;;; RESOURCE
;;;
(defgeneric load-resource-variant (resource uninitialized-variant-ptr))


;;;
;;; REGISTRY
;;;
(defclass resource-registry ()
  ((resource-table :initform (make-hash-table :test 'equal))))


(defun make-resource-registry ()
  (make-instance 'resource-registry))


(defun register-resource (registry path resource)
  (with-slots (resource-table) registry
    (a:when-let ((existing-resource (gethash path resource-table)))
      (error "Resource already registered at ~A: ~A" path existing-resource))
    (setf (gethash path resource-table) resource)))


(defun find-resource (registry path)
  (with-slots (resource-table) registry
    (gethash path resource-table)))
