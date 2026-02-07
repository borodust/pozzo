(cl:in-package #:pozzo)


(cffi:defcstruct pozzo-wrapper
  (g-object :pointer)
  (p-object :pointer))


(defun unwrap (ptr)
  (c-ref ptr (:struct pozzo-wrapper) :g-object))

(defun %get-pozzo-object (ptr)
  (c-ref ptr (:struct pozzo-wrapper) :p-object))


(defun make-pozzo-wrapper (godot-object pozzo-object)
  (with-slots (wrapper-registry) *pozzo*
    (let ((wrapper (memalloc '(:struct pozzo-wrapper))))
      (c-val ((wrapper (:struct pozzo-wrapper)))
        (setf (wrapper :g-object) godot-object
              (wrapper :p-object) pozzo-object))
      wrapper)))


(defun destroy-pozzo-wrapper (wrapper)
  (memfree wrapper))
