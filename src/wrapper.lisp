(cl:in-package #:pozzo)


(cffi:defcstruct pozzo-wrapper
  (g-object :pointer)
  (p-object :pointer))

(declaim (inline unwrap))
(defun unwrap (ptr)
  (c-ref ptr (:struct pozzo-wrapper) :g-object))


(define-compiler-macro unwrap (ptr)
  `(c-ref ,ptr (:struct pozzo-wrapper) :g-object))


(declaim (inline %get-pozzo-object))
(defun %get-pozzo-object (ptr)
  (c-ref ptr (:struct pozzo-wrapper) :p-object))


(define-compiler-macro get-pozzo-object (ptr)
  `(c-ref ,ptr (:struct pozzo-wrapper) :p-object))


(declaim (inline make-pozzo-wrapper))
(defun make-pozzo-wrapper (godot-object pozzo-object)
  (let ((wrapper (memalloc '(:struct pozzo-wrapper))))
    (c-val ((wrapper (:struct pozzo-wrapper)))
      (setf (wrapper :g-object) godot-object
            (wrapper :p-object) pozzo-object))
    wrapper))


(declaim (inline destroy-pozzo-wrapper))
(defun destroy-pozzo-wrapper (wrapper)
  (memfree wrapper)
  (values))
