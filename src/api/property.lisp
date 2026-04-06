(cl:in-package #:pozzo)


(defclass extension-property ()
  ((name :initarg :name :initform "" :reader %name-of)
   (variant-kind :initarg :variant-kind :reader variant-kind-of)
   (class-name :initarg :class :initform nil :reader class-name-of)
   (reader :initarg :reader :initform nil :reader reader-name-of)
   (writer :initarg :writer :initform nil :reader writer-name-of)))
