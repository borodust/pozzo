(cl:in-package #:pozzo)



(defun level->godot (pozzo-level)
  (ecase pozzo-level
    (:core :initialization-core)
    (:servers :initialization-servers)
    (:scene :initialization-scene)
    (:editor :initialization-editor)
    (:max :max-initialization-level)))


(defun level->pozzo (godot-level)
  (ecase godot-level
    (:initialization-core :core)
    (:initialization-servers :servers)
    (:initialization-scene :scene)
    (:initialization-editor :editor)
    (:max-initialization-level :max)))


(defclass extension ()
  ((name :initarg :name :reader %name-of)
   (path :initarg :path :reader %path-of)
   (level :reader %level-of)
   (class-table :initform (make-hash-table :test 'eq) :reader %class-table-of)
   (script-table :initform (make-hash-table :test 'eq) :reader %script-table-of)
   (class-library-pointer :type cffi:foreign-pointer
                          :initform (cffi:null-pointer)
                          :reader class-library-pointer-of)
   (init-callback :initarg :init :reader initializer-name-of)
   (level-init-callback :initarg :level-init :reader level-initializer-name-of)
   (level-deinit-callback :initarg :level-deinit :reader level-deinitializer-name-of)))


(defmethod initialize-instance :after ((this extension) &key (level :scene))
  (with-slots ((this-level level)) this
    (setf this-level (level->godot level))))


(declaim (inline %update-class-library-pointer))
(defun %update-class-library-pointer (ptr extension)
  (with-slots (class-library-pointer) extension
    (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
      ;; cannot optimize away SAP conversion, but it's fine
      ;; irrelevant as we do this operation once anyway
      (setf class-library-pointer ptr))))


(defun make-extension (name &rest keys &key &allow-other-keys)
  (apply #'make-instance 'extension
         :name name
         :path (let ((*print-case* :downcase))
                 (format nil "libgodot://pozzo/~(~A::~A~)"
                         (package-name (symbol-package name))
                         (symbol-name name)))
         keys))



(defun %add-extension-class (extension class-name &rest initargs &key level &allow-other-keys)
  (with-slots (class-table) extension
    (unless (gethash class-name class-table)
      (unless level
        (setf (getf initargs :level) (level->pozzo (%level-of extension))))
      (setf
       (gethash class-name class-table)
       (apply #'make-instance 'extension-class
              :name class-name
              initargs)))))


(defun %add-extension-script (extension script-name &rest initargs
                              &key level &allow-other-keys)
  (with-slots (script-table) extension
    (unless (gethash script-name script-table)
      (unless level
        (setf (getf initargs :level) (level->pozzo (%level-of extension))))
      (setf
       (gethash script-name script-table)
       (apply #'make-instance 'extension-script
              :name script-name
              initargs)))))


(defun %add-extension-class-method (extension class-name method-name
                                    &key bind virtual static pure const
                                      call-function-name
                                      ptrcall-function-name
                                      vcall-function-name
                                      parameters
                                      return-type)
  (with-slots (class-table) extension
    (a:if-let ((class (gethash class-name class-table)))
      (unless (gethash method-name (%method-table-of class))
        (let ((method (make-instance 'extension-class-method
                                     :name method-name
                                     :bind bind
                                     :virtual virtual
                                     :static static
                                     :pure pure
                                     :const const
                                     :call-function-name call-function-name
                                     :ptrcall-function-name ptrcall-function-name
                                     :vcall-function-name vcall-function-name
                                     :return-type (make-instance 'extension-property
                                                                 :variant-kind (get-class-variant-kind return-type))
                                     :parameters (loop for (name type) in parameters
                                                       collect (make-instance 'extension-property
                                                                              :name name
                                                                              :variant-kind (get-class-variant-kind type))))))
          (setf (gethash method-name (%method-table-of class)) method)
          (when virtual
            (setf (gethash bind (%vcall-table-of class)) vcall-function-name))
          t))
      (error "Class ~A not found in extension ~A" class-name (%name-of extension)))))


(defun %add-extension-class-signal (extension class-name signal-name
                                    &key properties)
  (with-slots (class-table) extension
    (a:if-let ((class (gethash class-name class-table)))
      (unless (gethash signal-name (%signal-table-of class))
        (setf (gethash signal-name (%signal-table-of class))
              (loop for (prop-name prop-type) in properties
                    collect (make-instance 'extension-property
                                           :name prop-name
                                           :variant-kind (godot-extension-variant-kind prop-type)
                                           :class prop-type))))
      (error "Class ~A not found in extension ~A" class-name (%name-of extension)))))


(defmacro defpextension (name &body options)
  (let ((init-cb-name (format-secret-symbol name 'ext-init))
        (level-init-cb-name (format-secret-symbol name 'ext-level-init))
        (level-deinit-cb-name (format-secret-symbol name 'ext-level-deinit))
        (props (a:alist-plist options)))
    (destructuring-bind (&key
                           ((:level (level)) '(:scene))
                           ((:init-level (init-fu)) '(nil))
                           ((:fini-level (fini-fu)) '(nil)))
        props
      (a:with-gensyms (cb-level cb-library-ptr)
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (register-extension ',name :init ',init-cb-name
                                        :level ,level
                                        :level-init ',level-init-cb-name
                                        :level-deinit ',level-deinit-cb-name))
           (defprotocallback (,level-init-cb-name
                              %gdext:initialize-callback)
               (,cb-library-ptr ,cb-level)
             (shout-errors
               (initialize-extension-level ',name
                                           ,cb-library-ptr
                                           ,cb-level
                                           ,init-fu)))
           (defprotocallback (,level-deinit-cb-name
                              %gdext:deinitialize-callback)
               (,cb-library-ptr ,cb-level)
             (shout-errors
               (release-extension-level ',name
                                        ,cb-library-ptr
                                        ,cb-level
                                        ,fini-fu)))
           (defprotocallback (,init-cb-name %gdext:initialization-function)
               (interface-get-proc-address ,cb-library-ptr init-struct)
             (declare (ignore interface-get-proc-address))
             (shout-errors
               (if (initialize-extension ',name ,cb-library-ptr init-struct) 1 0))))))))


(defmacro do-extension-classes ((class-var extension &key level) &body body)
  `(loop for ,class-var being the hash-value of (%class-table-of ,extension)
         ,@(when level
             `(when (eq ,level (%level-of ,class-var))))
         do (progn ,@body)))
