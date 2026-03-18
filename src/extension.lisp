(cl:in-package #:pozzo)


(declaim (special *result*))


(defmacro defpclass (name &body slots-and-opts)
  (destructuring-bind (slots &rest opts) slots-and-opts
    (destructuring-bind (&key extension properties signals inherit godot) (a:alist-plist opts)
      (declare (ignore properties godot))
      (let* ((extension-name (first extension))
             (parent-name (first inherit))
             (struct-name (format-secret-symbol name 'class-struct))
             (bind-name (format nil "~{~A~}"
                                (mapcar #'nstring-capitalize
                                        (ppcre:split "\\W+"
                                                     (substitute #\_ #\% (string name))))))
             (ctor-name (format-secret-symbol name 'class-constructor))
             (dtor-name (format-secret-symbol name 'class-destructor)))
        (multiple-value-bind (struct-slots initforms properties)
            (loop for (name type . slot-opts) in slots
                  for (exposed initform reader writer)
                    = (destructuring-bind (&key initform exposed reader writer)
                          slot-opts
                        (list exposed initform reader writer))
                  collect (list name type) into struct-slots
                  collect initform into initforms
                  when exposed
                    collect (list name type (a:symbolicate '%get- name) (a:symbolicate '%set- name))
                      into properties
                  finally (return (values struct-slots initforms properties)))
          (a:with-gensyms (ptr)
            `(progn
               (eval-when (:compile-toplevel :load-toplevel :execute)
                 (register-extension-class ',name ',extension-name
                                           :bind ,bind-name
                                           :parent ',parent-name
                                           :struct ',struct-name
                                           :properties ',properties
                                           :constructor ',ctor-name
                                           :destructor ',dtor-name
                                           :signals ',signals))
               (cffi:defcstruct ,struct-name
                 ,@struct-slots)
               (defun ,ctor-name ()
                 (let ((,ptr (memalloc '(:struct ,struct-name))))
                   (c-val ((,ptr (:struct ,struct-name)))
                     ,@(loop for initform in initforms
                             for (slot-name) in struct-slots
                             when initform
                               collect `(setf (,ptr ,(a:make-keyword slot-name)) ,initform)))
                   ,ptr))
               (defun ,dtor-name (ptr)
                 (memfree ptr))
               ,@(loop for (property-name property-type reader writer) in properties
                       append `((pozzo:defpmethod ,reader ((self ,name)) ,property-type
                                  (let ((self (%get-pozzo-object self)))
                                    (c-val ((self (:struct ,struct-name)))
                                      (setf $result (self ,(a:make-keyword property-name))))))
                                (pozzo:defpmethod ,writer ((self ,name) (value ,property-type)) :void
                                  (let ((self (%get-pozzo-object self)))
                                    (c-val ((self (:struct ,struct-name))
                                            (value ,property-type))
                                      (setf (self ,(a:make-keyword property-name)) value)))))))))))))


(defclass extension ()
  ((name :initarg :name :reader %name-of)
   (path :initarg :path :reader %path-of)
   (class-table :initform (make-hash-table :test 'eq) :reader %class-table-of)
   (class-library-pointer :type cffi:foreign-pointer
                          :initform (cffi:null-pointer)
                          :reader class-library-pointer-of
                          :writer %update-class-library-pointer)
   (init-callback :initarg :init :reader initializer-name-of)
   (level-init-callback :initarg :level-init :reader level-initializer-name-of)
   (level-deinit-callback :initarg :level-deinit :reader level-deinitializer-name-of)))


(defclass extension-class ()
  ((name :initarg :name :reader %name-of)
   (parent :initarg :parent :reader %parent-name-of)
   (bind :initarg :bind :reader %bind-of)
   (struct :initarg :struct :reader %struct-name-of)
   (constructor :initarg :constructor :reader %constructor-name-of)
   (destructor :initarg :destructor :reader %destructor-name-of)
   (method-table :initform (make-hash-table :test 'eq) :reader %method-table-of)
   (vcall-table :initform (make-hash-table :test 'equal) :reader %vcall-table-of)
   (property-table :initform (make-hash-table :test 'eq) :reader %property-table-of)
   (signal-table :initform (make-hash-table :test 'eq) :reader %signal-table-of)))


(defmethod initialize-instance :after ((this extension-class) &key parent properties signals)
  (with-slots ((this-parent parent) property-table signal-table) this
    (setf this-parent (or parent '%godot:object))
    (loop for (name type reader writer) in properties
          do (setf (gethash name property-table)
                   (make-instance 'extension-property
                                  :name name
                                  :variant-kind (%gdext.util:godot-extension-variant-kind type)
                                  :class type
                                  :reader reader
                                  :writer writer)))
    (loop for signal-def in signals
          for (name . parameters) = (a:ensure-list signal-def)
          do (setf (gethash name signal-table)
                   (loop for (param-name param-type) in parameters
                         collect (make-instance 'extension-property
                                                :name param-name
                                                :variant-kind (%gdext.util:godot-extension-variant-kind param-type)
                                                :class param-type))))))


(defclass extension-property ()
  ((name :initarg :name :initform "" :reader %name-of)
   (variant-kind :initarg :variant-kind :reader variant-kind-of)
   (class-name :initarg :class :initform nil :reader class-name-of)
   (reader :initarg :reader :initform nil :reader reader-name-of)
   (writer :initarg :writer :initform nil :reader writer-name-of)))


(defclass extension-class-method ()
  ((name :initarg :name :reader %name-of)
   (bind :initarg :bind :reader %bind-of)
   (virtual :initarg :virtual :reader virtualp)
   (static :initarg :static :reader staticp)
   (pure :initarg :pure :reader purep)
   (const :initarg :const :reader constp)
   (parameters :initarg :parameters :reader parameters-of)
   (return-type :initarg :return-type :reader return-type-of)
   (call-function-name :initarg :call-function-name
                       :reader call-function-name-of)
   (ptrcall-function-name :initarg :ptrcall-function-name
                          :reader ptrcall-function-name-of)
   (vcall-function-name :initarg :vcall-function-name
                        :reader vcall-function-name-of)))


(defun make-extension (name &rest keys &key &allow-other-keys)
  (apply #'make-instance 'extension
         :name name
         :path (let ((*print-case* :downcase))
                 (format nil "libgodot://pozzo/~(~A::~A~)"
                         (package-name (symbol-package name))
                         (symbol-name name)))
         keys))



(defun %add-extension-class (extension class-name &rest initargs &key &allow-other-keys)
  (with-slots (class-table) extension
    (unless (gethash class-name class-table)
      (setf
       (gethash class-name class-table)
       (apply #'make-instance 'extension-class
              :name class-name
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
                                                                 :variant-kind (if (eq :void return-type)
                                                                                   :nil
                                                                                   (%gdext.util:godot-extension-variant-kind return-type)))
                                     :parameters (loop for (name type) in parameters
                                                       collect (make-instance 'extension-property
                                                                              :name name
                                                                              :variant-kind (%gdext.util:godot-extension-variant-kind type))))))
          (setf (gethash method-name (%method-table-of class)) method)
          (when virtual
            (setf (gethash bind (%vcall-table-of class)) vcall-function-name))
          t))
      (error "Class ~A not found in extension ~A" class-name (%name-of extension)))))


(defmacro defpextension (name)
  (let ((init-cb-name (format-secret-symbol name 'ext-init))
        (level-init-cb-name (format-secret-symbol name 'ext-level-init))
        (level-deinit-cb-name (format-secret-symbol name 'ext-level-deinit)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (register-extension ',name :init ',init-cb-name
                                    :level-init ',level-init-cb-name
                                    :level-deinit ',level-deinit-cb-name))
       (defprotocallback (,level-init-cb-name
                          %gdext.types:initialize-callback)
           (class-library-ptr init-level)
         (shout-errors
           (initialize-extension-level ',name class-library-ptr init-level)))
       (defprotocallback (,level-deinit-cb-name
                          %gdext.types:deinitialize-callback)
           (class-library-ptr deinit-level)
         (shout-errors
           (release-extension-level ',name class-library-ptr deinit-level)))
       (defprotocallback (,init-cb-name %gdext.types:initialization-function)
           (interface-get-proc-address class-library-ptr init-struct)
         (declare (ignore interface-get-proc-address))
         (shout-errors
           (if (initialize-extension ',name class-library-ptr init-struct) 1 0))))))


(defmacro do-extension-classes ((class-var extension) &body body)
  `(loop for ,class-var being the hash-value of (%class-table-of ,extension)
         do (progn ,@body)))


(defun prepare-method-name-and-opts (name-and-opts)
  (destructuring-bind (name &rest opts) (a:ensure-list name-and-opts)
    (list* name (loop with opt = (first opts)
                      and rest = (rest opts)
                      while opt
                      append (if (member opt '(:pure :virtual :static :const))
                                 (prog1 (list opt t)
                                   (setf opt (first rest)
                                         rest (rest rest)))
                                 (prog1 (list opt (first rest))
                                   (setf opt (cdar rest)
                                         rest (cddr rest))))))))


(defun initialize-variant-from-value (uninitialized-variant-ptr value-ptr class-name)
  (let* ((variant-kind (%gdext.util:godot-extension-variant-kind class-name))
         (variant-ctor (%gdext.interface:get-variant-from-type-constructor variant-kind)))
    (%gdext.util:funcall-prototype variant-ctor %gdext.types:variant-from-type-constructor-func
                                   uninitialized-variant-ptr
                                   value-ptr)))


(defun release-variant (variant-ptr)
  (%gdext.interface:variant-destroy variant-ptr))


(defun get-variant-internal-ptr (variant-ptr)
  (let* ((variant-kind (%gdext.interface:variant-get-type variant-ptr))
         (func-ptr (%gdext.interface:variant-get-ptr-internal-getter variant-kind)))
    (%gdext.util:funcall-prototype func-ptr %gdext.types:variant-get-internal-ptr-func
                                   variant-ptr)))


(defmacro defpmethod (name-and-opts parameters return-type &body body)
  (destructuring-bind (name &key virtual pure static const) (prepare-method-name-and-opts name-and-opts)
    (multiple-value-bind (instance-var class-name)
        (destructuring-bind (var-or-class-name &optional class-name)
            (first parameters)
          (if static
              (progn
                (assert (null class-name))
                (values (gensym) var-or-class-name))
              (progn
                (assert class-name)
                (values var-or-class-name class-name))))
      (let* ((fu-name (format-secret-symbol class-name name '-method))
             (ptrcall-name (format-secret-symbol class-name name '-ptrcall))
             (call-name (format-secret-symbol class-name name '-call))
             (vcall-name (format-secret-symbol class-name name '-vcall))
             (bind-name (format nil "~{~A~^_~}"
                                (mapcar #'nstring-downcase (ppcre:split "\\W+"
                                                                        (substitute #\_ #\% (string name)))))))
        (a:with-gensyms (method-data-var cb-args-var cb-ret-var err-var)
          (multiple-value-bind (arg-init arg-names variant-init)
              (loop for (name) in (rest parameters)
                    for i from 0
                    collect `(cffi:mem-aref ,cb-args-var :pointer ,i) into arg-init
                    collect `(get-variant-internal-ptr
                              (cffi:mem-aref ,cb-args-var :pointer ,i))
                      into variant-init
                    collect name into arg-names
                    finally (return (values arg-init arg-names variant-init)))
            `(progn
               ,@(unless pure
                   `((defun ,fu-name (,instance-var ,@arg-names)
                       (declare (ignorable ,instance-var))
                       ,@(if (eq :void return-type)
                             body
                             `((let (($result *result*))
                                 (c-val (($result ,return-type))
                                   ,@body)))))
                     ,@(if virtual
                           `((defprotocallback (,vcall-name %gdext.types:class-call-virtual-with-data)
                                 (,instance-var funame ,method-data-var ,cb-args-var ,cb-ret-var)
                               (declare (ignore funame ,method-data-var)
                                        (ignorable ,instance-var ,cb-args-var ,cb-ret-var))
                               (shout-errors
                                 #++(shout "VCALL ~A" ',vcall-name)
                                 (let (,@(unless (eq :void return-type)
                                           `((*result* ,cb-ret-var))))
                                   (c-val ((,instance-var (:struct pozzo-wrapper)))
                                     (,fu-name (,instance-var &) ,@arg-init))
                                   (values)))))
                           `((defprotocallback (,ptrcall-name %gdext.types:class-method-ptr-call)
                                 (,method-data-var ,instance-var ,cb-args-var ,cb-ret-var)
                               (declare (ignore ,method-data-var)
                                        (ignorable ,cb-args-var ,cb-ret-var))
                               (shout-errors
                                 #++(shout "PTRCALL ~A" ',ptrcall-name)
                                 (let (,@(unless (eq :void return-type)
                                           `((*result* ,cb-ret-var))))
                                   (c-val ((,instance-var (:struct pozzo-wrapper)))
                                     (,fu-name (,instance-var &) ,@arg-init))
                                   (values))))
                             (defprotocallback (,call-name %gdext.types:class-method-call)
                                 (,method-data-var ,instance-var ,cb-args-var argc ,cb-ret-var ,err-var)
                               (declare (ignore ,method-data-var)
                                        (ignorable ,instance-var ,cb-args-var ,cb-ret-var))
                               (shout-errors
                                 #++(shout "CALL ~A" ',call-name)
                                 (c-val ((,err-var %gdext.types:call-error))
                                   (setf (,err-var :error) :ok)
                                   (when (< argc ,(length arg-names))
                                     (setf (,err-var :error) :error-too-few-arguments
                                           (,err-var :expected) ,(length arg-names))
                                     (return-from ,call-name (values)))
                                   (when (> argc ,(length arg-names))
                                     (setf (,err-var :error) :error-too-many-arguments
                                           (,err-var :expected) ,(length arg-names))
                                     (return-from ,call-name (values)))
                                   (c-with (,@(unless (eq :void return-type)
                                                `((result ,return-type))))
                                     (let (,@(unless (eq :void return-type)
                                               `((*result* (result &)))))
                                       (c-val ((,instance-var (:struct pozzo-wrapper)))
                                         (,fu-name (,instance-var &) ,@variant-init))
                                       ,@(unless (eq :void return-type)
                                           `((initialize-variant-from-value ,cb-ret-var (result &) ',return-type)))))
                                   (values))))))))
               (eval-when (:compile-toplevel :load-toplevel :execute)
                 (register-extension-class-method ',name ',class-name
                                                  :bind ,bind-name
                                                  :static ,static
                                                  :virtual ,virtual
                                                  :pure ,pure
                                                  :const ,const
                                                  ,@(if virtual
                                                        `(:vcall-function-name ',vcall-name)
                                                        `(:call-function-name ',call-name
                                                          :ptrcall-function-name ',ptrcall-name))
                                                  :parameters ',(rest parameters)
                                                  :return-type ',return-type)))))))))
