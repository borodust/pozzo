(cl:in-package #:pozzo)


(declaim (special *result*))


(defmacro defpclass (name &body slots-and-opts)
  (destructuring-bind (slots &rest opts) slots-and-opts
    (destructuring-bind (&key extension signals inherit level
                           ((:string-name (string-name)) '(nil)))
        (a:alist-plist opts)
      (let* ((extension-name (first extension))
             (parent-name (first inherit))
             (struct-name (format-secret-symbol name 'class-struct))
             (bind-name (or string-name
                            (format nil "~{~A~}"
                                    (mapcar #'nstring-capitalize
                                            (ppcre:split "\\W+"
                                                         (substitute #\_ #\%
                                                                     (format nil "~A-~A"
                                                                             (package-name (symbol-package name))
                                                                             (symbol-name name))))))))
             (level (first level))
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
                                           :level ,level))
               (cffi:defcstruct ,struct-name
                 ,@struct-slots)
               ,@(loop for (slot-name slot-type) in struct-slots
                       append (let ((accessor-name (a:symbolicate name '- slot-name)))
                                `((declaim (inline ,accessor-name))
                                  (defun ,accessor-name (instance)
                                    (declare (optimize (speed 3) (safety 0)))
                                    (c-ref (%get-pozzo-object instance)
                                           (:struct ,struct-name)
                                           ,(a:make-keyword slot-name)))
                                  (declaim (inline (setf ,accessor-name)))
                                  (defun (setf ,accessor-name) (value instance)
                                    (declare (optimize (speed 3) (safety 0)))
                                    (setf (c-ref (%get-pozzo-object instance)
                                                 (:struct ,struct-name)
                                                 ,(a:make-keyword slot-name))
                                          value)))))
               ,@(loop for signal-def in signals
                       collect (destructuring-bind (signal-name &rest signal-properties)
                                   (a:ensure-list signal-def)
                                 `(defpsignal ,signal-name (,name)
                                    ,@signal-properties)))
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
                       append (let ((accessor-name (a:symbolicate name '- (a:make-keyword property-name))))
                                `((pozzo:defpmethod ,reader ((self ,name)) ,property-type
                                    (pozzo:preturn (,accessor-name self)))
                                  (pozzo:defpmethod ,writer ((self ,name) (value ,property-type)) :void
                                    (setf (,accessor-name self) value))))))))))))


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
   (class-library-pointer :type cffi:foreign-pointer
                          :initform (cffi:null-pointer)
                          :reader class-library-pointer-of
                          :writer %update-class-library-pointer)
   (init-callback :initarg :init :reader initializer-name-of)
   (level-init-callback :initarg :level-init :reader level-initializer-name-of)
   (level-deinit-callback :initarg :level-deinit :reader level-deinitializer-name-of)))


(defmethod initialize-instance :after ((this extension) &key (level :scene))
  (with-slots ((this-level level)) this
    (setf this-level (level->godot level))))


(defclass extension-class ()
  ((name :initarg :name :reader %name-of)
   (parent :initarg :parent :reader %parent-name-of)
   (bind :initarg :bind :reader %bind-of)
   (level :reader %level-of)
   (struct :initarg :struct :reader %struct-name-of)
   (constructor :initarg :constructor :reader %constructor-name-of)
   (destructor :initarg :destructor :reader %destructor-name-of)
   (method-table :initform (make-hash-table :test 'eq) :reader %method-table-of)
   (vcall-table :initform (make-hash-table :test 'equal) :reader %vcall-table-of)
   (property-table :initform (make-hash-table :test 'eq) :reader %property-table-of)
   (signal-table :initform (make-hash-table :test 'eq) :reader %signal-table-of)))


(defmethod initialize-instance :after ((this extension-class)
                                       &key parent properties (level :scene))
  (with-slots ((this-parent parent) (this-level level) property-table) this
    (setf this-parent (or parent '%godot:object))
    (loop for (name type reader writer) in properties
          do (setf (gethash name property-table)
                   (make-instance 'extension-property
                                  :name name
                                  :variant-kind (godot-extension-variant-kind type)
                                  :class type
                                  :reader reader
                                  :writer writer)))
    (setf this-level (level->godot level))))


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
                                                                                   (godot-extension-variant-kind return-type)))
                                     :parameters (loop for (name type) in parameters
                                                       collect (make-instance 'extension-property
                                                                              :name name
                                                                              :variant-kind (godot-extension-variant-kind type))))))
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
    (destructuring-bind (&key ((:level (level)) '(:scene))
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


(declaim (inline initialize-variant-from-value))
(defun initialize-variant-from-value (uninitialized-variant-ptr value-ptr class-name)
  (let* ((variant-kind (godot-extension-variant-kind class-name))
         (variant-ctor (%gdext:get-variant-from-type-constructor variant-kind)))
    (funcall-prototype variant-ctor %gdext:variant-from-type-constructor-func
                                   uninitialized-variant-ptr
                                   value-ptr)))


(declaim (inline release-variant))
(defun release-variant (variant-ptr)
  (%gdext:variant-destroy variant-ptr))


(declaim (inline get-variant-internal-ptr))
(defun get-variant-internal-ptr (variant-ptr)
  (let* ((variant-kind (%gdext:variant-get-type variant-ptr))
         (func-ptr (%gdext:variant-get-ptr-internal-getter variant-kind)))
    (funcall-prototype func-ptr %gdext:variant-get-internal-ptr-func
                                   variant-ptr)))


(defun prepare-arguments (args-var parameters)
  (multiple-value-bind (args arg-types arg-init arg-names variant-init)
      (loop for (name type) in parameters
            for i from 0
            collect `(cffi:mem-aref ,args-var :pointer ,i) into arg-init
            collect `(get-variant-internal-ptr
                      (cffi:mem-aref ,args-var :pointer ,i))
              into variant-init
            collect name into arg-names
            collect type into arg-types
            collect (list name type) into args
            finally (return (values args arg-types arg-init arg-names variant-init)))
    (list :args args
          :arg-names arg-names
          :arg-types arg-types
          :arg-values arg-init
          :variant-values variant-init)))


(defun preturn (value)
  (declare (ignore value))
  (error "This is a stub. Never call this function outside of the lexical scope of defpmethod's body"))


(defun expand-preturn (block-name result-ptr-var result-type result-value-var)
  `(progn
     (unless (cffi:null-pointer-p ,result-ptr-var)
       (setf (c-ref ,result-ptr-var ,result-type) ,result-value-var))
     (return-from ,block-name (values))))


(defun expand-godot-call-callback (call-name implementing-function-name return-type parameters)
  (a:with-gensyms (cb-method-data-var cb-instance-var cb-args-var cb-argc-var cb-ret-var cb-err-var
                                      result-var)
    (destructuring-bind (&key arg-names arg-types arg-values variant-values &allow-other-keys)
        (prepare-arguments cb-args-var parameters)
      `(defprotocallback (,call-name %gdext:class-method-call)
           (,cb-method-data-var ,cb-instance-var ,cb-args-var ,cb-argc-var ,cb-ret-var ,cb-err-var)
         (declare (ignore ,cb-method-data-var)
                  (ignorable ,cb-instance-var ,cb-args-var ,cb-ret-var))
         (shout-errors
           #++(shout "CALL ~A" ',call-name)
           (c-val ((,cb-err-var %gdext:call-error))
             (setf (,cb-err-var :error) :ok)

             (when (< ,cb-argc-var ,(length arg-names))
               (setf (,cb-err-var :error) :error-too-few-arguments
                     (,cb-err-var :expected) ,(length arg-names))
               (return-from ,call-name (values)))

             (when (> ,cb-argc-var ,(length arg-names))
               (setf (,cb-err-var :error) :error-too-many-arguments
                     (,cb-err-var :expected) ,(length arg-names))
               (return-from ,call-name (values)))

             ,@(loop for arg-value in arg-values
                     for arg-type in arg-types
                     for i from 0
                     for variant-kind = (get-class-variant-kind arg-type)
                     collect `(unless (eq ,variant-kind
                                          (%gdext:variant-get-type ,arg-value))
                                (setf (,cb-err-var :error) :error-invalid-argument
                                      (,cb-err-var :expected) ,(cffi:foreign-enum-value
                                                                '%gdext:variant-type
                                                                variant-kind)
                                      (,cb-err-var :argument) ,i)
                                (return-from ,call-name (values))))
             ,(if (eq :void return-type)
                  `(,implementing-function-name ,cb-instance-var (cffi:null-pointer) ,@variant-values)
                  `(if (cffi:null-pointer-p ,cb-ret-var)
                       (,implementing-function-name ,cb-instance-var (cffi:null-pointer) ,@variant-values)
                       (c-with ((,result-var ,return-type))
                         (,implementing-function-name ,cb-instance-var (,result-var &) ,@variant-values)
                         (initialize-variant-from-value ,cb-ret-var (,result-var &) ',return-type)))))
           (values))))))


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
        (a:with-gensyms (method-data-var cb-args-var cb-ret-var result-var)
          (destructuring-bind (&key arg-names
                                 ((:arg-values arg-init))
                               &allow-other-keys)
              (prepare-arguments cb-args-var (rest parameters))
            `(progn
               ,@(unless pure
                   `((declaim (inline ,fu-name))
                     (defun ,fu-name (,instance-var ,result-var ,@arg-names)
                       (declare (ignorable ,instance-var ,result-var))
                       (c-val (,@(loop for (name type) in (rest parameters)
                                       collect `(,name ,type)))
                         ,@(if (eq :void return-type)
                               body
                               `((macrolet ((pozzo::preturn (result)
                                              (expand-preturn ',fu-name ',result-var ',return-type result)))
                                   ,@body))))
                       (values))
                     ,@(if virtual
                           `((defprotocallback (,vcall-name %gdext:class-call-virtual-with-data)
                                 (,instance-var funame ,method-data-var ,cb-args-var ,cb-ret-var)
                               (declare (ignore funame ,method-data-var)
                                        (ignorable ,instance-var ,cb-args-var ,cb-ret-var))
                               (shout-errors
                                 #++(shout "VCALL ~A" ',vcall-name)
                                 (,fu-name ,instance-var ,(if (eq :void return-type)
                                                              '(cffi:null-pointer)
                                                              cb-ret-var)
                                           ,@arg-init)
                                 (values))))
                           `((defprotocallback (,ptrcall-name %gdext:class-method-ptr-call)
                                 (,method-data-var ,instance-var ,cb-args-var ,cb-ret-var)
                               (declare (ignore ,method-data-var)
                                        (ignorable ,cb-args-var ,cb-ret-var))
                               (shout-errors
                                 #++(shout "PTRCALL ~A" ',ptrcall-name)
                                 (,fu-name ,instance-var ,(if (eq :void return-type)
                                                              '(cffi:null-pointer)
                                                              cb-ret-var)
                                           ,@arg-init)
                                 (values)))
                             ,(expand-godot-call-callback call-name fu-name return-type (rest parameters))))))
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


(defmacro defpsignal (name (&rest classes) &body properties)
  `(progn
     ,@(loop for class-name in classes
             append (let ((signal-emitter-name (a:symbolicate '@ class-name '+ name)))
                      (multiple-value-bind (prop-names variant-names)
                          (loop for (name type) in properties
                                collect name into prop-names
                                collect (a:make-gensym 'variant) into variant-names
                                finally (return (values prop-names variant-names)))
                        (a:with-gensyms (instance)
                          `((eval-when (:compile-toplevel :load-toplevel :execute)
                              (register-extension-class-signal ',name ',class-name
                                                               :properties ',properties))
                            (declaim (inline ,signal-emitter-name))
                            (defun ,signal-emitter-name (,instance ,@prop-names)
                              (with-variants (,@(loop for (prop-name prop-type) in properties
                                                      for variant-name in variant-names
                                                      collect `(,variant-name ,prop-type ,prop-name)))
                                (emit-signal ,instance ',name ,@variant-names))))))))))
