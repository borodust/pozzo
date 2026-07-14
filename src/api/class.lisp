(cl:in-package #:pozzo)


(defmacro defpclass (name &body slots-and-opts)
  (destructuring-bind (slots &rest opts) slots-and-opts
    (destructuring-bind (&key signals inherit level
                           ((:extension (extension-name)) '(nil))
                           ((:string-name (string-name)) '(nil))
                           ((:init (init-fu)) '(nil))
                           ((:deinit (fini-fu)) '(nil)))
        (a:alist-plist opts)
      (let* ((implicit-extension-p (null extension-name))
             (extension-name (or extension-name (format-secret-symbol name 'class-implicit-extension)))
             (parent-name (first inherit))
             (struct-name (format-secret-symbol name 'class-data))
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
               ,@(when implicit-extension-p
                   `((eval-when (:compile-toplevel :load-toplevel :execute)
                       (defpextension ,extension-name
                         (:level ,(or level :scene))))))
               (eval-when (:compile-toplevel :load-toplevel :execute)
                 (register-extension-class ',name ',extension-name
                                           :bind ,bind-name
                                           :parent ',parent-name
                                           :struct ',struct-name
                                           :properties ',properties
                                           :constructor ',ctor-name
                                           :destructor ',dtor-name
                                           :level ,level
                                           :init ',init-fu
                                           :deinit ',fini-fu))
               (cffi:defcstruct ,struct-name
                 ,@struct-slots)
               ,@(loop for (slot-name slot-type) in struct-slots
                       append (let ((accessor-name (a:symbolicate name '- slot-name)))
                                `((declaim (inline ,accessor-name))
                                  (defun ,accessor-name (instance)
                                    (c-ref (%get-pozzo-object instance)
                                           (:struct ,struct-name)
                                           ,(a:make-keyword slot-name)))
                                  (declaim (inline (setf ,accessor-name)))
                                  (defun (setf ,accessor-name) (value instance)
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
                   ,(when initforms
                      `(c-val ((,ptr (:struct ,struct-name)))
                         ,@(loop for initform in initforms
                                 for (slot-name) in struct-slots
                                 when initform
                                   collect `(setf (,ptr ,(a:make-keyword slot-name)) ,initform))))
                   ,ptr))
               (defun ,dtor-name (ptr)
                 (memfree ptr))
               ,@(loop for (property-name property-type reader writer) in properties
                       append (let ((accessor-name (a:symbolicate name '- (a:make-keyword property-name))))
                                `((pozzo:defpmethod ,reader ((self ,name)) ,property-type
                                    (pozzo:preturn (,accessor-name self)))
                                  (pozzo:defpmethod ,writer ((self ,name) (value ,property-type)) :void
                                    (setf (,accessor-name self) value))))))))))))


(defclass extension-class (extension-prototype)
  ((name :initarg :name :reader %name-of)
   (parent :initarg :parent :reader %parent-name-of)
   (bind :initarg :bind :reader %bind-of)
   (level :reader %level-of)
   (struct :initarg :struct :reader %struct-name-of)
   (constructor :initarg :constructor :reader %constructor-name-of)
   (destructor :initarg :destructor :reader %destructor-name-of)
   (vcall-table :initform (make-hash-table :test 'equal) :reader %vcall-table-of)
   (init :initform nil :initarg :init :reader %init-of)
   (deinit :initform nil :initarg :deinit :reader %deinit-of)))


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


(defmethod expand-prototype-method-wrappers ((class extension-class)
                                             method-name
                                             parameters
                                             return-type
                                             &key virtual pure static const)
  (let* ((class-name (%name-of class))
         (ptrcall-name (format-secret-symbol class-name method-name '-ptrcall))
         (call-name (format-secret-symbol class-name method-name '-call))
         (vcall-name (format-secret-symbol class-name method-name '-vcall))
         (bind-name (format nil "~{~A~^_~}"
                            (mapcar #'nstring-downcase (ppcre:split "\\W+"
                                                                    (substitute #\_ #\% (string method-name)))))))
    (a:with-gensyms (cb-instance-var
                     result-var
                     cb-method-data-var
                     cb-args-var
                     cb-argc-var
                     cb-ret-var
                     cb-err-var)
      (destructuring-bind (&key ((:arg-values arg-init))
                           &allow-other-keys)
          (prepare-arguments cb-args-var parameters)
        `(,@(unless pure
              (if virtual
                  `((defprotocallback (,vcall-name %gdext:class-call-virtual-with-data)
                        (,cb-instance-var funame ,cb-method-data-var ,cb-args-var ,cb-ret-var)
                      (declare (ignore funame ,cb-method-data-var)
                               (ignorable ,cb-args-var ,cb-ret-var))
                      (shout-errors
                        #++(shout "VCALL ~A" ',vcall-name)
                        (funcall-pmethod '(,class-name ,method-name)
                                         ,cb-instance-var ,(if (eq :void return-type)
                                                               '(cffi:null-pointer)
                                                               cb-ret-var)
                                         ,@arg-init)
                        (values))))
                  `((defprotocallback (,ptrcall-name %gdext:class-method-ptr-call)
                        (,cb-method-data-var ,cb-instance-var ,cb-args-var ,cb-ret-var)
                      (declare (ignore ,cb-method-data-var)
                               (ignorable ,cb-args-var ,cb-ret-var))
                      (shout-errors
                        #++(shout "PTRCALL ~A" ',ptrcall-name)
                        (funcall-pmethod '(,class-name ,method-name)
                                         ,cb-instance-var ,(if (eq :void return-type)
                                                               '(cffi:null-pointer)
                                                               cb-ret-var)
                                         ,@arg-init)
                        (values)))
                    (defprotocallback (,call-name %gdext:class-method-call)
                        (,cb-method-data-var ,cb-instance-var ,cb-args-var ,cb-argc-var ,cb-ret-var ,cb-err-var)
                      (declare (ignore ,cb-method-data-var)
                               (ignorable ,cb-args-var ,cb-ret-var))
                      (shout-errors
                        #++(shout "CALL ~A" ',call-name)
                        (with-variant-arguments
                            (,@(mapcar #'first parameters))
                            (,cb-args-var ,cb-argc-var ,cb-err-var ,@parameters)
                          (with-variant-result (,result-var) (,cb-ret-var ,return-type)
                            (funcall-pmethod '(,class-name ,method-name)
                                             ,cb-instance-var ,result-var ,@(mapcar #'first parameters))))
                        (values))))))
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (register-extension-class-method ',method-name ',class-name
                                             :bind ,bind-name
                                             :static ,static
                                             :virtual ,virtual
                                             :pure ,pure
                                             :const ,const
                                             ,@(if virtual
                                                   `(:vcall-function-name ',vcall-name)
                                                   `(:call-function-name ',call-name
                                                     :ptrcall-function-name ',ptrcall-name))
                                             :parameters ',parameters
                                             :return-type ',return-type)))))))


(defun init-extension-class (extension-class)
  (a:when-let ((init-fu (%init-of extension-class)))
    (funcall init-fu)))


(defun deinit-extension-class (extension-class)
  (a:when-let ((deinit-fu (%deinit-of extension-class)))
    (funcall deinit-fu)))
