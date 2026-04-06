(cl:in-package #:pozzo)


(defclass extension-script (extension-prototype)
  ((name :initarg :name
         :initform (error ":name missing")
         :reader %name-of)
   (path :initarg :path :initform nil)
   (global :initarg :global :initform nil)
   (level :initarg :level :initform nil)
   (instance :initform nil)))


(defun initialize-extension-script (script)
  (with-slots (instance name path) script
    (setf instance (construct name))
    (when path
      (let ((full-path (format nil "res://~A.pzo" path)))
        (with-godot-string (path-str full-path)
          (%godot:resource+set-path instance path-str)
          (c-with ((err %godot:error))
            (%godot:resource-saver+save (%godot:resource-saver)
                                        (err &)
                                        instance
                                        path-str
                                        :none)
            (unless (eq err :ok)
              (shout "Failed to save script ~A at ~A: ~A"
                     name full-path err))))))))


(defcfunproto pozzo-script-method :void
  (instance %gdext:script-instance-data-ptr)
  (argv (:pointer %gdext:const-variant-ptr))
  (argc %gdext:int)
  (result %gdext:variant-ptr)
  (error-info (:pointer %gdext:call-error)))


(defun make-script-instance (extension script-name script object)
  (declare (ignore extension script-name script object))
  (cffi:null-pointer))


(defmacro defpscript (name &body slots-and-opts)
  (destructuring-bind (slots &rest opts) slots-and-opts
    (let ((struct-name (format-secret-symbol name 'script-data)))
      (destructuring-bind (&key ((:for (instance-base-type)) '(%godot:object))
                             ((:level (level)) '(nil))
                             ((:extension (extension-name)) (error "Extension must be povided"))
                             ((:path (path)) '(nil))
                             ((:global (global-p)) '(nil)))
          (a:alist-plist opts)
        `(progn
           (cffi:defcstruct ,struct-name
             ,@slots)
           (defpclass ,name
             ((method-dict %godot:dictionary))
             (:inherit %godot:script-extension)
             (:extension ,extension-name))
           (defpmethod (%get-instance-base-type :virtual) ((self ,name)) %godot:string-name
             (preturn-with (result)
               (initialize-godot-string-name result
                                             ,(get-class-bind-name instance-base-type))))
           (defpmethod (%can-instantiate :virtual) ((self ,name)) %godot:bool
             (preturn 1))
           (defpmethod (%instance-create :virtual) ((self ,name) (obj (:pointer %godot:object)))
               (:pointer :void)
             (preturn (make-script-instance ',extension-name ',name self obj)))

           (eval-when (:compile-toplevel :load-toplevel :execute)
             (register-extension-script ',name ',extension-name
                                        :path ,path
                                        :global ,global-p
                                        :level ,level)))))))


(defprotocallback (call-script-method %gdext:script-instance-call)
    (instance-var method-string-name argv argc result error-info)
  (shout-errors
    (let* ((script (%get-instance-script isntance-var))
           (method-dict (%get-script-method-dict script)))
      #++(%godot:dictionary+get ))))


(defmethod expand-prototype-method-wrappers ((class extension-script)
                                             method-name
                                             parameters
                                             return-type
                                             &key virtual pure static const)
  (let* ((script-name (%name-of class))
         (call-name (format-secret-symbol script-name method-name '-call))
         (bind-name (format nil "~{~A~^_~}"
                            (mapcar #'nstring-downcase
                                    (ppcre:split "\\W+"
                                                 (substitute #\_ #\%
                                                             (string method-name)))))))
    (a:with-gensyms (result-var
                     cb-instance-var
                     cb-args-var
                     cb-argc-var
                     cb-ret-var
                     cb-err-var)
      `((defprotocallback (,call-name call-script-method)
            (,cb-instance-var ,cb-args-var ,cb-argc-var ,cb-ret-var ,cb-err-var)
          (shout-errors
            #++(shout "SCALL ~A" ',call-name)
            (with-variant-arguments
                (,@(mapcar #'first parameters))
                (,cb-args-var ,cb-argc-var ,cb-err-var ,@parameters)
              (with-variant-result (,result-var) (,cb-ret-var ,return-type)
                (funcall-pmethod '(,script-name ,method-name)
                                 ,cb-instance-var ,result-var ,@(mapcar #'first parameters)))))
          (values))
        (eval-when (:compile-toplevel :load-toplevel :execute)
          (register-extension-script-method ',method-name ',script-name
                                            :bind ,bind-name
                                            :static ,static
                                            :virtual ,virtual
                                            :pure ,pure
                                            :const ,const
                                            :call-function-name ',call-name
                                            :parameters ',parameters
                                            :return-type ',return-type))))))
