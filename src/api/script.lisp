(cl:in-package #:pozzo)

;;;
;;; METHOD
;;;
(defclass script-method (extension-class-method)
  ((bind-hash :initform nil :accessor %bind-hash-of)))


(defun make-script-method (method-name
                           &rest initargs
                           &key &allow-other-keys)
  (apply #'make-instance 'script-method
         :name method-name
         initargs))

;;;
;;; SCRIPT
;;;
(defclass extension-script (extension-prototype)
  ((name :initarg :name
         :initform (error ":name missing")
         :reader %name-of)
   (path :initarg :path :reader %path-of)
   (struct-name :initarg :data-struct-name
                :reader %data-struct-name-of)
   (base-type :initarg :base-type
              :reader %base-type-of)
   (level :initarg :level :initform nil)
   (address :initform 0 :accessor %address-of)
   (method-table :initform (make-hash-table :test 'eq))
   (hash-method-map :initform (make-hash-table :test 'eql))))


(defun initialize-extension-script (script)
  (with-slots (instance name path hash-method-map) script
    (setf instance (construct name))
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
                   name full-path err)))))))


(defcfunproto pozzo-script-method :void
  (instance %gdext:script-instance-data-ptr)
  (argv (:pointer %gdext:const-variant-ptr))
  (argc %gdext:int)
  (result %gdext:variant-ptr)
  (error-info (:pointer %gdext:call-error)))


(defun add-script-method (script method-name &rest initargs
                          &key &allow-other-keys)
  (with-slots (method-table) script
    (setf (gethash method-name method-table) (apply #'make-script-method
                                                    method-name
                                                    initargs))))


(defun %sync-method-hashes (script)
  (with-slots (method-table hash-method-map) script
    (c-with ((hash %godot:int))
      (loop for method being the hash-value of method-table
            unless (%bind-hash-of method)
              do (with-godot-string-name (sname (%bind-of method))
                   (%godot:string-name+hash sname (hash &))
                   (setf (%bind-hash-of method) hash
                         (gethash hash hash-method-map) method))))))


(defun find-script-method-by-hash (script method-hash)
  (with-slots (hash-method-map) script
    (a:if-let ((method (gethash method-hash hash-method-map)))
      (cffi:get-callback (call-function-name-of method))
      (progn
        (%sync-method-hashes script)
        (a:when-let ((method (gethash method-hash hash-method-map)))
          (cffi:get-callback (call-function-name-of method)))))))


(defmacro defpscript (name &body slots-and-opts)
  (destructuring-bind (slots &rest opts) slots-and-opts
    (let ((struct-name (format-secret-symbol name 'script-data)))
      (destructuring-bind (&key ((:for (instance-base-type)) '(%godot:object))
                             ((:level (level)) '(nil))
                             ((:path (path)) '(nil)))
          (a:alist-plist opts)
        `(progn
           (cffi:defcstruct ,struct-name
             ,@slots)
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (%register-script ',name
                               :path ,path
                               :base-type ',instance-base-type
                               :level ,level
                               :data-struct-name ',struct-name)))))))


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
      `((defprotocallback (,call-name pozzo-script-method)
            (,cb-instance-var
             ,cb-args-var
             ,cb-argc-var
             ,cb-ret-var
             ,cb-err-var)
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
          (%register-script-method ',method-name ',script-name
                                   :bind ,bind-name
                                   :static ,static
                                   :virtual ,virtual
                                   :pure ,pure
                                   :const ,const
                                   :call-function-name ',call-name
                                   :parameters ',parameters
                                   :return-type ',return-type))))))


;;;
;;; REGISTRY
;;;
(defclass script-registry ()
  ((script-table :initform (make-hash-table :test 'eq))
   (path-script-map :initform (make-hash-table :test 'equal))
   (address-script-map :initform (make-hash-table :test 'eql))))


(defun make-script-registry ()
  (make-instance 'script-registry))


(declaim (inline find-script-by-address))
(defun find-script-by-address (registry address)
  (with-slots (address-script-map) registry
    (gethash address address-script-map)))


(defun ensure-script-address-mapping (registry path address)
  (with-slots (path-script-map address-script-map) registry
    (a:if-let ((script (gethash path path-script-map)))
      (prog1 script
        (a:when-let ((current-script-address
                      (gethash address address-script-map)))
          (remhash current-script-address address-script-map))
        (setf (gethash address address-script-map) script
              (%address-of script) address))
      (error "No script registered at path ~A" path))))


(defun register-script (registry script-name &rest initargs
                        &key &allow-other-keys)
  (with-slots (script-table path-script-map) registry
    (unless (gethash script-name script-table)
      (let ((script (apply #'make-instance 'extension-script
                           :name script-name
                           initargs)))
        (setf (gethash script-name script-table) script
              (gethash (%path-of script) path-script-map) script)
        script))))


(defun register-script-method (registry method-name script-name
                               &rest initargs
                               &key &allow-other-keys)
  (with-slots (script-table) registry
    (a:if-let ((script (gethash script-name script-table)))
      (apply #'add-script-method script method-name initargs)
      (error "Script ~A not found" script-name))))
