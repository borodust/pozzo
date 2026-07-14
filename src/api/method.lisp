(cl:in-package #:pozzo)


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


(declaim (inline initialize-variant-from-value))
(defun initialize-variant-from-value (uninitialized-variant-ptr value class-name)
  (let* ((variant-kind (godot-extension-variant-kind class-name))
         (variant-ctor (%gdext:get-variant-from-type-constructor variant-kind)))
    (flet ((%funcall-ctor (variant-ctor uninitialized-variant-ptr value-ptr)
             (funcall-prototype variant-ctor %gdext:variant-from-type-constructor-func
                                uninitialized-variant-ptr
                                value-ptr)))
      (declare (inline %funcall-ctor))
      (cond
        ((eq variant-kind :object)
         (c-with ((ptr :pointer))
           (setf ptr value)
           (%funcall-ctor variant-ctor uninitialized-variant-ptr (ptr &))))

        ((cffi:pointerp value)
         (%funcall-ctor variant-ctor uninitialized-variant-ptr value))

        ((eq class-name '%godot:bool)
         (c-with ((val %godot:bool))
           (setf val value)
           (%funcall-ctor variant-ctor uninitialized-variant-ptr (val &))))

        ((eq class-name '%godot:int)
         (c-with ((val %godot:int))
           (setf val (truncate value))
           (%funcall-ctor variant-ctor uninitialized-variant-ptr (val &))))

        ((eq class-name '%godot:float)
         (c-with ((val %godot:float))
           (setf val (float value 0d0))
           (%funcall-ctor variant-ctor uninitialized-variant-ptr (val &))))

        ((eq class-name '%godot:string)
         (with-godot-string (val (the string value))
           (%funcall-ctor variant-ctor uninitialized-variant-ptr val)))

        ((eq class-name '%godot:string-name)
         (with-godot-string-name (val (the string value))
           (%funcall-ctor variant-ctor uninitialized-variant-ptr val)))

        (t (error "Unexpected variant initialization value ~A for variant kind ~A" value variant-kind))))))


(define-compiler-macro initialize-variant-from-value (&whole whole uninitialized-variant-ptr value class-name)
  (let* ((class-name (when (and (listp class-name)
                                (eq 'quote (first class-name)))
                       (second class-name)))
         (variant-kind (when class-name
                         (godot-extension-variant-kind class-name))))
    (a:with-gensyms (variant-ctor value-ptr)
      (flet ((%expand-direct (value)
               `(funcall-prototype (%gdext:get-variant-from-type-constructor :object)
                                   %gdext:variant-from-type-constructor-func
                                   ,uninitialized-variant-ptr
                                   ,value))
             (%expand-object ()
               `(c-with ((,value-ptr :pointer))
                  (setf ,value-ptr ,value)
                  (funcall-prototype (%gdext:get-variant-from-type-constructor :object)
                                     %gdext:variant-from-type-constructor-func
                                     ,uninitialized-variant-ptr
                                     (,value-ptr &))))

             (%expand-primitive (type value)
               `(c-with ((,value-ptr ,type))
                  (setf ,value-ptr ,value)
                  (funcall-prototype ,variant-ctor
                                     %gdext:variant-from-type-constructor-func
                                     ,uninitialized-variant-ptr
                                     (,value-ptr &))))
             (%expand-string (macro value)
               `(,macro (,value-ptr (the string ,value))
                        (funcall-prototype ,variant-ctor
                                           %gdext:variant-from-type-constructor-func
                                           ,uninitialized-variant-ptr
                                           ,value-ptr))))
        (cond
          ((eq variant-kind :object)
           (%expand-object))

          ((member class-name '(%godot:bool
                                %godot:int
                                %godot:float
                                %godot:string
                                %godot:string-name))
           (a:once-only (value)
             `(let ((,variant-ctor (%gdext:get-variant-from-type-constructor ,variant-kind)))
                (if (cffi:pointerp ,value)
                    ,(%expand-direct value)
                    ,(ecase class-name
                       ((%godot:bool %godot:int %godot:float) (%expand-primitive class-name value))
                       (%godot:string (%expand-string 'with-godot-string value))
                       (%godot:string-name (%expand-string 'with-godot-string-name value)))))))

          (t whole))))))


(declaim (inline release-variant))
(defun release-variant (variant-ptr)
  (%gdext:variant-destroy variant-ptr))


(declaim (inline get-variant-internal-ptr))
(defun get-variant-internal-ptr (variant-ptr)
  (let* ((variant-kind (%gdext:variant-get-type variant-ptr))
         (func-ptr (%gdext:variant-get-ptr-internal-getter variant-kind)))
    (funcall-prototype func-ptr %gdext:variant-get-internal-ptr-func
                                   variant-ptr)))


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
      (multiple-value-bind (forms decls docstr)
          (a:parse-body body :documentation t)
        (let ((class (get-extension-class class-name))
              (parameters (rest parameters)))
          `(progn
             (define-prototype-method ,class-name ,name
                 (,instance-var ,@parameters)
                 ,return-type
                 ,docstr
                 ,decls
               ,@forms)
             ,@(expand-prototype-method-wrappers class
                                                 name
                                                 parameters
                                                 return-type
                                                 :virtual virtual
                                                 :pure pure
                                                 :static static
                                                 :const const)))))))
