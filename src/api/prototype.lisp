(cl:in-package #:pozzo)


(defclass extension-prototype ()
  ((property-table :initform (make-hash-table :test 'eq)
                   :reader %property-table-of)
   (signal-table :initform (make-hash-table :test 'eq)
                 :reader %signal-table-of)
   (method-table :initform (make-hash-table :test 'eq)
                 :reader %method-table-of)))

;;;
;;; PROTO METHOD
;;;
(defmacro preturn (value)
  (declare (ignore value))
  (error "This is a stub. Never call this macro outside of the lexical scope of defpmethod's body"))


(defmacro preturn-with ((result-ptr-var) &body body)
  (declare (ignore result-ptr-var body))
  (error "This is a stub. Never call this macro outside of the lexical scope of defpmethod's body"))


(defun expand-preturn (block-name result-ptr-var result-type result-value-var)
  `(progn
     (unless (cffi:null-pointer-p ,result-ptr-var)
       (setf (c-ref ,result-ptr-var ,result-type) ,result-value-var))
     (return-from ,block-name (values))))


(defun expand-preturn-with (block-name result-ptr-var result-value-ptr-var result-value-body)
  `(progn
     (unless (cffi:null-pointer-p ,result-ptr-var)
       (let ((,result-value-ptr-var ,result-ptr-var))
         ,@result-value-body))
     (return-from ,block-name (values))))


(defun format-full-method-name (prototype-name method-name)
  (format-secret-symbol prototype-name method-name '-method))


(defun funcall-pmethod (proto-method-name instance-ptr result-ptr &rest args)
  (destructuring-bind (prototype-name method-name) proto-method-name
    (apply (format-full-method-name prototype-name method-name)
           instance-ptr result-ptr args)))


(define-compiler-macro funcall-pmethod (&whole whole proto-method-name instance-ptr result-ptr
                                               &rest args)
  (if (and (eq 'quote (first proto-method-name))
           (listp (second proto-method-name)))
      (destructuring-bind (prototype-name method-name)
          (second proto-method-name)
        `(,(format-full-method-name prototype-name method-name)
          ,instance-ptr ,result-ptr ,@args))
      whole))


(defmacro define-prototype-method (prototype-name method-name parameters return-type docstring declarations &body body)
  (destructuring-bind (instance-var &rest parameters) parameters
    (let ((parameter-names (mapcar #'first parameters)))
      (a:with-gensyms (result-var)
        (let ((fu-name (format-full-method-name prototype-name method-name))
              (ignored (loop for decl in declarations
                             for decl-substance = (second decl)
                             when (eq 'ignore (first decl-substance))
                               append (rest decl-substance))))
          `(progn
             (declaim (inline ,fu-name))
             (defun ,fu-name (,instance-var ,result-var ,@parameter-names)
               ,@(when docstring (list docstring))
               (declare (ignorable ,instance-var ,result-var))
               ,@declarations
               (c-val ,(loop for param-decl in parameters
                             unless (member (first param-decl) ignored)
                               collect param-decl)
                 ,@(if (eq :void return-type)
                       body
                       `((macrolet ((pozzo::preturn (result)
                                      (expand-preturn ',fu-name ',result-var ',return-type result))
                                    (pozzo::preturn-with ((result-ptr-var) &body body)
                                      (expand-preturn-with ',fu-name ',result-var result-ptr-var body)))
                           ,@body))))
               (values))))))))


(defgeneric expand-prototype-method-wrappers (prototype
                                              method-name
                                              parameters
                                              return-type
                                              &key &allow-other-keys))


(defmacro with-variant-arguments ((&rest variant-names) (cb-args-var cb-argc-var cb-err-var &rest parameters)
                                  &body body)
  (destructuring-bind (&key arg-names arg-types arg-values variant-values &allow-other-keys)
      (prepare-arguments cb-args-var parameters)
    (a:with-gensyms (block-name)
      `(block ,block-name
         (c-val ((,cb-err-var %gdext:call-error))
           (setf (,cb-err-var :error) :ok)

           (when (< ,cb-argc-var ,(length arg-names))
             (setf (,cb-err-var :error) :error-too-few-arguments
                   (,cb-err-var :expected) ,(length arg-names))
             (return-from ,block-name (values)))

           (when (> ,cb-argc-var ,(length arg-names))
             (setf (,cb-err-var :error) :error-too-many-arguments
                   (,cb-err-var :expected) ,(length arg-names))
             (return-from ,block-name (values)))
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
                              (return-from ,block-name (values))))
           (let (,@(mapcar #'list variant-names variant-values))
             ,@body))))))


(defmacro with-variant-result ((result-var) (cb-ret-var return-type) &body body)
  (a:with-gensyms (c-result-var)
    (if (eq :void return-type)
        `(let ((,result-var (cffi:null-pointer)))
           (declare (ignorable ,result-var))
           ,@body)
        `(if (cffi:null-pointer-p ,cb-ret-var)
             (let ((,result-var (cffi:null-pointer)))
               (declare (ignorable ,result-var))
               ,@body)
             (c-with ((,c-result-var ,return-type))
               (let ((,result-var (,c-result-var &)))
                 (unwind-protect
                      (progn ,@body)
                   (initialize-variant-from-value ,cb-ret-var
                                                  ,result-var
                                                  ',return-type))))))))
