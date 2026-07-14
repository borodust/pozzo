(cl:in-package :pozzo)


(defvar *pozzo-class-bind-map* (make-hash-table :test 'eq))


(defun register-class-bind-mapping (class-name bind-name)
  (a:if-let ((bind-info (gethash class-name *pozzo-class-bind-map*)))
    (setf (car bind-info) bind-name)
    (setf (gethash class-name *pozzo-class-bind-map*) (cons bind-name (make-hash-table :test 'eq)))))


(defun register-method-bind-mapping (class-name method-name bind-name)
  (a:if-let ((bind-info (gethash class-name *pozzo-class-bind-map*)))
    (let ((method-bind-map (cdr bind-info)))
      (setf (gethash method-name method-bind-map) bind-name))
    (error "Pozzo class ~A bind name not found" class-name)))


(defun get-class-bind-name (class-name)
  (a:if-let ((bind-info (gethash class-name *pozzo-class-bind-map*)))
    (car bind-info)
    (godot-extension-bind-name class-name)))


(defun enum-registered-p (enum-name)
  (typep (ignore-errors (cffi::parse-type enum-name)) 'cffi::foreign-enum))


(defun get-class-variant-kind (class-name)
  (cond
    ((gethash class-name *pozzo-class-bind-map*)
     :object)
    ((or (eq class-name :pointer) (and (listp class-name) (eq :pointer (first class-name))))
     :int)
    ((eq :void class-name)
     :nil)
    ((enum-registered-p class-name)
     :int)
    (t
     (godot-extension-variant-kind class-name))))


(defun get-method-bind-name (class-name method-name)
  (a:if-let ((bind-info (gethash class-name *pozzo-class-bind-map*)))
    (a:if-let ((bind-name (gethash method-name (cdr bind-info))))
      bind-name
      (error "Pozzo method ~A of class ~A bind name not found" method-name class-name))
    (godot-extension-method-bind-name class-name method-name)))


(declaim (inline memalloc))
(defun memalloc (type &optional (count 1))
  (%gdext:mem-alloc2 (* (cffi:foreign-type-size type) count) 0))


(define-compiler-macro memalloc (type &optional (count 1))
  (flet ((%expand-body (size)
           `(%gdext:mem-alloc2 ,(if (and (numberp size)
                                         (numberp count))
                                    (* size count)
                                    `(* ,size ,count))
                               0)))
    (cond
      ((and (listp type)
            (eq 'quote (first type)))
       (%expand-body (cffi:foreign-type-size (second type))))

      ((keywordp type)
       (%expand-body (cffi:foreign-type-size type)))

      (t (%expand-body `(cffi:foreign-type-size ,type))))))


(cffi:defcfun (%memset "memset") (:pointer :void)
  (data-ptr (:pointer :void))
  (value :int)
  (byte-count :size))


(defun memzero (ptr type &optional count)
  (%memset ptr 0 (* (cffi:foreign-type-size type) count)))


(define-compiler-macro memzero (&whole whole ptr type &optional count)
  (labels ((%expand-size (type)
             (if (numberp count)
                 (* (cffi:foreign-type-size type) count)
                 `(* ,(cffi:foreign-type-size type) ,count)))
           (%expand-body (type)
             `(%memset ,ptr 0 ,(%expand-size type))))
    (cond
      ((and (listp type)
            (eq 'quote (first type)))
       (%expand-body (second type)))

      ((keywordp type)
       (%expand-body type))

      (t whole))))


(defun memallocz (type &optional (count 1))
  (let ((ptr (memalloc type count)))
    (memzero ptr type)
    ptr))


(define-compiler-macro memallocz (&whole whole type &optional (count 1))
  (if (or (and (listp type)
               (eq 'quote (first type)))
          (keywordp type))
      (a:with-gensyms (ptr)
        (if (numberp count)
            `(let ((,ptr (memalloc ,type ,count)))
               (memzero ,ptr ,type ,count)
               ,ptr)
            (a:once-only (count)
              `(let ((,ptr (memalloc ,type ,count)))
                 (memzero ,ptr ,type ,count)
                 ,ptr))))
      whole))


(declaim (inline memfree))
(defun memfree (ptr)
  (%gdext:mem-free2 ptr 0))


(define-compiler-macro memfree (ptr)
  `(%gdext:mem-free2 ,ptr 0))


(defmacro c-with ((&rest bindings) &body body)
  (labels ((%expand-c-with (bindings body)
             (if bindings
                 (destructuring-bind (var type &key count zero) (first bindings)
                   `(let ((,var (memalloc ',type ,@(when count (list count)))))
                      ,@(when zero
                          `(memzero ,var ,type ,(or count 1)))
                      (unwind-protect
                           (c-val ((,var ,type))
                             ,(%expand-c-with (rest bindings) body))
                        (memfree ,var))))
                 `(progn ,@body))))
    (%expand-c-with bindings body)))


(defun shout (control-string &rest args)
  (apply #'format *standard-output* (concatenate 'string "~&" control-string) args)
  (finish-output *standard-output*))


;;;
;;; GODOT STRING
;;;
(declaim (inline initialize-godot-string))
(defun initialize-godot-string (ptr &optional lisp-string case)
  (if lisp-string
      (cffi:with-foreign-string (content-ptr (string lisp-string) :encoding :utf-8)
        (if case
            (c-with ((tmp %godot:string))
              (%gdext:string-new-with-utf8-chars (tmp &) content-ptr)
              (%godot:make-string ptr)
              (case case
                (:pascal (%godot:string+to-pascal-case (tmp &) ptr))
                (:snake (%godot:string+to-snake-case (tmp &) ptr))
                (:kebab (%godot:string+to-kebab-case (tmp &) ptr))
                (:camel (%godot:string+to-camel-case (tmp &) ptr)))
              (%godot:destroy-string (tmp &)))
            (%gdext:string-new-with-utf8-chars ptr content-ptr)))
      (%godot:make-string ptr)))


(declaim (inline destroy-godot-string))
(defun destroy-godot-string (ptr)
  (%godot:destroy-string ptr))


(defmacro with-godot-string ((var &optional lisp-string case) &body body)
  `(c-with ((,var %godot:string))
     (initialize-godot-string (,var &) ,lisp-string ,case)
     (unwind-protect
          (progn ,@body)
       (destroy-godot-string (,var &)))))


(defmacro with-godot-strings (bindings &body body)
  (if bindings
      (destructuring-bind (var &optional lisp-string case) (a:ensure-list (first bindings))
        `(with-godot-string (,var ,lisp-string ,case)
           (with-godot-strings ,(rest bindings)
             ,@body)))
      `(progn ,@body)))

;;;
;;; GODOT STRING NAME
;;;
(declaim (inline initialize-godot-string-name))
(defun initialize-godot-string-name (ptr &optional lisp-string case)
  (if lisp-string
      (cffi:with-foreign-string (content-ptr (string lisp-string) :encoding :utf-8)
        (if case
            (c-with ((tmp %godot:string))
              (initialize-godot-string (tmp &) lisp-string case)
              (%godot:make-string-name@2 ptr (tmp &))
              (%godot:destroy-string (tmp &)))
            (%gdext:string-name-new-with-utf8-chars ptr content-ptr)))
      (%godot:make-string-name ptr)))


(declaim (inline destroy-godot-string-name))
(defun destroy-godot-string-name (ptr)
  (%godot:destroy-string-name ptr))


(defmacro with-godot-string-name ((var &optional lisp-string case) &body body)
  `(c-with ((,var %godot:string-name))
     (initialize-godot-string-name (,var &) ,lisp-string ,case)
     (unwind-protect
          (progn ,@body)
       (destroy-godot-string-name (,var &)))))


(defmacro with-godot-string-names (bindings &body body)
  (if bindings
      (destructuring-bind (var &optional lisp-string case) (a:ensure-list (first bindings))
        `(with-godot-string-name (,var ,lisp-string ,case)
           (with-godot-string-names ,(rest bindings)
             ,@body)))
      `(progn ,@body)))


(declaim (inline godot-string-name-to-lisp))
(defun godot-string-name-to-lisp (ptr)
  (c-with ((godot-string %godot:string))
    (%godot:make-string@2 (godot-string &) ptr)
    (unwind-protect
         (godot-string-to-lisp (godot-string &))
      (destroy-godot-string (godot-string &)))))


(defun initialize-godot-property (prop name variant-type
                                  &key class hint-kind hint usage)
  (c-val ((prop %gdext:property-info))
    (setf (prop :type) variant-type)

    (setf (prop :name) (memalloc '%godot:string-name))
    (initialize-godot-string-name (prop :name) (string name) :snake)

    (setf (prop :class-name) (memalloc '%godot:string-name))
    (initialize-godot-string-name (prop :class-name)
                                  (when class
                                    (get-class-bind-name class)))

    (setf (prop :hint) (cffi:foreign-enum-value '%godot:property-hint
                                                (or hint-kind :none)))
    (setf (prop :hint-string) (memalloc '%godot:string))
    (initialize-godot-string (prop :hint-string) hint)

    (setf (prop :usage) (cffi:foreign-bitfield-value '%godot:property-usage-flags
                                                     (or (a:ensure-list usage)
                                                         (list :default)))))
  prop)


(declaim (inline make-godot-property))
(defun make-godot-property (name variant-type &rest keys &key &allow-other-keys)
  (let ((prop (memalloc '%gdext:property-info)))
    (apply #'initialize-godot-property prop name variant-type keys)
    prop))


(declaim (inline release-godot-property))
(defun release-godot-property (prop)
  (c-val ((prop %gdext:property-info))
    (destroy-godot-string-name (prop :name))
    (memfree (prop :name))

    (destroy-godot-string-name (prop :class-name))
    (memfree (prop :class-name))

    (destroy-godot-string (prop :hint-string))
    (memfree (prop :hint-string))))


(declaim (inline destroy-godot-property))
(defun destroy-godot-property (prop)
  (release-godot-property prop)
  (memfree prop))

;;;
;;; PACKED STRING ARRAY
;;;
(declaim (inline initialize-godot-packed-string-array))
(defun initialize-godot-packed-string-array (ptr &rest strings)
  (%godot:make-packed-string-array ptr)
  (dolist (str strings)
    (with-godot-string (gstr str)
      (%godot:packed-string-array+push-back ptr (cffi:null-pointer) gstr))))


(define-compiler-macro initialize-godot-packed-string-array (ptr &rest strings)
  (a:once-only (ptr)
    (a:with-gensyms (gstr res)
      `(progn
         (%godot:make-packed-string-array ,ptr)
         ,@(loop for str in strings
                 collect `(with-godot-string (,gstr ,str)
                            (c-with ((,res %godot:bool))
                              (%godot:packed-string-array+push-back ,ptr
                                                                    (,res &)
                                                                    ,gstr))))))))


(declaim (inline make-godot-packed-string-array))
(defun make-godot-packed-string-array (&rest strings)
  (let ((ptr (memalloc '%godot:string)))
    (apply #'initialize-godot-packed-string-array ptr strings)
    ptr))


(declaim (inline destroy-godot-packed-string-array))
(defun destroy-godot-packed-string-array (ptr)
  (%godot:destroy-packed-string-array ptr)
  (memfree ptr)
  (values))


(defmacro shout-errors (&body body)
  (a:with-gensyms (retry drop-out)
    `(let (result)
       (tagbody
        start
          (flet ((,retry ()
                   (go start))
                 (,drop-out (&optional v)
                   (setf result v)
                   (go end)))
            (declare (dynamic-extent (function ,retry) (function ,drop-out)))
            (restart-bind ((retry-shout-body #',retry)
                           (skip-shout-body #',drop-out))
              (handler-bind ((serious-condition (lambda (c)
                                                  (format *debug-io* "~&")
                                                  (backtrace:print-condition c *debug-io*)
                                                  (backtrace:print-backtrace-to-stream *debug-io*)
                                                  (finish-output *debug-io*)
                                                  (break "~A" c))))
                (setf result (progn ,@body)))))
        end)
       result)))


(defun format-secret-symbol (symbol &rest postfixes)
  (a:format-symbol '%%pozzo "~A:~A~~~{~A~}" (package-name (symbol-package symbol)) symbol postfixes))


(defmacro with-variant ((var type value-ptr) &body body)
  (a:with-gensyms (variant)
    (a:once-only (value-ptr)
      `(c-with ((,variant %godot:variant))
         (initialize-variant-from-value (,variant &)
                                        ,value-ptr
                                        ',type)
         (unwind-protect
              (let ((,var (,variant &)))
                ,@body)
           (release-variant (,variant &)))))))


(defmacro with-variants (bindings &body body)
  (if bindings
      `(with-variant ,(first bindings)
         (with-variants ,(rest bindings)
           ,@body))
      `(progn ,@body)))


(declaim (inline construct))
(defun construct (class-name)
  (with-godot-string-name (class-string-name (the string (get-class-bind-name class-name)))
    (let ((obj-ptr (%gdext:classdb-construct-object2 class-string-name)))
      (%godot:object+notification obj-ptr %godot:+object+notification-postinitialize+ 0)
      obj-ptr)))


(declaim (inline destruct))
(defun destruct (object-ptr)
  (%gdext:object-destroy object-ptr))


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
