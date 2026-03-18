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
    (%gdext.util:godot-extension-bind-name class-name)))


(defun get-method-bind-name (class-name method-name)
  (a:if-let ((bind-info (gethash class-name *pozzo-class-bind-map*)))
    (a:if-let ((bind-name (gethash method-name (cdr bind-info))))
      bind-name
      (error "Pozzo method ~A of class ~A bind name not found" method-name class-name))
    (%gdext.util:godot-extension-method-bind-name class-name method-name)))


(defun memalloc (type &optional (count 1))
  (%gdext.interface:mem-alloc2 (* (cffi:foreign-type-size type) count) 0))


(define-compiler-macro memalloc (type &optional (count 1))
  (flet ((%expand-body (size)
           `(%gdext.interface:mem-alloc2 ,(if (and (numberp size)
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


(defun memfree (ptr)
  (%gdext.interface:mem-free2 ptr 0))


(define-compiler-macro memfree (ptr)
  `(%gdext.interface:mem-free2 ,ptr 0))


(defmacro c-with ((&rest bindings) &body body)
  (labels ((%expand-c-with (bindings body)
             (if bindings
                 (destructuring-bind (var type &key count) (first bindings)
                   `(let ((,var (memalloc ',type ,@(when count (list count)))))
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
              (%gdext.interface:string-new-with-utf8-chars (tmp &) content-ptr)
              (%godot:make-string ptr)
              (case case
                (:pascal (%godot:string+to-pascal-case (tmp &) ptr))
                (:snake (%godot:string+to-snake-case (tmp &) ptr))
                (:kebab (%godot:string+to-kebab-case (tmp &) ptr))
                (:camel (%godot:string+to-camel-case (tmp &) ptr)))
              (%godot:destroy-string (tmp &)))
            (%gdext.interface:string-new-with-utf8-chars ptr content-ptr)))
      (%godot:make-string ptr)))


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
            (%gdext.interface:string-name-new-with-utf8-chars ptr content-ptr)))
      (%godot:make-string-name ptr)))


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


(defun godot-string-name-to-lisp (ptr)
  (c-with ((godot-string %godot:string))
    (%godot:make-string@2 (godot-string &) ptr)
    (unwind-protect
         (godot-string-to-lisp (godot-string &))
      (destroy-godot-string (godot-string &)))))


(defun initialize-godot-property (prop name variant-type
                            &key class hint-kind hint usage)
  (c-val ((prop %gdext.types:property-info))
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


(defun make-godot-property (name variant-type &rest keys &key &allow-other-keys)
  (let ((prop (memalloc '%gdext.types:property-info)))
    (apply #'initialize-godot-property prop name variant-type keys)
    prop))


(defun release-godot-property (prop)
  (c-val ((prop %gdext.types:property-info))
    (destroy-godot-string-name (prop :name))
    (memfree (prop :name))

    (destroy-godot-string-name (prop :class-name))
    (memfree (prop :class-name))

    (destroy-godot-string (prop :hint-string))
    (memfree (prop :hint-string))))


(defun destroy-godot-property (prop)
  (release-godot-property prop)
  (memfree prop))


(defmacro shout-errors (&body body)
  `(let (result)
     (tagbody
      start
        (restart-case
            (handler-bind ((serious-condition (lambda (c)
                                                (format *debug-io* "~&")
                                                (backtrace:print-condition c *debug-io*)
                                                (backtrace:print-backtrace-to-stream *debug-io*)
                                                (finish-output *debug-io*)
                                                (break "~A" c))))
              (setf result (progn ,@body)))
          (retry-shout-body ()
            (go start))
          (skip-shout-body (&optional v)
            (setf result v)
            (go end)))
      end)
     result))


(defun format-secret-symbol (symbol &rest postfixes)
  (a:format-symbol '%%pozzo "~A:~A~~~{~A~}" (package-name (symbol-package symbol)) symbol postfixes))
