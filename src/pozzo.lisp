(cl:in-package #:pozzo)


(defclass pozzo ()
  ((godot-instance :initform nil)
   (extension-registry :initform (make-hash-table :test 'eq))
   (class-extension-table :initform (make-hash-table :test 'eq))
   (class-metadata-table :initform (make-hash-table :test 'eql))
   (wrapper-registry :initform (make-hash-table :test 'eql :size 127))

   (iter-result :initform (cffi:foreign-alloc '%godot:bool))
   (action-queue :initform (muth:make-guarded-reference (list)))
   (string-name-cache :initform (make-hash-table :test 'eq))))


(defvar *pozzo* (make-instance 'pozzo))


(defun pozzo-started-p ()
  (with-slots (godot-instance) *pozzo*
    (not (null godot-instance))))


(defun prepare-pozzo ()
  (when (pozzo-started-p)
    (error "Pozzo is already started")))


(defun initialize-pozzo-extensions ()
  (with-slots (extension-registry) *pozzo*
    (loop for extension being the hash-value of extension-registry
          do (%load-extension extension)))
  (shout "Extensions initialized"))


(defun start-pozzo (godot-instance)
  (with-slots ((this-godot-instance godot-instance)
               extension-registry)
      *pozzo*
    (when this-godot-instance
      (error "Godot instance already acquired"))
    (setf this-godot-instance godot-instance)

    (c-with ((result %godot:bool))
      (%godot:godot-instance+start godot-instance (result &))
      (when (zerop result)
        (error "Failed to start Godot instance")))
    (shout "Godot instance started")))


(defun iterate-pozzo ()
  (with-slots (godot-instance iter-result action-queue) *pozzo*
    (c-val ((iter-result %godot:bool))
      (%godot:godot-instance+iteration godot-instance (iter-result &))
      (loop for action in (muth:with-guarded-reference (action-queue)
                            (prog1 action-queue
                              (setf action-queue nil)))
            do (funcall action))
      (= iter-result 0))))


(defun stop-pozzo ()
  (with-slots (godot-instance iter-result string-name-cache) *pozzo*
    (cffi:foreign-free iter-result)
    (setf godot-instance nil)
    (loop for string-name-ptr being the hash-value of string-name-cache
          do (destroy-godot-string-name string-name-ptr)
             (memfree string-name-ptr))
    (clrhash string-name-cache)))


(defun %push-action (action)
  (with-slots (action-queue) *pozzo*
    (unless (pozzo-started-p)
      (error "Pozzo is offline: cannot enqueue an action"))
    (muth:with-guarded-reference (action-queue)
      (push action action-queue))))


(defmacro do-by-pozzo (() &body body)
  `(%push-action
    (lambda () ,@body)))


;; FIXME: make sure that symbols here such as classes and methods are separate:
;; technically class and method can have the same name, but bind string-name name
;; could be different
(defmacro %cache-string-name (symbol string &optional case)
  (a:once-only (symbol string)
    `(with-slots (string-name-cache) *pozzo*
       ;; we use symbol to allow fastest lookup with 'eq test
       ;; this leads to slight overhead for symbols with the same symbol-name
       ;; but it's 8 bytes per synonym, because Godot also keeps track of string-name synonyms
       (a:if-let ((cached (gethash ,symbol string-name-cache)))
         (the cffi:foreign-pointer
              cached)
         (let ((string-name-ptr (memalloc '%godot:string-name)))
           (initialize-godot-string-name string-name-ptr (the string ,string) ,@(when case (list case)))
           (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
             ;; we ignore SAP to pointer conversion (boxing) here
             ;; it's gonna happen once per cached symbol - no problem
             (setf (gethash ,symbol string-name-cache) string-name-ptr)))))))


(declaim (inline symbol-string-name))
(defun symbol-string-name (symbol)
  (%cache-string-name symbol (symbol-name symbol) :snake))


(declaim (inline class-string-name))
(defun class-string-name (class-name)
  (%cache-string-name class-name (get-class-bind-name class-name)))


(declaim (inline method-string-name))
(defun method-string-name (class-name method-name)
  (%cache-string-name method-name (get-method-bind-name class-name method-name)))


(defun register-extension (extension-name &rest keys &key &allow-other-keys)
  (with-slots (extension-registry) *pozzo*
    (unless (gethash extension-name extension-registry)
      (let ((extension (apply #'make-extension extension-name keys)))
        (setf (gethash extension-name extension-registry) extension)
        (when (pozzo-started-p)
          (do-by-pozzo ()
            (%load-extension extension)))))))


(defun %register-extension-class-metadata (metadata-id class)
  (with-slots (class-metadata-table) *pozzo*
    (setf (gethash metadata-id class-metadata-table) class)))


(defun %find-extension-class-by-metadata-id (metadata-id)
  (with-slots (class-metadata-table) *pozzo*
    (gethash metadata-id class-metadata-table)))


(defun find-extension-method (method-name class-name)
  (with-slots (class-extension-table extension-registry) *pozzo*
    (a:when-let ((extension-name (gethash class-name class-extension-table)))
      (a:when-let ((extension (gethash extension-name extension-registry)))
        (a:when-let ((class (gethash class-name (%class-table-of extension))))
          (gethash method-name (%method-table-of class)))))))


(defun find-extension (extension-name)
  (with-slots (extension-registry) *pozzo*
    (gethash extension-name extension-registry)))


(defun get-extension (extension-name)
  (a:if-let ((extension (find-extension extension-name)))
    extension
    (error "Extension ~A not found" extension-name)))


(cffi:defcstruct pozzo-method
  (argc :uint16))


(cffi:defcstruct pozzo-class-info
  (class-name %godot:string-name)
  (parent-name %godot:string-name))


(defun make-pozzo-class-info (class-name parent-name)
  (let ((ptr (memalloc '(:struct pozzo-class-info))))
    (c-val ((ptr (:struct pozzo-class-info)))
      (initialize-godot-string-name (ptr :class-name &)
                                    (get-class-bind-name class-name))
      (initialize-godot-string-name (ptr :parent-name &)
                                    (get-class-bind-name parent-name)))
    ptr))


(defun destroy-pozzo-class-info (ptr)
  (c-val ((ptr (:struct pozzo-class-info)))
    (destroy-godot-string-name (ptr :class-name))
    (destroy-godot-string-name (ptr :parent-name)))
  (memfree ptr))


(defprotocallback (ptrcall-extension-class-method
                   %gdext:class-method-ptr-call)
    (method-userdata class-instance-ptr args ret)
  (declare (ignore method-userdata class-instance-ptr args ret))
  (values))


(defprotocallback (call-extension-class-method
                   %gdext:class-method-call)
    (method-userdata class-instance-ptr args argc ret error)
  (declare (ignore method-userdata class-instance-ptr argc args ret error))
  (values))


(defprotocallback (create-extension-class-instance
                   %gdext:class-create-instance-2)
    (class-info notify-postinitialize-p)
  (shout-errors
   (let ((class (%find-extension-class-by-metadata-id (cffi:pointer-address class-info))))
     (c-val ((class-info (:struct pozzo-class-info)))
       (let* ((obj-ptr (%gdext:classdb-construct-object2 (class-info :parent-name &)))
              (wrapper-ptr (make-pozzo-wrapper obj-ptr (funcall (%constructor-name-of class)))))
         (%gdext:object-set-instance obj-ptr (class-info :class-name &) wrapper-ptr)
         (unless (zerop notify-postinitialize-p)
           (%godot:object+notification obj-ptr 0 0))
         obj-ptr)))))


(defprotocallback (free-extension-class-instance
                   %gdext:class-free-instance)
    (class-info instance-ptr)
  (shout-errors
   (let ((class (%find-extension-class-by-metadata-id (cffi:pointer-address class-info))))
     (c-val ((instance-ptr (:struct pozzo-wrapper)))
       (funcall (%destructor-name-of class) (instance-ptr :p-object)))
     (destroy-pozzo-wrapper instance-ptr)))
  (values))


(defprotocallback (get-extension-class-virtual-call-data
                   %gdext:class-get-virtual-call-data-2)
    (class-info func-string-name func-hash)
  (declare (ignore func-hash))
  (shout-errors
   (a:if-let ((extension-class (%find-extension-class-by-metadata-id (cffi:pointer-address class-info))))
     (let ((lisp-name (godot-string-name-to-lisp func-string-name)))
       (a:if-let ((callback-name (gethash lisp-name (%vcall-table-of extension-class))))
         (get-protocallback callback-name)
         (cffi:null-pointer)))
     (cffi:null-pointer))))


(defprotocallback (call-extension-class-virtual-with-data
                   %gdext:class-call-virtual-with-data)
    (instance-ptr func-string-name fuptr args ret)
  (funcall-prototype fuptr %gdext:class-call-virtual-with-data
                     instance-ptr func-string-name (cffi:null-pointer) args ret)
  (values))


(defun initialize-extension-level (extension-name
                                   class-library-ptr
                                   init-level
                                   init-fu)
  (let ((extension (get-extension extension-name)))
    (shout "Extension ~S: ~A" (%name-of extension) init-level)

    (with-slots (extension-registry) *pozzo*
      (do-extension-classes (extension-class extension :level init-level)
        (c-with ((creation-info %gdext:class-creation-info-5))
          (let ((class-info (make-pozzo-class-info (%name-of extension-class)
                                                   (%parent-name-of extension-class))))
            (setf (creation-info :is-virtual) 0
                  (creation-info :is-abstract) 0
                  (creation-info :is-exposed) 1
                  (creation-info :is-runtime) 0
                  (creation-info :icon-path) (cffi:null-pointer)
                  (creation-info :set-func) (cffi:null-pointer)
                  (creation-info :get-func) (cffi:null-pointer)
                  (creation-info :get-property-list-func) (cffi:null-pointer)
                  (creation-info :free-property-list-func) (cffi:null-pointer)
                  (creation-info :property-can-revert-func) (cffi:null-pointer)
                  (creation-info :property-get-revert-func) (cffi:null-pointer)
                  (creation-info :validate-property-func) (cffi:null-pointer)
                  (creation-info :notification-func) (cffi:null-pointer)
                  (creation-info :to-string-func) (cffi:null-pointer)
                  (creation-info :reference-func) (cffi:null-pointer)
                  (creation-info :unreference-func) (cffi:null-pointer)
                  (creation-info :create-instance-func) (get-protocallback 'create-extension-class-instance)
                  (creation-info :free-instance-func) (get-protocallback 'free-extension-class-instance)
                  (creation-info :recreate-instance-func) (cffi:null-pointer)
                  (creation-info :get-virtual-func) (cffi:null-pointer)
                  (creation-info :get-virtual-call-data-func) (get-protocallback 'get-extension-class-virtual-call-data)
                  (creation-info :call-virtual-with-data-func) (get-protocallback 'call-extension-class-virtual-with-data)
                  (creation-info :class-userdata) class-info)
            (c-val ((class-info (:struct pozzo-class-info)))
              (%register-extension-class-metadata (cffi:pointer-address (class-info &)) extension-class)
              (%gdext:classdb-register-extension-class5 class-library-ptr
                                                        (class-info :class-name &)
                                                        (class-info :parent-name &)
                                                        (creation-info &)))))
        (loop for method being the hash-value of (%method-table-of extension-class)
              unless (virtualp method)
                do (%register-method method (%name-of extension-class) extension))
        (loop for property being the hash-value of (%property-table-of extension-class)
              do (%register-property property (%name-of extension-class) extension))
        (loop for signal-name being the hash-key of (%signal-table-of extension-class)
                using (hash-value signal-properties)
              do (%register-signal signal-name signal-properties (%name-of extension-class) extension))))
    (when init-fu
      (funcall init-fu extension-name (level->pozzo init-level))))
  (values))


(defun release-extension-level (extension-name
                                class-library-ptr
                                deinit-level
                                deinit-fu)
  (let ((extension (get-extension extension-name)))
    (do-extension-classes (extension-class extension :level deinit-level)
      ;; FIXME: release class metadata
      (let ((class-string-name (class-string-name (%name-of extension-class))))
        (c-with ((class-exists %godot:bool))
          (%godot:class-db+class-exists (%godot:class-db)
                                        (class-exists &)
                                        class-string-name)
          (unless (zerop class-exists)
            (%gdext:classdb-unregister-extension-class class-library-ptr
                                                       class-string-name)))))
    (when deinit-fu
      (funcall deinit-fu extension-name (level->pozzo deinit-level))))
  (values))


(defun initialize-extension (extension-name class-library-ptr init-struct)
  (let ((extension (get-extension extension-name)))
    (%update-class-library-pointer class-library-ptr extension)
    (c-val ((init-struct %gdext:initialization))
      (setf (init-struct :minimum-initialization-level) (%level-of extension)
            (init-struct :userdata) class-library-ptr
            (init-struct :initialize) (get-protocallback (level-initializer-name-of extension))
            (init-struct :deinitialize) (get-protocallback (level-deinitializer-name-of extension)))))
  t)


(defun %load-extension (extension)
  (c-with ((result %godot:gdextension-manager+load-status))
    (with-godot-string (path-ptr (%path-of extension))
      (%godot:gdextension-manager+load-extension-from-function (%godot:gdextension-manager)
                                                               (result &)
                                                               path-ptr
                                                               (get-protocallback
                                                                (initializer-name-of extension))))
    (unless (eq result :ok)
      (error "Failed to load extension: ~A" result))))


(defun %register-method (extension-class-method class-name extension)
  (a:if-let ((extension-class (gethash class-name (%class-table-of extension))))
    (let* ((method-parameters (parameters-of extension-class-method))
           (return-type (return-type-of extension-class-method))
           (argc (length method-parameters)))
      (let ((class-string-name (class-string-name (%name-of extension-class))))
        (c-with ((method-info %gdext:class-method-info)
                 (arguments-info %gdext:property-info :count argc)
                 (arguments-metadata %gdext:class-method-argument-metadata :count argc)
                 (return-type-info %gdext:property-info)
                 (return-type-metadata %gdext:class-method-argument-metadata))
          (loop for parameter in method-parameters
                for i from 0
                do (initialize-godot-property (arguments-info i &)
                                              (%name-of parameter)
                                              (variant-kind-of parameter))
                   (setf (arguments-metadata i) :none))
          (unless (eq :nil (variant-kind-of return-type))
            (initialize-godot-property (return-type-info &)
                                       ""
                                       (variant-kind-of return-type))
            (setf
             return-type-metadata
             (case return-type
               (%godot:int :int-is-int64)
               (%godot:float :real-is-double)
               (t :none))))
          (let ((method-string-name (method-string-name class-name (%name-of extension-class-method))))
            (shout "Registering method ~A of class ~A"
                   (godot-string-name-to-lisp method-string-name)
                   (godot-string-name-to-lisp class-string-name))
            (setf (method-info :name) method-string-name
                  (method-info :method-userdata) (cffi:null-pointer)
                  (method-info :call-func) (if (purep extension-class-method)
                                               (cffi:null-pointer)
                                               (get-protocallback
                                                (call-function-name-of extension-class-method)))
                  (method-info :ptrcall-func) (if (purep extension-class-method)
                                                  (cffi:null-pointer)
                                                  (get-protocallback
                                                   (ptrcall-function-name-of extension-class-method)))
                  (method-info :method-flags) (cffi:foreign-bitfield-value '%gdext:class-method-flags
                                                                           :flags-default)
                  (method-info :has-return-value) (if (eq :nil (variant-kind-of return-type)) 0 1)
                  (method-info :return-value-info) (if (eq :nil (variant-kind-of return-type))
                                                       (cffi:null-pointer)
                                                       (return-type-info &))
                  (method-info :return-value-metadata) (if (eq :nil (variant-kind-of return-type))
                                                           :none
                                                           return-type-metadata)
                  (method-info :argument-count) argc
                  (method-info :arguments-info) (arguments-info &)
                  (method-info :arguments-metadata) (arguments-metadata &)
                  (method-info :default-argument-count) 0
                  (method-info :default-arguments) (cffi:null-pointer))

            (%gdext:classdb-register-extension-class-method
             (class-library-pointer-of extension)
             class-string-name
             (method-info &))

            (loop for i from 0 below (length method-parameters)
                  do (release-godot-property (arguments-info i &)))
            (unless (eq :nil (variant-kind-of return-type))
              (release-godot-property (return-type-info &)))))))
    (error "Class ~A not found" class-name)))


(defun %register-property (extension-property class-name extension)
  (a:if-let ((extension-class (gethash class-name (%class-table-of extension))))
    (let ((class-string-name (class-string-name class-name)))
      (c-with ((property-info %gdext:property-info))
        (initialize-godot-property (property-info &)
                                   (%name-of extension-property)
                                   (variant-kind-of extension-property))
        (let ((writer-string-name (method-string-name class-name
                                                      (writer-name-of extension-property)))
              (reader-string-name (method-string-name
                                   class-name
                                   (reader-name-of extension-property))))
          (shout "Registering property ~A of ~A (reader: ~A, writer: ~A)"
                 (godot-string-name-to-lisp (property-info :name))
                 (godot-string-name-to-lisp class-string-name)
                 (godot-string-name-to-lisp reader-string-name)
                 (godot-string-name-to-lisp writer-string-name))
          (%gdext:classdb-register-extension-class-property
           (class-library-pointer-of extension)
           class-string-name
           (property-info &)
           writer-string-name
           reader-string-name))
        (release-godot-property (property-info &))))
    (error "Class ~A not found" class-name)))


(defun %register-signal (signal-name signal-properties class-name extension)
  (a:if-let ((extension-class (gethash class-name (%class-table-of extension))))
    (let ((class-string-name (class-string-name class-name))
          (signal-string-name (symbol-string-name signal-name)))
      (let ((prop-count (length signal-properties)))
        (c-with ((properties-info %gdext:property-info :count prop-count))
          (loop for property in signal-properties
                for i from 0
                do (initialize-godot-property (properties-info i &)
                                              (%name-of property)
                                              (variant-kind-of property)))
          (shout "Registering signal ~A of ~A"
                 (godot-string-name-to-lisp signal-string-name)
                 (godot-string-name-to-lisp class-string-name))
          (%gdext:classdb-register-extension-class-signal
           (class-library-pointer-of extension)
           class-string-name
           signal-string-name
           (properties-info &)
           prop-count)
          (loop for i from 0 below prop-count
                do (release-godot-property (properties-info i &))))))
    (error "Class ~A not found" class-name)))


(defun %unload-extension (extension)
  (c-with ((result %godot:gdextension-manager+load-status))
    (with-godot-string (path-ptr (%path-of extension))
      (%godot:gdextension-manager+unload-extension (%godot:gdextension-manager)
                                                   (result &)
                                                   path-ptr))
    (unless (eq result :ok)
      (error "Failed to unload extension: ~A" result))))


(defun register-extension-class (class-name extension-name &rest initargs
                                 &key bind level &allow-other-keys)
  (with-slots (extension-registry class-extension-table) *pozzo*
    (a:if-let ((extension (gethash extension-name extension-registry)))
      (when (apply #'%add-extension-class extension class-name initargs)
        (register-class-bind-mapping class-name bind)
        (setf (gethash class-name class-extension-table) extension-name)
        (when (pozzo-started-p)
          (do-by-pozzo ()
            (%unload-extension extension)
            (%load-extension extension))))
      (error "Extension ~A not found" extension-name))))


(defun register-extension-class-method (method-name class-name &rest keys &key bind &allow-other-keys)
  (with-slots (extension-registry class-extension-table) *pozzo*
    (a:if-let ((extension-name (gethash class-name class-extension-table)))
      (let ((extension (gethash extension-name extension-registry)))
        (a:when-let ((new-extension-class-method (apply #'%add-extension-class-method extension class-name method-name keys)))
          (register-method-bind-mapping class-name method-name bind)
          (when (pozzo-started-p)
            (do-by-pozzo ()
              (unless (virtualp new-extension-class-method)
                (%register-method new-extension-class-method class-name extension))))))
      (error "Class ~A not found" class-name))))


(defun register-extension-class-signal (signal-name class-name &rest keys &key properties &allow-other-keys)
  (with-slots (extension-registry class-extension-table) *pozzo*
    (a:if-let ((extension-name (gethash class-name class-extension-table)))
      (let ((extension (gethash extension-name extension-registry)))
        (when (apply #'%add-extension-class-signal extension class-name signal-name keys)
          (when (pozzo-started-p)
            (do-by-pozzo ()
              (%register-signal signal-name properties class-name extension)))))
      (error "Class ~A not found" class-name))))


(declaim (inline emit-signal))
(defun emit-signal (instance signal-name &rest variants)
  (c-with ((signal-name-variant %godot:variant)
           (result-variant %godot:variant))
    (initialize-variant-from-value (signal-name-variant &) (symbol-string-name signal-name) '%godot:string-name)

    (apply #'%godot:object+emit-signal (unwrap instance) (result-variant &) (signal-name-variant &) variants)

    (prog1 (c-ref (get-variant-internal-ptr result-variant) %godot:error)
      (release-variant (signal-name-variant &))
      (release-variant (result-variant &)))))


(define-compiler-macro emit-signal (instance signal-name &rest variants)
  (a:with-gensyms (signal-name-variant result-variant)
    `(c-with ((,signal-name-variant %godot:variant)
              (,result-variant %godot:variant))
       (initialize-variant-from-value (,signal-name-variant &) (symbol-string-name ,signal-name) '%godot:string-name)

       (%godot:object+emit-signal (unwrap ,instance) (,result-variant &) (,signal-name-variant &) ,@variants)

       (prog1 (c-ref (get-variant-internal-ptr ,result-variant) %godot:error)
         (release-variant (,signal-name-variant &))
         (release-variant (,result-variant &))))))


(declaim (inline construct))
(defun construct (class-name)
  (with-godot-string-name (class-string-name (the string (get-class-bind-name class-name)))
    (let ((obj-ptr (%gdext:classdb-construct-object2 class-string-name)))
      (%godot:object+notification obj-ptr 0 0)
      obj-ptr)))


(declaim (inline destruct))
(defun destruct (object-ptr)
  (%gdext:object-destroy object-ptr))
