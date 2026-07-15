(cl:in-package #:pozzo)


(defclass pozzo ()
  ((godot-instance :initform nil)
   (extension-registry :initform (make-hash-table :test 'eq))
   (class-extension-table :initform (make-hash-table :test 'eq))
   (class-metadata-table :initform (make-hash-table :test 'eql))
   (wrapper-registry :initform (make-hash-table :test 'eql :size 127))
   (module-registry :initform (make-hash-table :test 'eq))

   (root-extension :initform (make-extension 'root))
   (script-registry :initform (make-script-registry))

   (resource-registry :initform (make-resource-registry))

   (opaque-script-instance-info)
   (stop-iterating-p :initform (cffi:foreign-alloc '%godot:bool))
   (action-queue :initform (muth:make-guarded-reference (list)))
   (string-name-cache :initform (make-hash-table :test 'eq))
   (opaque-script-language-object :initform (cffi:null-pointer))))


(defvar *pozzo* (make-instance 'pozzo))


(defun pozzo-started-p ()
  (with-slots (godot-instance) *pozzo*
    (not (null godot-instance))))


(defun prepare-pozzo ()
  (when (pozzo-started-p)
    (error "Pozzo is already started")))


(declaim (inline initialize-root-extension))
(defun initialize-root-extension (class-lib-ptr)
  (with-slots (root-extension) *pozzo*
    (%update-class-library-pointer class-lib-ptr root-extension)))


(declaim (inline initialize-root-extension-level))
(defun initialize-root-extension-level (class-lib-ptr init-level)
  (initialize-extension-level 'root class-lib-ptr init-level nil))


(declaim (inline deinitialize-root-extension-level))
(defun deinitialize-root-extension-level (class-lib-ptr deinit-level)
  (release-extension-level 'root class-lib-ptr deinit-level nil))


(declaim (inline initialize-extensions))
(defun initialize-extensions (init-level)
  (with-slots (extension-registry) *pozzo*
    (loop for extension being the hash-value of extension-registry
          when (eq init-level (%level-of extension))
            do (%load-extension extension)
               (shout "Extension ~A initialized" (%name-of extension)))))


(declaim (inline make-script-instance-info))
(defun make-script-instance-info ()
  (let ((info (memallocz '(:struct %gdext:script-instance-info-3))))
    (c-val ((info (:struct %gdext:script-instance-info-3)))
      (setf (info :call-func) (get-protocallback 'script-instance-call-method)
            (info :notification-func) (get-protocallback 'script-instance-notification)
            (info :has-method-func) (get-protocallback 'script-instance-has-method)
            (info :get-owner-func) (get-protocallback 'script-instance-get-owner)
            (info :get-script-func) (get-protocallback 'script-instance-get-script)
            (info :get-language-func) (get-protocallback 'script-instance-get-language)
            (info :free-func) (get-protocallback 'script-instance-free)))
    info))


(defun start-pozzo (godot-instance)
  (with-slots ((this-godot-instance godot-instance)
               extension-registry
               module-registry
               opaque-script-instance-info)
      *pozzo*
    (when this-godot-instance
      (error "Godot instance already acquired"))
    (setf this-godot-instance godot-instance
          opaque-script-instance-info (make-script-instance-info))


    (c-with ((result %godot:bool))
      (%godot:godot-instance+start godot-instance (result &))
      (unless result
        (error "Failed to start Godot instance")))
    (loop for name being the hash-key in *module-registry*
          do (let ((module (make-module name)))
               (initialize-module module *pozzo*)
               (setf (gethash name module-registry) module)))
    (shout "Godot instance started")))


(defun iterate-pozzo ()
  (with-slots (godot-instance stop-iterating-p action-queue module-registry) *pozzo*
    (c-val ((stop-iterating-p %godot:bool))
      (%godot:godot-instance+iteration godot-instance (stop-iterating-p &))
      (loop for action in (muth:with-guarded-reference (action-queue)
                            (prog1 action-queue
                              (setf action-queue nil)))
            do (funcall action))
      (loop for module being the hash-value in module-registry
            do (iterate-module module))
      (not stop-iterating-p))))


(defun stop-pozzo ()
  (with-slots (godot-instance stop-iterating-p string-name-cache module-registry) *pozzo*
    (cffi:foreign-free stop-iterating-p)
    (setf godot-instance nil)
    (loop for string-name-ptr being the hash-value of string-name-cache
          do (destroy-godot-string-name string-name-ptr)
             (memfree string-name-ptr))
    (clrhash string-name-cache)
    (loop for module being the hash-value in module-registry
          do (release-module module))
    (clrhash module-registry)))


(defun %push-action (action &key (error t))
  (with-slots (action-queue) *pozzo*
    (muth:with-guarded-reference (action-queue)
      (if (pozzo-started-p)
          (push action action-queue)
          (when error
            (error "Pozzo is offline: cannot enqueue an action"))))))


(defmacro do-by-pozzo ((&key if-running) &body body)
  `(%push-action
    (lambda () ,@body)
    ,@(when if-running '(:error nil))))


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
    (unless (find-extension extension-name)
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
  (with-slots (class-extension-table) *pozzo*
    (a:when-let ((extension-name (gethash class-name class-extension-table)))
      (a:when-let ((extension (find-extension extension-name)))
        (a:when-let ((class (gethash class-name (%class-table-of extension))))
          (gethash method-name (%method-table-of class)))))))


(defun find-extension (extension-name)
  (with-slots (extension-registry root-extension) *pozzo*
    (if (eq extension-name 'root)
        root-extension
        (gethash extension-name extension-registry))))


(defun get-extension (extension-name)
  (a:if-let ((extension (find-extension extension-name)))
    extension
    (error "Extension ~A not found" extension-name)))


(defun get-extension-class (extension-class-name)
  (with-slots (class-extension-table script-registry) *pozzo*
    (a:if-let ((script (find-script script-registry extension-class-name)))
      script
      (a:if-let ((extension-name (gethash extension-class-name class-extension-table)))
        (let ((extension (get-extension extension-name)))
          (a:if-let ((class (gethash extension-class-name (%class-table-of extension))))
            class
            (error "Class ~A not found in extension ~A" extension-class-name extension-name)))
        (error "Extension not found for class ~A" extension-class-name)))))


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
          (when (find-pmethod class 'initialize)
            (funcall-pmethod `(,(%name-of class) initialize) wrapper-ptr (cffi:null-pointer)))
          (when notify-postinitialize-p
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


(declaim (inline initialize-extension-level))
(defun initialize-extension-level (extension-name
                                   class-library-ptr
                                   init-level
                                   init-fu)
  (let ((extension (get-extension extension-name)))
    (shout "Extension ~S: ~A" (%name-of extension) init-level)
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
            (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
              ;; ignore sap->integer conversion warning
              ;; irrelevant, because it doesn't affect global performance
              (%register-extension-class-metadata (cffi:pointer-address (class-info &)) extension-class))
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
            do (%register-signal signal-name signal-properties (%name-of extension-class) extension))
      (init-extension-class extension-class))
    (when init-fu
      (funcall init-fu (level->pozzo init-level))))
  (values))


(declaim (inline release-extension-level))
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
          (when class-exists
            (%gdext:classdb-unregister-extension-class class-library-ptr
                                                       class-string-name)
            (deinit-extension-class extension-class)))))
    (when deinit-fu
      (funcall deinit-fu (level->pozzo deinit-level))))
  (values))


(declaim (inline initialize-extension))
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
                                 &key bind &allow-other-keys)
  (with-slots (class-extension-table) *pozzo*
    (let ((extension (get-extension extension-name)))
      (when (apply #'%add-extension-class extension class-name initargs)
        (register-class-bind-mapping class-name bind)
        (setf (gethash class-name class-extension-table) extension-name)
        (when (pozzo-started-p)
          (do-by-pozzo ()
            (%unload-extension extension)
            (%load-extension extension)))))))


(defun %register-script (script-name &rest initargs
                         &key &allow-other-keys)
  (with-slots (script-registry) *pozzo*
    (apply #'register-script script-registry script-name initargs)))


(declaim (inline %find-script-by-address))
(defun %find-script-by-address (address)
  (with-slots (script-registry) *pozzo*
    (find-script-by-address script-registry address)))


(declaim (inline %get-script))
(defun %get-script (name)
  (with-slots (script-registry) *pozzo*
    (a:if-let ((script (find-script script-registry name)))
      script
      (error "Script with name ~A not found" name))))


(defstruct %script-info
  name
  path
  base-type)


(defun script-name (info)
  (%script-info-name info))


(defun script-path (info)
  (%script-info-path info))


(defun script-base-type (info)
  (%script-info-base-type info))


(defun list-scripts ()
  (with-slots (script-registry) *pozzo*
    (let ((result (list)))
      (do-extension-scripts (script script-registry)
        (push (make-%script-info :name (%name-of script)
                                 :path (%path-of script)
                                 :base-type (%base-type-of script))
              result))
      result)))


(declaim (inline %ensure-script-mappings))
(defun %ensure-script-mappings (script-extension-instance-ptr)
  (with-slots (script-registry) *pozzo*
    (a:if-let ((script (find-script-by-address script-registry
                                               (cffi:pointer-address script-extension-instance-ptr))))
      script
      (c-with ((path %godot:string))
        (%godot:resource+get-path (unwrap script-extension-instance-ptr) (path &))
        (ensure-script-instance-mapping script-registry
                                        (godot-string-to-lisp (path &))
                                        script-extension-instance-ptr)))))


(defun register-extension-class-method (method-name class-name &rest keys &key bind &allow-other-keys)
  (with-slots (extension-registry class-extension-table) *pozzo*
    (a:if-let ((extension-name (gethash class-name class-extension-table)))
      (let ((extension (get-extension extension-name)))
        (a:when-let ((new-extension-class-method (apply #'%add-extension-class-method extension class-name method-name keys)))
          (register-method-bind-mapping class-name method-name bind)
          (when (pozzo-started-p)
            (do-by-pozzo ()
              (unless (virtualp new-extension-class-method)
                (%register-method new-extension-class-method class-name extension))))))
      (error "Class ~A not found" class-name))))


(defun %register-script-method (method-name script-name &rest keys
                                &key &allow-other-keys)
  (with-slots (script-registry) *pozzo*
    (apply #'register-script-method
           script-registry method-name script-name keys)))


(defun register-extension-class-signal (signal-name class-name &rest keys &key properties &allow-other-keys)
  (with-slots (extension-registry class-extension-table) *pozzo*
    (a:if-let ((extension-name (gethash class-name class-extension-table)))
      (let ((extension (get-extension extension-name)))
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


(defun register-opaque-script-language (obj-ptr)
  (with-slots (opaque-script-language-object) *pozzo*
    (unless (cffi:null-pointer-p opaque-script-language-object)
      (error "Pozzo Opaque Script language already registered"))
    (c-with ((err %godot:error))
      (%godot:engine+register-script-language (%godot:engine)
                                              (err &)
                                              obj-ptr)
      (unless (eq err :ok)
        (error "Failed to register PZOpaqueScript as an engine language"))
      (setf opaque-script-language-object obj-ptr))))


(defun register-resource-format-loader (obj-ptr)
  (%godot:resource-loader+add-resource-format-loader
   (%godot:resource-loader)
   obj-ptr
   t))


(declaim (inline opaque-script-language-object))
(defun opaque-script-language-object ()
  (slot-value *pozzo* 'opaque-script-language-object))


(declaim (inline opaque-script-default-instance-info))
(defun opaque-script-default-instance-info ()
  (slot-value *pozzo* 'opaque-script-instance-info))


;;;
;;; RESOURCES
;;;
(defun resource (path)
  (with-slots (resource-registry) *pozzo*
    (find-resource resource-registry path)))


(defun (setf resource) (value path)
  (with-slots (resource-registry) *pozzo*
    (register-resource resource-registry path value)))
