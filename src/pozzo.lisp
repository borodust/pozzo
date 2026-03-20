(cl:in-package #:pozzo)


(defclass pozzo ()
  ((godot-instance :initform nil)
   (extension-registry :initform (make-hash-table :test 'eq))
   (class-extension-table :initform (make-hash-table :test 'eq))
   (class-metadata-table :initform (make-hash-table :test 'eql))
   (wrapper-registry :initform (make-hash-table :test 'eql :size 127))

   (iter-result :initform (cffi:foreign-alloc '%godot:bool))
   (action-queue :initform (muth:make-guarded-reference (list)))))


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
  (with-slots (godot-instance) *pozzo*
    (setf godot-instance nil)))


(defun %push-action (action)
  (with-slots (action-queue) *pozzo*
    (unless (pozzo-started-p)
      (error "Pozzo is offline: cannot enqueue an action"))
    (muth:with-guarded-reference (action-queue)
      (push action action-queue))))


(defmacro do-by-pozzo (() &body body)
  `(%push-action
    (lambda () ,@body)))


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
                   %gdext.types:class-method-ptr-call)
    (method-userdata class-instance-ptr args ret)
  (declare (ignore method-userdata class-instance-ptr args ret))
  (values))


(defprotocallback (call-extension-class-method
                   %gdext.types:class-method-call)
    (method-userdata class-instance-ptr args argc ret error)
  (declare (ignore method-userdata class-instance-ptr argc args ret error))
  (values))


(defprotocallback (create-extension-class-instance
                   %gdext.types:class-create-instance-2)
    (class-info notify-postinitialize-p)
  (declare (ignore notify-postinitialize-p))
  (shout-errors
    (let ((class (%find-extension-class-by-metadata-id (cffi:pointer-address class-info))))
      (c-val ((class-info (:struct pozzo-class-info)))
        (let* ((obj-ptr (%gdext.interface:classdb-construct-object2 (class-info :parent-name &)))
               (wrapper-ptr (make-pozzo-wrapper obj-ptr (funcall (%constructor-name-of class)))))
          (%gdext.interface:object-set-instance obj-ptr (class-info :class-name &) wrapper-ptr)
          (unless (zerop notify-postinitialize-p)
            (%godot:object+notification obj-ptr 0 0))
          obj-ptr)))))


(defprotocallback (free-extension-class-instance
                   %gdext.types:class-free-instance)
    (class-info instance-ptr)
  (shout-errors
    (let ((class (%find-extension-class-by-metadata-id (cffi:pointer-address class-info))))
      (c-val ((instance-ptr (:struct pozzo-wrapper)))
        (funcall (%destructor-name-of class) (instance-ptr :p-object)))
      (destroy-pozzo-wrapper instance-ptr)))
  (values))


(defprotocallback (get-extension-class-virtual-call-data
                   %gdext.types:class-get-virtual-call-data-2)
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
                   %gdext.types:class-call-virtual-with-data)
    (instance-ptr func-string-name fuptr args ret)
  (%gdext.util:funcall-prototype fuptr %gdext.types:class-call-virtual-with-data
                                 instance-ptr func-string-name (cffi:null-pointer) args ret)
  (values))


(defun initialize-extension-level (extension-name class-library-ptr init-level)
  (let ((extension (get-extension extension-name)))
    (shout "Extension ~A: ~A" (%name-of extension) init-level)
    (when (eq init-level :initialization-scene)
      (with-slots (extension-registry) *pozzo*
        (do-extension-classes (extension-class extension)
          (c-with ((creation-info %gdext.types:class-creation-info-5))
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
                (%gdext.interface:classdb-register-extension-class5 class-library-ptr
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
                do (%register-signal signal-name signal-properties (%name-of extension-class) extension))))))
  (values))


(defun release-extension-level (extension-name class-library-ptr deinit-level)
  (let ((extension (get-extension extension-name)))
    (when (eq deinit-level :initialization-scene)
      (do-extension-classes (extension-class extension)
        ;; FIXME: release class metadata
        (with-godot-string-name (class-string-name (%name-of extension-class) :pascal)
          (c-with ((class-exists %godot:bool))
            (%godot:class-db+class-exists (%godot:class-db)
                                          (class-exists &)
                                          class-string-name)
            (unless (zerop class-exists)
              (%gdext.interface:classdb-unregister-extension-class class-library-ptr
                                                                   class-string-name)))))))
  (values))


(defun initialize-extension (extension-name class-library-ptr init-struct)
  (let ((extension (get-extension extension-name)))
    (%update-class-library-pointer class-library-ptr extension)
    (c-val ((init-struct %gdext.types:initialization))
      (setf (init-struct :minimum-initialization-level) :initialization-scene
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
      (with-godot-string-name (class-string-name (%name-of extension-class) :pascal)
        (c-with ((method-info %gdext.types:class-method-info)
                 (arguments-info %gdext.types:property-info :count argc)
                 (arguments-metadata %gdext.types:class-method-argument-metadata :count argc)
                 (return-type-info %gdext.types:property-info)
                 (return-type-metadata %gdext.types:class-method-argument-metadata))
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
          (with-godot-string-name (method-string-name
                                   (substitute
                                    #\_ #\%
                                    (string (%name-of extension-class-method)))
                                   :snake)
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
                  (method-info :method-flags) (cffi:foreign-bitfield-value '%gdext.types:class-method-flags
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

            (%gdext.interface:classdb-register-extension-class-method
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
    (with-godot-string-name (class-string-name (get-class-bind-name class-name))
      (c-with ((property-info %gdext.types:property-info))
        (initialize-godot-property (property-info &)
                                   (%name-of extension-property)
                                   (variant-kind-of extension-property))
        (with-godot-string-names ((writer-string-name (get-method-bind-name
                                                       class-name
                                                       (writer-name-of extension-property)))
                                  (reader-string-name (get-method-bind-name
                                                       class-name
                                                       (reader-name-of extension-property))))
          (shout "Registering property ~A of ~A (reader: ~A, writer: ~A)"
                 (godot-string-name-to-lisp (property-info :name))
                 (godot-string-name-to-lisp class-string-name)
                 (godot-string-name-to-lisp reader-string-name)
                 (godot-string-name-to-lisp writer-string-name))
          (%gdext.interface:classdb-register-extension-class-property
           (class-library-pointer-of extension)
           class-string-name
           (property-info &)
           writer-string-name
           reader-string-name))
        (release-godot-property (property-info &))))
    (error "Class ~A not found" class-name)))


(defun %register-signal (signal-name signal-properties class-name extension)
  (a:if-let ((extension-class (gethash class-name (%class-table-of extension))))
    (with-godot-string-names ((class-string-name (get-class-bind-name class-name))
                              (signal-string-name signal-name :snake))
      (let ((prop-count (length signal-properties)))
        (c-with ((properties-info %gdext.types:property-info :count prop-count))
          (loop for property in signal-properties
                for i from 0
                do (initialize-godot-property (properties-info i &)
                                              (%name-of property)
                                              (variant-kind-of property)))
          (shout "Registering signal ~A of ~A"
                 (godot-string-name-to-lisp signal-string-name)
                 (godot-string-name-to-lisp class-string-name))
          (%gdext.interface:classdb-register-extension-class-signal
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


(declaim (inline emit-signal))
(defun emit-signal (instance signal-name &rest variants)
  (with-godot-string-name (signal-string-name signal-name :snake)
    (c-with ((signal-name-variant %godot:variant)
             (result-variant %godot:variant))
      (initialize-variant-from-value (signal-name-variant &) signal-string-name '%godot:string-name)

      (apply #'%godot:object+emit-signal (unwrap instance) (result-variant &) (signal-name-variant &) variants)

      (prog1 (c-ref (get-variant-internal-ptr result-variant) %godot:error)
        (release-variant (signal-name-variant &))
        (release-variant (result-variant &))))))
