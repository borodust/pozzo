(cl:in-package #:pozzo)


;;;
;;; SCRIPT
;;;
(defun init-opaque-script-language ()
  (register-opaque-script-language (construct 'opaque-script-language)))


(defpclass opaque-script-language
  ()
  (:inherit %godot:script-language-extension)
  (:extension root)
  (:level :core)
  (:init init-opaque-script-language))


(defpmethod (%get-name :virtual) ((self opaque-script-language)) %godot:string
  (preturn-with (result)
    (initialize-godot-string result "Pozzo Opaque Script")))


(defpmethod (%get-type :virtual) ((self opaque-script-language)) %godot:string
  (preturn-with (result)
    (initialize-godot-string result "PZOpaqueScript")))


(defpmethod (%get-extension :virtual) ((self opaque-script-language))
    %godot:string
  (preturn-with (result)
    (initialize-godot-string result "pzo")))


(defpmethod (%init :virtual) ((self opaque-script-language)) :void)


(defpmethod (%finish :virtual) ((self opaque-script-language)) :void)


(defpmethod (%frame :virtual) ((self opaque-script-language)) :void)


(defpmethod (%thread-enter :virtual) ((self opaque-script-language)) :void)


(defpmethod (%thread-exit :virtual) ((self opaque-script-language)) :void)


(defpmethod (%reload-all-scripts :virtual) ((self opaque-script-language)) :void)


(defpmethod (%reload-scripts :virtual) ((self opaque-script-language)) :void)


(defpmethod (%reload-tool-script :virtual) ((self opaque-script-language)) :void)


(defpmethod (%get-recognized-extensions :virtual) ((self opaque-script-language))
    %godot:packed-string-array
  (preturn-with (result)
    (initialize-godot-packed-string-array result "pzo")))


(defpmethod (%handles-global-class-type :virtual) ((self opaque-script-language)) %godot:bool
  (preturn nil))


;;;
;;; SCRIPT EXTENSION
;;;
(defpclass opaque-script-extension
  ()
  (:level :core)
  (:inherit %godot:script-extension)
  (:extension root))


(defpmethod (%get-instance-base-type :virtual) ((self opaque-script-extension))
    %godot:string-name
  (preturn-with (result)
    (let ((script (%ensure-script-mappings self)))
      (initialize-godot-string-name result
                                    (get-class-bind-name
                                     (%base-type-of script))))))


(defpmethod (%can-instantiate :virtual) ((self opaque-script-extension))
    %godot:bool
  (preturn t))


(defpmethod (%instance-create :virtual) ((self opaque-script-extension)
                                         (obj (:pointer %godot:object)))
    (:pointer :void)
  (let ((script (%ensure-script-mappings self)))
    (preturn (make-script-instance script self obj))))


(defpmethod (%get-language :virtual) ((self opaque-script-extension))
    (:pointer %godot:object)
  (preturn (opaque-script-language-object)))


(defpmethod (%is-abstract :virtual) ((self opaque-script-extension))
    %godot:bool
  (preturn nil))


(defpmethod (%is-valid :virtual) ((self opaque-script-extension))
    %godot:bool
  (preturn t))


(defpmethod (%has-source-code :virtual) ((self opaque-script-extension))
    %godot:bool
  (preturn nil))


(defpmethod (%get-source-code :virtual) ((self opaque-script-extension))
    %godot:string
  (preturn-with (ret)
    (initialize-godot-string ret "")))


(defpmethod (%reload :virtual) ((self opaque-script-extension)
                                (keep-state-p %godot:bool))
    %godot:error
  (declare (ignore keep-state-p))
  (preturn :ok))


(cffi:defcstruct script
  ;; pointer to OpaqueScriptExtension class instance
  (script :pointer)
  ;; pointer to script instance (attachable to Godot object) data
  (data :pointer))


(defmacro with-script ((script-var &optional error-handler) instance &body body)
  (a:once-only (instance)
    (a:with-gensyms (script-ptr)
      `(let ((,script-ptr (get-pozzo-object ,instance)))
         (c-val ((,script-ptr (:struct script)))
           (a:if-let ((,script-var (%find-script-by-address
                                    (cffi:pointer-address (,script-ptr :script)))))
             (progn ,@body)
             ,(if error-handler
                  `(funcall ,error-handler)
                  `(error "Script not found for the instance ~A" ,instance))))))))


(defun make-script-instance (script script-extension-object godot-object)
  (let* ((pozzo-object (memalloc '(:struct script)))
         (data (memallocz `(:struct ,(%struct-name-of script))))
         (wrapper (make-pozzo-wrapper godot-object pozzo-object)))
    (c-val ((pozzo-object (:struct script)))
      (setf (pozzo-object :script) script-extension-object
            (pozzo-object :data) data))
    (%gdext:script-instance-create3 (opaque-script-default-instance-info)
                                    wrapper)))


(defprotocallback (script-instance-call-method %gdext:script-instance-call)
    (instance method-string-name argv argc result error-info)
  (shout-errors
    (with-script (script) instance
      (c-with ((method-hash %godot:int))
        (%godot:string-name+hash method-string-name (method-hash &))
        (a:when-let ((method-ptr (find-script-method-by-hash script method-hash)))
          (funcall-prototype method-ptr pozzo-script-method
                             instance
                             argv
                             argc
                             (or result (cffi:null-pointer))
                             error-info))))
    (values)))


(defprotocallback (script-instance-has-method %gdext:script-instance-has-method)
    (instance method-string-name)
  (with-script (script (constantly 0)) instance
    (c-with ((method-hash %godot:int))
      (%godot:string-name+hash method-string-name (method-hash &))
      (if (find-script-method-by-hash script method-hash) 1 0))))


(defprotocallback (script-instance-get-owner %gdext:script-instance-get-owner)
    (instance)
  (unwrap instance))


(defprotocallback (script-instance-get-script %gdext:script-instance-get-script)
    (instance)
  (let ((script-ptr (get-pozzo-object instance)))
    (c-ref script-ptr (:struct script) :script)))


(defprotocallback (script-instance-free %gdext:script-instance-free)
    (instance)
  (let ((script-ptr (get-pozzo-object instance)))
    (c-val ((script-ptr (:struct script)))
      (memfree (script-ptr :data)))
    (memfree script-ptr))
  (destroy-pozzo-wrapper instance)
  (values))


(defprotocallback (script-instance-get-language %gdext:script-instance-get-language)
    (instance)
  (declare (ignore instance))
  (opaque-script-language-object))


(defprotocallback (script-instance-notification %gdext:script-instance-notification-2)
    (instance what reversed)
  (with-script (script) instance
    (a:when-let (method-ptr (find-script-method script 'notice))
      (with-variants ((what-v %godot:int what)
                      (reversed-v %godot:bool reversed))
        (c-with ((argv :pointer :count 2)
                 (error-info %gdext:call-error :zero t))
          (setf (argv 0) what-v
                (argv 1) reversed-v)
          (funcall-prototype method-ptr pozzo-script-method
                             instance
                             (argv &)
                             2
                             (cffi:null-pointer)
                             (error-info &))))))
  (values))


(declaim (inline attach-script))
(defun attach-script (godot-object script-name)
  (let ((script (%get-script script-name)))
    (c-with ((ptr :pointer))
      (setf ptr (ensure-script-instance script))
      (with-variant (var %godot:script-extension ptr)
        (%godot:object+set-script godot-object var)))))


;;;
;;; RESOURCE LOADER
;;;
(defun init-resource-format-loader ()
  (register-resource-format-loader (construct 'resource-format-loader)))


(defpclass resource-format-loader
  ((schema-prefix %godot:string))
  (:inherit %godot:resource-format-loader)
  (:level :servers)
  (:extension root)
  (:init init-resource-format-loader))


(defpmethod initialize ((self resource-format-loader))
    :void
  (initialize-godot-string (resource-format-loader-schema-prefix self)
                           "pozzo://"))


(defpmethod (%recognize-path :virtual) ((self resource-format-loader)
                                        (path %godot:string)
                                        (type %godot:string))
    %godot:bool
  (declare (ignore type))
  (c-with ((result %godot:bool))
    (%godot:string+begins-with path
                               (result &)
                               (resource-format-loader-schema-prefix self))
    (preturn result)))


(defpmethod (%exists :virtual) ((self resource-format-loader)
                                (path %godot:string))
    %godot:bool
  (preturn (and (resource (godot-string-to-lisp path)) t)))


(defpmethod (%load :virtual) ((self resource-format-loader)
                              (path %godot:string)
                              (original-path %godot:string)
                              (use-sub-threads %godot:bool)
                              (cache-mode %godot:int))
    %godot:variant
  (let ((rsc (resource (godot-string-to-lisp path))))
    (preturn-with (result)
      (load-resource-variant rsc result))))
