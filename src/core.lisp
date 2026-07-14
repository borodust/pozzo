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


(defpclass pozzo-resource-format-loader
  ()
  (:inherit %godot:resource-format-loader)
  (:extension root))


(defpmethod (%exists :virtual) ((self pozzo-resource-format-loader)) %godot:bool
  (preturn t))


(defpmethod (%recognize-path :virtual) ((self pozzo-resource-format-loader)
                                        (path %godot:string)
                                        (type %godot:string))
    %godot:bool
  (declare (ignore type))
  (c-with ((result %godot:bool))
    (with-godot-string (proto "pozzo://")
      (%godot:string+begins-with path (result &) proto))
    (preturn result)))


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
    ;; FIXME:
    (initialize-godot-string-name result
                                  (get-class-bind-name '%godot:node))))


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
    (instance-var method-string-name argv argc result error-info)
  (shout-errors
    (let ((script-ptr (get-pozzo-object instance-var)))
      (c-val ((script-ptr (:struct script)))
        (a:if-let ((script (%find-script-by-address
                            (cffi:pointer-address (script-ptr :script)))))
          (c-with ((method-hash %godot:int))
            (%godot:string-name+hash method-string-name (method-hash &))
            (a:when-let ((method-ptr (find-script-method-by-hash script method-hash)))
              (funcall-prototype method-ptr pozzo-script-method
                                 instance-var
                                 argv
                                 argc
                                 result
                                 error-info)))
          (progn
            #++(report-missing-method-error error-info)))))
    (values)))


(defprotocallback (script-instance-has-method %gdext:script-instance-has-method)
    (instance-var method-string-name)
  (let ((script-ptr (get-pozzo-object instance-var)))
    (c-val ((script-ptr (:struct script)))
      (a:if-let ((script (%find-script-by-address
                          (cffi:pointer-address (script-ptr :script)))))
        (c-with ((method-hash %godot:int))
          (%godot:string-name+hash method-string-name (method-hash &))
          (if (find-script-method-by-hash script method-hash) 1 0))
        0))))


(defprotocallback (script-instance-get-owner %gdext:script-instance-get-owner)
    (instance-var)
  (unwrap instance-var))


(defprotocallback (script-instance-get-script %gdext:script-instance-get-script)
    (instance-var)
  (let ((script-ptr (get-pozzo-object instance-var)))
    (c-ref script-ptr (:struct script) :script)))


(defprotocallback (script-instance-free %gdext:script-instance-free)
    (instance-var)
  (let ((script-ptr (get-pozzo-object instance-var)))
    (c-val ((script-ptr (:struct script)))
      (memfree (script-ptr :data)))
    (memfree script-ptr))
  (destroy-pozzo-wrapper instance-var)
  (values))


(defprotocallback (script-instance-get-language %gdext:script-instance-get-language)
    (instance-var)
  (declare (ignore instance-var))
  (opaque-script-language-object))
