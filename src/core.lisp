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
