(cl:in-package #:pozzo)


(defpextension core
  (:level :servers)
  (:init-level 'init-core-extension))


(defun init-core-extension (level)
  (when (eq :servers level)
    (let ((opaque-script-lang-ptr (construct 'opaque-script-language)))
      (%godot:engine+register-script-language (%godot:engine) (cffi:null-pointer) opaque-script-lang-ptr))))

;;;
;;; SCRIPT
;;;
(defpclass opaque-script-language
  ()
  (:inherit %godot:script-language-extension)
  (:extension core))


(defpmethod (%get-name :virtual) ((self opaque-script-language)) %godot:string
  (preturn-with (result)
    (initialize-godot-string result "Pozzo Opaque Script")))


(defpmethod (%get-type :virtual) ((self opaque-script-language)) %godot:string
  (preturn-with (result)
    (initialize-godot-string result "PZOpaqueScript")))


(defpmethod (%get-extension :virtual) ((self opaque-script-language)) %godot:string
  (preturn-with (result)
    (initialize-godot-string result "pzo")))


(defpclass pozzo-resource-format-loader
  ()
  (:inherit %godot:resource-format-loader)
  (:extension core))


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
