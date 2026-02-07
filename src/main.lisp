(cl:in-package #:pozzo)


(%gdext.util:defprotocallback (level-init-func
                                 %gdext.types:initialize-callback)
    (userdata init-level)
  (declare (ignore userdata init-level))
  (values))


(%gdext.util:defprotocallback (level-deinit-func
                                 %gdext.types:deinitialize-callback)
    (userdata deinit-level)
  (declare (ignore userdata deinit-level))
  (values))


(defun init-godot (init-record-ptr)
  (cffi:with-foreign-object (godot-version '%gdext.types:godot-version2)
    (%gdext.interface:get-godot-version2 godot-version)
    (cffi:with-foreign-slots (((major %gdext.types:major)
                               (minor %gdext.types:minor)
                               (patch %gdext.types:patch))
                              godot-version %gdext.types:godot-version2)
      (format *standard-output* "~&Godot version: ~A.~A.~A"
              major minor patch)))

  (cffi:with-foreign-slots (((min-init-level %gdext.types:minimum-initialization-level)
                               (userdata %gdext.types:userdata)
                               (level-init-func %gdext.types:initialize)
                               (level-deinit-func %gdext.types:deinitialize))
                              init-record-ptr %gdext.types:initialization)
      (setf min-init-level 4
            userdata (cffi:null-pointer)
            level-init-func (cffi:callback level-init-func)
            level-deinit-func (cffi:callback level-deinit-func))))


(%gdext.util:defprotocallback (libgodot-init
                                 %gdext.types:initialization-function)
    (get-proc-addr-ptr class-lib-ptr init-record-ptr)
  (declare (ignore class-lib-ptr))
  (%gdext.util:initialize-interface get-proc-addr-ptr)
  (init-godot init-record-ptr)
  1)


(defun run-with-godot (instance)
  (%gdext.util:initialize-extension '%godot:godot-instance)
  (%godot:godot-instance+start@1126i1g instance))


(defun run ()
  (let ((argc 1)
        (exec-path (namestring
                    (asdf:system-relative-pathname :pozzo "."))))
    (cffi:with-foreign-string (exec-path-ptr exec-path)
      (cref:c-with ((argv :pointer :count argc))
        (setf (argv 0) exec-path-ptr)
        (float-features:with-float-traps-masked t
          (let ((instance (%libgodot:create-godot-instance argc (argv &)
                                                           (cffi:callback libgodot-init))))
            (if (cffi:null-pointer-p instance)
                (error "Failed to create Godot instance")
                (unwind-protect
                     (run-with-godot instance)
                  (%libgodot:destroy-godot-instance instance)))))))))
