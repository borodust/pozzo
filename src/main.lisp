(cl:in-package #:pozzo)


(cffi:define-foreign-library (godot
                              :search-path (asdf:system-relative-pathname :pozzo "bin/"))
  (:linux "libgodot.so"))


(defprotocallback (level-init-func
                               %gdext.types:initialize-callback)
    (userdata init-level)
  (declare (ignore userdata))
  (shout "LibGodot: ~A" init-level)
  (values))


(defprotocallback (level-deinit-func
                                 %gdext.types:deinitialize-callback)
    (userdata deinit-level)
  (declare (ignore userdata deinit-level))
  (values))


(defun init-godot (init-record-ptr)
  (c-with ((godot-version %gdext.types:godot-version-2))
    (%gdext.interface:get-godot-version2 (godot-version &))
    (format *standard-output* "~&Godot version: ~A.~A.~A"
            (godot-version :major)
            (godot-version :minor)
            (godot-version :patch)))

  (c-val ((init-record-ptr %gdext.types:initialization))
    (setf (init-record-ptr :minimum-initialization-level) :initialization-scene
          (init-record-ptr :userdata) (cffi:null-pointer)
          (init-record-ptr :initialize) (cffi:callback level-init-func)
          (init-record-ptr :deinitialize) (cffi:callback level-deinit-func))))


(defprotocallback (libgodot-init
                                 %gdext.types:initialization-function)
    (get-proc-addr-ptr class-lib-ptr init-record-ptr)
  (declare (ignore class-lib-ptr))
  (bind-interface get-proc-addr-ptr)
  (init-godot init-record-ptr)
  1)


(defun run-with-godot (instance)
  (start-pozzo instance)
  (unwind-protect
       (loop while (iterate-pozzo))
    (stop-pozzo)))


(defun run-with-godot-args (body args)
  (destructuring-bind (&key ((:path project-path)) (editor t) &allow-other-keys) args
    (let* ((exec-path (namestring
                       (asdf:system-relative-pathname :pozzo "bin/godot")))
           (args (append
                  (list exec-path)
                  (when project-path
                    (list "--path" (namestring project-path)))
                  (when editor
                    (list "-e"))
                  (list "--")))
           (foreign-args
             (loop for arg in args
                   collect (cffi:foreign-string-alloc arg :encoding :utf-8)))
           (argc (length args)))
      (unwind-protect
           (cffi-c-ref:c-with ((argv :pointer :count argc))
             (loop for foreign-arg in foreign-args
                   for i from 0
                   do (setf (argv i) foreign-arg))
             (funcall body argc (argv &)))
        (loop for foreign-arg in foreign-args
              do (cffi:foreign-string-free foreign-arg))))))


(defmacro with-godot-args ((argc-var argv-var) arg-list &body body)
  `(run-with-godot-args
    (lambda (,argc-var ,argv-var) ,@body)
    ,arg-list))


(defun run-main (args)
  (prepare-pozzo)
  (with-godot-args (argc argv) args
    (let ((instance (%libgodot:create-godot-instance argc argv
                                                     (cffi:callback libgodot-init))))
      (if (cffi:null-pointer-p instance)
          (error "Failed to create Godot instance")
          (unwind-protect
               (run-with-godot instance)
            (%libgodot:destroy-godot-instance instance))))))


(defun enter (&rest args &key ((:path project-path)) (editor t) (blocking nil))
  (declare (ignore project-path editor))
  (flet ((%main ()
           (cffi:load-foreign-library 'godot)
           (float-features:with-float-traps-masked t
             (unwind-protect
                  (run-main args)
               (cffi:close-foreign-library 'godot)))))
    (trivial-main-thread:call-in-main-thread #'%main :blocking blocking))
  (values))
