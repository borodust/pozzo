(cl:in-package #:pozzo)


(cffi:define-foreign-library (godot
                              :search-path (asdf:system-relative-pathname :pozzo "bin/"))
  (:linux "libgodot.so"))


(defprotocallback (level-init-func
                   %gdext.types:initialize-callback)
    (userdata init-level)
  (declare (ignore userdata))
  (shout "LibGodot: ~A" init-level)
  (when (eq init-level :initialization-scene)
    (initialize-pozzo-extensions))
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
          (init-record-ptr :initialize) (get-protocallback 'level-init-func)
          (init-record-ptr :deinitialize) (get-protocallback 'level-deinit-func))))


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
  (destructuring-bind (&key ((:path project-path)) editor ((:godot-arguments godot-args)) &allow-other-keys) args
    (multiple-value-bind (godot-arg-path godot-rest-args)
        (loop with godot-arg-path = nil
              with rest-args = godot-args
              while rest-args
              for switch = (first rest-args)
              if (string= switch "--path")
                do (setf godot-arg-path (second rest-args)
                         rest-args (cddr rest-args))
              else
                collect switch into godot-rest-args
                and do (setf rest-args (rest rest-args))
              finally (return (values godot-arg-path godot-rest-args)))
      (let* ((project-path (or (and project-path (namestring project-path))
                               godot-arg-path))
             (exec-path (namestring
                         (fad:merge-pathnames-as-file (fad:pathname-as-directory project-path)
                                                      ".pozzo/pozzo.sh")))
             (args (append
                    (list exec-path)
                    (when project-path
                      (list "--path" project-path))
                    (when editor
                      (list "-e"))
                    godot-rest-args
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
                do (cffi:foreign-string-free foreign-arg)))))))


(defmacro with-godot-args ((argc-var argv-var) arg-list &body body)
  `(run-with-godot-args
    (lambda (,argc-var ,argv-var) ,@body)
    ,arg-list))


(defun parse-option-keyword (value)
  (make-keyword (uiop:standard-case-symbol-name value)))


(defparameter *pozzo-command-line-opts*
  (unix-opts:make-options
   (list (list :name :system
               :description "ASDF system to load"
               :long "system"
               :arg-parser #'identity
               :meta-var "SYSTEM")
         (list :name :wait
               :description "Wait for Pozzo to exit"
               :long "wait")
         (list :name :repl-server
               :description "Start SWANK or SLYNK"
               :arg-parser #'parse-option-keyword
               :default :none
               :long "repl-server"))))


(defun run-main (&rest args &key &allow-other-keys)
  (prepare-pozzo)
  (with-godot-args (argc argv) args
    (let ((instance (%libgodot:create-godot-instance argc argv
                                                     (get-protocallback 'libgodot-init))))
      (if (cffi:null-pointer-p instance)
          (error "Failed to create Godot instance")
          (unwind-protect
               (run-with-godot instance)
            (%libgodot:destroy-godot-instance instance))))))


(defun get-pid ()
  "Return the process ID of the current Lisp process."
  #+sbcl (sb-unix:unix-getpid)
  #+ccl (ccl:getpid)
  #+ecl (ext:getpid)
  #+abcl (system:get-pid)
  #+clasp (si:getpid)
  #+clisp (os:process-id)
  #+allegro (excl:getpid)
  #+lispworks (system::getpid)
  #+scl (unix:unix-getpid)
  #-(or sbcl ccl ecl abcl clasp clisp allegro lispworks scl)
  (error "GET-PID not implemented for this Lisp implementation."))


(defmacro with-repl-client-stream ((stream-var) &body body)
  (a:with-gensyms (sock)
    `(handler-case
         (usocket:with-client-socket (,sock ,stream-var #(127 0 0 1) 9000
                                            :timeout 3)
           (let ((*print-case* :downcase))
             ,@body
             (terpri stream))
           (finish-output stream))
       (serious-condition (c) (shout "REPL client request failed: ~A" c)))))


(defun request-repl-server-registration (repl-server-kind port)
  (with-repl-client-stream (stream)
    (prin1 `(:kind :register
             :repl-server ,repl-server-kind
             :port ,port
             :comment ,(format nil "PID ~A" (get-pid)))
           stream)))


(defun request-repl-server-removal (port)
  (with-repl-client-stream (stream)
    (prin1 `(:kind :remove
             :port ,port)
           stream)))


(defmacro with-repl-server ((&key type) &body body)
  (a:with-gensyms (port)
    (a:once-only (type)
      `(let ((,port (ecase ,type
                      (:swank (swank:create-server :port 0 :dont-close t))
                      (:slynk (slynk:create-server :port 0 :dont-close t))
                      (:none nil))))
         (declare (ignorable ,port))
         (unwind-protect
              (progn
                (case ,type
                  (:swank (request-repl-server-registration :swank ,port))
                  (:slynk (request-repl-server-registration :slynk ,port)))
                ,@body)
           (unless (eq ,type :none)
             (request-repl-server-removal ,port))
           (case ,type
             (:swank (swank:stop-server ,port))
             (:slynk (slynk:stop-server ,port))))))))


(defun parse-command-line-arguments ()
  (loop with arg-target = :user
        for arg in (uiop:command-line-arguments)
        if (a:starts-with #\+ arg)
          do (a:eswitch (arg :test #'string=)
               ("+user" (setf arg-target :user))
               ("+pozzo" (setf arg-target :pozzo))
               ("+godot" (setf arg-target :godot)))
        else if (eq arg-target :user)
               collect arg into user-args
        else if (eq arg-target :pozzo)
               collect arg into pozzo-args
        else if (eq arg-target :godot)
               collect arg into godot-args
        finally (return (values pozzo-args godot-args user-args))))


(defun enter (&rest args
              &key ((:path project-path)) editor (blocking nil)
              &allow-other-keys)
  (declare (ignore project-path editor))
  (multiple-value-bind (pozzo-args godot-args user-args)
      (parse-command-line-arguments)
    (let ((pozzo-args (unix-opts:get-opts pozzo-args *pozzo-command-line-opts*)))
      (a:when-let ((system (getf pozzo-args :system)))
        (asdf:load-system system :verbose t))
      (flet ((%main ()
               (cffi:load-foreign-library 'godot)
               (float-features:with-float-traps-masked t
                 (unwind-protect
                      (with-repl-server (:type (getf pozzo-args :repl-server))
                        (apply #'run-main (list* :user-arguments user-args
                                                 :godot-arguments godot-args
                                                 (append args pozzo-args))))
                   (cffi:close-foreign-library 'godot)))))
        (trivial-main-thread:call-in-main-thread #'%main :blocking (or blocking
                                                                       (getf pozzo-args :wait))))))
  (values))
