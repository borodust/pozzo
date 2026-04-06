(cl:in-package #:pozzo)


(defmacro defpsignal (name (&rest classes) &body properties)
  `(progn
     ,@(loop for class-name in classes
             append (let ((signal-emitter-name (a:symbolicate '@ class-name '+ name)))
                      (multiple-value-bind (prop-names variant-names)
                          (loop for (name type) in properties
                                collect name into prop-names
                                collect (a:make-gensym 'variant) into variant-names
                                finally (return (values prop-names variant-names)))
                        (a:with-gensyms (instance)
                          `((eval-when (:compile-toplevel :load-toplevel :execute)
                              (register-extension-class-signal ',name ',class-name
                                                               :properties ',properties))
                            (declaim (inline ,signal-emitter-name))
                            (defun ,signal-emitter-name (,instance ,@prop-names)
                              (with-variants (,@(loop for (prop-name prop-type) in properties
                                                      for variant-name in variant-names
                                                      collect `(,variant-name ,prop-type ,prop-name)))
                                (emit-signal ,instance ',name ,@variant-names))))))))))
