(cl:in-package :pozzo)


(defun handle-godot-invocation (socket)
  (let ((stream (usocket:socket-stream socket)))
    (destructuring-bind (&key system arguments &allow-other-keys)
        (conspack:decode-stream stream)
      (shout "System: ~A, Arguments: ~{~A~^, ~}" system arguments)
      (uiop:launch-program (list* "sbcl" "--end-toplevel-options"
                                  (append arguments "--"))))))


(defun listen-for-godot-invocations ()
  (usocket:with-socket-listener (server-socket #(127 0 0 1) 26045
                                               :reuse-address t
                                               :element-type '(unsigned-byte 8))
    (loop
      (usocket:with-connected-socket (client-socket
                                      (usocket:socket-accept server-socket))
        (shout "Invocation received")
        (handler-case
            (handle-godot-invocation client-socket)
          (serious-condition (c) (shout "Failed to handle Godot invocation: ~A" c)))))))
