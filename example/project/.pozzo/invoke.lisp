(require 'uiop)
(require 'asdf)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (unless (probe-file quicklisp-init)
    (error "Failed to find Quicklisp"))
  (load quicklisp-init))

(asdf:load-systems :usocket :cl-conspack)

(defpackage #:pozzo.runner
  (:use #:cl)
  (:local-nicknames))
(in-package #:pozzo.runner)


(defun redirect-invocation ()
  (usocket:with-client-socket (sock stream #(127 0 0 1) 26045
                                    :element-type '(unsigned-byte 8))
    (conspack:encode (list :system "pozzo/example" :arguments (uiop:command-line-arguments)) :stream stream)
    (finish-output stream)))


(redirect-invocation)
