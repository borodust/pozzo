(require 'uiop)
(require 'asdf)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (unless (probe-file quicklisp-init)
    (error "Failed to find Quicklisp setup file"))
  (load quicklisp-init))

(asdf:load-system :pozzo :verbose t)
(pozzo:enter)
