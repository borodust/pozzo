(cl:defpackage #:pozzo
  (:use :cl :alexandria)
  (:import-from #:cffi-c-ref
                #:c-val
                #:c-ref)
  (:import-from #:%gdext.util
                #:defprotocallback
                #:get-protocallback
                #:funcall-prototype
                #:defcfunproto)
  (:import-from #:%godot.util
                #:bind-gdext-interface
                #:bind-godot-constants
                #:godot-string-to-lisp
                #:godot-extension-bind-name
                #:godot-extension-variant-kind)
  (:local-nicknames (#:a #:alexandria)
                    (#:backtrace #:trivial-backtrace))
  (:export #:enter

           #:defpextension
           #:defpclass
           #:defpscript
           #:defpsignal
           #:defpmethod
           #:preturn
           #:preturn-with

           #:emit-signal
           #:unwrap

           #:c-with
           #:c-val
           #:c-ref

           #:initialize-variant-from-value
           #:release-variant
           #:symbol-string-name

           #:with-variant
           #:with-variants
           #:initialize-godot-string
           #:release-godot-string
           #:with-godot-string
           #:with-godot-strings
           #:initialize-godot-string-name
           #:release-godot-string-name
           #:with-godot-string-name
           #:with-godot-string-names

           #:memalloc
           #:memfree

           #:construct
           #:destruct

           #:attach-script
           #:notice

           #:register-module
           #:make-module
           #:initialize-module
           #:release-module
           #:iterate-module))


(cl:defpackage #:%%pozzo
  (:use))
