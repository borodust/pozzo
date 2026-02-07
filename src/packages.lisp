(cl:defpackage #:pozzo
  (:use :cl :alexandria)
  (:import-from #:cffi-c-ref
                #:c-val
                #:c-ref)
  (:import-from #:%gdext.util
                #:defprotocallback
                #:funcall-prototype
                #:bind-interface
                #:godot-string-to-lisp)
  (:local-nicknames (#:a #:alexandria)
                    (#:backtrace #:trivial-backtrace))
  (:export #:enter

           #:defpextension
           #:defpclass
           #:defpmethod
           #:$result

           #:emit-signal
           #:unwrap))


(cl:defpackage #:%%pozzo
  (:use))
