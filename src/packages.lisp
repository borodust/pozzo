(cl:defpackage #:pozzo
  (:use :cl :alexandria)
  (:import-from #:cffi-c-ref
                #:c-val
                #:c-ref)
  (:import-from #:%gdext.util
                #:defprotocallback
                #:get-protocallback
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
           #:unwrap

           #:c-with
           #:c-val
           #:c-ref

           #:initialize-variant-from-value
           #:release-variant))


(cl:defpackage #:%%pozzo
  (:use))
