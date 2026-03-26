(cl:defpackage #:pozzo
  (:use :cl :alexandria)
  (:import-from #:cffi-c-ref
                #:c-val
                #:c-ref)
  (:import-from #:%gdext.util
                #:defprotocallback
                #:get-protocallback
                #:funcall-prototype)
  (:import-from #:%godot.util
                #:bind-interface
                #:godot-string-to-lisp
                #:godot-extension-bind-name
                #:godot-extension-variant-kind)
  (:local-nicknames (#:a #:alexandria)
                    (#:backtrace #:trivial-backtrace))
  (:export #:enter

           #:defpextension
           #:defpclass
           #:defpsignal
           #:defpmethod
           #:preturn

           #:emit-signal
           #:unwrap

           #:c-with
           #:c-val
           #:c-ref

           #:initialize-variant-from-value
           #:release-variant
           #:symbol-string-name

           #:with-variant
           #:with-variants))


(cl:defpackage #:%%pozzo
  (:use))
