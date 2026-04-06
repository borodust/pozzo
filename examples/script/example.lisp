(cl:defpackage #:pozzo.example.script
  (:use #:cl)
  (:export #:run))
(cl:in-package #:pozzo.example.script)


#++(pozzo:defpscript snitch
  ()
  (:for %godot:node))


#++(pozzo:defpmethod (%notification :virtual) ((self snitch)) :void)


(defun run ()
  (pozzo:enter :path (asdf:system-relative-pathname :pozzo/examples "examples/script/project/") :if-does-not-exist :create :editor t))
