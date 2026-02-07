(asdf:defsystem :pozzo
  :description "Common Lisp Interactive Environment for Godot"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:alexandria :float-features
               :cffi :cffi-c-ref
               :cl-ppcre :trivial-main-thread
               :cl-muth :trivial-backtrace
               :pz-godot)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "wrapper")
               (:file "extension")
               (:file "pozzo")
               (:file "main")))


(asdf:defsystem :pozzo/example
  :description "Pozzo example project"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:alexandria :pozzo)
  :pathname "example/"
  :serial t
  :components ((:file "example")))
