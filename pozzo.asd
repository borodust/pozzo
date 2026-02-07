(asdf:defsystem :pozzo
  :description "Common Lisp Interactive Environment for Godot"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:alexandria :float-features :cffi-c-ref :pz-godot)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "main")))
