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
               :cl-conspack :usocket
               :bordeaux-threads :cl-fad
               :unix-opts :uiop
               :slynk :swank
               :pz-godot)
  :pathname "src/"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "wrapper")
               (:module "api"
                :serial t
                :components ((:file "prototype")
                             (:file "extension")
                             (:file "class")
                             (:file "script")
                             (:file "method")
                             (:file "property")
                             (:file "signal")
                             (:file "module")))
               (:file "pozzo")
               (:file "core")
               (:file "main")))


(asdf:defsystem :pozzo/examples
  :description "Pozzo examples"
  :version "1.0.0"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (:alexandria :pozzo)
  :pathname "examples/"
  :components ((:module "sprite"
                :components ((:file "example")))
               (:module "canvas"
                :components ((:file "example")))
               (:module "script"
                :components ((:file "example")))))
