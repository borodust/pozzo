(cl:defpackage #:pozzo.example.script
  (:use #:cl)
  (:export #:run))
(cl:in-package #:pozzo.example.script)


(pozzo:defpscript canvas
  ((time-passed %godot:float))
  (:for %godot:node-2d))


(pozzo:defpmethod hey ((self canvas)) :void
  (format *standard-output* "~&HEY!")
  (finish-output *standard-output*)
  (values))


(pozzo:defpmethod %ready ((self canvas)) :void
  (format *standard-output* "~&READY!")
  (finish-output *standard-output*)
  (values))


(pozzo:defpmethod %process ((self canvas)  (delta %godot:float)) :void
  (format *standard-output* "~&PROCESSING! ~A" delta)
  (finish-output *standard-output*)
  (values))


(pozzo:defpmethod pozzo:notice ((self canvas)
                                (what %godot:int)
                                (reversed %godot:bool))
    :void
  (format *standard-output* "~&NOTIFIED! ~A" (list what reversed))
  (finish-output *standard-output*)
  (values))


;;;
;;; SCENE SETUP
;;;
(pozzo:defpclass scene
  ()
  (:inherit %godot:scene-tree))


(declaim (inline add-child))
(defun add-child (parent child)
  (%godot:node+add-child parent child 0 :disabled))


(pozzo:defpmethod (%initialize :virtual) ((self scene)) :void
  (pozzo:c-with ((root %godot:window))
    (%godot:scene-tree+get-root (pozzo:unwrap self) (root &))

    (pozzo:c-with ((zero-pos %godot:vector-2i))
      (%godot:make-vector-2i@3 (zero-pos &) 0 0)
      (%godot:window+set-position root (zero-pos &))

      (let ((world (pozzo:construct '%godot:node-2d)))
        (pozzo:with-godot-string-name (name "World")
          (%godot:node+set-name world name))
        (%godot:node-2d+set-position world (zero-pos &))
        (add-child root world)

        (let ((camera (pozzo:construct '%godot:camera-2d)))
          (pozzo:with-godot-string-name (name "Camera")
            (%godot:node+set-name camera name))
          (%godot:camera-2d+set-anchor-mode camera :fixed-top-left)
          (%godot:node-2d+set-position camera (zero-pos &))
          (add-child world camera))

        (let ((canvas (pozzo:construct '%godot:node-2d)))
          (pozzo:with-godot-string-name (name "Canvas")
            (%godot:node+set-name canvas name))
          (%godot:node-2d+set-position canvas (zero-pos &))

          (pozzo:attach-script canvas 'canvas)
          (pozzo:with-godot-string-name (method-name "hey")
            (pozzo:c-with ((ret %godot:variant))
              (%gdext:object-call-script-method canvas method-name
                                                (cffi:null-pointer)
                                                0
                                                (ret &)
                                                (cffi:null-pointer))))

          (add-child world canvas))))))


(defun run ()
  (pozzo:enter :main 'scene
               :rendering-method "forward_plus"))
