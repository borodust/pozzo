(cl:defpackage #:pozzo.example.canvas
  (:use #:cl)
  (:export #:run))
(cl:in-package #:pozzo.example.canvas)


(defvar *canvas-example* nil)


(defclass canvas-example () ())


(defun initialize (extension-name level)
  (declare (ignore extension-name level)))


(defun finalize (extension-name level)
  (declare (ignore extension-name))
  (when (eq level :scene)
    (setf *canvas-example* nil)))


(pozzo:defpextension extension
  (:level :scene)
  (:init-level 'initialize)
  (:fini-level 'finalize))


(pozzo:defpclass scene
  ()
  (:inherit %godot:scene-tree)
  (:extension extension))


(declaim (inline add-child))
(defun add-child (parent child)
  (%godot:node+add-child parent child 0 :disabled))


(pozzo:defpmethod (%initialize :virtual) ((self scene)) :void
  (pozzo:c-with ((root %godot:window))
    (%godot:scene-tree+get-root (pozzo:unwrap self) (root &))

    (pozzo:c-with ((zero-pos %godot:vector-2))
      (%godot:make-vector-2@3 (zero-pos &) 0d0 0d0)
      (%godot:node-2d+set-position root (zero-pos &))

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

        (let ((canvas (pozzo:construct 'canvas)))
          (pozzo:with-godot-string-name (name "Canvas")
            (%godot:node+set-name canvas name))
          (%godot:node-2d+set-position canvas (zero-pos &))
          (add-child world canvas))))))


(pozzo:defpclass canvas
  ((time-passed %godot:float :initform 0d0))
  (:inherit %godot:node-2d)
  (:extension extension))


(pozzo:defpmethod (%process :virtual) ((self canvas) (delta %godot:float)) :void
  (%godot:canvas-item+queue-redraw (pozzo:unwrap self))
  (incf (canvas-time-passed self) delta))


(pozzo:defpmethod (%draw :virtual) ((self canvas)) :void
  (let* ((time (canvas-time-passed self))
         (x (+ 300 (* 100 (cos (* 2 time)))))
         (y (+ 300 (* 100 (sin (* 1.5 time)))))
         (r (abs (sin (* 0.2 time))))
         (g (abs (cos (* 0.2 time))))
         (b (abs (- r g)))
         (radius (+ 100 (* 50 (sin (* 0.1 time)))))
         (filled 1)
         (width 0)
         (antialiased 1))
    (pozzo:c-with ((pos %godot:vector-2)
                   (col %godot:color))

      (%godot:make-color@3 (col &) 0.2d0 0.25d0 0.3d0)
      (%godot:rendering-server+set-default-clear-color
       (%godot:rendering-server)
       (col &))

      (%godot:make-color@3 (col &) (float r 0d0) (float g 0d0) (float b 0d0))
      (%godot:make-vector-2@3 (pos &) (float x 0d0) (float y 0d0))
      (%godot:canvas-item+draw-circle (pozzo:unwrap self)
                                      (pos &)
                                      radius
                                      (col &)
                                      filled
                                      width
                                      antialiased))))


(defun run ()
  (setf *canvas-example* (make-instance 'canvas-example))
  (pozzo:enter :main 'scene
               :rendering-method "forward_plus"))
