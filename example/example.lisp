(cl:defpackage #:pozzo.example
  (:use #:cl)
  (:export #:run))
(cl:in-package #:pozzo.example)


(pozzo:defpextension example)


(pozzo:defpclass hello-godot
  ((power %godot:float)
   (level %godot:int :initform 0 :exposed t))
  (:inherit %godot:node-2d)
  (:signals blip
            (boop (pressure %godot:float)))
  (:extension example))


(pozzo:defpmethod do-it ((self hello-godot) (delta %godot:float)) :void
  delta)

(pozzo:defpmethod calc ((self hello-godot)) %godot:int)

(pozzo:defpmethod (%wake :virtual :pure) ((self hello-godot)) %godot:int)

(defparameter *total-processing-time* 0d0)
(defparameter *total-processing-count* 0)

(pozzo:defpmethod (%process :virtual) ((self hello-godot) (delta %godot:float)) :void
  (pozzo::c-val ((delta %godot:float))
    (when (zerop (mod *total-processing-count* 1000))
      (pozzo::shout "BLIP")
      (pozzo:emit-signal (pozzo:unwrap self) "blip"))
    (incf *total-processing-time* delta)
    (incf *total-processing-count*)))


(pozzo:defpmethod (ping :static) ((hello-godot)) %godot:int
  (pozzo::shout-errors
    (pozzo::shout "PONG: ~A" (pozzo:$result &))
    (unless (cffi:null-pointer-p (pozzo:$result &))
      (setf pozzo:$result 24))))

(pozzo:defpmethod pfft ((self hello-godot)) :void)


(defun run ()
  (pozzo:enter :path (asdf:system-relative-pathname :pozzo/example "example/project/")))
