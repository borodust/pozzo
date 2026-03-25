(cl:defpackage #:pozzo.example
  (:use #:cl)
  (:export #:run))
(cl:in-package #:pozzo.example)

;;
;; Adapted from
;; https://docs.godotengine.org/en/4.6/tutorials/scripting/gdextension/gdextension_c_example.html#a-demo-project
;;

(pozzo:defpextension example)


(pozzo:defpclass hello-godot
  ((time-passed %godot:float
                :initform 0d0)
   (amplitude %godot:float
              :initform 10d0
              :exposed t)
   (speed %godot:float
          :initform 1d0
          :exposed t))
  (:inherit %godot:sprite-2d)
  (:signals (position-changed (new-position %godot:vector-2))
            (bytes-consed (new-ceiling %godot:int)))
  (:extension example))


(pozzo:defpmethod (%process :virtual) ((self hello-godot) (delta %godot:float)) :void
  (incf (hello-godot-time-passed self) (* (hello-godot-speed self) delta))

  (pozzo:c-with ((new-pos %godot:vector-2))
    (let ((x (+ (hello-godot-amplitude self)
                (* (hello-godot-amplitude self)
                   (sin (* (hello-godot-time-passed self) 2)))))
          (y (+ (hello-godot-amplitude self)
                (* (hello-godot-amplitude self)
                   (cos (* (hello-godot-time-passed self) 1.5))))))
      (%godot:make-vector-2@3 (new-pos &)
                              (float x 0d0)
                              (float y 0d0))
      (%godot:node-2d+set-position (pozzo:unwrap self) (new-pos &))
      (pozzo:c-with ((new-pos-variant %godot:variant))
        (pozzo:initialize-variant-from-value (new-pos-variant &)
                                             (new-pos &)
                                             '%godot:vector-2)
        (pozzo:emit-signal self 'position-changed (new-pos-variant &))
        (pozzo:symbol-string-name 'position-changed)
        (pozzo:release-variant (new-pos-variant &)))))

  (pozzo:emit-signal self 'bytes-consed
                     #+sbcl (sb-ext:get-bytes-consed)
                     #-sbcl 0))


(pozzo:defpmethod string-length ((self hello-godot) (str %godot:string)) %godot:int
  (pozzo:return-value (length (pozzo::godot-string-to-lisp str))))


(defun run (&key editor)
  (pozzo:enter :path (asdf:system-relative-pathname :pozzo/example "example/project/") :editor editor))
