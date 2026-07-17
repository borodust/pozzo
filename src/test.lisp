(cl:in-package #:pozzo)


(defvar *test-suite* nil)
(defvar *test-results* nil)


(defgeneric iterate-test-suite (suite))


(pozzo:defpclass test-harness-loop ()
  (:inherit %godot:main-loop))


(pozzo:defpmethod (%process :virtual) ((self test-harness-loop) (delta %godot:float)) %godot:bool
  (declare (ignore delta))
  (multiple-value-bind (exit-p result) (iterate-test-suite *test-suite*)
    (push result *test-results*)
    (pozzo:preturn (and exit-p t))))


(defun run-tests (suite)
  (let ((*test-results* nil)
        (*test-suite* suite))
    (pozzo:enter :main 'test-harness-loop :blocking t)
    (reverse *test-results*)))
