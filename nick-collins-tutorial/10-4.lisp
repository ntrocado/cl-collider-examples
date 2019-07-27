(in-package :sc-user)

(defsynth mouse-demo ()
  (let ((mx (mouse-x.kr 0.0 1.0))
	(my (mouse-y.kr 0.0 1.0))
	(impulse (impulse.kr 15)))
    (send-trig.kr impulse 100 mx)
    (send-trig.kr impulse 101 my)))

(synth 'mouse-demo)

(defvar *mx* 0)
(defvar *my* 0)

(defun osc-responder (node id value)
  (declare (ignore node))
  (case id
    (100 (setf *mx* value))
    (101 (setf *my* value))))

(add-reply-responder "/tr" #'osc-responder)

(defparameter *post-mouse-pos* t)

(bt:make-thread
 (lambda ()
   (loop :while *post-mouse-pos*
	 :do (format t "~&mx: ~a; my: ~a~%" *mx* *my*)
	 :do (sleep 0.5)))
 :name "Print mouse position")

;; eval to stop
(eval-when (:execute)
  (setf *post-mouse-pos* nil))
