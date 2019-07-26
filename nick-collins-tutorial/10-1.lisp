;;; To send general OSC to a known IP...
;;; This needs a previously defined "default" synth.

(sc-osc:send-message (sc-osc:osc-device "127.0.0.1" 57110) "/s_new" "default" (sc::get-next-id *s*) 0 0)

;; If sending messages to a SuperCollider server, this is better:
(let ((id (sc::get-next-id *s*))) 
  (send-message *s* "/s_new" "default" id 0 1)
  (sc::sched-add (sc::scheduler *s*) (+ (now) 1) #'send-message *s* "/n_free" id))

;;; Receiving OSC messages from the server
;;; The following is the functionality currently offered by cl-collider:

;;; register to receive a trigger message
(flet ((osc-responder (node id value)
	 (format t "~&Received trigger! ~a ~a ~a~%" node id value)))
  (add-reply-responder "/tr" #'osc-responder))

(play (send-trig.ar (impulse.ar 0.5)))

;;; keep it running and now run these lines:
(flet ((osc-responder (node id value)
	 (format t "~&New reaction! ~a ~a ~a~%" node id value)))
  (add-reply-responder "/tr" #'osc-responder))

;;; remove the reply responder
(remove-reply-responder "/tr")
