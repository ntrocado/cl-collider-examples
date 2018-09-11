(play (sin-osc.ar 440 0 0.1))
(play (sin-osc.ar 440 0 (line.kr 0.1 0.0 1.0)))

;;; Envelopes

;;; TODO: plot envelopes

(play (* (sin-osc.ar 440 0 0.1)
	 (env-gen.kr (env '(1 0) '(1.0)))))

(play (saw.ar (env-gen.ar (env '(1000 20) '(1.0))) 0.1))

(play (saw.ar (env-gen.ar (env '(1000 20) '(0.5)))
	      (env-gen.ar (env '(0.1 0) '(2.0)))))

(play (saw.ar (env-gen.kr (env '(1000 20) '(0.5)))
	      (env-gen.kr (env '(0.1 0) '(2.0)))))

;; FM sound
(play (sin-osc.ar (sin-osc.ar 10 0 10 440)
		  0.0
		  (env-gen.kr (env '(0.5 0.0) '(1.0)) :act :free)))

(play (saw.ar (env-gen.kr (env '(500 100) '(1.0)) :act :free)))

(play (saw.ar (sin-osc.ar 1 0 10 440) (line.kr 0 1 1 :act :free)))

(play (saw.ar (sin-osc.ar 1 0 10 440) (x-line.kr 0.0001 1 1 :act :free)))

;;; Releasing envelopes

(play (* (env-gen.ar (env '(0 0.1 0) '(0.1 0.9)) :act :free)
	 (sin-osc.ar 330)))

(defparameter *a* (play (* (env-gen.ar (asr 0.1 0.1 1.0) :act :free)
			   (sin-osc.ar 330))))

(release *a*) ; it's not possible to specify the fade out time

;; Using a gate argument:
(defsynth env-release ((gate 1))
  (out.ar '(0 1)
	  (* (env-gen.ar (asr 0.1 0.1 0.9) :gate gate :act :free)
	     (sin-osc.ar 330))))

(defparameter *a* (synth 'env-release))

(ctrl *a* :gate 0)

;;;

(defparameter *e* (env '(0.2 1.0 0.0) '(0.1 3.0) :lin 1))

(defsynth env-release-node ((gate 1))
  (out.ar '(0 1)
	  (* (env-gen.ar *e* :gate gate :act :free)
	     (sin-osc.ar 550 0 0.1))))

(defparameter *a* (synth 'env-release-node))

(ctrl *a* :gate 0)

;;; Looping envelopes

(defparameter *e* (env '(0.0 0.0 1.0 0.0) '(0.5 1.0 2.0) 0 2 0))

(defsynth env-loop-node ((gate 1))
  (out.ar '(0 1)
	  (* (env-gen.ar *e* :gate gate :act :free)
	     (sin-osc.ar 550 0 0.1))))

(defparameter *a* (synth 'env-loop-node))

(ctrl *a* :gate 0)

;;;

(defparameter *e* (env '(0.0 1.0 -1.0 0.0) '(0.01 0.01 2.0) 0 2 0))

(defsynth fast-env ((gate 1))
  (out.ar '(0 1)
	  (env-gen.ar *e* :gate gate :time-scale (mouse-x.kr 0.1 2.0) :act :free)))

(defparameter *a* (synth 'fast-env))

(ctrl *a* :gate 0)
