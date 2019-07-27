(in-package :sc-user)

;;; Check if output sample rate is 44100Hz
(server-status)

;;; warning; LOUD, awkward on ear
(play (lf-saw.ar (+ 4410 (round (mouse-x.kr 0 10) 1)) 0 0.5))

;;; aliasing if mouse moved left
(play (lf-saw.ar (+ 1102.5 (round (mouse-x.kr 0 10) 1)) 0 0.5))

;;; no aliasing
(play (saw.ar (+ 1102.5 (round (mouse-x.kr 0 10) 1)) 0.5))


;;; Chorusing

(play (saw.ar 440 0.2)) ; plain

(play (mix (saw.ar (mapcar (alexandria:curry #'* 440)
			   '(0.99 1.01))
		   0.2)))

(play
 (let ((num-detune 4))
   (mix (saw.ar (mapcar (alexandria:curry #'* 440)
			(loop :repeat num-detune
			      :collect (1+ (random 0.01))))
		0.2))))

(play
 (mix (loop :repeat 4
	    :for freq-mult := (+ 1
				 (sin-osc.ar (lf-noise1.kr (+ 0.25
							      (random 0.25))
							   4
							   5)
					     (random pi)
					     0.01))
	    :collect (lf-saw.ar (* 440 freq-mult) (random pi) 0.2))))

(play
 (mix (loop :for i :below 3
	    :collect (funcall (alexandria:whichever #'lf-tri.ar
						    #'lf-cub.ar
						    #'lf-par.ar)
			      (* 110 (expt 2 i))
			      (random pi)
			      (dbamp (neg (random 10)))))))

;;; Now to work on the source+filter model for subtractive synthesis

;;; Standard filter
(proxy :z
       (resonz.ar (mix (saw.ar (mapcar (alexandria:curry #'* 440)
				       '(0.99 1 1.01))
			       0.3))
		  (mouse-x.kr 100 20000 :exponential)
		  (mouse-y.kr 0.1 1.0 :linear)
		  0.5))
(free :z)

;;; BEQSuite
(proxy :z
       (b-lowpass4.ar (mix (saw.ar (mapcar (alexandria:curry #'* 440)
				       '(0.99 1 1.01))
				   0.3))
		      (mouse-x.kr 100 20000 :exponential)
		      (mouse-y.kr 0.1 1.0 :linear)
		      0.5))
(free :z)

;;; can distort at high gain
(proxy :z
       (moog-ff.ar (mix (saw.ar (mapcar (alexandria:curry #'* 440)
				       '(0.99 1 1.01))
				   0.3))
		      (mouse-x.kr 100 20000 :exponential)
		   (mouse-y.kr 0.1 4.0 :linear)))
(free :z)


;;; Demand Rate UGens

(play (poll.kr (impulse.kr 10) ;print the output
	       (let ((sequence (d-seq '(-0.3 0.5 0.0 0.4) +inf+)))
		 (demand.ar (impulse.ar 10) 0 sequence))
	       "d-seq"))

;;; nesting is possible
(play (poll.kr (impulse.kr 10)
	       (let ((sequence (d-seq `(-0.3 ,(d-rand '(-1 1) 1) 0.0 0.4) +inf+)))
		 (demand.ar (impulse.ar 10) 0 sequence))
	       "d-seq"))

;;; musical use
(play
 (let* ((sequence (d-seq `(60 ,(d-rand '(48 72) 1) 63 62.8) +inf+))
	(freq (midicps (demand.kr (impulse.kr (mouse-x.kr 1 100)) 0 sequence))))
   (saw.ar freq 0.1)))

;;; multichannel use 1
(play
 (let* ((sequence (d-seq `(60 ,(d-rand '(47 73) 1) 63 61.5) +inf+))
	(freq (midicps (demand.kr (impulse.kr '(5 5.1)) 0 sequence))))
   (sync-saw.ar freq 300 0.1)))

;;; multichannel use 2
(play
 (let* ((sequence (d-seq `((60 48) ,(d-rand '(48 72) 1) 63 (61 62.8) (55 62.5) (63 62.1)) +inf+))
	(freq (midicps (demand.kr (impulse.kr 5) 0 sequence))))
   (list (sync-saw.ar (first freq) (lf-noise0.kr 7 100 230) 0.1)
	 (sync-saw.ar (second freq) (lf-noise2.kr 17 400 630) 0.1))))

;;; interaction of durations for holding current value and output value sequence
(duty.ar (d-seq '(0.025 0.05) +inf+)
	 :reset 0
	 :level (d-seq '(-0.5 0.5 0 -1 1) +inf+))

;;; putting various things together: rhythmic synthesis
(play
 (let* ((tempo 0.5)
	(freq (midicps (duty.kr (d-seq (mapcar (alexandria:curry #'* tempo)
					       '(0.25 0.25 0.5 0.75 0.75 0.75 0.25 0.25 0.25))
				       +inf+)
				:reset 0
				:level (d-seq `(60 62 63 65 67 55 53
						   ,(d-rand '(51 49 58 70) 1) 70
						   ,(d-rand '(70 48 72 36) 1))
					      +inf+))))
	(filter-freq (duty.kr (d-seq (mapcar (alexandria:curry #'* tempo)
					     '(0.25 0.25 0.25 0.25 1.0))
				     +inf+)
			      :reset 0
			      :level (d-seq (loop :repeat 16
						  :collect (lin-exp (random 1.0) 0 1 300 5000))
					    +inf+)))
	(source (mix (sync-saw.ar (mapcar (alexandria:curry #'sc::*~ (lag.kr freq 0.05))
					  '(1 0.5 0.25 1.01 1.25))
				  (lf-noise2.kr (mapcar (alexandria:curry #'* tempo 2)
							'(0.25 0.5 1 2 4))
						200 300)
				  0.1)))
	(filtered (b-lowpass4.ar source (lag.kr filter-freq 0.0625) 0.5)))
   (pan2.ar filtered (lf-noise1.kr tempo 0.25))))


(play
 (let* ((trig (impulse.kr 8 '(0 0.1)))
	(freq (midicps (demand.kr trig 0 (d-rand '(60 63 60 63 65 63 70 67 60 62 60 63 65 63 70 67 67 72 75 72 67 70 63 55) +inf+))))
	(source (mix
		 (loop :for i :below 4
		       :collect (lf-saw.ar (* (mapcar (alexandria:curry
						       #'sc::*~
						       (lag.kr freq
							       (mouse-y.kr 0.0 0.15)))
						      `(,(* 0.25 1.5) 0.125))
					      (+ (expt 2 i)
						 (sin-osc.ar (lf-noise1.kr (+ 0.25 (random 0.25))
									   4 5)
							     (random pi)
							     0.01)))
					   (random pi)
					   0.2))))
	(env (env-gen.ar (env '(0 1 0) '(0.01 0.25)) :gate trig))
	(filter (b-lowpass.ar (* 0.5 source)
			      (+ 300 (* (mouse-x.kr 100 20000 :exponential)
					env))
			      0.2
			      env)))
   (pan2.ar filter 0.0)))

;;; using InterplEnv
;;; NOTE: InterplEnv is currently deprecated, with its functionality supported by Env

(defun round-to (n d)
  "(round-to 2.23 0.1) -> 2.2; (round-to 2.23 0.5) -> 2.0."
  (* (round n d) d))

(play
 (let* ((freq (midicps (i-env-gen.kr (env (alexandria:shuffle '(60 62 63 67 70 67 70 72 48))
					  (make-list 8 :initial-element 0.125))
				     (round (phasor.ar (> (lf-noise0.kr 1) 0)
						       (* 0.5 (/ 1.0 (sample-rate.ir)))
						       0.0 1.0)
					    1/8))))
	(source (mix
		 (loop :for i :below 5
		       :collect (saw.ar (+ (* freq (* 0.25 (expt 2 i)))
					   (sin-osc.ar (lf-noise1.kr (alexandria:random-elt
								      '(0.125 0.25 0.5))
								     7 8)
						       (random pi)
						       0.01))
					0.2))))
	(filter (b-lowpass.ar (* 0.5 source)
			      (+ 1000 (* 2000 (env-gen.ar (env '(0 1 0)
							       '(0.01 0.25))
							  :gate (impulse.kr 2))))
			      0.2)))
   (limiter.ar (g-verb.ar (+ (* filter 0.25)
			     (pan2.ar filter))))))
