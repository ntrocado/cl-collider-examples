(play (impulse.ar (mouse-x.kr 1 100)))

(play (dust.ar (mouse-x.kr 1 100)))

(play (sin-osc.ar (* (stepper.ar (impulse.ar 10) 0 1 10 1)
		     100)
		  0 0.1))

(play (saw.ar (select.kr (stepper.kr (impulse.kr 4 0.1) 0 0 7 1)
			 (midicps '(72 63 67 72 55 62 63 60)))
	      0.1))

(play (saw.ar (select.kr (stepper.kr (impulse.kr (mouse-x.kr 1 40) 0.1) 0 0 7 1)
			 (midicps '(72 63 67 72 55 62 63 60)))
	      0.1))

;;; ...

;;; slowed down by factor of 10 to be heard as held pitches
(play
 (sin-osc.ar (* 400
		(+ 1 (env-gen.ar (env '(0 1 0 0.5 -0.4)
				      (make-list 4 :initial-element 0.1)
				      :step)
				 :gate (impulse.kr 1))))))

;;; use midicps on output to get scales
(play
 (sin-osc.ar (midicps (env-gen.ar (env '(63 63 60 55 60)
				       (make-list 4 :initial-element 0.125)
				       :step)
				  :gate (impulse.kr 2)))))

(play
 (let* ((trig (impulse.ar 3))
	(sound (mix (lf-pulse.ar (mapcar (alexandria:curry #'* 110)
					 '(1 5/2))
				 0.0 0.5 0.2)))
	(env (env-gen.ar (perc 0.02 0.2) :gate trig)))
   (pan2.ar (* sound env) 0.0)))


(defsynth mytriggersynth ((trig 0))
  (let ((env (env-gen.ar (env '(2 2 1) '(0.0 0.5) :exp)
			 :gate trig)))
    (out.ar 0 (pan2.ar (resonz.ar (saw.ar (* env 440))
				  1000 0.1)
		       0.0))))

(defparameter *a* (synth 'mytriggersynth))

(ctrl *a* :trig 1)
;;;TODO: Compare with behavior in Supercollider
