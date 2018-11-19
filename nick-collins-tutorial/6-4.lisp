(defsynth impulse ()
  (out.ar 0 (pan2.ar (saw.ar 440 (decay2.ar (impulse.ar 1) 0.001 0.1 0.5))
		     0.0)))

(defsynth continuous ()
  (out.ar 0 (pan2.ar (white-noise.ar 0.1)
		     0.0)))

(defparameter *a* (make-group :id 1))

(defparameter *x* (synth 'impulse :to *a* :pos :head))

;;; delay

(defsynth fx-example-delay ((delay-time 0.1))
  (let* ((input (in.ar 0 2))
	 (effect (delay-n.ar input 1 delay-time)))
    (out.ar 0 effect)))

(free *x*)
(defparameter *x* (synth 'impulse :to *a* :pos :head))
(defparameter *y* (synth 'fx-example-delay :to *a* :pos :tail))

(free *y*)
(defparameter *y* (synth 'fx-example-delay
			 :delay-time 0.3
			 :to *a* :pos :tail))
(free *y*)

;;; vibrato

(play
 (let* ((source (saw.ar 440 0.1))
	(fx (delay-c.ar source 0.01 (sin-osc.ar (+ (random 5) 5) 0 0.0025 0.0075))))
   fx))

;;; chorusing

(play
 (let* ((n 10)
	(source (* (env-gen.ar (env '(0 1 0) '(0.1 0.5))
			       :gate (impulse.kr 2))
		   (saw.ar 440 0.5)))
	(fx (mix (loop :repeat n
		       :for max-delay-time := (+ 0.01 (random 0.02))
		       :for half := (* max-delay-time 0.5)
		       :for quarter := (* max-delay-time 0.25)
		       :collect (delay-c.ar source
					    max-delay-time
					    (lf-noise1.kr (+ 5 (random 10))
							  0.01 0.02))))))
   fx))


;;; reverb

(defsynth fx-example-reverb ((delay-time 0.01) (decay-time 1))
  (let* ((input (in.ar 0 2))
  	 (numc 4)
  	 (numa 6)
  	 (temp (delay-n.ar input 0.08 0.048))
  	 (temp (mix (loop :repeat numc
  			  :collect (comb-l.ar temp 0.1 (+ 0.01 (random 0.09)) 5)))))
    (loop :repeat numa
	  :do (setf temp (allpass-n.ar temp
				       0.051
				       (list (+ delay-time (random 0.04))
					     (+ delay-time (random 0.04)))
				       decay-time)))
    (out.ar 0 (* 0.2 temp))))

(defparameter *y* (synth 'fx-example-reverb :to *a* :pos :tail))

(free *y*)
