(play (pan2.ar (sin-osc.ar 440 0 0.1) 0.0))

(play (pan2.ar (sin-osc.ar (mouse-x.kr 440 880) 0 0.1) 0.0))

(labels ((rrand (mi ma)
	   "Random number between <mi>nimum and <ma>ximum."
	   (+ mi (random (- ma mi))))
	 (exp-rand (&optional (rate 1.0))
	   "Exponential distributed random number."
	   (- (/ (log (- 1 (* (- 1 (exp (- rate))) (random 1.0))))
		 rate)))
	 (exp-rrand (mi ma &optional (rate 1.0))
	   "Exponential distributed random number between <mi>minum and <ma>ximum."
	   (+ mi (* (exp-rand rate) (- ma mi))))
	 (rand2 (n)
	   "Random number between -n and n."
	   (rrand (- n) n)))
  (let ((n 3))
    (play
     (resonz.ar (mix (loop
		       :repeat n
		       :for freq := (rrand 50 560.3)
		       :for num-cps := (rrand 2 20)
		       :collect
		       (pan2.ar (gendy1.ar (random 6)
					   (random 6)
					   (random 1.0)
					   (random 1.0)
					   freq
					   freq
					   (random 1.0)
					   (random 1.0)
					   num-cps
					   (sin-osc.kr (exp-rrand 0.02 0.2)
						       0
						       (/ num-cps 2)
						       (/ num-cps 2))
					   (/ 0.5 (sqrt n)))
				(rand2 1.0))))
		(mouse-x.kr 100 2000)
		(mouse-y.kr 0.01 1.0)))))

(play (pan2.ar (sin-osc.ar 440 0 0.1) 0.0))
