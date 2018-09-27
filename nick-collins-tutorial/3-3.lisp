(defsynth event ((freq 440) (amp 0.5) (pan 0.0))
  (let ((env (env-gen.ar (env '(0 1 1 0) '(0.01 0.1 0.2)) :act :free)))
    (out.ar 0 (pan2.ar (* (blip.ar freq) env amp) pan))))

(synth 'event)

(synth 'event :freq 880 :amp 0.2 :pan 1.0)

(progn
  (synth 'event)
  (sleep 1.0)
  (synth 'event))

(progn
  (synth 'event)
  (sleep 1.0)
  (synth 'event)
  (sleep 1.0)
  (synth 'event)
  (sleep 1.0)
  (synth 'event))

(loop :repeat 4 :do (progn (synth 'event) (sleep 1.0)))

(loop :repeat 8
      :do (progn
	    (synth 'event :freq (+ 440 (random 441)))
	    (sleep 1.0)))

(loop :repeat 8
      :do (progn
	    (synth 'event :freq (+ 440 (random 441)))
	    (sleep (+ 0.2 (random 0.6)))))

(progn
  (loop :repeat 8
      :do (progn
	    (synth 'event :freq (+ 110 (random 221)))
	    (sleep (+ 0.2 (random 0.6)))))
  (loop :repeat 4
      :do (progn
	    (synth 'event)
	    (sleep 0.25))))


;;; TODO: tempo clock

;;; schedule things in the future using functionality provided by clocks

(at (+ 1.0 (now))
  (synth 'event))
