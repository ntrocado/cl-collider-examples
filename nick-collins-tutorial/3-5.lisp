;;; We need the synth defined in Chapter 3.3
(defsynth event ((freq 440) (amp 0.5) (pan 0.0))
  (let ((env (env-gen.ar (env '(0 1 1 0) '(0.01 0.1 0.2)) :act :free)))
    (out.ar 0 (pan2.ar (* (blip.ar freq) env amp) pan))))

;;; In Common Lisp there isn't a standard way of forking.
;;; The same result can be obtained with threads.

(ql:quickload "bordeaux-threads")

(loop :for j :from 0 :upto 3
      :do
	 (bt:make-thread (lambda ()
			   (loop :for i :from 0 :upto 7
				 :do
				    (synth 'event
					   :freq (midicps (+ 48
							     (* i 3.3)
							     j))
					   :amp (- 1.0 (/ i 8)))
				    (sleep 0.5))))
	 (sleep 4.0))

;;; Two quickly created sounds

(defsynth sound1 ((freq 440) (amp 0.1))
  (let ((sound (* (lpf.ar (saw.ar freq) 2000)
		  (line.kr 1 0 0.1 :act :free)
		  amp)))
    (out.ar 0 (pan2.ar sound 0.0))))

(defsynth sound2 ((freq 440) (amp 0.1))
  (let ((sound (* (hpf.ar (lf-par.ar freq) 1000)
		  (line.kr 0 1 0.5 :act :free)
		  amp)))
    (out.ar 0 (pan2.ar sound 0.0))))

(synth 'sound1)

(synth 'sound2)

