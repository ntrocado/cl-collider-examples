;;; https://www.youtube.com/watch?v=dLMSR2Kjq6Y

(in-package :sc-user)

(load "../util.lisp")

(defsynth fm ((freq 500) (m-ratio 1) (c-ratio 1) (index 1) (i-scale 5)
	      (amp .2) (atk .01) (rel 3) (c-atk 4) (c-rel -4) (pan 0))
  (let* ((ienv (env-gen.kr (env (list index (* index i-scale) index)
				(list atk rel)
				(list c-atk c-rel))))
	 (env (env-gen.kr (perc atk rel 1 (list c-atk c-rel))
			  :act :free))
	 (mod (sin-osc.ar (* freq m-ratio) 0 (* freq m-ratio ienv)))
	 (car (* (sin-osc.ar (+ (* freq c-ratio)
				mod))
		 env amp)))
    (out.ar 0 (pan2.ar car pan))))

(synth 'fm :freq (midicps 35) :rel 4 :index 20 :i-scale .05 :m-ratio .5)

;;; fix the pseudo Ugen definition
(in-package :sc)
(defun pm-osc.ar (car-freq mod-freq &optional (pm-index 0.0) (mod-phase 0.0) (mul 1.0) (add 0.0))
  (sin-osc.ar car-freq (mod~ (sin-osc.ar mod-freq mod-phase pm-index) (* 2 pi)) mul add))


(in-package :sc-user)

(play (dupl (* (sin-osc.ar (+ 500 (sin-osc.ar 4 0 (* 4 50))))
	       .2)))

(play (dupl (* (sin-osc.ar 500 (sin-osc.ar 4 0 10))
	       .2)))

(play (dupl (* (pm-osc.ar 500 4 50)
	       .2)))


(defsynth fm2 ((freq 500) (m-ratio 1) (c-ratio 1) (index 1) (i-scale 5)
	      (amp .2) (atk .01) (rel 3) (c-atk 4) (c-rel -4) (pan 0))
  (let* ((ienv (env-gen.kr (env (list index (* index i-scale) index)
				(list atk rel)
				(list c-atk c-rel))))
	 (env (env-gen.kr (perc atk rel 1 (list c-atk c-rel))
			  :act :free))
	 (mod2 (sin-osc.ar (/ freq 10) 0 (* (/ freq 10) ienv)))
	 (mod (sin-osc.ar (+ (* freq m-ratio) mod2) 0 (* freq m-ratio ienv)))
	 (car (* (sin-osc.ar (+ (* freq c-ratio)
				mod))
		 env amp)))
    (out.ar 0 (pan2.ar car pan))))

(synth 'fm2 :rel 3)
