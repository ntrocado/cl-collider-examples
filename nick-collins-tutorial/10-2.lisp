;;; cl-collider takes care of writing synth definition files to disk and loading them into the server.
;;; It would be possible to reproduce this by calling the relevant "internal" functions, but it might well be simpler to just read the source code (in synthdef.lisp) and see what's going on below the surface.

;;; Using classes that encapsulate these commands
(defsynth lf-par ((freq 440) (pan 0.0))
  (let ((osc (lf-par.ar (lf-par.ar (exp-rand.ir 1 80) 0 (* freq 0.02) freq)
			0 0.1)))
    (out.ar 0 (pan2.ar osc pan))))

(defparameter *a* (synth 'lf-par))

(defparameter *b* (synth 'lf-par
			 :freq (+ 200 (random 500))
			 :pan (- 1 (random 2.0))))

(group-free-all)

;;; play
(play (sin-osc.ar (list (mouse-x.kr 440 880) (mouse-y.kr 330 660))
		  0 0.1))

(play (sin-osc.ar (list (mouse-x.kr 220 770) (mouse-y.kr 110 550))
		  0 0.1))

(let ((synth (play (lpf.ar (pan2.ar (comb-n.ar (impulse.ar 9 0.5 0.1) 0.2 0.2 30)
				    (mouse-x.kr -1.0 1.0))
			   (mouse-y.kr 500 10000)))))
  (sc::sched-add (sc::scheduler *s*) (+ (now) 5) #'free synth))
