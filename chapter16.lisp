;;;; Supercollider Book - Lisp version of the code
;;;; Chapter 16

(ql:quickload :cl-patterns)

;;; Figure 16.1

(defsynth gabor (&key out (freq 440) (sustain 1) pan (amp 0.1) (width 0.25))
  (let* ((env (lf-gauss.ar sustain width :loop 0 :action 2))
	 (son (f-sin-osc.ar freq (* 0.5 pi) env)))
    (offset-out.ar out (pan2.ar son pan amp))))

;;; 16.4

(proxy :grain
  (with-controls ((env-buf -1) (density 10) (grain-dur 0.1) (amp 0.2))
    (let* ((trig (impulse.kr density))
	   (pan (mouse-x.kr -1 1))
	   (freq-dev (white-noise.kr (mouse-y.kr 400 0))))
      (* (grain-sin.ar 2 trig grain-dur (+ 440 freq-dev) pan env-buf) amp))))

