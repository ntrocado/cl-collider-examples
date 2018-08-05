(play (sin-osc.ar 440 0 2))

(play (sin-osc.ar 440 0 0.1))

(play (sin-osc.ar 440 0 (mouse-y.kr 1.0 0.1)))

(play (sin-osc.ar 440 0 0.1 (mouse-y.kr 0.9 -0.9)))

(play (sin-osc.ar 440 0 (mouse-x.kr 0.1 1.0) (mouse-y.kr 0.9 -0.9)))

(play
 (let ((cutoff (sin-osc.ar 1 0 (mouse-x.kr 0.0 1700.0) 2000.0)))
   (lpf.ar (white-noise.ar) cutoff)))

(play (* 0.1 (sin-osc.ar)))
(play (sin-osc.ar 440 0 0.1))

(play (+ 0.5 (* 0.1 (sin-osc.ar))))
(play (sin-osc.ar 440 0 0.1 0.5))

;;; Limit the volume of a single SinOsc
(play (sin-osc.ar 440 0.0 0.1))
(play (* 0.1 (sin-osc.ar)))
(play (sin-osc.ar 440 0.0 (dbamp -20)))

;;; Modulation
(play (sin-osc.ar (sin-osc.ar 3 0 40 440) 0 0.1))

