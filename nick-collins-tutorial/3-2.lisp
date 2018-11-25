(defsynth sine ()
  (out.ar 0 (sin-osc.ar (rand.ir 440 880) 0 0.1)))

(synth 'sine)

(defparameter *a* (synth 'sine))
(defparameter *b* (synth 'sine))
(defparameter *c* (synth 'sine))

(free *a*)
(free *b*)
(free *c*)

;;; Adding arguments

(defsynth sine ((freq 440) (amp 0.1))
  (out.ar 0 (sin-osc.ar freq 0 amp)))

(synth 'sine)
(synth 'sine :freq 880)

(defparameter *a* (synth 'sine))
(defparameter *b* (synth 'sine :freq 550))
(defparameter *c* (synth 'sine :freq 660 :amp 0.5))

(ctrl *c* :freq 1000)
(ctrl *b* :amp 0.3 :freq 100)

(free *a*)
(free *b*)
(free *c*)

;;; Recovering synth definition metadata:
(print (get-synthdef-metadata 'sine))


