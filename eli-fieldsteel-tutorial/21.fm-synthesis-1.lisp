;;; https://www.youtube.com/watch?v=UoXMUQIqFk4

(in-package :sc-user)

(defmacro poll (ugen &optional (freq 10))
  `(poll.kr (impulse.kr ,freq) ,ugen))

(defun dupl (expr)
  (make-list 2 :initial-element expr))

(play
 (dupl (* (sin-osc.ar (+ (poll (mouse-y.kr 200 5000 :exp))
			 (sin-osc.ar (poll (mouse-x.kr 1 2000 :exp))
				     0 (range (lf-noise0.kr 8) 20 10000))))
	  0.2)))

(defsynth fm ((car-hz 500) (mod-hz 100) (mod-amp 200) (atk .01) (rel 1) (amp .2) (pan 0))
  (out.ar 0 (pan2.ar (* (env-gen.kr (perc atk rel) :act :free)
			(sin-osc.ar (+ car-hz
				       (sin-osc.ar mod-hz 0 mod-amp)))
			amp)
		     pan)))


(in-package :cl-patterns)

(start-clock-loop :tempo 1 :force t)

(pb :p
  :instrument :fm
  :dur 1/8
  :car-hz (pexprand 20 10000)
  :mod-hz (pexprand 20 10000)
  :mod-amp (pwhite 0 10000)
  :amp (pexprand .1 .5)
  :atk (pexprand .001 .05)
  :rel (pexprand .05 1.2)
  :pan (pwhite -1.0 1.0))

(play :p)
