(ql:quickload '("cl-patterns"
		"cl-patterns/supercollider"))

(defparameter *clock* (make-clock (/ 110 60)))


;;; Figure 6.1

(in-package :sc-user)

(defsynth sine ((gate 1)
		(out 0)
		(freq 400)
		(amp 0.4)
		(pan 0)
		(ar 1)
		(dr 1))
  (let* ((audio (sin-osc.ar freq 0 amp))
	 (audio (* audio (linen.kr gate ar 1 dr :act :free)))
	 (audio (pan2.ar audio pan)))
    (offset-out.ar out audio)))

(in-package :cl-patterns)

;;; Figure 6.2

(defparameter *e*
  (event :type :note
	 :instrument :sine
	 :freq 400
	 :amp 0.1
	 :pan 0
	 :ar 2
	 :dr 4
	 :sustain 2))

(play *e*)

;;; Figure 6.4

(defparameter *a*
  (pbind :dur 0.2
	 :freq (pseq '(100 200 300 400 500 600 700 800) 1)))

;;; Figure 6.5

(defparameter *p*
  (pbind :instrument :sine
	 :detune '(0 1 3)
	 :freq (pseq (seq-range 100 1200 100)
		     (* 4 5 7))
	 :db (pseq '(-20 -40 -30 -40) :inf)
	 :pan (pseq '(-1 0 1 0) :inf)
	 :dur (pseq '(0.2 0.2 0.2 0.2 0.4 0.4 0.8) :inf)
	 :legato (pseq '(2 0.5 0.75 0.5 0.25) :inf)))

(play *p*)

;;; Figure 6.6
;;; 'Group' event keyword is not implemented yet.
