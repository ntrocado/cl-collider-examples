;;;; Supercollider Book - Lisp version of the code
;;;; Chapter 6

;;; Cl-patterns is a library providing the equivalent functionality of patterns in SuperCollider
(ql:quickload "cl-patterns/supercollider")

(in-package :cl-patterns)

(defparameter *clock* (make-clock 110/60))

;;; Figure 6.1

(in-package :sc-user)

(defsynth sine ((gate 1)
		(out 0)
		(freq 400)
		(amp 0.4)
		(pan 0)
		(ar 1)
		(dr 1)
		(detune 0))
  (let* ((audio (sin-osc.ar (+ freq detune) 0 amp))
	 (audio (* audio (env-gen.kr (asr ar 1 dr) :gate gate :act :free)))
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

(pbind :dur 0.2
       :freq (pseq '(100 200 300 400 500 600 700 800) 1))

;;; Figure 6.5

(pb :p
  :instrument :sine
  :detune '(0 1 3)
  :freq (pseq (seq-range 100 1200 100)
	      (* 4 5 7))
  :db (pseq '(-20 -40 -30 -40) :inf)
  :pan (pseq '(-1 0 1 0) :inf)
  :dur (pseq '(0.2 0.2 0.2 0.2 0.4 0.4 0.8) :inf)
  :legato (pseq '(2 0.5 0.75 0.5 0.25) :inf))

(play :p)
(stop :p)

;;; Figure 6.6
;;; 'Group' event keyword is not implemented yet.

;;; Figure 6.7
;; 2nd inversion - e loudest
(play (event :instrument :sine
	     :degree '(-3 0 2)
	     :sustain 2
	     :db '(-20 -20 -10)))
;; 2nd inversion - c loudest
(play (event :instrument :sine
	     :degree '(-3 0 2)
	     :sustain 2
	     :db '(-20 -10 -20)))
;; note "fattened"
(play (event :instrument :sine
	     :degree 0
	     :sustain 2
	     :detune '(0 3 5)))
;; each detune is assigned to a different pitch, fat free
(play (event :instrument :sine
	     :degree '(-3 2 4)
	     :sustain 2
	     :detune '(0 3 5))
;; detune rotates through each note in the chord
(play (event :instrument :sine
	     :degree '(-3 2 4)
	     :sustain 2
	     :detune '(0 0 0 3 3 3 5 5 5))))

;;; 6.3.2 Combining Patterns

(defparameter *a* (pbind :instrument :sine
			 :dur (pseq '(0.4) 5)))
(defparameter *b* (pbind :instrument :sine
			 :degree (pseq '(10 6))
			 :dur (pseq '(0.5) 4)))
(play (pseq (list *a* *b*) 2))
(play (ppar (list *a* *b*)))
