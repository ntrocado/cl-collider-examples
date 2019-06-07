;;; SuperCollider's pattern system is emulated, for a good part, in the cl-patterns library.

(ql:quickload :cl-patterns/supercollider)

;;; First we need to define a default synth (mirroring the one in SC)

(in-package :sc-user)

(defsynth default ((out 0) (freq 440) (amp 0.1) (pan 0) (gate 1))
  (let ((z (* (lpf.ar (mix (var-saw.ar (mapcar (lambda (x) (+ x freq))
					       (list 0
						     (- (random 0.4))
						     (random 0.4)))
				       0 0.3))
		      (x-line.kr (+ 4000 (random 1000))
				 (+ 2500 (random 700))))
	      (linen.kr gate 0.01 0.7 0.3 :act :free))))
    (offset-out.ar out (pan2.ar z pan amp))))

;;; Start the clock

(in-package :cl-patterns)

(start-clock-loop 60/60)

;;; Now back to the tutorial...

(defparameter *a* (play (pbind :quant 1.0)))

(stop *a*)

(play (pbind :freq 440 :quant 1.0))

(play (pbind :dur 0.125
	     :midinote (pseq (mapcar (alexandria:curry #'+ 60)
				     '(0 4 0 7 4 0 0)))
	     :amp (prand '(0.125 0.2 0.25))
	     :quant 1.0))

;;; Stop everything with:
(clock-clear-tasks)


(play (pbind :freq 770))

(play (pbind :freq (pseq '(100 200 300))))


(pseq '(0 1 2 3))
(prand '(0 1 2 3) 5)
(pxrand '(0 1 2 3))
(pwrand '(0 1 2 3) '(0.5 0.3 0.1 0.1) 1)
(pfunc (lambda () (random 4)))

;;; Get results like this:
(next-n (pfunc (lambda () (random 4))) 5)
;;-> (1 3 0 0 3)


(defparameter *a* (pseq '(1 3 400) 1))
(defparameter *x* (as-pstream *a*))
(print (next *x*)) ; eval several times


(let* ((a (pshuf '(1 2 3)))
       (x (as-pstream a))
       (y (as-pstream a)))
  (print (next-n x 10))
  (print (next-n y 10)))


(let ((a (pshuf '(1 1 0 1 0) 3)))
  (play (pbind :dur 0.5
	       :midinote (p+ 60 (p* a 7))
	       :amp (p* a 0.1))))


;;; Nesting
(pseq (list (pseq '(100 200 300) 2) 400 500 600))

(play (pbind :freq (pseq (list (pseq '(100 200 300) 2) 400 500 600))))

(play (pbind :freq (pseq (list (prand '(440 442 445 448) 1)
			       (pxrand '(840 741 642) 2)))))

(loop :with a := (as-pstream (pseq (list (prand '(440 442 445 448) 1)
					 (pxrand '(840 741 642) 2))))
      :repeat 20
      :do (print (next a)))

;;; Let's bring some of the hidden variables into view
(let ((*clock* (make-clock 1.5)))
  (play (pbind :freq (pseq '(440 660 990 880 770))
	       :dur (pseq '(1.0 0.5))
	       :legato 0.5
	       :pan (pseq '(0.5 -0.5))
	       :instrument :default)))

;;; Run me first
(in-package :sc-user)

(defsynth alicepavelinstr ((out 0) (alice 440) (pavel 0.5) (pan 0.0) (gate 1))
  (let ((z (* (resonz.ar (pulse.ar alice pavel) (x-line.kr 5000 1000) 0.1 5)
	      (linen.kr gate 0.01 0.1 0.3 :act :free))))
    (out.ar out (pan2.ar z pan))))

(in-package :cl-patterns)

(setf (tempo *clock*) 1.5)

(play (pbind :alice (pseq (mapcar (alexandria:curry #'* 440) '(1 2 3)))
	     :pavel (pseq '(0.1 0.5 0.8))
	     :dur (pseq '(0.5 0.25 0.25))
	     :legato 0.5
	     :instrument :alicepavelinstr))

;;; Cobinding of properties: AFAIK there isn't a direct way to translate this example, even though there are ways of achieving the same result. For example:
(let ((properties `((440 0.4)
		    (330 0.1)
		    (,(pfin (pfunc (lambda () (random 550)))
			    1)
		     ,(pfin (pfunc (lambda () (random 0.8)))
			    1)))))
  (play (pbind :freq (pseq (mapcar #'first properties))
	       :amp (pseq (mapcar #'second properties)))))

;;; Checking already decides properties
(play (pbind :freq (pseq (list 440 330 (pfin (pfunc (lambda ()
						      (+ (random 550) 40)))
					     1)))
	     :amp (pfunc (lambda ()
			   (print *event*)
			   (if (> (event-value *event* :freq) 350)
			       (progn
				 (print "here")
				 (random-range 0.1 0.5))
			       0.05)))))


;;; Two simultaneous voices using ppar
(let* ((melody-vals '((60 0.75) (64 0.5) (66 0.5) (69 0.25)
		      (67 0.75) (64 0.5) (60 0.5) (57 0.25)))
       (melody-pat (pbind :midinote (pseq (mapcar #'first melody-vals))
			  :dur (pseq (mapcar #'second melody-vals))))
       (bass-pat (pbind :midinote (pseq '(48 42))
			:dur 1)))
  (play (ppar (list melody-pat bass-pat))))


;;; Henon map/attractor
;;; Common Lisp doesn't come with co-routines, but this is a possible translation:

(let ((x0 0)
      (y0 0))
  (defun henon (&optional (init nil))
    (if init
	(setf x0 0
	      y0 0)
	(let ((x (+ y0 (- 1 (* 1.4 x0 x0))))
	      (y (* 0.3 x0)))
	  (setf x0 x
		y0 y)
	  (event :pan x :degree (truncate (* y 14)))))))

(defparameter *b* (pbind :scale :major
			 :dur 0.125
			 :embed (pfunc #'henon)
			 :init (henon t)))

(define-pbind-special-init-key :init value)

(progn
  (play (pbind :embed *b*
	       :octave 4
	       :dur 0.375))
  (sleep 4.0)
  (play *b*))


;;; Normal patterns without pbind

;;; Run me first
(in-package :sc-user)

(defsynth pulsepan (freq)
  (out.ar 0 (pan2.ar (* (lf-cub.ar (* freq 0.5) (rand.ir 0.4 0.6) 0.2)
			(x-line.kr 0.001 1.0 0.9 :act :free))
		     (rand.ir -1.0 1.0))))

(in-package :cl-patterns)

(let ((p (as-pstream (pseq (list 100 200 300 330 478 (prand '(987 789) 1))))))
  (loop :repeat 10 ;otherwise infinite
	:for next := (next p)
	:do (sc:synth 'pulsepan :freq next)
	:do (sleep 0.5)))


;;; SynthDescLib

(in-package :sc-user)

(defsynth nickinstr ((out 0) (freq 440) (amp 0.1) (pan 0) (gate 1))
  (let ((z (* (lpf.ar (mix (lf-saw.ar (mapcar (lambda (x) (* x freq))
				      	      '(0.99 1 1.01))
				      0.0 amp))
		      (x-line.kr 5000 1000 1.5))
	      (env-gen.kr (env '(0 1 0) '(0.01 0.01) :lin 1)
			  :gate gate
			  :act :free))))
    (out.ar out (pan2.ar z pan))))

(in-package :cl-patterns)

(play (pbind :dur 1.25
	     :midinote (pseq (mapcar (alexandria:curry #'+ 60) '(0 5 0 7 4 0 0)))
	     :amp (prand '(0.125 0.2 0.25))
	     :instrument (pseq '(:nickinstr :default))
	     :pan (prand '(-1 0 1))))
