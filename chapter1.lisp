;;;; Supercollider Book - Lisp version of the code
;;;; Chapter 1

;;; Load cl-collider and boot the server
(ql:quickload "sc")
(in-package :sc-user)
(setf *s* (make-external-server "localhost" :port 4444))
(server-boot *s*)
;;;

;;; 1.1 Hello World

(play (sin-osc.ar (lf-noise0.kr 12 600 1000) 0.3))

(play (rlpf.ar (dust.ar '(12 15)) (lf-noise1.ar (list (/ 1 3) (/ 1 4)) 1500 1600) 0.02))

;;; Figure 1.1
;;; TODO
(play
 (let ((sines 5)
       (speed 6))
   (mix (loop :repeat sines
	      :collect (let ((x nil))
			 (pan2.ar (sin-osc.ar (* (1+ x) 100)
					      0
					      )))) )))
;;;

;;; 1.2 Messages and Arguments

;;; 1.3 Nesting

;;; Figure 1.3
(play (comb-n.ar (sin-osc.ar (midicps (lf-noise1.ar 3 24 (lf-saw.ar '(5 5.123) 0 3 80)))
			     0 0.4)
		 1 0.3 2))
;;;

;;; 1.4 Receiver.message, Comments

;;; 1.5 Enclosures

;;; 1.6 Multichannel Expansion

(play (blip.ar 25 (lf-noise0.kr 5 12 14) 0.3))	     ; single channel
(play (blip.ar 25 (lf-noise0.kr '(5 10) 12 14) 0.3)) ; stereo
(play (blip.ar 25 (lf-noise0.kr '(5 10 12 25) 12 14) 0.3))     ; quad
(play (blip.ar 25 (lf-noise0.kr '(5 4 7 9 5 1 9 2) 12 14) 0.3))  ; 8 channel

;;; 1.7 Help!

(play (pm-osc.ar 440 550 7)) ; like an FM radio
(play (pm-osc.ar 440 (mouse-y.kr 1 550) (mouse-x.kr 1 15)))

;;; Figure 1.4
(play (blip.ar
       ;; frequency
       (t-rand.kr 100 1000			  ; range
		  (impulse.kr (line.kr 1 20 60))) ; trigger
       ;; number of harmonics or VCF
       (t-rand.kr 1 10				  ; range
		  (impulse.kr (line.kr 1 20 60))) ; trigger
       ;; mul, or amplitude, VCA
       (linen.kr (impulse.kr (line.kr 1 20 60))	  ; trigger
		 0				  ; attack
		 0.5				  ; sustain
		 (/ 1 (line.kr 1 20 60))))))	  ; trigger
;;;

;;; 1.8 Variables

(play (let ((r (mouse-x.kr 1/3 10))) (sin-osc.ar 440 0 (linen.kr (impulse.kr r) 0 1 (/ 1 r)))))

;;; Figure 1.5
(defparameter *p*
  (play
   (let* ((r (line.kr 1 20 60))
	  (t! (impulse.kr r))
	  (e (linen.kr t! 0 0.5 (/ 1 r)))
	  (f (t-rand.kr 1 10 t!)))
     (blip.ar (* f 100) f e))))

(free *p*)
;;;

;;; Figure 1.6
(play
 (let* ((r (impulse.kr 10))
	(c (t-rand.kr 100 5000 r))
	(m (t-rand.kr 100 5000 r)))
   (* 0.3 (pm-osc.ar c m 12))))

(play
 (let* ((rate 4)
	(carrier (+ (* (lf-noise0.kr rate)
		       500)
		    700))
	(mod-ratio (mouse-x.kr 1 2.0)))
   (* 0.3 (pm-osc.ar carrier (* carrier mod-ratio) 12))))
;;;

;;; 1.9 Synth Definitions

(defsynth sine! ()
  (out.ar 0 (sin-osc.ar)))
(sine!)
(defsynth sine! ()
  (out.ar 1 (sin-osc.ar)))
(sine!)

(defsynth different-tones ((freq 440))
  (let ((out (* 0.3 (sin-osc.ar freq))))
    (out.ar 0 out)))

(different-tones 440)

(defsynth different-tones (&key (freq 440))
  (let ((out (* 0.3 (sin-osc.ar freq))))
    (out.ar 0 out)))

(different-tones :freq 550)
(different-tones)

(defparameter *a* (different-tones :freq (midicps 64)))
(defparameter *b* (different-tones :freq (midicps 67)))
(defparameter *c* (different-tones :freq (midicps 72)))
(ctrl *a* :freq (midicps 65))
(ctrl *c* :freq (midicps 71))
(progn
  (ctrl *a* :freq (midicps 64))
  (ctrl *c* :freq (midicps 72)))
(free *a*)
(free *b*)
(free *c*)

;;; Figure 1.7
(defsynth pm-crotale (&key (midi 60) (tone 3) (art 1) (amp 0.8) (pan 0))
  (declare (ignore amp))
  (let* ((freq (midicps midi))
	 (envl (perc 0 art))
	 (mod! (+ 5 (/ 1 (i-rand.ir 2 6))))
	 (out (pm-osc.ar freq
			 (* mod! freq)
			 (env-gen.kr envl :time-scale art :level-scale tone)
			 0
			 (env-gen.kr envl :time-scale art :level-scale 0.3)))
	 (out (pan2.ar out pan))
	 (out (* out (env-gen.kr envl :time-scale (* 1.3 art) :level-scale (rand.ir 0.1 0.5) :act :free))))
    (out.ar 0 out)))

;; Then run this a bunch of times:
(pm-crotale :midi (+ 48 (random 24)) :tone (+ 1 (random 5)))
;;;

;;; 1.10 Buses, Buffers, and Nodes

(defparameter *houston* (buffer-read "c:/Program Files/SuperCollider-3.8.0/sounds/a11wlk01-44_1.aiff"))
(defparameter *chouston* (buffer-read "c:/Program Files/SuperCollider-3.8.0/sounds/a11wlk01.wav"))

(play (play-buf.ar 1 *houston*))
(play (play-buf.ar 1 *chouston*))

;;; Figure 1.8
(print (list (bufnum *houston*) (chanls *houston*) (sc::path *houston*) (frames *houston*)))
(print (list (bufnum *chouston*) (chanls *chouston*) (sc::path *chouston*) (frames *chouston*)))

;;phasing
(play
 (let* ((frames! (frames *houston*))
	(rate '(1 1.01))
	(trigger (impulse.kr rate)))
   (play-buf.ar 1 *houston* 1 :trig trigger :start-pos (* frames!
							 (line.kr 0 1 60)
							 (env-gen.kr (linen 0.01 0.96 0.01 trigger))
							 rate))))
;;;

;;; Figure 1.9
(progn
  ;; if these haven't been used they will hold 0
  (defparameter *kbus1* (bus-control)) ; a control bus
  (defparameter *kbus2* (bus-control)) ; a control bus
  (play
   (let ((speed (+ (* (in.kr *kbus1* 1) 0.2) 1))
	 (direction (in.kr *kbus2*)))
     (play-buf.ar 1 *houston* (* speed direction) :loop t))))

(progn
  ;; now start the controls
  (play (out.kr *kbus1* (lf-noise0.kr 12)))
  (play (out.kr *kbus2* (lf-clip-noise.kr 1/4))))

;; Now start the second buffer with the same control input buses,
;; but send it to the right channel using (out.ar 1 etc.)
(play
   (let ((speed (+ (* (in.kr *kbus1* 1) 0.2) 1))
	 (direction (in.kr *kbus2*)))
     (out.ar 1 (play-buf.ar 1 *houston* (* speed direction) :loop t)))))
;;;

(defparameter *kbus3* (bus-control))
(defparameter *kbus4* (bus-control))
(play (out.kr *kbus3* (range (sin-osc.ar 3) 340 540)))
(play (out.kr *kbus4* (range (lf-pulse.kr 6) 240 640)))
(defsynth switch (&key (freq 440))
  (out.ar 0 (sin-osc.ar freq 0 0.3)))
(defparameter *x* (switch))
;; Currently the only way to route a control bus to a synth's arg is:
;; (see https://github.com/byulparan/cl-collider/issues/33)
(sc::send-message *s* "/n_map" (sc::id *x*) "freq" (busnum *kbus3*))
(sc::send-message *s* "/n_map" (sc::id *x*) "freq" (busnum *kbus4*))

;;; Figure 1.10
(play
 (out.ar 0 (pan2.ar (* (play-buf.ar 1 *houston* 1 :loop t)
		       (sin-osc.ar (lf-noise0.kr 12 500 600)))
		    0.5)))
(play
 (let* ((source (play-buf.ar 1 *chouston* 1 :loop t))
	(delay (allpass-c.ar source 2 '(0.65 1.15) 10)))
   (out.ar 0 (+ (pan2.ar source) delay))))
;;;

;;; Figure 1.11
;; Create and name buses
(defparameter *delay* (bus-audio :server *s* :chanls 2))
(defparameter *mod* (bus-audio :server *s* :chanls 2))
(defparameter *gate* (bus-audio :server *s* :chanls 2))
(defparameter *k5* (bus-control))

(defparameter *control-syn* (play (out.kr *k5* (lf-noise0.kr 4))))

(defparameter *delay-syn*
  (play (out.ar 0 (allpass-c.ar (in.ar *delay* 2)
				2 '(0.65 1.15) 10))
	:to *control-syn* :pos :after))

(defparameter *mod-syn*
  (play (out.ar *delay* (* (in.ar *mod* 2)
			      (sin-osc.ar (+ (* (in.kr *k5*)
						500)
					     1100))))
	:to *delay-syn* :pos :before))

(defparameter *gate-syn*
  (play (out.ar (list 0 *mod*) (* (in.ar *gate* 2)
			    (max 0 (in.kr *k5*))))
	:to *mod-syn* :pos :before))

(defparameter *pb-group* (make-group :pos :before :to *control-syn*))

(play (out.ar *gate* (pan2.ar (play-buf.ar 1 1 *houston* :loop t) 0.5))
      :to *pb-group*)
(play (out.ar *gate* (pan2.ar (play-buf.ar 1 1 *chouston* :loop t) 0.5))
      :to *pb-group*)
;;;

;;; 1.11 Arrays, Iteration, and Logical Expressions
