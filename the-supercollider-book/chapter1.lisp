;;;; Supercollider Book - Lisp version of the code
;;;; Chapter 1

;;; Load cl-collider and boot the server
(ql:quickload "cl-collider")
(in-package :sc-user)
(setf *s* (make-external-server "localhost" :port 4444))
(server-boot *s*)
;;;

;;; 1.1 Hello World

(play (sin-osc.ar (lf-noise0.kr 12 600 1000) 0.3))

(play (rlpf.ar (dust.ar '(12 15)) (lf-noise1.ar (list (/ 1 3) (/ 1 4)) 1500 1600) 0.02))

;;; Figure 1.1
(play
 (let ((sines 5)
       (speed 6))
   (mix (/ (loop :for x :from 1 :upto sines
		 :collect (pan2.ar
			   (sin-osc.ar (* x 100) 0
				       (max 0
					    (+ (lf-noise1.kr speed)
					       (line.kr 1 -1 30))))
			   (- 1 (random 2.0))))
	   sines))))

;;; 1.2 Messages and Arguments

;;; 1.3 Nesting

;;; Figure 1.3
(play (comb-n.ar (sin-osc.ar (midicps (lf-noise1.ar 3 24 (lf-saw.ar '(5 5.123) 0 3 80)))
			     0 0.4)
		 1 0.3 2))

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
		 (/ 1 (line.kr 1 20 60)))))	  ; trigger

;;; 1.8 Variables

(play (let ((r (mouse-x.kr 1/3 10)))
	(sin-osc.ar 440 0 (linen.kr (impulse.kr r) 0 1 (/ 1 r)))))

;;; Figure 1.5
(defparameter *p*
  (play
   (let* ((r (line.kr 1 20 60))
	  (t! (impulse.kr r))
	  (e (linen.kr t! 0 0.5 (/ 1 r)))
	  (f (t-rand.kr 1 10 t!)))
     (blip.ar (* f 100) f e))))

(free *p*)

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

;;; 1.9 Synth Definitions

(defsynth sine! ()
  (out.ar 0 (sin-osc.ar)))
(synth 'sine!)
(defsynth sine! ()
  (out.ar 1 (sin-osc.ar)))
(synth 'sine!)

(defsynth different-tones ((freq 440))
  (let ((out (* 0.3 (sin-osc.ar freq))))
    (out.ar 0 out)))

(synth 'different-tones :freq 440)

(defparameter *a* (synth 'different-tones :freq (midicps 64)))
(defparameter *b* (synth 'different-tones :freq (midicps 67)))
(defparameter *c* (synth 'different-tones :freq (midicps 72)))
(ctrl *a* :freq (midicps 65))
(ctrl *c* :freq (midicps 71))
(progn
  (ctrl *a* :freq (midicps 64))
  (ctrl *c* :freq (midicps 72)))
(free *a*)
(free *b*)
(free *c*)

;;; Figure 1.7
(defsynth pm-crotale ((midi 60) (tone 3) (art 1) (amp 0.8) (pan 0))
  (declare (ignore amp))
  (let* ((freq (midicps midi))
	 (envl (perc 0 art))
	 (mod! (+ 5 (/ 1 (i-rand.ir 2 6))))
	 (out (pm-osc.ar freq
			 (* mod! freq)
			 (env-gen.kr envl
				     :time-scale art
				     :level-scale tone)
			 0
			 (env-gen.kr envl
				     :time-scale art
				     :level-scale 0.3)))
	 (out (pan2.ar out pan))
	 (out (* out
		 (env-gen.kr envl
			     :time-scale (* 1.3 art)
			     :level-scale (rand.ir 0.1 0.5)
			     :act :free))))
    (out.ar 0 out)))

;; Then run this a bunch of times:
(synth 'pm-crotale :midi (+ 48 (random 24)) :tone (+ 1 (random 5)))

;;; 1.10 Buses, Buffers, and Nodes

(defparameter *houston*
  (buffer-read (merge-pathnames #p"sounds/a11wlk01-44_1.aiff"
				*sc-synth-program*)))
(defparameter *chouston*
  (buffer-read (merge-pathnames #p"sounds/a11wlk01.wav"
				*sc-synth-program*)))

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
							  (env-gen.kr (linen 0.01 0.96 0.01) :gate trigger)
							  rate))))

;;; Speed and direction change
(play
 (let ((speed (+ (* (lf-noise0.ar 12) 0.2) 1))
       (direction (lf-clip-noise.kr 1/3)))
   (play-buf.ar 1 *houston* (* speed direction) :loop 1)))

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
     (out.ar 1 (play-buf.ar 1 *houston* (* speed direction) :loop t))))


(defparameter *kbus3* (bus-control))
(defparameter *kbus4* (bus-control))
(play (out.kr *kbus3* (range (sin-osc.ar 3) 340 540)))
(play (out.kr *kbus4* (range (lf-pulse.kr 6) 240 640)))
(defsynth switch ((freq 440))
  (out.ar 0 (sin-osc.ar freq 0 0.3)))
(defparameter *x* (synth 'switch))
;; (see https://github.com/byulparan/cl-collider/issues/33)
(map-bus *x* :freq (busnum *kbus3*))
(map-bus *x* :freq (busnum *kbus4*))

;;; Figure 1.10
(play
 (out.ar 0 (pan2.ar (* (play-buf.ar 1 *houston* 1 :loop t)
		       (sin-osc.ar (lf-noise0.kr 12 500 600)))
		    0.5)))
(play
 (let* ((source (play-buf.ar 1 *chouston* 1 :loop t))
	(delay (allpass-c.ar source 2 '(0.65 1.15) 10)))
   (out.ar 0 (+ (pan2.ar source) delay))))

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

;;; 1.11 Arrays, Iteration, and Logical Expressions

;;; Figure 1.12
(let ((a '("C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")))
  (loop :for count :upto 50
	:for p := (+ (random 36) 36)
	:do (print (list count p (elt a (mod p (length a))) (- (round (/ p 12)) 1)))
	:do (sleep 1)))

;;; Figure 1.13
(defun coin (val)
  (> val (random 1.0)))

(defparameter *stop-this* nil)

(let ((a '("C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B"))
      (density 0.7))
  (loop :while (not *stop-this*)
	:for midi := (alexandria:random-elt '(0 2 4 7 9))
	:for oct := (alexandria:random-elt '(48 60 72))
	:if (coin density)
	  :do (print (list (+ midi oct) (elt a midi) (round (/ oct 12))))
	  :and :do (synth 'pm-crotale
		   	  :midi (+ midi oct)
		   	  :tone (+ 1 (random 6))
		   	  :art (+ 0.3 (random 1.7))
		   	  :amp (+ 0.3 (random 0.3))
		   	  :pan (- 1 (random 2.0)))
	:else :do (print "rest")
	:do (sleep 0.2)))

(setf *stop-this* t) ; Eval this to stop.

;;; 1.12 How to "Do" an Array

;;; Figure 1.15
;;; Corrected to use <fund>, which was supposedly the original intention
(let ((fund 220))
  (play
   (* 0.3
      (mix (list (sin-osc.ar fund 0 (max 0 (lf-noise1.kr 12)))
		 (* 1/2
		    (sin-osc.ar (* 2 fund) 0 (max 0 (lf-noise1.kr 12))))
		 (* 1/3
		    (sin-osc.ar (* 3 fund) 0 (max 0 (lf-noise1.kr 12))))
		 (* 1/4
		    (sin-osc.ar (* 4 fund) 0 (max 0 (lf-noise1.kr 12))))
		 (* 1/5
		    (sin-osc.ar (* 5 fund) 0 (max 0 (lf-noise1.kr 12))))
		 (* 1/6
		    (sin-osc.ar (* 6 fund) 0 (max 0 (lf-noise1.kr 12)))))))))

;;; Figure 1.16
(play (* 0.7 (mix
	      (loop :for count :upto 12
		    :for harm := (* 110 (1+ count))
		    :collect
		    (* (sin-osc.ar harm 0
				   (max '(0 0)
					(sin-osc.kr (/ (1+ count) 4))))
		       (/ 1 (1+ count)))))))

;;; Figure 1.18
(play (mix 
  (flet ((rrand (mi ma)
	   (+ mi (random (- ma mi)))))
    (let ((num-res 5)
	  (bells 20)
	  (scale (mapcar #'midicps '(60 62 64 67 69))))
      (loop :repeat bells
	    :for freqs := (loop :repeat num-res
				:collect(* (1+ (rrand 1 15))
					   (alexandria:random-elt scale)))
	    :for amps := (loop :repeat num-res
			       :collect (rrand 0.3 0.9))
	    :for rings := (loop :repeat num-res
				:collect (rrand 1.0 4.0))
	    :for specs := (mapcar (lambda (y) (mapcar (lambda (x)
							(round x 0.01))
						      y))
				  (list freqs amps rings))
	    :for pan := (softclip (lf-noise1.kr (* (rrand 3 6) 2)))
	    :do (print specs)
	    :collect (pan2.ar (klank.ar specs
					(dust.ar 1/6 0.03))
			      pan))))))

;;; Figure 1.19

;;; Adapted for simplicity (in my view at least...)
;;; Uses the cl-patterns library

(defsynth simple-blip ((midi 60) (tone 10) (art 0.125) (amp 0.2) (pan -1))
  (let ((out (pan2.ar (* (blip.ar (midicps midi) tone)
			 (env-gen.kr (perc 0.01 art)))
		      pan))
	(amp (- amp (* (- midi 60) 0.02))))
    (detect-silence.ar out 1.0e-4 :act :free)
    (out.ar 0 (* out amp))))

(ql:quickload "cl-patterns/supercollider")

(in-package :cl-patterns)

(defparameter *clock* (make-clock 8))

(defparameter *inst* (make-array 3))
(defparameter +notes+ '("C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B"))
(defparameter *stop-me* nil)

(defmacro wrap-elt (sequence index)
  `(elt ,sequence
	(mod ,index (length ,sequence))))

(defun scale-steps (scale steps)
  (loop :for i :upto steps
	:collect (+ (alexandria:random-elt scale)
		    (alexandria:random-elt '(48 60)))))

(progn
  (setf *stop-me* nil)
  (loop :until *stop-me*
	:with scale-add := '(4 5 11 nil 10 3 6 nil)
	:with pseq := (make-array 3)
	:for cnt1 :from 0
	:for cnt1%3 := (mod cnt1 3)
	:for steps := (+ 6 (random 6))
	:for harm-seq := (loop :repeat steps :collect (+ 1.0 (random 4.0)))
	:for dur-seq := (loop :repeat steps :collect (+ 0.01 (random 0.89)))
	:for scale := (cond
			;; Every 24 times
			((zerop (mod cnt1 24))
			 '(0 2 7 9))
			;; Every 6 times
			((zerop (mod cnt1 6))
			 (append (list (wrap-elt scale-add
						 (- (round (/ cnt1 6)) 1)))
				 scale))
			(t scale))
	:for pitch-seq := (scale-steps scale steps)
	:for mod12-pitch-seq := (mapcar (alexandria:rcurry #'mod 12) pitch-seq)
	;; Print values
	:do (format t "~%Iteration: ~a (~a)~%scale: ~a~%MIDI seq: ~a~%Sequence (notes): ~a~%"
		    cnt1
		    (wrap-elt '(center right left) cnt1)
		    scale
		    mod12-pitch-seq
		    (mapcar (alexandria:curry #'elt +notes+) mod12-pitch-seq))
	;; Stop previous task if present
	:when (eq (type-of (aref *inst* cnt1%3)) 'task)
	  :do (stop (aref *inst* cnt1%3))
	;; Start new one
	:do (setf (aref *inst* cnt1%3)
		  (play (pbind :instrument :simple-blip
			       :midi (pwalk pitch-seq 1)
			       :tone (pwalk harm-seq 1)
			       :art (pwalk dur-seq 1)
			       :amp (+ 0.1 (random 0.2))
			       :pan (wrap cnt1 -1 2))))
	:do (sleep 12)))

(setf *stop-me* t) ; Stop new sequences
(stop (aref *inst* 0)) ; Stop center sequence 
(stop (aref *inst* 1)) ; Stop right sequence
(stop (aref *inst* 2)) ; Stop left sequence

(in-package :sc-user)

;;; Figure 1.20
(play (let* ((trigger (impulse.kr 10))
	     (wave (sin-osc.kr 1/10))
	     (scale 1)
	     (offset 0)
	     (wave (+ (* wave scale) offset))
	     (label (format nil "scale = ~a, offset = ~a" scale offset)))
	(poll.kr trigger (round wave 0.01) label)))

;;; Figure 1.21
(play
 (let* ((scale 300)
	(offset 600)
	(trigger (impulse.kr 10))
	(control (sin-osc.ar 1/4))
	(control (+ (* control scale) offset)))
   (sin-osc.ar (poll.kr trigger (abs control)))))
;; faux Theremin
(play (sin-osc.ar (sin-osc.ar 8 0 10 (mouse-x.kr 440 1760 :linear))))

;;; Figure 1.22
(play
 (let* ((rate 3.0)
	(trigger (impulse.kr rate))
	(control (lf-noise0.kr rate))
	(carrier 62)
	(mod-ratio 4.125)
	(index 10)
	(carrier-f (midicps carrier)))
   (poll.kr trigger carrier-f "carrier")
   (poll.kr trigger index "index")
   (poll.kr trigger mod-ratio "mod-ratio")
   (pm-osc.ar carrier-f (* carrier-f mod-ratio) index)))

;;; Figure 1.22 - Solutions
(play
 (let* ((rate 3.0)
	(trigger (impulse.kr rate))
	(control (lf-noise0.kr rate))
	(carrier (+ (* control 24) 60))
	;; (mod-ratio (+ control 5))
	(mod-ratio (sin-osc.ar 1/5 0 1 5))
	;; (index (+ (* control 7) 8))
	(index (lf-noise1.kr 1/5 7 8))
	(carrier-f (midicps carrier)))
   (poll.kr trigger carrier "carrier")
   (poll.kr trigger index "index")
   (poll.kr trigger mod-ratio "mod-ratio")
   (pm-osc.ar carrier-f (round (* carrier-f mod-ratio) 0.125) index)))

;;; Figure 1.23
(defsynth pm-osc-ex ((left 10) (right 10) (index-low 4) (index-high 12))
  (let* ((trigger (impulse.kr [left right]))
	 (pitch (round (t-rand.kr 36 72 trigger) 1))
	 (timbre (lf-noise0.kr 1/20 0.2 2))
	 (env (linen.kr trigger 0.01 1 (/ 1 [left right])))
	 (index (+ (* env index-high) index-low))
	 (pitch (midicps pitch))
	 (out (pm-osc.ar pitch (* pitch timbre) index 0 env)))
    (out.ar 0 out)))

(defparameter *a* (synth 'pm-osc-ex))

(ctrl *a* :left 4)
(ctrl *a* :right 5)
(ctrl *a* :index-low 1)
(ctrl *a* :index-high 4)

;;; Figure 1.24
(defsynth latch-demo ((rate 9))
  (let* ((latch-rate (* rate (lf-noise0.kr 1/10 0.03 1.6)))
	 (index (latch.kr (lf-saw.kr latch-rate 0 5 6)
			  (impulse.kr rate)))
	 (freq (midicps
		(round
		 (latch.kr (lf-saw.kr latch-rate 0
				      (max 0 (lf-noise1.kr 1/5 24 10))
				      (lf-noise0.kr 1/7 12 60))
			   (impulse.kr rate))
		 1)))
	 (ratio (lf-noise1.kr 1/10 2 5))
	 (env (env-gen.kr (perc 0 (/ (lf-noise0.kr rate 1 1.5) rate))
			  :gate (impulse.kr rate)
			  :level-scale (min 0.8 (max 0 (lf-noise1.kr [5 5] 2 1)))))
	 (out (pm-osc.ar [freq (* freq 1.5)]
			 (* freq ratio)
			 index
			 0.0
			 env)))
    (out.ar 0 out)))

(defparameter *a* (synth 'latch-demo))

(ctrl *a* :rate 10)
(ctrl *a* :rate 15)
(ctrl *a* :rate 6)

(free *a*)

;;; 1.15 When Bad Code Happens to Good People (Debugging)

;;; Figure 1.25
(defun exp-rand (&optional (rate 1.0))
  (- (/ (log (- 1 (* (- 1 (exp (- rate))) (random 1.0))))
	rate)))

(defun rrand (mi ma)
  (+ mi (random (- ma mi))))
  
(defun exp-rrand (mi ma)
  (+ mi (* (exp-rand) (- ma mi))))

(play
 (let* ((burst-freq 500)
	(burst-env (env-gen.kr (perc 0 0.05)
			       :gate (dust.kr 1/5)
			       :level-scale 0.1))
	(burst (sin-osc.ar burst-freq 0 burst-env))
	(freqs (loop :repeat 10 :collect (exp-rrand 100 1000)))
	(amps (loop :repeat 10 :collect (rrand 0.01 0.1)))
	(rings (loop :repeat 10 :collect (rrand 1.0 6.0)))
	(bell (pan2.ar (klank.ar (list freqs amps rings) burst)
		       (rrand -1.0 1.0)))
	(delay (allpass-n.ar bell 2.5
			     (list  (lf-noise1.kr 7 1.5 1.6)
				    (lf-noise1.kr 7 1.5 1.6))
			     1 0.8)))
   ;; (poll.kr 100 burst-env "env")
   ;; (poll.kr 100 burst "burst")
   ;; (pprint (list freqs amps rings))
   (+ bell delay)))

