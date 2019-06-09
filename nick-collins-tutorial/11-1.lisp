(in-package :sc-user)

;;; physical model of a stiff string

(defun material-params (material)
  (ccase material
    (nylon (values 2e+7 2000))
    (steel (values 2e+11 7800))))

(defun make-mode-freqs (modes fl beta beta2)
  (let ((arr (make-array modes)))
    (loop :for i :below modes
	  :for n := (1+ i)
	  :for fr := (* n fl (+ 1 beta beta2 (* n n pi pi beta2 0.125)))
	  :if (< fr 21000) :do (setf (aref arr i) fr)
	    :else :do (setf (aref arr i) 21000))
    arr))

(defun decay-time-func (freq a density mu)
  (let* ((m (* (* a 0.5)
	       (sqrt (* 2 pi (/ freq 1.5e-5)))))
	 (calc (* 2 m (/ m (+ (* 2 (sqrt 2) m) 1))))
	 (t1 (* (/ density (* 2 pi 1.2 freq))
		calc))
	 (e1-dive2 0.01)
	 (t2 (/ e1-dive2 (* pi freq)))
	 (t3 (/ 1.0 (* 8 mu 1 freq freq 1))))
    (/ 1 (+ (/ 1 t1) (/ 1 t2) (/ 1 t3)))))

(defun make-mode-amps (modes mode-freqs a density mu)
  (let ((arr (make-array modes)))
    (loop :for i :below modes
	  :do (setf (aref arr i) (decay-time-func (aref mode-freqs i) a density mu)))
    arr))

(defun stiff-string (material &key (modes 10))
  (multiple-value-bind (e density) (material-params material)
    (let* ((a 0.01)
	   (s (* pi a a))
	   (k (* a 0.5))
	   (mu (* density s))
	   (t! 100000)
	   (c (sqrt (/ t! mu)))
	   (l 1.8)
	   (fl (/ c (* 2 l)))
	   (beta (* (* a (/ a l))
		    (sqrt (* pi (/ e t!)))))
	   (beta2 (* beta beta))
	   (mode-freqs (make-mode-freqs modes fl beta beta2))
	   (mode-amps (make-mode-amps modes mode-freqs a density mu))
	   (output (sc::*~ (env-gen.ar (env '(0 1 1 0) '(0 10 0)) :act :free)
			   (mix (loop :for amps :across mode-amps
				      :for freqs :across mode-freqs
				      :collect (sc::*~ (x-line.ar 1.0 amps 10.0)
						       (sin-osc.ar freqs 0 (/ 1.0 modes))))))))
      (declare (ignore k)) ; k is never used...
      (pan2.ar output 0))))

(trace make-mode-freqs make-mode-amps)

(play (stiff-string 'steel))

;;; piano sound by James McCartney

;; hear the energy impulse without any comb resonation
(play
 (let* ((strike (impulse.ar 0.01))
	(env (decay2.ar strike 0.008 0.04))
	(noise (lf-noise2.ar 3000 env)))
   (* 10 noise)))

;; single strike with comb resonation
(play
 (let* ((strike (impulse.ar 0.01))
	(env (decay2.ar strike 0.008 0.04))
	(pitch (+ 36 (random 54))))
   (pan2.ar (mix (loop :for detune :in '(-0.05 0 0.04)
		       :for delay-time := (/ 1 (midicps (+ pitch detune)))
		       :collect (comb-l.ar (lf-noise2.ar 3000 env)
					   delay-time
					   delay-time
					   6)))
	    (/ (- pitch 36) (1- 27)))))

;; synthetic piano patch
(play
 (mix (loop :repeat 6
	    :collect
	    (let* ((pitch (+ 36 (random 54)))
		   (strike (impulse.ar (+ 0.1 (random 0.4))
				       (random (* 2 pi))
				       0.1))
		   (hammer-env (decay2.ar strike 0.008 0.04)))
	      (pan2.ar (mix (loop :for detune :in '(-0.05 0 0.04)
				  :for delay-time := (/ 1 (midicps (+ pitch detune)))
				  :collect (comb-l.ar (lf-noise2.ar 3000 hammer-env)
						      delay-time
						      delay-time
						      6)))
		       (/ (- pitch 36) (1- 27)))))))

;;; Karplus-Strong

(play (pluck.ar (white-noise.ar 0.1)
		(impulse.kr 1)
		(reciprocal 440)
		(reciprocal 440)
		10
		(mouse-x.kr -0.999 0.999)))

;;; broken down as individual UGens

(play
 (let* ((freq 440)
	(time (reciprocal freq))
	(ex (white-noise.ar (env-gen.kr (env '(1.0 1.0 0.0 0.0)
					     `(,time 0 100)))))
	(local (local-in.ar 1))
	(filter (lpz-1.ar (+ ex local)))
	(delay (delay-n.ar filter time (- time (control-dur.ir)))))
   (poll.kr (impulse.kr 0) (control-dur.ir))
   (local-out.ar (* delay 0.95))
   (out.ar 0 (pan2.ar filter 0.0))))

;;; modulate the length of the delay to make a vibrato

(play
 (let* ((freq 440)
	(time (reciprocal freq))
	(ex (white-noise.ar (env-gen.kr (env '(1.0 1.0 0.0 0.0)
					     `(,time 0 100)))))
	(freq (sin-osc.ar 6 0 10 freq))
	(time (reciprocal freq))
	(local (local-in.ar 1))
	(filter (lpz-1.ar (+ ex local)))
	(delay (delay-n.ar filter (reciprocal 430) (- time (control-dur.ir)))))
   (local-out.ar (* delay 0.99))
   (out.ar 0 (pan2.ar filter 0.0))))

;;; Contributions from Thor Magnusson

;; we use a noise ugen to generate a burst
(play
 (let* ((att 0)
	(dec 0.001)
	(burst-env (env-gen.kr (perc att dec)
			       :gate (impulse.kr 1))))
   (pink-noise.ar burst-env)))

;; but then we use Comb delay to create the delay line that creates the tone

;; let's create a synthdef using Karplus-Strong
(defsynth ks-guitar (note pan rand delay-time (noise-type 1))
  (declare (ignore noise-type))
  (let* ((env (env '(1 1 0) '(2 0.001)))
	 (x (decay.ar (impulse.ar 0 0 rand) (+ 0.1 rand) (white-noise.ar)))
	 (x (comb-l.ar x 0.05 (reciprocal note) delay-time (env-gen.ar env :act :free)))
	 (x (pan2.ar x pan)))
    (out.ar 0 (leak-dc.ar x))))

;; and play the synthdef
(loop :repeat 20
      :do (synth 'ks-guitar
		 :note (+ 220 (random 400))
		 :pan (- (random 2.0) 1.0)
		 :rand (+ 0.1 (random 0.1))
		 :delay-time (+ 2 (random 1.0)))
      :do (sleep (+ (random 1.0) 0.5)))

;; here using patterns
(ql:quickload :cl-patterns/supercollider)
(enable-backend :supercollider)
(start-clock-loop 120/60)
(in-package :cl-patterns)
(pb :ks-pattern
  :instrument :ks-guitar
  :note (pseq (mapcar #'midinote-freq '(60 61 63 66)))
  :dur (pseq '(0.25 0.5 0.25 1))
  :rand (prand '(0.2 0.15 0.15 0.11))
  :pan (- (random 2.0) 1.0)
  :delay-time (+ 2.0 (random 1.0)))
(play :ks-pattern)
(stop :ks-pattern)

(in-package :sc-user)
;; compare using whitenoise and pinknoise as an exciter
;; whitenoise
(proxy :noise-exciter
       (let* ((att 0)
	      (dec 0.001)
	      (delay-decay 0.5)
	      (midi-pitch 69)
	      (delay-time (reciprocal (midicps midi-pitch)))
	      (burst-env (env-gen.kr (perc att dec) :gate (impulse.kr (/ 1 delay-decay))))
	      (burst (white-noise.ar burst-env)))
	 (comb-l.ar burst delay-time delay-time delay-decay 1.0 burst)))
;; pinknoise
(proxy :noise-exciter
       (let* ((att 0)
	      (dec 0.001)
	      (delay-decay 0.5)
	      (midi-pitch 69)
	      (delay-time (reciprocal (midicps midi-pitch)))
	      (burst-env (env-gen.kr (perc att dec) :gate (impulse.kr (/ 1 delay-decay))))
	      (burst (pink-noise.ar burst-env)))
	 (comb-l.ar burst delay-time delay-time delay-decay 1.0 burst)))

;; note that delay-time is controlling the pitch here
(defsynth ks-pluck ((midi-pitch 69) (delay-decay 1.0))
  (let* ((att 0)
	 (dec 0.001)
	 (delay-time (mapcar (alexandria:compose #'reciprocal #'midicps)
			     (list midi-pitch (+ midi-pitch 12))))
	 (burst-env (env-gen.kr (perc att dec)))
	 (signal-out (pink-noise.ar burst-env))
	 (signal-out (comb-l.ar signal-out delay-time delay-time delay-decay 1.0 signal-out)))
    (detect-silence.ar signal-out 1.0e-4 :act :free)
    (out.ar 0 signal-out)))

;; then run this payback task
(defparameter *play* t)
(loop :while *play*
      :do (synth 'ks-pluck
		 :midi-pitch (+ 30 (random 60))
		 :delay-decay (+ 0.1 (random 2.9)))
      :do (sleep (alexandria:whichever 0.125 0.125 0.25)))
(setf *play* nil)


;;; Two examples from the STK physical modeling kit

;;; The STK UGens are found within sc3-plugins and are not defined in cl-collider by default.
;;; Therefore, first we have to define them with defugen...

;; mandolin
;; (for some reason the mandolin UGen doesn't work on my system, so this is untested...
(sc::defugen (stk-mandolin "StkMandolin")
    (&optional (freq 520) (body-size 64) (pick-position 64) (string-damping 69) (string-detune 10) (after-touch 64) (trig 1) (mul 1.0) (add 0.0))
  ((:ar
    (madd (sc::multinew sc::new 'sc::ugen freq body-size pick-position string-damping string-detune after-touch trig) mul add))))

(play (* 3 (stk-mandolin.ar)))

;; violin bow
(sc::defugen (stk-bowed "StkBowed")
    (&optional (freq 220) (bow-pressure 64) (bow-position 64) (vib-freq 64) (vib-gain 64) (loudness 64) (gate 1) (attack-rate 1) (decay-rate 1) (mul 1.0) (add 0.0))
  ((:ar
    (madd (sc::multinew sc::new 'sc::ugen freq bow-pressure bow-position vib-freq vib-gain loudness gate attack-rate decay-rate) mul add))))

(defsynth bow (freq (bow-pressure 64) (bow-position 64) (vib-freq 64) (vib-gain 64) (loudness 64))
  (let* ((sig (stk-bowed.ar freq bow-pressure bow-position vib-freq vib-gain loudness))
	 (sig (* sig (env-gen.ar (linen) :act :free))))
    (out.ar '(0 1) (* sig 10))))

(defparameter *play* t)
(loop :repeat 100
      :while *play*
      :do (synth 'bow
		 :freq (+ 200 (random 240))
		 :bow-pressure (+ 22 (random 42))
		 :bow-position (+ 22 (random 42))
		 :vib-freq (+ 22 (random 22))
		 :vib-gain (+ 22 (random 22)))
      :do (sleep 1))
(setf *play* nil)
