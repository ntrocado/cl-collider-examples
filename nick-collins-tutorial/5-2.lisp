(defun rrand (n1 n2)
  (if (= n1 n2)
      n1
      (let ((mi (min n1 n2))
	    (ma (max n1 n2)))
	(+ mi (random (- ma mi))))))

(defun exp-rand (mi ma)
  (lin-exp (random 1.0) 0 1.0 mi ma))

(defun rand2 (n)
  (- (random (* 2 n)) n))

(defparameter *b*
  (buffer-read
   (merge-pathnames #p"sounds/a11wlk01.wav"
		    (make-pathname
		     :directory (pathname-directory *sc-synth-program*)))))

;;; simple sine grain synthdef

(defsynth sinegrain (pan freq amp)
  (let ((grain (* (sin-osc.ar freq 0 amp)
		  (- (x-line.kr 1.001 0.001 0.1 :act :free)
		     0.001))))
    (out.ar 0 (pan2.ar grain pan))))

;;; listen to a single grain

(synth 'sinegrain
       :freq (rrand 100 1000)
       :amp (exp-rand 0.05 0.1)
       :pan (rand2 1.0))

;;; schedule 100 random grains over 1 second

(bt:make-thread
 (lambda ()
   (loop :for i :below 100
	 :do (synth 'sinegrain
		    :freq (rrand 100 10000)
		    :amp (exp-rand 0.05 0.1)
		    :pan (rand2 1.0))
	 :do (sleep 0.01))))

;;; schedule 200 random grains over time, decreasing the range of allowed random frequencies and lowering the density over time

(bt:make-thread
 (lambda ()
   (loop :for i :below 200
	 :for time-prop := (expt (/ i 199.0) 3)
	 :do (synth 'sinegrain
		    :freq (exp-rand 100 (- 5000 (* 20 i)))
		    :amp (exp-rand 0.05 0.1)
		    :pan (rand2 1.0))
	 :do (sleep (rrand (max (* time-prop 0.1) 0.01)
			   (* time-prop 0.3))))))

;;; simple playbuf grain synthdef

(defsynth sf-grain ((bufnum 0) (pan 0.0) (start-pos 0.0) (amp 0.1) (dur 0.04))
  (let ((grain (- (* (play-buf.ar 1 bufnum (buf-rate-scale.kr bufnum)
				  :start-pos (* (buf-frames.ir bufnum)
						start-pos))
		     (env-gen.kr (perc 0.01 dur) :act :free))
		  0.001)))
    (out.ar 0 (pan2.ar (* grain amp) pan)))) ; amp is missing

(defparameter *b*
  (buffer-read
   (merge-pathnames #p"sounds/a11wlk01.wav"
		    (make-pathname
		     :directory (pathname-directory *sc-synth-program*)))))

;;; individual grain

(synth 'sf-grain :bufnum (bufnum *b*)
		 :start-pos (random 1.0)
		 :amp (exp-rand 0.005 0.1)
		 :pan (rand2 1.0))

;;; schedule 200 random soundfile playback grains over time, with random offset positions into the soundfile and lowering the density over time

(bt:make-thread
 (lambda ()
   (loop :for i :below 200
	 :for time-prop := (expt (/ i 199.0) 3)
	 :do (synth 'sf-grain
		    :bufnum (bufnum *b*)
		    :start-pos (rrand 0.0 time-prop)
		    :amp (exp-rand 0.005 0.1)
		    :pan (rand2 1.0))
	 :do (sleep (rrand (max (* time-prop 0.1) 0.01)
			   (* time-prop 0.4))))))

;;; The sound can be made to disappear into a stream of tiny quanta and reappear, coalescing out of distinct particles

;; eval at the repl
(ql:quickload :5-2-granular)
(5-2-granular:main)
;; and see 5-2-granular.lisp
