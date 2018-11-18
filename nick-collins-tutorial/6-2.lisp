(defparameter *a* (bus-audio))
(defparameter *c* (bus-control))
;; at the repl:
(busnum *a*)
(busnum *c*)

;;; any argument of a synth can be mapped to by control buses:
(defsynth map-example ((freq 440))
  (out.ar 0 (sin-osc.ar freq 0 0.1)))
(defparameter *g* (synth 'map-example))
(control-set (busnum *c*) 660)
(map-bus *g* :freq *c*)
(control-set (busnum *c*) 770)
(defparameter *h* (play (out.kr (busnum *c*) (sin-osc.ar 550 0 100 1000))))
(free *h*)
(ctrl *g* :freq 550)


;;; here is an additional example

(defparameter *b*
  (buffer-read
   (merge-pathnames #p"sounds/a11wlk01.wav"
		    (make-pathname
		     :directory (pathname-directory *sc-synth-program*)))))

(defsynth playbuf ((out 0) (bufnum 0) (rate 1) (trigger 1) (start-pos 0) (loop 1))
  (out.ar out (pan2.ar (play-buf.ar 1
				    bufnum
				    (* rate (buf-rate-scale.kr bufnum))
				    :trig trigger
				    :start-pos (* (buf-frames.ir bufnum)
						  start-pos)
				    :loop loop)
		       0.0)))

(defsynth playbuf-controls ()
  (out.kr 0 (impulse.kr (lf-noise0.kr 0.5 5 6)))
  (out.kr 1 (lf-noise0.kr 0.25 0.5 0.5)))

(synth 'playbuf-controls)

(defparameter *a* (synth 'playbuf
			 :out 0
			 :bufnum (bufnum *b*)
			 :rate 1
			 :trigger "c0"
			 :start-pos "c1"))

(ctrl *a* :start-pos "c")
(ctrl *a* :start-pos 30000)
