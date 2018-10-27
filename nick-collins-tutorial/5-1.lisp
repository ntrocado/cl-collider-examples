(defparameter *b* (buffer-alloc (* 10 44100)))

(bufnum *b*)

(buffer-free *b*)

(defparameter *b*
  (buffer-read (merge-pathnames #p"sounds/a11wlk01.wav"
				(make-pathname :directory (pathname-directory *sc-synth-program*)))))

(defsynth playbuf ((out 0) (bufnum 0) (rate 1) (trigger 1) (start-pos 0) (loop 1))
  (out.ar out (pan2.ar (play-buf.ar 1
				    bufnum
				    (* rate (buf-rate-scale.kr bufnum))
				    :trig trigger
				    :start-pos (* (buf-frames.ir bufnum)
						  start-pos)
				    :loop loop)
		       0.0)))

(synth 'playbuf :out 0 :bufnum (bufnum *b*))

(synth 'playbuf :out 0 :bufnum (bufnum *b*) :rate 0.5)


;;; Example with GUI controlling Synth

With running server eval at the repl:

(ql:quickload "5-1-buffers-gui")
(5-1-buffers-gui:main)

