(defparameter *b* (buffer-alloc (* 10 44100)))

(bufnum *b*)

(buffer-free *b*)

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

(synth 'playbuf :out 0 :bufnum (bufnum *b*))

(synth 'playbuf :out 0 :bufnum (bufnum *b*) :rate 0.5)


;;; Example with GUI controlling Synth

With running server eval at the repl:

(ql:quickload "5-1-buffers-gui")
(5-1-buffers-gui:main)

;;; BufRd

(defparameter *b*
  (buffer-read
   (merge-pathnames #p"sounds/a11wlk01.wav"
		    (make-pathname
		     :directory (pathname-directory *sc-synth-program*)))))

(defsynth bufrd ((out 0) (bufnum 0))
  (out.ar out
	  (pan2.ar (buf-rd.ar 1
			      bufnum
			      (lag.ar (k2a.ar (* (buf-frames.ir (bufnum *b*))
						 (mouse-x.kr 0.0 1.0)))
				      (mouse-y.kr 0.0 1.0)))
		   0.0)))

(synth 'bufrd)

;;; DiskIn

(defparameter *b*
  (buffer-cue-soundfile 
   (merge-pathnames #p"sounds/a11wlk01.wav"
		    (make-pathname
		     :directory (pathname-directory *sc-synth-program*)))
   :chanls 1))

(defsynth disk-in ()
    (out.ar 0 (disk-in.ar 1 (bufnum *b*))))

(synth 'disk-in)

;;; Wavetables and oscillators

(defparameter *b* (buffer-alloc 512 :chanls 1))

(wavetable *b* :sine1 (mapcar (alexandria:curry #'/ 1.0)
			      (alexandria:iota 6 :start 1))
	       :as-wavetable nil)

(play (osc-n.ar (bufnum *b*)
		(mouse-x.kr 10 1000)
		0 0.1))
