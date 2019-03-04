;;; Figure 15.1
(defparameter *x*
  (play (let* ((in (sound-in.ar 0))
	       (amp (amplitude.ar in)))
	  (destructuring-bind (freq has-freq) (pitch.kr in)
	    (declare (ignore has-freq))
	    (* (lf-tri.ar (* freq '(1 2)))
	       amp)))))

(free *x*)


;;; Figure 15.2 Loudness

(defparameter *b* (buffer-alloc 1024 :chanls 1))

(defparameter *x*
  (play (let* ((in (sound-in.ar 0))
	       (fft (fft (bufnum *b*) in))
	       (loudness (loudness.kr fft)))
	  (poll.kr (impulse.kr 20) loudness)
	  (out.ar 0 (pan2.ar in)))))

(free *x*)
(buffer-free *b*)


;;; Figure 15.3 MFCC

(defparameter *b* (buffer-alloc 1024 :chanls 1))

(defparameter *x*
  (play (let* ((in (sound-in.ar 0))
	       (fft (fft (bufnum *b*) in))
	       (array (mfcc.kr fft)))
	  (print (length array))
	  (out.kr 0 array)
	  (out.ar 0 (pan2.ar in)))))

(defparameter *c* (bus-control :chanls 13))

(print (loop :for i :from 0 :below 13
	     :collect (control-get i)))


;;; Figure 15.4 Onsets

(defparameter *b* (buffer-alloc 512))

(defparameter *x*
  (play
   (let* ((sig (sound-in.ar 0))
	  (chain (fft *b* sig))
	  (onsets (onsets.kr chain (mouse-x.kr 0 1) :complex))
	  (trigger (send-trig.kr onsets))
	  (pips (sin-osc.ar 880 0 (env-gen.kr (perc 0.001 0.1 0.2)
					      :gate onsets))))
     (declare (ignore trigger))
     (out.ar 0 (+ (* sig 0.1)
		  pips)))))

(add-reply-responder "/tr" (lambda (&rest msg) (format t "~{~a~^, ~}~%" msg)))

(free *x*)
(remove-reply-responder "/tr")
(buffer-free *b*)


;;; Figure 15.5

(defparameter *b* (buffer-alloc 1024 :chanls 1))

(defsynth beat-track ()
  (let ((source (sound-in.ar 0)))
    (destructuring-bind (trackb trackh trackq tempo)
	(beat-track.kr (fft (bufnum *b*) source))
      (let ((bsound (pan2.ar (lpf.ar (* (white-noise.ar)
					(decay.kr trackb 0.05))
				     1000)
			     0.0))
	    (hsound (pan2.ar (bpf.ar (* (white-noise.ar)
					(decay.kr trackh 0.05))
				     3000 0.66)
			     -0.5))
	    (qsound (pan2.ar (hpf.ar (* (white-noise.ar)
					(decay.kr trackq 0.05))
				     5000)
			     0.5)))
	(poll.kr (impulse.kr 1) tempo)
	(out.ar 0 bsound)))))

(defparameter *x* (synth 'beat-track))

(free *x*)
(buffer-free *b*)


;;; Figure 15.6 KeyTrack

(defparameter *d* (buffer-read "/path/to/soundfile"))

(defparameter *b* (buffer-alloc 4096 :chanls 1))

(defparameter *x*
  (play (let* ((in (play-buf.ar 1 (bufnum *d*) (buf-rate-scale.kr (bufnum *d*))
				:loop 1))
	       (fft (fft (bufnum *b*) in))
	       (key (keytrack.kr fft)))
	  (poll.kr (impulse.kr 1) key)
	  (out.ar 0 (pan2.ar in)))))

(free *x*)
(buffer-free *b*)


;;; Figure 15.7 Simple melodic transcription

;;; TODO
;;; with SendTrig instead of SharedOut

;;; Figure 15.8 OnlineMIDI

;;; cl-collider doesn't have midi built-in

