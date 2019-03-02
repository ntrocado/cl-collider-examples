;;; Figure 15.1
(defparameter *x*
  (play (let* ((in (sound-in.ar 0))
	       (amp (amplitude.ar in)))
	  (destructuring-bind (freq has-freq) (pitch.kr in)
	    (declare (ignore has-freq))
	    (* (lf-tri.ar (* freq '(1 2)))
	       amp)))))

(free *x*)

;;; Figure 15.2
(defparameter *b* (buffer-alloc 1024 :chanls 1))

(defparameter *x*
  (play (let* ((in (sound-in.ar 0))
	       (fft (fft (bufnum *b*) in))
	       (loudness (loudness.kr fft)))
	  (poll.kr (impulse.kr 20) loudness)
	  (out.ar 0 (pan2.ar in)))))

(free *x*)
(buffer-free *b*)

