(in-package :sc-user)

;;; example 1
(defparameter *b* (buffer-alloc 1024))

(play
 (let* ((in (white-noise.ar 0.8))
	(chain (fft *b* in)))
   (list (ifft.ar chain) in)))

(buffer-free *b*)

;;; example 2
(defparameter *b* (buffer-alloc 1024))

(play
 (let* ((in (white-noise.ar 0.8))
	(chain (fft *b* in))
	(chain (pv-brick-wall chain (line.kr -1 1 10))))
   (pan2.ar (ifft.ar chain) 0.0)))

(buffer-free *b*)

;;; example 3
(defparameter *b* (buffer-alloc 1024))
(defparameter *c* (buffer-alloc 1024))
(defparameter *d* (buffer-alloc 1024))

(play
 (let* ((in1 (saw.ar 440 0.8))
	(in2 (sound-in.ar 0))
	(chain1 (fft *b* in1))
	(chain2 (fft *c* in2))
	(copy-chain (pv-copy chain2 *d*))
	(chain1 (pv-mag-mul chain1 chain2))
	(copy-chain (pv-mag-freeze copy-chain (lf-noise0.kr 10))))
   (list (* 0.25 (ifft.ar chain1))
	 (ifft.ar copy-chain))))

(buffer-free *b*)
(buffer-free *c*)
(buffer-free *d*)

;;; example 4
(defparameter *b* (buffer-alloc 1024))
(defparameter *c*
  (buffer-read
   (merge-pathnames #p"sounds/a11wlk01.wav"
		    (make-pathname
		     :directory (pathname-directory *sc-synth-program*)))))

(play
 (let* ((in (play-buf.ar 1 *c* (buf-rate-scale.kr *c*) :loop 1))
	(chain (fft *b* in))
	(chain (pv-collect chain
			   (frames *b*)
			   (lambda (mag phase index)
			     (declare (ignore phase index))
			     (let ((noise (lf-noise1.kr (+ 0.5 (random 0.6)))))
			       (list (* noise mag)
				     (range noise (- pi) pi))))
			   :frombin 0
			   :tobin 250
			   :zeroothers 1)))
   (pan2.ar (ifft.ar chain) 0.0)))
