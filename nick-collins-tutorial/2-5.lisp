;;; Fat Chorus

(play (mix (saw.ar '(440 443 437) 0.1)))

(play (mix (resonz.ar (+ (saw.ar '(440 443 437))
			 (sin-osc.ar 100 0 100))
		      (x-line.kr 10000 10 10)
		      (line.kr 1 0.05 10)
		      (* (lf-saw.kr (line.kr 3 17 3) 0 0.5 0.5)
			 (line.kr 1 0 10)))))

;;; Sample playback rate modulation
;;Set the correct path first:
(defparameter resource-dir "c:/Program Files/SuperCollider-3.9.3/sounds/")
(defparameter *b* (buffer-read (merge-pathnames resource-dir "a11wlk01.wav")))

(play
 (let* ((mod-freq (mouse-x.kr 1 4400 :exponential))
	(mod-index (mouse-y.kr 0.0 10.0 :linear))
	(modulator (sin-osc.kr mod-freq 0 (* mod-freq mod-index) 440)))
   (play-buf.ar 1 *b* (* (buf-rate-scale.kr *b*)
			 (/ modulator 440))
		:loop t)))

;;; Return to the bell

(defun rrand (mi ma)
  "Random number between <mi>nimum and <ma>ximum."
  (if (= mi ma)
      mi
      (+ mi (random (- ma mi)))))

(play
 (let* ((spectrum '(0.5 1 1.19 1.56 2 2.51 2.66 3.01 4.1))
	(amplitudes '(0.25 1 0.8 0.5 0.9 0.4 0.3 0.6 0.1))
	(num-partials (length spectrum))
	(mod-freqs1 (loop :repeat num-partials
			  :collect (1+ (random 5))))
	(mod-freqs2 (loop :repeat num-partials
			  :collect (+ 0.1 (random 3.0))))
	(decay-times (loop :for i :from 1 :upto num-partials
			   :collect (rrand 2.5
					   (+ 2.5
					      (* 5
						 (- 1.0
						    (/ i num-partials))))))))
   (mix (mapcar (lambda (partial amplitude mod-freq1 mod-freq2 decay-time)
		  (let ((freq (* (+ partial (sin-osc.kr mod-freq1 0 0.005))
				 500))
			(amp (* 0.1
				(line.kr 1 0 decay-time)
				(sin-osc.ar mod-freq2 0 0.1 0.9)
				amplitude)))
		    (pan2.ar (sin-osc.ar freq 0 amp) (rrand -1.0 1.0))))
		spectrum amplitudes mod-freqs1 mod-freqs2 decay-times))))
