;;; Spatialization

;; equal power
(play (pan2.ar (pink-noise.ar 0.2) (mouse-x.kr -1 1)))

;;linear crossfade
(play (lin-pan2.ar (pink-noise.ar 0.2) (mouse-x.kr -1 1)))

;; multichannel
;; direct to speaker
(play (out.ar 1 (pink-noise.ar 0.1)))
;; between speakers
(play (pan-az.ar 8 (pink-noise.ar 0.2) (mouse-x.kr 0 2)))


;;; Ambisonics

(play
 (destructuring-bind (w x y) (pan-b2.ar (pink-noise.ar 0.2) (mouse-x.kr -1 1))
   (decode-b2.ar 2 w x y)))


;;; Simulation of space

(play
 (let ((distance (mouse-x.kr 1 100)))
   (/ (lpf.ar (white-noise.ar 0.5) (- 10000 (* distance 80)))
      distance)))

;; Doppler effect: pitch shift due to change of radial distance of object from observer
(play (saw.ar 440 0.2))
(play
 (let ((radial-distance (line.kr 10 -10 5 :act :free)))
   (delay-c.ar (saw.ar 440 0.2) 1.0 (/ (abs radial-distance) 340.0))))

;; Doppler effect: pitch shift proportional to radial distance
(play
 (let* ((source (saw.ar (demand.kr (impulse.kr (lf-noise0.kr 0.5 0.1 2))
				   0 (d-seq (mapcar #'midicps '(63 60)) +inf+))))
	(radial-distance (env-gen.ar (env '(34 -34) '(10)) :act :free))
	(absolute-rd (abs radial-distance))
	(doppler-shift (delay-c.ar source 1.0 (/ absolute-rd 340.0)))
	(amplitude (reciprocal (max absolute-rd 1.0))))
   (pan2.ar (* amplitude doppler-shift) 0.0)))


