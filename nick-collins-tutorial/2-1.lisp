;;; The tutorial uses graphical elements from the SuperCollider IDE, like the frequency analyzer. In order to hack in LISP and still have access to these tools, one solution is:
;;;
;;; (a) open the SuperCollider IDE and run the following code:
;;; (
;;; o = Server.local.options;
;;; o = ServerOptions.new;
;;; o.maxLogins = 2;
;;; t = Server(\Local, NetAddr("127.0.0.1", 57110), o);
;;; t.makeWindow;
;;; t.boot;
;;; FreqScope.new(server: t);
;;; )
;;;
;;; (b) on the LISP side, after loading the cl-collider library, start the server with:
(in-package #:sc-user)
(setf *s* (make-external-server "localhost" :port 57110 :just-connect-p t))
(server-boot *s*)
;;; Now you can use the visualization tools from the SuperCollider IDE to analyse the audio signal.


;;; Subtractive Synthesis

(play (white-noise.ar 0.1))

(play (lpf.ar (white-noise.ar 0.1) 1000))

(play (lpf.ar (white-noise.ar 0.1)
	      (line.kr 10000 1000 10)))

(play (resonz.ar (lf-noise0.ar 400) 1000 0.1))

(play (resonz.ar (lf-noise0.ar 400)
		 (line.kr 10000 1000 10)
		 0.1))

(play
 (let* ((source (lf-noise0.ar 400))
	(line (line.kr 10000 1000 10))
	(filter (resonz.ar source line 0.1)))
   filter))

;;; Additive Synthesis

(play (sin-osc.ar))

(play
 (+ (sin-osc.ar 400 0 0.1)
    (sin-osc.ar 660 0 0.1)))

(play (sin-osc.ar '(400 660) 0 0.1))
;; or:
(named-readtables:in-readtable :sc)
(play (sin-osc.ar [400 660] 0 0.1))

(play
 (pan2.ar (white-noise.ar 0.1) (mouse-x.kr -1 1)))

(play
 (mix (sin-osc.ar '(400 660) 0 0.1)))

(play
 (pan2.ar (mix (sin-osc.ar '(400 660) 0 0.1))
	  (mouse-x.kr -1 1)))

;;; Sawtooth wave

(play
 (let* ((n 10)
	(wave (mix (loop :for i :upto n
			 :for mult := (* (expt -1 i)
					 (/ 0.5 (+ i 1)))
			 :collect (* (sin-osc.ar (* 440 (+ i 1)))
				     mult)))))
   (pan2.ar (/ wave n) 0.0)))

;;; Square wave

(play
 (let* ((n 10)
	(wave (mix (loop :for i :upto n
			 :for harmonic-number := (+ (* i 2)
						    1)
			 :collect (* (/ (sin-osc.ar (* 440 harmonic-number))
					harmonic-number)
				     0.25)))))
   (pan2.ar wave 0.0)))

;;; Triangle wave

(play
 (let* ((n 10)
	(wave (mix (loop :for i :upto n
			 :for harmonic-number := (+ (* i 2)
						    1)
			 :for mult := (* (expt -1
					       (/ (- harmonic-number 1)
						  2))
					 (/ 1.0
					    (* harmonic-number harmonic-number)))
			 :collect (/ (* (sin-osc.ar (* 440 i))
					mult)
				     n)))))
   (pan2.ar wave 0.0)))

;;; Bell sound

(mapcar (alexandria:curry #'* 500)
	'(0.5 1 1.19 1.56 2 2.51 2.66 3.01 4.1))

(play (mix (sin-osc.ar (mapcar (alexandria:curry #'* 500)
			       '(0.5 1 1.19 1.56 2 2.51 2.66 3.01 4.1))
		       0
		       0.1)))

(play (mix (sin-osc.ar (mapcar (alexandria:curry #'* 500)
			       '(0.5 1 1.19 1.56 2 2.51 2.66 3.01 4.1))
		       0
		       (mapcar (alexandria:curry #'* 0.1)
			       '(0.25 1 0.8 0.5 0.9 0.4 0.3 0.6 0.1)))))

;;;

(play
 (let ((n 10))
   (mix (sin-osc.ar (mapcar (alexandria:curry #'* 250)
			    (alexandria:iota 10 :start 1))
		    0
		    (/ 1 n)))))


