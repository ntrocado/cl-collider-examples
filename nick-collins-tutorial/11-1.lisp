(in-package :sc-user)

;;; physical model of a stiff string

(defun material-params (material)
  (ccase material
    (nylon (values 2e+7 2000))
    (steel (values 2e+11 7800))))

(defun make-mode-freqs (modes fl beta beta2)
  (let ((arr (make-array modes)))
    (loop :for i :below modes
	  :for n := (1+ i)
	  :for fr := (* n fl (+ 1 beta beta2 (* n n pi pi beta2 0.125)))
	  :if (< fr 21000) :do (setf (aref arr i) fr)
	    :else :do (setf (aref arr i) 21000))
    arr))

(defun decay-time-func (freq a density mu)
  (let* ((m (* (* a 0.5)
	       (sqrt (* 2 pi (/ freq 1.5e-5)))))
	 (calc (* 2 m (/ m (+ (* 2 (sqrt 2) m) 1))))
	 (t1 (* (/ density (* 2 pi 1.2 freq))
		calc))
	 (e1-dive2 0.01)
	 (t2 (/ e1-dive2 (* pi freq)))
	 (t3 (/ 1.0 (* 8 mu 1 freq freq 1))))
    (/ 1 (+ (/ 1 t1) (/ 1 t2) (/ 1 t3)))))

(defun make-mode-amps (modes mode-freqs a density mu)
  (let ((arr (make-array modes)))
    (loop :for i :below modes
	  :do (setf (aref arr i) (decay-time-func (aref mode-freqs i) a density mu)))
    arr))

(defun stiff-string (material &key (modes 10))
  (multiple-value-bind (e density) (material-params material)
    (let* ((a 0.01)
	   (s (* pi a a))
	   (k (* a 0.5))
	   (mu (* density s))
	   (t! 100000)
	   (c (sqrt (/ t! mu)))
	   (l 1.8)
	   (fl (/ c (* 2 l)))
	   (beta (* (* a (/ a l))
		    (sqrt (* pi (/ e t!)))))
	   (beta2 (* beta beta))
	   (mode-freqs (make-mode-freqs modes fl beta beta2))
	   (mode-amps (make-mode-amps modes mode-freqs a density mu))
	   (output (sc::*~ (env-gen.ar (env '(0 1 1 0) '(0 10 0)) :act :free)
			   (mix (loop :for amps :across mode-amps
				      :for freqs :across mode-freqs
				      :collect (sc::*~ (x-line.ar 1.0 amps 10.0)
						       (sin-osc.ar freqs 0 (/ 1.0 modes))))))))
      (declare (ignore k)) ; k is never used...
      (pan2.ar output 0))))

(trace make-mode-freqs make-mode-amps)

(play (stiff-string 'steel))
