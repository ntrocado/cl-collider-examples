;;; Figure 7.3

(proxy :p
       (with-controls ((x 0))
	 (let* (;; (x (sin-osc.kr 4))
		(y (sin-osc.kr 13))
		(z (sin-osc.ar (* 0.2
				  (+ 600
				     (* 500
					(mod (* x y)
					     0.4)))))))
	   z)))

(proxy :foo (sin-osc.kr 10))
(ctrl :p :f :foo)

(defparameter *x* (sin-osc.kr 4))
(defparameter *z*)

(proxy :p (with-controls ((f 200)) (sin-osc.ar f 0 0.3)))
