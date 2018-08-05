(defparameter *a* (play (* 0.1 (sin-osc.ar 440))))
(free *a*)

;;;Arguments (one more or less equivalent way...)
(proxy :a (with-controls ((freq 440)) (* 0.1 (sin-osc.ar freq))))
(ctrl :a :freq 330)

(defun rrand (mi ma)
  "Random number between <mi>nimum and <ma>ximum."
  (+ mi (random (- ma mi))))

(proxy :a (with-controls ((freq 440) (amp 0.1)) (* (sin-osc.ar freq) amp)))
(ctrl :a :freq (rrand 220 440))
(ctrl :a :amp (rrand 0.05 0.2))
