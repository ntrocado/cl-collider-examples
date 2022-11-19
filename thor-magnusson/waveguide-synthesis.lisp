;;; https://leanpub.com/ScoringSound/read#leanpub-auto-waveguide-synthesis

(in-package #:sc-user)

(defsynth waveguide-flute ((scl .2) (pch 72) (ipress .9) (ibreath .09)
			   (ifeedbk1 .4) (ifeedbk2 .4) (dur 1) (gate 1) (amp 2) (vibrato .2))
  (declare (ignore scl gate))
  (let* ((sr (sample-rate.ir))
	 (cr (control-rate.ir))
	 (block (reciprocal cr))
	 (ifqc (midicps pch))
	 ;; noise envelope
	 (kenv1 (env-gen.kr (env (list 0 (* 1.1 ipress) ipress ipress 0)
				 (list .06 .2 (- dur .46) .2))))
	 ;; overall envelope
	 (kenv2 (env-gen.kr (env (list 0 amp amp 0)
				 (list .1 (- dur .02) .1))
			    :act :free))
	 ;; vibrato envelope
	 (kenvibr (* (env-gen.kr (env (list 0 0 1 1 0)
				      (list .5 .5 (- dur 1.5) .5)))
		     vibrato))
	 ;; create airflow and vibrato
	 (aflow1 (lf-clip-noise.ar sr kenv1))
	 (kvibr (sin-osc.ar 5 0 (* .1 kenvibr)))
	 (asum1 (+ (* ibreath aflow1) kenv1 kvibr))
	 (afqc (- (+ (- (reciprocal ifqc) (/ asum1 20000) (/ 9 sr))
		     (/ ifqc 12000000))
		  block))
	 (fdbck-array (local-in.ar 1))
	 (aflute1 fdbck-array)
	 (asum2 (+ asum1 (* aflute1 ifeedbk1)))
	 ;; (ax (delay-l.ar asum2 (* (reciprocal ifqc) .5) (* afqc .5)))
	 (ax (delay-c.ar asum2
			 (* (- (reciprocal ifqc) block)
			    .5)
			 (+ (- (* afqc .5)
			       (/ asum1 ifqc cr))
			    .001)))
	 (apoly (- ax (cubed ax)))
	 (asum3 (+ apoly (* aflute1 ifeedbk2)))
	 (avalue (lpf.ar asum3 2000))
	 (aflute1 (delay-c.ar avalue (- (reciprocal ifqc) block) afqc))
	 (fdbck-array (list aflute1))
	 (signal-out avalue))
    (local-out.ar fdbck-array)
    (offset-out.ar 0 (list (* signal-out kenv2) (* signal-out kenv2)))))

(synth 'waveguide-flute
       :amp .5 :dur 5 :ipress .9 :ibreath .00536 :ifeedbk .4 :ifeedbk .4 :pch 60 :vibrato .2)
