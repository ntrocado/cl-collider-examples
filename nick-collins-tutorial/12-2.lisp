(in-package :sc-user)

(defsynth voice-sound1 ((voiced 1) (freq 440) (amp 0.1))
  (let* ((source (if voiced
		     (impulse.ar freq)
		     (white-noise.ar 0.2)))
	 (filter (b-lowpass.ar (bpf.ar source 2000 0.1 source)
			       4000 0.25 100)))
    (out.ar 0 (* amp filter))))

(defparameter *a* (synth 'voice-sound1))

(ctrl *a* :voiced 0)
