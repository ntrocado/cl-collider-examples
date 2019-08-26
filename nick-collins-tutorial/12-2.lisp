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

;;; soprano 'a' sound, direct additive synthesis, one source per formant
(defsynth voice-sound2 ((voiced 1) (freq 440) (amp 0.1))
  (declare (ignore voiced freq))
  (let* ((formant-freqs '(800 1150 2900 3900 4950))
	 (formant-amps (mapcar (alexandria:compose #'dbamp
						   (alexandria:curry #'- 6))
			       '(0 -6 -32 -20 -50)))
	 (formant-bandwidths '(80 90 120 130 140))
	 (output (* (mix (sin-osc.ar formant-freqs 0 formant-amps))
		    amp)))
    (declare (ignore formant-bandwidths))
    (out.ar 0 (list output output))))

(synth 'voice-sound2)

;;; soprano 'a' sound, subtractive synthesis, pass source waveform through formant filtering
(defsynth voice-sound3 ((voiced 1) (freq 440) (amp 0.1))
  (let* ((formant-freqs '(800 1150 2900 3900 4950))
	 (formant-amps (mapcar (alexandria:compose #'dbamp
						   (alexandria:curry #'- 6))
			       '(0 -6 -32 -20 -50)))
	 (formant-bandwidths '(80 90 120 130 140))
	 (source (if voiced
		     (impulse.ar freq)
		     (white-noise.ar 0.2)))
	 (output (* (mix (bpf.ar source
				 formant-freqs
				 (/ formant-bandwidths formant-freqs)
				 formant-amps))
		    10 amp)))
    (out.ar 0 (list output output))))

(defparameter *a* (synth 'voice-sound3))

(ctrl *a* :voiced 0)

;;; let's tweak things by adding in some more complicated sources with vibrato

(defsynth voice-sound4 ((voiced 1) (freq 440) (amp 0.1))
  (let* ((vibrato-noise (lf-noise1.kr 10))
	 (formant-freqs '(800 1150 2900 3900 4950))
	 (formant-amps (mapcar (alexandria:compose #'dbamp
						   (alexandria:rcurry #'- 6))
			       '(0 -6 -32 -20 -50)))
	 (formant-bandwidths '(80 90 120 130 140))
	 (vibrato (midicps (+ (cpsmidi freq)
			      (* (line.kr 0.0 1.0 2.5)
				 (sin-osc.kr (+ 6 vibrato-noise) 0 0.5)))))
	 (periodic-source (lpf.ar (impulse.ar vibrato) 5000))
	 (aperiodic-source (pink-noise.ar 0.7))
	 (source (+ (* voiced periodic-source)
		    (* (- 1.0 voiced) aperiodic-source)))
	 (output (* (mix (bpf.ar source
				 formant-freqs
				 (/ formant-bandwidths formant-freqs)
				 formant-amps))
		    100 amp)))
    (out.ar 0 (list output output))))

(defparameter *a* (synth 'voice-sound4))

(ctrl *a* :voiced 0.8)

;;; For further realism, might modulate subtly the formant data, and experiment with other source waveforms than the impulse

(defsynth voice-sound5 ((voiced 1) (freq 440) (amp 0.1))
  (let* ((vibrato-noise (lf-noise1.kr 10))
	 (formant-freqs '(800 1150 2900 3900 4950))
	 (formant-amps (mapcar (alexandria:compose #'dbamp
						   (alexandria:rcurry #'- 6))
			       '(0 -6 -32 -20 -50)))
	 (formant-bandwidths '(80 90 120 130 140))
	 (vibrato (midicps (+ (cpsmidi freq)
			      (* (line.kr 0.0 1.0 2.5)
				 (sin-osc.kr (+ 6 vibrato-noise) 0 0.5)))))
	 (periodic-source (lpf.ar (pulse.ar vibrato
					    (lf-noise1.kr 1 0.25 0.5)
					    0.1
					    0.5)
				  5000))
	 (aperiodic-source (pink-noise.ar 0.7))
	 (source (+ (* voiced periodic-source)
		    (* (- 1.0 voiced) aperiodic-source)))
	 (output (* (mix (bpf.ar source
				 formant-freqs
				 (/ (+ formant-bandwidths
				       (lf-noise2.kr (lf-noise1.kr 1 0.5 4) 10))
				    formant-freqs)
				 formant-amps))
		    100 amp)))
    (out.ar 0 (list output output))))

(defparameter *a* (synth 'voice-sound5))

(ctrl *a* :voiced 0.7)

;;; Formlet

(play
 (formlet.ar (impulse.ar 440 0.5 0.1)
	     (mouse-x.kr 300 3000 :exponential)
	     0.01
	     (mouse-y.kr 0.1 1.0 :exponential)))

;;; Used for voice synthesis:

(defsynth voice-sound6 ((voiced 1) (freq 440) (amp 0.1) (resonance-scaling 5))
  (let* ((vibrato-noise (lf-noise1.kr 10))
	 (formant-freqs '(800 1150 2900 3900 4950))
	 (formant-amps (mapcar (alexandria:compose #'dbamp
						   (alexandria:rcurry #'- 6))
			       '(0 -6 -32 -20 -50)))
	 (formant-bandwidths '(80 90 120 130 140))
	 (vibrato (midicps (+ (cpsmidi freq)
			      (* (line.kr 0.0 1.0 2.5)
				 (sin-osc.kr (+ 6 vibrato-noise) 0 0.5)))))
	 (periodic-source (lpf.ar (impulse.ar vibrato)
				  5000))
	 (aperiodic-source (pink-noise.ar 0.7))
	 (source (+ (* voiced periodic-source)
		    (* (- 1.0 voiced) aperiodic-source)))
	 (output (* (mix (formlet.ar source formant-freqs 0.001
				     (* resonance-scaling
					(reciprocal formant-bandwidths))
				     formant-amps))
		    10 amp)))
    (out.ar 0 (list output output))))

(defparameter *a* (synth 'voice-sound6))

(ctrl *a* :voiced 0.9)
(ctrl *a* :resonance-scaling 20)
(ctrl *a* :resonance-scaling 2)

;;; Vocoding

(defsynth vocoder ((freq 200) (voiced 1) (amp 4))
  (let* ((centre-freqs (mapcar (alexandria:curry #'* 440)
			       (alexandria:iota 10 :start 1)))
	 ;; (amps (mapcar #'dbamp (make-list 10 :initial-element 0)))
	 (bandwidths (mapcar (alexandria:curry #'* 200)
			     (alexandria:iota 10 :start 1)))
	 (rq (/ bandwidths centre-freqs))
	 (analysis-signal (sound-in.ar))
	 (periodic-source (saw.ar freq))
	 (aperiodic-source (pink-noise.ar 0.7))
	 (synthesis-signal (+ (* voiced periodic-source)
			      (* (- 1.0 voiced) aperiodic-source)))
	 (analysis-filters (amplitude.kr (bpf.ar analysis-signal centre-freqs rq)))
	 (synthesis-filters (* analysis-filters (bpf.ar synthesis-signal
							centre-freqs
							rq))))
    (out.ar 0 (make-list 2 :initial-element (* amp (mix synthesis-filters))))))

(defparameter *a* (synth 'vocoder))

(ctrl *a* :freq 100)
(ctrl *a* :voiced 0.2)
