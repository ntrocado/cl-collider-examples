;;; Ring Modulation
(play
 (let* ((carr-freq (mouse-x.kr 440 5000 :exponential))
	(mod-freq (mouse-y.kr 1 5000 :exponential))
	(carrier (sin-osc.ar carr-freq 0 0.5))
	(modulator (sin-osc.ar mod-freq 0 0.5)))
   (* carrier modulator)))

;;; Amplitude Modulation
(play (sin-osc.ar 440 0 0.5))
(play (sin-osc.ar 440 0 0.5 0.5))

(play
 (let* ((carr-freq (mouse-x.kr 440 5000 :exponential))
	(mod-freq (mouse-y.kr 1 5000 :exponential))
	(carrier (sin-osc.ar carr-freq 0 0.5))
	(modulator (sin-osc.ar mod-freq 0 0.25 0.25)))
   (* carrier modulator)))

;;; Frequency Modulation

;;; For an example of how to create a GUI with commonqt/qtools
;;; see 2-4-fm-mod.asd and 2-4-fm-mod.lisp.
;;; To try the application run at the repl:
(ql:quickload "2-4-fm-mod")
(2-4-fm-mod:main)

;;; Control FM through the modulation index
(play
 (let ((mod-freq (mouse-x.kr 1 440 :exponential))
       (mod-index (mouse-y.kr 0.0 10.0)))
   (sin-osc.ar (sin-osc.ar mod-freq 0 (* mod-freq mod-index) 440) 0 0.25)))

;; Harmonicity ratio
(play
 (let* ((carr-freq 440)
	(harmonicity (round (mouse-x.kr 0 10) 1))
	(mod-index (mouse-y.kr 0.0 10.0))
	(mod-freq (* carr-freq harmonicity)))
   (sin-osc.ar (+ carr-freq
		  (* (sin-osc.ar mod-freq)
		     mod-freq
		     mod-index))
	       0.0
	       0.1)))

;;; Phase Modulation

(play
 (let ((mod-freq (mouse-x.kr 1 1000 :exponential))
       (mod-index (mouse-y.kr 0.0 100.0))
       (conversion (/ (* 2 pi) 44100)))
   (sin-osc.ar 0 (+ (phasor.ar 0 (* 440 conversion) 0 (* 2 pi))
		    (* (* mod-freq mod-index)
		       conversion
		       (sin-osc.ar mod-freq)))
	       0.25)))

