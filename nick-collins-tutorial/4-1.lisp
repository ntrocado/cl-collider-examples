(setf *s* (make-external-server "localhost" :port 4444))
(server-boot *s*)

(play (sin-osc.ar (mouse-x.kr 20 20000 :linear) 0 0.1))

(play (sin-osc.ar (mouse-x.kr 20 20000 :exponential) 0 0.1))

;;;

(let ((vals '(100 200 300)))
  (play
   (* (sin-osc.ar (index.ar (local-buf-list vals) (mouse-x.kr 0
							      (- (length vals)
								 0.001))))
      0.2)))

(play
 (let* ((num-harm 11)
	(base-freq 66)
	(vals (mapcar (alexandria:curry #'* base-freq)
		      (alexandria:iota num-harm :start 1 :step 1))))
   (sin-osc.ar (index.ar (local-buf-list vals)
			 (mouse-x.kr 0 (- num-harm 0.001)))
	       0 0.1)))

;;;

(play
 (let* ((mx (mouse-x.kr 0.0 1.0))
	(trig (> mx 0.5)))
   (sin-osc.ar 440 0 (* 0.1 trig))))

(play
 (let* ((mx (mouse-x.kr 0.0 1.0))
	(my (mouse-y.kr 0.0 1.0))
	(trig (if (* (> mx 0.3) (< mx 0.5) (> my 0.3) (< my 0.7)) 1 0)))
   (sin-osc.ar 440 0 (* 0.1 trig))))

;;; Strummable guitar

(play
 (let* ((pitch '(52 57 62 67 71 76))
	(mouse-x (mouse-x.kr))
	(out (mix (loop :for p :in pitch
			:for i :from 0
			:for trigger := (abs (hpz-1.kr (> mouse-x
							  (+ 0.25 (* i 0.1)))))
			:for pluck := (pink-noise.ar (decay.kr trigger 0.05))
			:for period := (/ 1 (midicps p))
			:for string := (comb-l.ar pluck period period 4)
			:collect (pan2.ar string (- (* i 0.2) 0.5)))))
	(out (lpf.ar out 12000)))
   (leak-dc.ar out)))

;;;

(play
 (sin-osc.ar (mouse-button.kr 400 440) 0 0.1))

;;; Keyboard - doesn't work on my system...

(play
 (sin-osc.ar 800 0 (key-state.kr 0 0 0.1)))

