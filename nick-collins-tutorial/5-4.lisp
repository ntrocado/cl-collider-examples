(defsynth short-sample (buffer (dur 0.05) (pos 0.0) (amp 0.5) (pan 0.0))
  (let ((source (play-buf.ar 1 buffer 1.0
			     :start-pos  (* pos (buf-frames.ir buffer))))
	(env (env-gen.ar (env '(0 1 1 0) `(0.01 ,dur 0.01)) :act :free)))
    (out.ar 0 (pan2.ar (* env source amp) pan))))

(defparameter *b*
  (buffer-read
   (merge-pathnames #p"sounds/a11wlk01.wav"
		    (make-pathname
		     :directory (pathname-directory *sc-synth-program*)))))

;;; test the defsynth
(synth 'short-sample
       :buffer *b*
       :pos 0.5
       :dur 0.05)

;;; schedule synths over time
(loop :for count :below 10
      :do (synth 'short-sample
		 :buffer *b*
		 :pos 0.5
		 :dur (* 0.1 count))
      :do (sleep 0.05))

;;; another example
(let ((grain-spacing 0.05)
      (env (env '(0 1 0.3 1.0) '(2.0 2.5 0.5))))
  (loop :for count :below 100
	:for time := (* count grain-spacing)
	:do (synth 'short-sample
		   :buffer *b*
		   :pos (env-at env time)
		   :dur (+ 0.01 (random 0.04)))
	:do (sleep grain-spacing)))
