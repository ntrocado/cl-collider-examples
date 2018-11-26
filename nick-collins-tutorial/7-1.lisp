;;; MIDI
;;; I couldn't find a library that offers MIDI input and output in Win/Mac/Linux.
;;; A way around this is to use a tool to convert incoming and outgoing MIDI messages to OSC: https://www.cockos.com/oscii-bot/
;;; And then implement an OSC responder in cl-collider.
;;; Anyway, I didn't test it yet...


;;; SoundIn

(play (sound-in.ar '(0 1) 0.1))
(play (sound-in.ar 1 0.1))

(play
 (* (sin-osc.ar (mouse-x.kr 0.001 110 :exp))
    (sound-in.ar 0 0.5)))

(play
 (pulse.ar 90 0.3 (amplitude.kr (sound-in.ar 0))))

(play
 (let* ((input (sound-in.ar 0 0.1))
	(input-amp (amplitude.kr input))
	(threshold 0.02)
	(gate (lag.kr (> input-amp threshold) 0.01)))
   (* input gate)))
