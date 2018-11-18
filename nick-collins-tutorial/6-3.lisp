(server-query-all-nodes)

;;; demo
(defsynth reverb () (out.ar 0 (comb-n.ar (in.ar 0 2) 0.1 0.1 4)))
(defsynth impulses () (out.ar 0 (impulse.ar '(0.9 1.19) 0 0.3)))

(progn
  (synth 'impulses)
  (synth 'reverb))

;;; with the reverb?
(progn
  (synth 'reverb)
  (synth 'impulses))

;;; controlled execution
(progn
  (defparameter *g* (make-group))
  (synth 'reverb :to *g* :pos :tail)
  (synth 'impulses :to *g* :pos :head))

;;; controlled execution
(progn
  (defparameter *g* (make-group))
  (synth 'impulses :to *g* :pos :head)
  (synth 'reverb :to *g* :pos :tail))

;;; other ways we might do this
(progn
  (defparameter *a* (synth 'impulses))
  (synth 'reverb :pos :after :to *a*))

(progn
  (defparameter *a* (synth 'reverb))
  (synth 'impulses :pos :before :to *a*))
