(defpackage #:5-2-granular
  (:use #:cl+qt
	#:sc)
  (:export #:main))

(in-package :5-2-granular)
(named-readtables:in-readtable :qtools)

(defun rrand (n1 n2)
  (if (= n1 n2)
      n1
      (let ((mi (min n1 n2))
	    (ma (max n1 n2)))
	(+ mi (random (- ma mi))))))

(defun exp-rand (mi ma)
  (lin-exp (random 1.0) 0 1.0 mi ma))

(defun rand2 (n)
  (if (zerop n)
      0
      (- (random (* 2 n)) n)))

;;; Audio

(defparameter *last-val* 0.0)
(defparameter *on* t)

(defparameter *b*
  (buffer-read
   (merge-pathnames #p"sounds/a11wlk01.wav"
		    (make-pathname
		     :directory (pathname-directory *sc-synth-program*)))))

(defsynth sf-grain ((bufnum 0) (pan 0.0) (start-pos 0.0) (amp 0.1) (dur 0.04))
  (let ((grain (- (* (play-buf.ar 1 bufnum (buf-rate-scale.kr bufnum)
				  :start-pos (* (buf-frames.ir bufnum)
						start-pos))
		     (env-gen.kr (perc 0.01 dur) :act :free))
		  0.001)))
    (out.ar 0 (pan2.ar (* grain amp) pan))))

;;; GUI

(define-widget main-window (QWidget)
	       ())

(define-subwidget (main-window slider)
    (make-instance 'qte:slider
		   :minimum 0.0
		   :maximum 1.0
		   :stepping 0.001
		   :default 0.0
		   :caption "Value"
		   :curve :lin))

(define-slot (main-window slider) ((value double))
  (declare (connected slider (value-changed double)))
  (setf *last-val* value))

(define-subwidget (main-window layout) (q+:make-qvboxlayout main-window)
  (setf (q+:window-title main-window) "5.2: Granular")
  (q+:add-widget layout slider))

(define-override (main-window close-event) (event)
  (declare (ignore event))
  (setf *on* nil))

(defun main ()
  (with-main-window (window 'main-window)
    (setf *on* t)
    (bt:make-thread
     (lambda ()
       (loop :for i :from 0
	     :for prop := (/ (mod i 300) 300)
	     :for time-start := (* prop 0.8)
	     :for time-end := (* prop (+ 0.8 (* 0.1 *last-val*)))
	     :while *on*
	     :do (synth 'sf-grain
			:bufnum (bufnum *b*)
			:start-pos (rrand time-start time-end)
			:amp (exp-rand 0.005 0.1)
			:pan (rand2 *last-val*)
			:dur (+ 0.1 (* *last-val* 0.5)))
	     :do (sleep (max (+ (* *last-val* 0.2) 0.01)
			     0.01)))))
    :name 'granular))
