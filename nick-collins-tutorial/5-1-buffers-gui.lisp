(defpackage #:5-1-buffers-gui
  (:use #:cl+qt
	#:sc)
  (:export #:main))

(in-package :5-1-buffers-gui)
(named-readtables:in-readtable :qtools)

;;; Audio

(defparameter *b*
  (buffer-read
   (merge-pathnames #p"sounds/a11wlk01.wav"
		    (make-pathname
		     :directory (pathname-directory *sc-synth-program*)))))

(defsynth playbuf ((out 0) (bufnum 0) (rate 1) (trigger 1) (start-pos 0) (loop 1))
  (out.ar out (pan2.ar (play-buf.ar 1
				    bufnum
				    (* rate (buf-rate-scale.kr bufnum))
				    :trig trigger
				    :start-pos (* (buf-frames.ir bufnum)
						  start-pos)
				    :loop loop)
		       0.0)))

(defparameter *a* (synth 'playbuf :out 0 :bufnum (bufnum *b*)))

;;; GUI

(define-widget main-window (QWidget)
	       ())

(define-subwidget (main-window rate-slider)
    (make-instance 'qte:slider
		   :minimum 0.5
		   :maximum 10
		   :stepping 0.1
		   :default 1.0
		   :caption "Rate"
		   :curve :exp))

(define-slot (main-window rate-slider) ((value double))
  (declare (connected rate-slider (value-changed double)))
  (ctrl *a* :rate value))

(define-subwidget (main-window trig-slider)
    (make-instance 'qte:slider
		   :minimum 0
		   :maximum 1
		   :stepping 1
		   :default 1
		   :caption "Trigger"
		   :curve :lin))

(define-slot (main-window trig-slider) ((value double))
  (declare (connected trig-slider (value-changed double)))
  (ctrl *a* :trigger value))

(define-subwidget (main-window start-pos-slider)
    (make-instance 'qte:slider
		   :minimum 0.0
		   :maximum 1.0
		   :stepping 0.01
		   :default 0
		   :caption "StartPos"
		   :curve :lin))

(define-slot (main-window start-pos-slider) ((value double))
  (declare (connected start-pos-slider (value-changed double)))
  (ctrl *a* :start-pos value))

(define-subwidget (main-window loop-slider)
    (make-instance 'qte:slider
		   :minimum 0
		   :maximum 1
		   :stepping 1
		   :default 1
		   :caption "Loop"
		   :curve :lin))

(define-slot (main-window loop-slider) ((value double))
  (declare (connected loop-slider (value-changed double)))
  (ctrl *a* :loop value))

(define-subwidget (main-window layout) (q+:make-qvboxlayout main-window)
  (setf (q+:window-title main-window) "5.1: Buffers and Sound Files")
  (q+:add-widget layout rate-slider)
  (q+:add-widget layout trig-slider)
  (q+:add-widget layout start-pos-slider)
  (q+:add-widget layout loop-slider))

(define-override (main-window close-event) (event)
  (declare (ignore event))
  (free *a*))

(defun main ()
  (with-main-window (window 'main-window)))
