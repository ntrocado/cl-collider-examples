(defpackage #:2-4-fm-mod
  (:use #:cl+qt
	#:sc)
  (:export #:main))

(in-package #:2-4-fm-mod)
(in-readtable :qtools)


;;; Define GUI

(define-widget main-window (QWidget)
  ())

(define-subwidget (main-window carrier-freq)
    (make-instance 'qte:slider
		   :minimum 20
		   :maximum 5000
		   :stepping 0.1
		   :default 440
		   :caption "Carrier Frequency"
		   :curve :exp))

(define-slot (main-window carrier-freq) ((value double))
  (declare (connected carrier-freq (value-changed double)))
  (ctrl :synth :carr-freq value))

(define-subwidget (main-window mod-freq)
    (make-instance 'qte:slider
		   :minimum 1
		   :maximum 5000
		   :stepping 0.1
		   :default 1
		   :caption "Mod Frequency"
		   :curve :exp))

(define-slot (main-window mod-freq) ((value double))
  (declare (connected mod-freq (value-changed double)))
  (ctrl :synth :mod-freq value))

(define-subwidget (main-window mod-depth)
    (make-instance 'qte:slider
		   :minimum 0.01
		   :maximum 5000
		   :stepping 0.01
		   :default 0.01
		   :caption "Mod Depth"
		   :curve :exp))

(define-slot (main-window mod-depth) ((value double))
  (declare (connected mod-depth (value-changed double)))
  (ctrl :synth :mod-depth value))

(define-subwidget (main-window layout) (q+:make-qvboxlayout main-window)
  (setf (q+:window-title main-window) "2.4: FM")
  (q+:add-widget layout carrier-freq)
  (q+:add-widget layout mod-freq)
  (q+:add-widget layout mod-depth))

(define-override (main-window close-event) (event)
  (declare (ignore event))
  (group-free-all))

;;; Audio

(defun server-start ()
  (setf *s* (make-external-server "localhost" :port 4444))
  (server-boot *s*))

(defun main ()
  (unless (and *s* (sc::boot-p *s*)) (server-start))
  (proxy :synth
	 (with-controls ((carr-freq 440)
			 (mod-freq 1)
			 (mod-depth 0.01))
	   (sin-osc.ar (+ carr-freq
			  (* mod-depth
			     (sin-osc.ar mod-freq)))
		       0 0.25)))
  (with-main-window (window 'main-window)))
