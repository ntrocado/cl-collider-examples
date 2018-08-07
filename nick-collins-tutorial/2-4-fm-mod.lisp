(defpackage #:2-4-fm-mod
  (:use #:cl+qt
	#:sc)
  (:export #:main))

(in-package #:2-4-fm-mod)
(in-readtable :qtools)

;;; Define GUI

;;; TODO: exponential slider

(define-widget main-window (QWidget)
	       ())

(defmacro with-slider ((val layout-name text min max default) &body body)
  (alexandria:with-gensyms (label slider spinbox)
    `(progn
       (define-subwidget (main-window ,label) (q+ make-qlabel ,text))
       (define-subwidget (main-window ,slider) (q+ make-qslider 1)
	 (setf (q+ minimum ,slider) ,min)
	 (setf (q+ maximum ,slider) ,max)
	 (setf (q+ value ,slider) ,default)
	 (setf (q+ minimum-width ,slider) 300))
       (define-subwidget (main-window ,spinbox) (q+ make-qspinbox)
	 (setf (q+ minimum ,spinbox) ,min)
	 (setf (q+ maximum ,spinbox) ,max)
	 (setf (q+ value ,spinbox) ,default))
       (define-subwidget (main-window ,layout-name) (q+ make-qhboxlayout)
	 (q+ add-widget ,layout-name ,label)
	 (q+ add-widget ,layout-name ,slider)
	 (q+ add-widget ,layout-name ,spinbox))
       (define-slot (main-window ,slider) ((,val int))
	 (declare (connected ,slider (value-changed int)))
	 (setf (q+ value ,spinbox) ,val)
	 ,@body)
       (define-slot (main-window ,spinbox) ((,val int))
	 (declare (connected ,spinbox (value-changed int)))
	 (setf (q+ value ,slider) ,val)
	 ,@body))))

(with-slider (val slider1 "Carrier Frequency" 20 5000 440)
  (ctrl :synth :carr-freq val))

(with-slider (val slider2 "Mod Frequency" 1 5000 1)
  (ctrl :synth :mod-freq val))

(with-slider (val slider3 "Mod Depth" 0 5000 0)
  (ctrl :synth :mod-depth val))

(define-subwidget (main-window layout) (q+:make-qvboxlayout main-window)
  (setf (q+:window-title main-window) "2.4: FM")
  (q+:add-layout layout slider1)
  (q+:add-layout layout slider2)
  (q+:add-layout layout slider3))

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
