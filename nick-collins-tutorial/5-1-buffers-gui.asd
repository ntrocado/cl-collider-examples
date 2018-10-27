(asdf:defsystem "5-1-buffers-gui"
  :description "Example code from Nick Collins' SuperCollider Tutorial section 5.1, GUI controlling a PlayBuf."
  :depends-on ("qtools"
	       "qtcore"
	       "qtgui"
	       "qtools-elements"
	       "cl-collider")
  :components ((:file "5-1-buffers-gui")))
