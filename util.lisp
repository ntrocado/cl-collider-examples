(defmacro poll (ugen &optional (freq 10))
  `(poll.kr (impulse.kr ,freq) ,ugen))

(defun dupl (expr)
  (make-list 2 :initial-element expr))
