(defun ackerman (m n) 
  (cond ((= m 0) (+ n 1))
        ((and (> m 0) (= n 0)) (ackerman (- m 1) 1))
        ((and (> m 0) (> n 0)) (ackerman (- m 1) (ackerman m (- n 1))))
        (t "invalid input")))

(format t "~a~%" (ackerman -2 14))
(format t "~a" (ackerman 3 7))
