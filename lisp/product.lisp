(load "sum")
(defun product1 (n1 n2)
  (cond
    ((zerop n1) 0)
    ((eql n1 1) n2)
    ((+ n2 (product1 (1- n1) n2)))))
  

;(trace product1)
;(write (product1 3 4))

(defun product2 (n1 n2)
  (let ((n3 n2))
    (cond
      ((zerop n1) 0)
      ((eql n1 1) n3)
      ((product2 (1- n1) (+ n2 n3))))))


  (trace product2)
  (write (product2 3 4))
