(defun annuity (&key amount
                (num-years 0)
                (num-installments (* num-years 12))
                roi-pa)
  (let ((roi-pm (/ roi-pa 12 100)))
    (/
     (* amount
        (+ roi-pm 1)
        (- (expt (+ roi-pm 1) num-installments) 1))
     roi-pm)))
(dotimes (i 10)
  (format t "~F~%" i (annuity :amount 10000 :num-years i :roi-pa 15)))


(defun EMI (&key principal num-years rate-of-interest)
  (* principal rate-of-interest
     (/ (expt (1+ rate-of-interest) num-years)
        (- (expt (1+ rate-of-interest) num-years) 1))))
