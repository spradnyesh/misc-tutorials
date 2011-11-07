(defun led (num &optional (divisor 1))
  "largest even divisor"
  (when (> num 0)
    (if (zerop (mod num 2))
        (led (/ num 2) (* 2 divisor))
        divisor)))
(defun rald (x y)
  "raise and last digit"
  (mod (expt x y) 10))
(defun last-digit (x y)
  (if (zerop (mod y 2))
      (mod (rald x (led y)) 10)
      (mod (* x (rald x (led (1- y)))) 10)))
