(defun sum (n1 n2)
  (assert
    (and (integerp n1) (>= n1 0))
    (n1)
    "N1 must be a non-negative integer, but it's ~S."
    n1)
  (assert
    (and (integerp n2) (>= n2 0))
    (n2)
    "n2 must be a non-negative integer, but it's ~S."
    n2)
  (if (zerop n1)
    n2
    (sum (1- n1) (1+ n2))))

(write (sum "3" "4"))
