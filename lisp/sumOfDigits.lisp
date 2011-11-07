(defun quotient (dividend divisor)
  (multiple-value-bind (quotient remainder)
              (floor (/ dividend divisor))
            (declare (ignore remainder))
            quotient))
(defun sum-of-digits (num)
  (if (< num 10)
      num
      (sum-of-digits
       (+ (mod num 10)
          (sum-of-digits
           (quotient num 10))))))
(defun sum-of-digits-of-sums (num)
  (let* ((a (random num))
        (b (- num a)))
    (format t "~A ~A" a b)
    (if (= (sum-of-digits num)
           (sum-of-digits
            (+ (sum-of-digits a)
               (sum-of-digits b))))
        (print "true")
        (print "false"))))
