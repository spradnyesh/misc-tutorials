(defun helper (n k s)
  (cond ((= k s) t)
        ((and (> k 1)
              (zerop (mod n k)))
         nil)
        (t (helper n (incf k) s))))
(defun is-prime (n)
  (helper n 2 (1+ (floor (sqrt n)))))
(defun next-prime (k)
  (if (is-prime (1+ k))
      (1+ k)
      (next-prime (1+ k))))
(defun p005 (n)
  (let ((product 1))
    ;; find product of all prime numbers < n as our starting point
    (do ((cur-prime 2 (next-prime cur-prime)))
        ((>= cur-prime n))
      (setf product (* product cur-prime)))
    ;; now b/n 1-n if (mod product i) != 0,
    ;; then continually multiply product by numbers in list of prime factors of i
    ;; until finally (mod product i) == 0
    ;; XXX: do _not_ set product *= (gcd product i),
    ;; because we might ending up increasing the product more than necessary
    (do ((i 1 (1+ i)))
        ((> i n) product)
      (unless (zerop (mod product i))
        (dolist (p (prime-factors i))
          )
        (format t "~A ~A ~A~%" product i (gcd product i))
        (setf product (* product (gcd product i)))))))
