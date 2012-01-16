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
(defun p003 (n)
  (do ((largest 0)
       (current-prime 2))
      ((= n 1) largest)
    (if (zerop (mod n current-prime))
        (setf n (/ n current-prime))
        (setf current-prime (next-prime current-prime)))
    (when (> current-prime largest)
      (setf largest current-prime))))