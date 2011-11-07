(defun primep (num)
  (when (> num 0)
    (if (< num 3) t
      (progn (dotimes (i (isqrt num) t)
           (when (zerop (mod num (+ i 2)))
             (return nil)))))))
(defun next-prime (num)
  (do ((i (+ num 1) (+ i 1)))
      ((eq (primep i) t) i)))


;;;for cycling through primes b/n 0 and 20
;;how the macro call should look like
;(do-primes (p 0 20)
;  (format t "~d" p))

;;how the expansion of macro should look like
;(do ((p (next-prime 0) (next-prime (+ p 1))))
;    ((> p 20))
;  (format t "~d" p))

(defmacro do-primes (var-and-range &rest body)
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))
