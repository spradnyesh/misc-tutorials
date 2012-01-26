(defun largest-power-of-5 (k)
    (let ((pow 0))
      (loop
         do (incf pow)
         until (> (expt 5 pow) k))
      (1- pow)))
(defun zero-factorial (n)
  ;; 0 zero is added every multiple of coz
  ;; 10 is also a multiple of 5 and the below takes care of it
  ;; 1 zero is added every multiple of 5 (due to <x>2 * <x>5)
  ;; 2 zeros are added every multiple of 25 (due to <x>4 * <x>25)
  ;; 3 zeros are added every multiple of 125 (due to <x>8 * <x>125)
  ;; ... add k zeros for every multiple of 5^k
  (loop for i to (largest-power-of-5 n)
     summing (floor n (expt 5 (1+ i)))))
(let ((n (read)))
  (loop for i upto (1- n)
     do (format t "~A~%" (zero-factorial (read)))))
(defun perf-test (&optional (n 100000))
    (dotimes (i n)
      (format t "~A~%" (zero-factorial (random 1000000000)))))
