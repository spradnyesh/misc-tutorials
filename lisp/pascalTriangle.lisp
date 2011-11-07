(defun pascal-triangle (n)
  "1
1 1
1 2 1
1 3 3 1
1 4 6 4 1
1 5 10 10 5 1
1 6 15 20 15 6 1"
  (check-type n integer)
  (cond ((< n 1) '())
        ((= n 1) '(1))
        (t (progn
             ;(format t "~a~%" '(1))
             (pascal-triangle-helper n 2 '(1 1))))))
(defun sum-next (lst)
  (check-type lst list)
  (if (< (length lst) 2) nil
      (cons (+ (first lst) (second lst))
            (sum-next (rest lst)))))
(defun pascal-triangle-helper (n i lst)
;i is actually (length lst) and is not really needed,
;since it can be computed at every call.
;but this way it is (only marginally) more performant ;-)
  ;(format t "~a~%" lst)
  (if (= i n) lst
      (pascal-triangle-helper n
                              (1+ i)
                              (append '(1)
                                      (sum-next lst)
                                      '(1)))))
