;;; sum1:-
;;;     decrement n1, and _simultaneously_ increment n2
;;; sum2:-
;;;     decrement n1, and accumulate 1's in the end
;;;
;;; sum2 is the normally done recursion; however sum1 is a _tail recursion_
;;; and hence is optimized by lisp compilers. so try and use fn's like sum1
;;; as much as possible :)

(defun sum1 (n1 n2)
  (if (zerop n1)
    n2
    (sum1 (1- n1) (1+ n2))))

;;(trace sum1)
;;(write (sum1 3 4))

(defun sum2 (n1 n2)
  (if (zerop n1)
    n2
    (1+ (sum2 (1- n1) n2))))

;;(trace sum2)
;;(write (sum2 3 4))
