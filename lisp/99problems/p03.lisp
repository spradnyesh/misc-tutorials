(defun kth (lst k)
  (cond ((< k 1) nil)
        ((= k 1) (first lst))
        (t (kth (rest lst) (decf k)))))
