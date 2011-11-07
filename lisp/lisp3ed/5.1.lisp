(defun skip-first-n (num lst)
  (if (= 0 num)
      lst
      (skip-first-n (1- num) (rest lst))))
