(defun keep-first-n (num lst)
  (if (= 0 num)
      nil
      (cons (first lst)
            (keep-first-n (1- num) (rest lst)))))
