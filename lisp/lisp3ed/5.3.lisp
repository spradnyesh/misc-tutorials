(defun keep-first-n-cleverly-aux (num lst rslt)
  (if (= 0 num)
      rslt
      (keep-first-n-cleverly-aux (1- num)
                                 (rest lst)
                                 (append rslt (list (first lst))))))
(defun keep-first-n-cleverly (num lst)
  (keep-first-n-cleverly-aux num lst nil))
