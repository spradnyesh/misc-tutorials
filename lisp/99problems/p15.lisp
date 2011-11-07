(defun repli-char (k char)
  (if (<= k 0)
      nil
      (cons char (repli-char (decf k) char))))
(defun repli (lst count)
  (if (null lst)
      nil
      (append (repli-char count (first lst))
              (repli (rest lst) count))))
