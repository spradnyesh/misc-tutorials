(defun repli-char (k char)
  (if (<= k 0)
      nil
      (cons char (repli-char (decf k) char))))
(defun dupli (lst)
  (if (null lst)
      nil
      (append (repli-char 2 (first lst))
              (dupli (rest lst)))))
