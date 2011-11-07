(defun repli-char (k char)
  (if (<= k 0)
      nil
      (cons char (repli-char (decf k) char))))
(defun decode (lst)
  (cond ((null lst) nil)
        ((typep (first lst) 'list)
         (append (repli-char
                  (first (first lst))
                  (second (first lst)))
                 (decode (rest lst))))
        (t (cons (first lst) (decode (rest lst))))))
