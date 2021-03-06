(defun rem-dup (char lst)
  (cond ((null lst) nil)
        ((= (length lst) 1) lst)
        ((eql char (first lst))
         (rem-dup char (rest lst)))
        (t (append (list char)
                   (rem-dup (first lst) (rest lst))))))
(defun compress-1 (lst)
  (rem-dup (first lst) (rest lst)))


(defun compress-2 (lst)
  (cond ((null lst) nil)
        ((null (rest lst)) lst)
        ((eql (first lst) (first (rest lst)))
         (compress-2 (rest lst)))
        (t (cons (first lst) (compress-2 (rest lst))))))
