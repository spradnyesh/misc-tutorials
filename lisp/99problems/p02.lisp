(defun my-but-last (lst)
  (cond ((null lst) nil)
        ((null (rest lst)) lst)
        ((null (rest (rest lst))) lst)
        (t (my-but-last (rest lst)))))
