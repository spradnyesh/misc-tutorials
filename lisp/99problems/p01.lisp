(defun my-last (lst)
  (cond ((null lst) nil)
        ((null (rest lst)) lst)
        (t (my-last (rest lst)))))
