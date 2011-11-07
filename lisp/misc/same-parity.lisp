(defun same-parity (&rest lst)
  (if (zerop (mod (first lst) 2))
      (helper 0 '() lst)
      (helper 1 '() lst)))
(defun helper (remainder result lst)
  (if (null lst)
      result
      (if (= remainder (mod (first lst) 2))
          (helper remainder (append result (list (first lst))) (rest lst))
          (helper remainder result (rest lst)))))
