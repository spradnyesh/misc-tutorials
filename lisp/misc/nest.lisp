(defun filter (predicate sequence)
  (cond ((null sequence) nil)
        ((funcall predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (t (filter predicate (cdr sequence)))))
(defun accumulate (op initial sequence)
  (if (null sequence)
      initial
      (funcall op (car sequence) (accumulate op initial (cdr sequence)))))
(defun enumerate-interval (low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (1+ low) high))))
