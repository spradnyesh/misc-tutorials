(defun quicksort (list)
  (let ((length (length list)))
    (if (< length 2) list
        (let ((split (nth (truncate (/ length 2)) list))
              less-list equal-list greater-list)
          (mapc
           #'(lambda (x)
               (cond
                 ((< x split)
                  (setf less-list (cons x less-list)))
                 ((= split x)
                  (setf equal-list (cons x equal-list)))
                 (t
                  (setf greater-list (cons x greater-list)))))
           list)
          (append (quicksort less-list)
                  equal-list
                  (quicksort greater-list))))))
