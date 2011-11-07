(defstruct (element (:conc-name el-))
  lst count)
(defun make-list-of-elements (lst)
  (when (not (null lst))
    (cons (make-element :lst (first lst)
                        :count 0)
          (make-list-of-elements (rest lst)))))
(defun update-count (lst)
  (dolist (l1 lst lst)
    (let ((len (length (el-lst l1))))
        (dolist (l2 lst)
          (when (= len (length (el-lst l2)))
            (incf (el-count l2)))))))
(defun sort-by-length (a b)
  (when (< (el-count a) (el-count b)) t))
(defun lfsort (lst)
  (map 'list
       #'el-lst
       (sort (update-count (make-list-of-elements lst))
             #'sort-by-length)))
