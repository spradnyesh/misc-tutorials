#|(defun single-pass (lst)
  (when (> (length lst) 1)
      (cons (second lst) (single-pass (rest (rest lst))))))|#
#|((defun single-pass (lst)
   (let ((rslt nil))
     (dotimes (i (/ (length lst) 2) rslt)
       (pop lst)
       (setf rslt (append rslt (list (pop lst)))))))
 (defun trim (lst)
   (loop do (setf lst (single-pass lst))
      while (> (length lst) 1))
   lst))|#
#|((defun make-num-array (n)
   (let ((arr (make-array n)))
     (dotimes (i n)
       (setf (aref arr i) (1+ i)))
     arr))
 (defun single-pass (arr)
   (let ((temp-arr (make-array (floor (length arr) 2))))
     (dotimes (i (floor (length arr) 2))
       (setf (aref temp-arr i) (aref arr (1+ (* i 2)))))
     temp-arr))
 (defun trim (n)
   (let ((arr (make-num-array n)))
     (loop while (> (length arr) 1)
        do (setf arr (single-pass arr)))
     arr)))|#
(defun trim (n)
  (let ((arr (make-array n)))
    (dotimes (i n)
      (setf (aref arr i) (1+ i)))
    (loop while (> (length arr) 1)
       do (let* ((l-by-2 (floor (length arr) 2))
                            (temp-arr (make-array l-by-2)))
                       (dotimes (i l-by-2)
                         (setf (aref temp-arr i) (aref arr (1+ (* i 2)))))
                       (setf arr temp-arr)))
    (aref arr 0)))
(let ((n (read)))
  (dotimes (i n)
    (format t "~A~%" (trim (read)))))
