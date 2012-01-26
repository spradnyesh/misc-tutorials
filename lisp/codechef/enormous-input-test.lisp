(let ((n (read))
      (k (read)))
  (declare (optimize (speed 3) (safety 0)))
  (loop for i upto (1- n)
     summing (if (zerop (mod (read) k))
                 1
                 0)))
