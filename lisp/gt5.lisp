(defun gt5 (obj)
  (> (length obj) 5))

(write (gt5 "abcdef"))
(write (gt5 "abcd"))
(write (gt5 '(1 2 3)))
(write (gt5 '(1 2 3 4 5 6 7)))
