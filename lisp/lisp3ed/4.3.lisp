(defun within-2-percent (long short)
  (when (< (- long short)
         (/ (* 2 long) 100.0))
      t))
(defun square (num)
  (* num num))
(defun rightp (a b c)
  (cond
    ;; when a is longest
    ((and (> a b) (> a c))
     (within-2-percent (square a) (+ (square b) (square c))))
    ;; when b is longest
    ((and (> b a) (> b c))
     (within-2-percent (square b) (+ (square a) (square c))))
    ;; when c is longest
    ((and (> c a) (> c b))
     (within-2-percent (square c) (+ (square a) (square b))))
    ;; default => (= a b c) => nil
    ))
