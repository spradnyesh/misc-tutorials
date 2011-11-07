(load "discrim.lisp")

(defun quad-roots (a b c)
  (let (r1 r2)
    (setq r1 (/ (+ (- b) (discrim a b c)) (* 2 a)))
    (setq r2 (/ (- (- b) (discrim a b c)) (* 2 a)))
    (list r1 r2)))

(write (quad-roots 1 5 4))

;ferdous: 9435118700
