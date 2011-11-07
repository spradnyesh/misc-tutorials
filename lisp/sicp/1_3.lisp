(define (square x) (* x x))
(define (f a b c)
  (define m1 (max a b))
  (define m2 (max b c))
  (+ (square m1) (square m2)))

(f 1 2 3)
(f 4 2 3)
