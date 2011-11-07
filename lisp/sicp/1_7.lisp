(define (sqrt-iter guess x)
  (define new-guess (improve guess x))
  (if (good-enough? guess new-guess)
    guess
    (sqrt-iter new-guess x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? x y)
  (< (abs (- (square guess) x)) 0.001))
(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 3)
