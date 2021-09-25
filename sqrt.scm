(define (average x y) (/ (+ x y) 2))

(define (improve guess target) (average guess (/ target guess)))

(define (abs x) (if (< x 0) (- x) x))

(define (good guess target) (< (abs (- (* guess guess) target)) 0.0001))

(define (sqrt_iter guess target) (if (good guess target) guess (sqrt_iter (improve guess target) target)))

(define (sqrt x) (sqrt_iter 1 x))

(sqrt 2)