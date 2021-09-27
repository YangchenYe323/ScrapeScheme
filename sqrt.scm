(define (average x y) (/ (+ x y) 2))

(define (improve guess target) (average guess (/ target guess)))

(define (abs x) (if (< x 0) (- x) x))

(define (good guess target) (< (abs (- (* guess guess) target)) 0.0001))

(define (sqrt_iter guess target) (if (good guess target) guess (sqrt_iter (improve guess target) target)))

(define (sqrt x) (sqrt_iter 1 x))

(sqrt 2)

; why self-defined if statement won't work:
; if is special in that it evaluates the second argument
; only if the predicate is false, self-defined procedures will
; evaluate both arguments no matter what
(define (new-if predicate then-clause else-clause) (cond (predicate then-clause) (else else-clause)))

(define (new_sqrt_iter guess target) (new-if (good guess target) guess (sqrt_iter (improve guess target) target)))

(define (new_sqrt x) (new_sqrt_iter 1 x))

;CAUTION: the following code will run forever
; (new_sqrt_iter 2)

; good procedure is not adequete for finding sqrt of very small and very large numbers
(sqrt 0.0000000423)

(define (good_enough guess target) (< (abs (/ (- guess (improve guess target)) guess)) 0.1))
(define (next_sqrt_iter guess target) (if (good_enough guess target) guess (sqrt_iter (improve guess target) target)))
(define (next_sqrt x) (next_sqrt_iter 1 x))
(next_sqrt 2)

