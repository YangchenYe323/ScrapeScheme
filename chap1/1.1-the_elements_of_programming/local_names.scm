(define (sqrt x)
  ; the evaluation of sqrt should not be changed
  ; if its enclosing environment has different bindings
  ; for 'square', 'good-enough?' or 'improve'
  ; a good practice is to define these names inside
  ; sqrt to make them bounded
  (define (square x) (* x x))
  (define (average x y) 
    (/ (+ x y) 2))
  (define (good-enough? guess x) 
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x) 
	  guess
	  (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(sqrt 5)