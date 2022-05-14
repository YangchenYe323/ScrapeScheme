; linear recursion
(define (factorial x) (if (= x 1) 1
								  (* x 
								     (factorial (- x 1)))))

; linear iteration
(define (another-factorial x)
  (define (fact-iter product counter x) (if (= counter x) product
  														  (fact-iter (* product (+ counter 1)) 
															         (+ counter 1) 
																	 x)))
  (fact-iter 1 1 x))

; exercise 1.9
(define (dec a) (- a 1))
(define (inc a) (+ a 1))

(define (plus a b)
	(if (= a 0)
		b
		(inc (plus (dec a) b))))
; this is a recursive process
; (+ 3 4)
; (inc (+ 2 4))
; (inc (inc (+ 1 4)))
; (inc (inc (inc (+ 0 4))))
; (inc (inc (inc 4)))
; (inc (inc 5))
; (inc 6)
; 7


(define (another-plus a b)
	(if (= a 0)
		b
		(another-plus (dec a) (inc b))))
; this is an iterative process:
; (+ 3 4)
; (+ 2 5)
; (+ 1 6)
; (+ 0 7)
; 7

; exercise 1.10
(define (A x y)
	(cond ((= y 0) 0)
		  ((= x 0) (* 2 y))
		  ((= y 1) 2)
		  (else (A (- x 1)
		  		   (A x (- y 1))))))
					
(A 1 10)
; (A 1 10)
; (A 0 (A 1 9))
; (* 2 (A 1 9))
; (* 2 (A 0 (A 1 8)))
; (* 2 (* 2 (A 1 8)))
; (* 2 (* 2 (A 0 (A 1 7))))
; (* 2 (* 2 (* 2 (A 1 7))))
; ...
; (*2 ... (* 2 (A 1 1)))
; 2^10

(A 2 4)
; (A 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 (A 0 (A 2 1))))
; (A 1 (A 1 (A 0 2)))
; (A 1 (A 1 4))
; (A 1 (A 0 (A 1 3)))
; (A 1 (A 0 (A 0 (A 1 2))))
; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
; (A 1 (A 0 (A 0 (A 0 2))))
; (A 1 (A 0 (A 0 4)))
; (A 1 (A 0 8))
; (A 1 16)
; 2^16

(A 3 3)
; (A 3 3)
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; (A 2 (A 1 (A 2 1)))
; (A 2 (A 1 2))
; (A 2 (A 0 (A 1 1)))
; (A 2 (A 0 2))
; (A 2 4)
; 2^16

(define (f n) (A 0 n))
; f(x) = 2x

(define (g n) (A 1 n))
; f(x) = 2^x

(define (h n) (A 2 n))
; (A 2 n) = (A 1 (A 2 n-1)) = 2^(A 2 n-1)
; h(x) = 2^(h(x-1))

; tree recursion
(define (fib n)
  (cond ((= n 0) 0)
		((= n 1) 1)
		(else (+ (fib (- n 1))
				 (fib (- n 2))))))

; iterative fig
(define (fib-iter first second counter target)
  (cond ((= counter target) first)
        (else (fib-iter second (+ first second) (+ counter 1) target))))

(define (better-fib n) (fib-iter 0 1 0 n))
; (fib-iter 0 1 0 5)
; (fib-iter 1 1 1 5)
; (fib-iter 1 2 2 5)
; (fib-iter 2 3 3 5)
; (fib-iter 3 5 4 5)
; (fib-iter 5 8 5 5)
; 5