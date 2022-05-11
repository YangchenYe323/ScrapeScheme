(define (count-change amount)
  (define (denomination kind)
    (cond ((= kind 5) 50)
		  ((= kind 4) 25)
		  ((= kind 3) 10)
		  ((= kind 2) 5)
		  ((= kind 1) 1)
		  (else 0)))

  (define (change-with-kinds amount kinds)
    (cond ((= amount 0) 1)
	      ((< amount 0) 0)
		  ((= kinds 0) 0)
		  ; change without using the first kind
		  (else (+ (change-with-kinds amount 
		  							  (- kinds 1))
				; change using at least one first kind
		           (change-with-kinds (- amount (denomination kinds)) 
				   					  kinds)))))
  (change-with-kinds amount 5)
)

; exercise 1.11
; recursive process
(define (f n)
  (if (< n 3) n
		(+ (f (- n 1))
		   (f (- n 2))
		   (f (- n 3)))))
		
; iterative process
(define (better-f n)
  (define (f-iter first second third count)
    (if (= count 0) first
	                (f-iter second third (+ first second third) (- count 1))))
  (f-iter 0 1 2 n))

; exercise 1.12
(define (pascal_at row column)
  (if (or (= column 0) (= column row)) 1
  									   (+ (pascal_at (- row 1) (- column 1))
										  (pascal_at (- row 1) column))))

; todo: exercise 1.13

; exercise 1.13
; (count-change 11)
; (change-with-kinds 11 5)
; (+ (change-with-kinds 11 4) (change-with-kinds -39 5))
; (+ (+ (change-with-kinds 11 3) (change-with-kinds -14 4)) 0)
; (+ (+ (+ (change-with-kinds 11 2) (change-with-kinds 1 3)) 0) 0)
; (+ (+ (+ (+ (change-with-kinds 11 1) (change-with-kinds 1 2)) (+ (change-with-kinds 1 2) (change-with-kinds -9 3))) 0) 0)
; ...

; exercise 1.14
(define (cube x) (* x x x))
; approximate sin(x) = 3*sin(x/3) - 4*sin(x/3)^2
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sin angle)
  (if (not (> (abs angle) 0.1)) 
  		angle
		(p (sin (/ angle 3.0)))))

; a:
; (sine 12.15)
; (p (sin 4.05))
; (p (p (sin 1.35)))
; (p (p (p (sin 0.45))))
; (p (p (p (p (sin 0.15)))))
; (p (p (p (p (p (sin 0.05))))))
; p is evaluated 5 times

; b:
; Theta(a) -> Theta(a) = Theta(a / 3) + p

; exponantiation linear
(define (expt b n)
  (if (= n 0)
    1
	(* b (expt b (- n 1)))))

; lograithmic exponentiation
; b^n = if n is even then (b^{n/2})^2 else b^{n-1}*b
(define (fast-expt b n)
  (define (even? n) (= (remainder n 2) 0))
  (define (square n) (* n n))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))))

; exercise 1.16
(define (fast-expt-iter b a n)
  (define (even? n) (= (remainder n 2) 0))
  (define (square n) (* n n))
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) a (/ n 2)))
		(else (fast-expt-iter b (* a b) (- n 1)))))

; exercise 1.17
; logarithmic multiplication
(define (mult a b)
  (define (double a) (+ a a))
  (define (even? n) (= (remainder n 2) 0))
  (define (half b) (/ b 2))
  (cond ((= b 0) 0)
  	    ((even? b) (double (mult a (half b))))
		(else (+ a (mult a (- b 1))))))

; exercise 1.18
(define (mult-iter a r b)
  (define (double a) (+ a a))
  (define (even? n) (= (remainder n 2) 0))
  (define (half b) (/ b 2))
  (cond ((= b 0) r)
        ((even? b) (mult-iter (double a) r (half b)))
		(else (mult-iter a (+ r a) (- b 1)))))

; exercise 1.19
; p' = q^2 + p^2, q' = q^2 + 2pq
(define (even? n) (= (remainder n 2) 0))
(define (square n) (* n n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
		  (fib-iter a
		  			b
					(+ (square p) (square q)) ; p'
					(+ (square q) (* 2 p q))
					(/ count 2)))
		(else (fib-iter (+ (* b q) (* a q) (* a p))
						(+ (* b p) (* a q))
						p
						q
						(- count 1)))))

(define (fib n)
  (fib-iter 1 0 0 1 n))


