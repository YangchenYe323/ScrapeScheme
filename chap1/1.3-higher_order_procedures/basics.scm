; procedures as arguments
(define (sum term next a b)
  (if (> a b)
    0
	(+ (term a) (sum term next (next a) b))))

; sum in range [a, b]
(define (id a) a)
(define (inc a) (+ a 1))
(define (sum-range a b)
  (sum id inc a b))

(sum-range 10 20)

; cube sume of range [a, b]
(define (cube a) (* a a a))
(define (cube-sum-range a b)
  (sum cube inc a b))
(cube-sum-range 5 6)

; pi series
(define (pi-sum a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term pi-next a b))

(* 8 (pi-sum 1 1000))

; integral approximation
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f add-dx (+ a (/ dx 2.0)) b)
      dx))

(integral cube 0 1 0.01)

; exercise 1.29
(define (simpson-integral f a b n)
  (define (h a b n) (/ (- b a) n))
  (define (k cur) (/ (- cur a) (h a b n)))
  (define (coefficient k)
    (cond ((= k 0) 1) ; first coefficient
	      ((> (+ a (* (+ k 1) (h a b n))) b) 1) ; last coefficient
		  ((= (remainder k 2) 0) 4) ; even
		  (else 2) ; odd
  	)) 
  (define (term cur)
    (* (f cur) (coefficient (k cur))))
  (define (next cur) (+ cur (h a b n)))
  (* (/ (h a b n) 3.0) (sum term next a b)))

(simpson-integral cube 0 1 100)

; test
; precise answer is 0.25
(integral cube 0 1 0.01) ; 0.2499875
(simpson-integral cube 0 1 100) ; 0.246716
(simpson-integral cube 0 1 1000) ; 0.249887
(simpson-integral cube 0 1 10000) ; 0.24996667

; exercise 1.30
; iterative sum
(define (sum-iter term next a b)
  (define (iter a result)
    (if (> a b) 
	  result
	  (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (integral-iter f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum-iter f add-dx (+ a (/ dx 2.0)) b)
      dx))
	
(integral-iter cube 0 1 0.01)

; exercise 1.31
(define (product term next a b)
  (if (> a b)
    1
	(* (term a) (product term next (next a) b))))

(define (factorial n)
  (define (next a) (+ a 1))
  (define (term a) a)
  (product term next 1 n))

(factorial 4)

; pi = 4 * (2*4*4*6*6*8...)/(3*3*5*5*7*7...)
(define (approximate-pi n)
  (define (denominator i) 
    (if (= (remainder i 2) 0)
      (+ i 1)
	  (+ i 2)))
  (define (enumerator i)
    (cond ((= i 1) 2)
	      ((= (remainder i 2) 0) (+ 2 i))
		  (else (+ 1 i))))
  (define (term i) 
    (/ (enumerator i) (denominator i)))
  (define (next i) (+ i 1))
  (* 4 (product term next 1 n)))

(approximate-pi 1000) ; horrible fraction

; exercise 1.32
(define (accumulate combiner null-value term next a b)
  (if (> a b)
    null-value
	(combiner (term a) (accumulate combiner null-value term next (next a) b))))

(define (product-acc term next a b)
  (accumulate * 1 term next a b))

(define (sum-acc term next a b)
  (accumulate + 0 term next a b))

; exercise 1.33
(define (filter-accumulate combiner filter null-value term next a b)
  (cond ((> a b) 
          null-value)
        ((filter a) 
		  (combiner 
		    (term a) 
			(filter-accumulate combiner filter null-value term next (next a) b)))
		(else 
		  (filter-accumulate combiner filter null-value term next (next a) b))))

; the sum of squares of the prime numbers in the interval a to b
(define (divides? a b) (= (remainder a b) 0))
(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
		(else (find-divisor n (+ test-divisor 1)))))
(define (smallest-divisor n) (find-divisor n 2))
(define (prime? n) (= (smallest-divisor n) n))

(define (square-sum-prime a b)
  (define (term a) (* a a))
  (define (next a) (+ a 1))
  (filter-accumulate + prime? 0 term next a b))

(square-sum-prime 0 10)

; product of all positive integers less than n that are coprime with n
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))
(define (product-coprime n)
  (define (filter x) (= (gcd x n) 1))
  (define (term x) x)
  (define (next x) (+ 1 x))
  (filter-accumulate * filter 1 term next 1 n))

(product-coprime 6) ; 1 * 5 = 5
(product-coprime 7) ; 1 * 2 * 3 * 4 * 5 * 6 = 720