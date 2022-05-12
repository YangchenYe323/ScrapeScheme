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

; lambda
(define (pi-sum-lambda a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2)))) ;term
       (lambda (x) (+ x 4)) ;next
       a
       b))

(pi-sum 1 10)
(pi-sum-lambda 1 10)

; let binding
; f(x, y) = ra^2 + yb + ab
; a = xy + 1, b = y - 1
(define (f x y)
  (let ((a (+ 1 (* x y))) ;a = xy + 1
        (b (- 1 y)))     ;b = y - 1
  (+ (* x a a)
     (* y b)
     (* a b))))

(f 3 6)

; exercise 1.34
(define (f g) (g 2))
(f (lambda (x) (* x (+ x 1))))
; (f f) ; object 2 is not applicable:
; (f f)
; (f 2)
; (2 2)

(define (search f neg-point pos-point)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ((mid-point (/ (+ neg-point pos-point) 2.0)))
       (if (close-enough? neg-point pos-point)
         mid-point
         (let ((test-value (f mid-point)))
            (cond ((> test-value 0) (search f neg-point mid-point))
                  ((< test-value 0) (search f mid-point pos-point))
                  (else mid-point))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (< a-value 0) (> b-value 0)) (search f a b))
          ((and (< b-value 0) (> a-value 0)) (search f b a))
          (else (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                       1.0
                       2.0)

(define (fixed-point f first-guess)
  (define (close-enough? x y) (< (abs (- x y)) 0.00001))
  (define (try x)
    (let ((next (f x)))
      (if (close-enough? x next)
        next
        (try next))))
  (try first-guess))

(fixed-point cos 1.0)

; sqrt based on fix point
(define (sqrt x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2)) 1.0))

(sqrt 2)

; exercise 1.35
(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
golden-ratio

; exercise 1.36
(define (fixed-point-print f first-guess)
  (define (close-enough? x y) (< (abs (- x y)) 0.00001))
  (define (try x)
    (let ((next (f x)))
          (newline)
          (display next)
          (if (close-enough? x next)
            next
            (try next))))
  (try first-guess))

(fixed-point-print (lambda (x) (/ (log 1000) (log x))) 1.5)

; exercise 1.37
(define (cont-frac n d k)
  (define (iter n d i k)
    (let ((ni (n i))
          (di (d i)))
        ;  (newline)
        ;  (display ni)
        ;  (display "$")
        ;  (display di)
         (if (= i k)
           (/ ni di)
           (/ ni (+ di (iter n d (+ 1 i) k))))))
  (iter n d 1 k))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           1000) ;the golden ratio

; iterative cont-frac
(define (cont-frac-iter n d res k)
  (let ((nk (n k))
        (dk (d k)))
    (if (= k 0)
      res
      (cont-frac-iter n 
                      d 
                      (/ nk (+ dk res))
                      (- k 1)))))
(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                0
                1000)

; exercise 1.38
; Euler Approximation of E
(+ 2 (cont-frac 
        (lambda (x) 1.0)
        (lambda (x) (cond ((= x 1) 1.0)
                          ((= x 2) 2.0)
                          ((= (remainder (- x 2) 3) 0) 
                          (+ 2.0 (* 2 (/ (- x 2) 3))))
                          (else 1.0)))
        1000))

; exercise 1.39
(define (tan-cf x k)
  (define (d i) (- (* 2.0 i) 1))
  (define (n i) (if (= x 1) 1 (* x x (- 0 1))))
  (cont-frac n d k))

(tan-cf 1 100)

; procedures as return value
(define (average a b) (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

; newton's method
(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x) (* x x x))
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(sqrt 2)

; exercise 1.40
(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* x x a)
                 (* b x)
                 c)))

(newtons-method (cubic 2 3 1) 1)

; exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

((double square) 2)

; exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

; exercise 1.43
(define (repeated f n)
  (define (iter x i)
    (if (= i 0)
      x
      (iter (f x) (- i 1))))
  (lambda (x) (iter x n)))

((repeated square 2) 5)

; exercise 1.44
(define (smooth f)
  (define dx 0.0001)
  (lambda (x) (/ (+ (f x)
                    (f (+ x dx))
                    (f (- x dx)))
                 3.0)))

(define (n-fold-smooth f n) ((repeated smooth n) f))

; todo: exercise 1.45

; exercise 1.46
(define (iterative-improve good-enough? improve)
  (define (p x) (if (good-enough? x) x (p (improve x))))
  p)

(define (sqrt-iterative-improve x)
  (define (good-enough? guess)
    (< (abs (- x (* guess guess))) 0.0001))
  (define (improve guess)
    (/(+ guess (/ x guess)) 2))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt-iterative-improve 2)