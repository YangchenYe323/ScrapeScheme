; exercise 2.2

(define (make-point x y)
  (cons x y))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (midpoint-segment seg)
  (let ((start (start-segment seg))
        (end (end-segment seg))) 
	   (make-point (/ (+ (x-point start) (x-point end)) 2)
                   (/ (+ (y-point start) (y-point end)) 2))))

(define s (make-point 0 0))
(print-point s); (0,0)

(define e (make-point 2 3))
(print-point e); (2,3)

(print-point (midpoint-segment (make-segment s e))); (1, 1.5)

; exercise 2.3
; represent a rectangle by its left bottom and right upper vertex
(define (make-rectangle left-bot right-up)
  (cons left-bot right-up))

(define (length rect)
  (- (x-point (cdr rect)) (x-point (car rect))))

(define (height rect)
  (- (y-point (cdr rect)) (y-point (car rect))))

(define (area rect)
  (* (length rect) (height rect)))

(define (perimeter rect)
  (* (+ (length rect) (height rect)) 
     2))

; test
(define r (make-rectangle (make-point 0 0)
						  (make-point 2 3)))

(length r) ;2
(height r) ;3
(area r) ;6
(perimeter r) ;10

; implement pair using procedures
(define (my-cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (my-car z) (z 0))

(define (my-cdr z) (z 1))

(define my-pair (my-cons 1 2))
(my-car my-pair); 1
(my-cdr my-pair); 2

; exercise 2.4
(define (another-cons x y)
  (lambda (m) (m x y)))

(define (another-car p)
  (p (lambda (a b ) a)))

(define (another-cdr p)
  (p (lambda (a b) b)))

(another-car (another-cons 1 2)); 1
(another-cdr (another-cons 3 1)); 1

; exercise 2.5
(define (fast-expt b n)
  (define (even? n) (= (remainder n 2) 0))
  (define (square n) (* n n))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
		(else (* b (fast-expt b (- n 1))))))

(define (arithmetic-cons a b)
  (* (fast-expt 2 a) (fast-expt 3 b)))

(define (arithmetic-car p)
  (define (log2 n)
    (if (= n 1)
	  0
	  (+ 1 (log2 (/ n 2)))))
  (define (log3 n)
    (if (= (remainder n 3) 0)
	  (log3 (/ n 3))
	  n))
  (log2 (log3 p)))

(define (arithmetic-cdr p)
  (define (log3 n)
    (if (= n 1)
	  0
	  (+ 1 (log3 (/ n 3)))))
  (define (log2 n)
    (if (= (remainder n 2) 0)
	  (log2 (/ n 2))
	  n))
  (log3 (log2 p)))

(arithmetic-car (arithmetic-cons 5 9)); 5

(arithmetic-cdr (arithmetic-cons 2 4)); 4

; exercise 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; (add-1 zero)
; (lambda (f) (lambda (x) (f ((zero f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
; (lambda (f) (lambda (x) (f x)))

; (add-1 one)
; (add-1 (lambda (g) (lambda (x) (g x))))
; (lambda (f) (lambda (x) (f (((lambda (g) (lambda (x) (g x))) f) x))))
; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; (lambda (f) (lambda (x) (f (f x))))

; addition: function composition

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
  				 (+ (upper-bound x) (upper-bound y))))

(define (mult-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
		(p3 (* (upper-bound x) (lower-bound y)))
		(p4 (* (upper-bound x) (upper-bound y))))
	    (make-interval (min p1 p2 p3 p4)
					   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mult-interval x
  				 (make-interval (/ 1.0 (upper-bound y))
				   				(/ 1.0 (lower-bound y)))))

; exercise 2.7
(define (make-interval a b) (cons a b))

(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

; exercise 2.8
(define (sub-interval x y)
  (add-interval x
  				(make-interval (- 0.0 (upper-bound y))
				               (- 0.0 (lower-bound y)))))

; exercise 2.9
; width(i1 + i2) = width((l1, u1) + (l2, u2)) = width((l1 + l2), (u1 + u2)) = (u1 + u2 - l1 - l2)/2
; = (u1 - l1)/2 + (u2 - l2)/2 = width(i1) + width(i2)

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; two intervals with the same width can have different width when multiplied or divided
(define i1 (make-interval 1 5)) ; width 2
(define i2 (make-interval 1 3)) ; width 1
(width (mult-interval i1 i2)) ; width 7
(width (div-interval i1 i2)) ; width 2.33


(define i3 (make-interval 1 5)) ; width 2
(define i4 (make-interval 5 7)); width 1
(width (mult-interval i3 i4)) ; width 15
(width (div-interval i3 i4)) ; width 0.42

; exercise 2.10
(define (div-interval-check i1 i2)
  (let ((l1 (lower-bound i1))
        (u1 (upper-bound i1))
		(l2 (lower-bound i2))
		(u2 (upper-bound i2)))
    (cond ((and (or (> 0 i1) (= 0 i1)) (or (< 0 u1) (= 0 u1))) error "i1 spans zero")
          ((and (or (> 0 i2) (= 0 i2)) (or (< 0 u2) (= 0 u2))) error "i2 spans zero")
		  (else (div-interval i1 i2)))))

; todo: exercise 2.11 - 2.16