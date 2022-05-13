; comcrete side: implement abstract data
(define (make-rat n d) (cons n d))

; reduce to lowest terms
(define (gcd a b)
  (if (= b 0) a (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d))) 
       (cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x)) 
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; abstract side: use abstract data
; n1/d1 + n2/d2 = (n1d2 + n2d1)/d1d2
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

; n1/d1 - n2/d2 = (n1d2 - n2d1)(d1d2)
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

; n1/d1 * n2/d2 = (n1n2)/(d1d2)
(define (mult-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

; (n1/d1)/(n2/d2) = (n1d2)/(d1n2)
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third)) ;1/2 + 1/3 = 5/6

(print-rat (sub-rat one-half one-third)) ;1/2 - 1/3 = 1/6

(print-rat (mult-rat one-half one-third)) ;1/2 * 1/3 = 1/6

(print-rat (add-rat one-third one-third)) ;1/3 + 1/3 = 2/3

; exercise 2.1
(define (make-rat-normalize x y)
  (cond ((= y 0) error "Denominator can't be zero")
        ((< (* x y) 0)
		  (make-rat (- (abs x)) (abs y)))
		(else
		  (make-rat (abs x) (abs y)))))
	
(print-rat (make-rat-normalize -2 -3)) ;2/3
(print-rat (make-rat-normalize 2 -3)) ;-2/3
(print-rat (make-rat-normalize 30 0)) ; error