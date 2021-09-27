10
(+ 3 5 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b))) b a)
(cond ((= a 4) 6) ((= b 4) (+ 6 7 a)) (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a) ((< a b) b) (else -1)) (+ a 1))

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

(define (sum_of_square_larger x y z) (cond ((and (< x y) (< x z)) (+ (* y y) (* z z))) ((and (< y x) (< y z)) (+ (* x x) (* z z))) (else (+ (* x x) (* y y)))))
(sum_of_square_larger 1 2 3)
(sum_of_square_larger 2 2 2)
(sum_of_square_larger 4 2 9)

; a + |b|
(define (a-plus-abs-b a b) ((if (> b 0) + -) a b))
(a-plus-abs-b 9 -9)

; infinite recursion
(define (p) (p))
(define (test x y) (if (= x 0) 0 y))
; application order: infinite recursion. normal order: 0
; (test 0 (p))
