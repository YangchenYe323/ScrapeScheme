1.1 - 1.5:

see [basics.scm](https://github.com/YangchenYe323/ScrapeScheme/chap1/basics.scm)

1.1:

```Scheme
10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6
(define a 3) ; a = 3
(define b (+ a 1)) ; b = 4
(= a b) ; #f
(if (and (> b a) (< b (* a b)))
	b
	a
) ; 4
(cond ((= a 4) 6)
	  ((= b 4) (+ 6 7 a))
	  (else 25)) ; 16
(+ 2 (if (> b a) b a)) ; 6
(* (cond ((> a b) a)
		 ((< a b) b)
		 (else -1))
   (+ a 1)) ; 16
```

1.2:

$\frac{5 + \frac{1}{2} + (2 - (3 - (6 + \frac{1}{5})))}{3(6 - 2)(2 - 7)}$

```scheme
(/ (+ 5
      (/ 1 2)
      (- 2
         (- 3
            (+ 6 (/ 1 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))
```

1.3:

```scheme
(define (square x) (* x x))
(define (square_sum x y) (+ 
                          (square x) 
                          (square y)))

(define (square_sum_larger x y z) (cond 
                                   ((and (< x y) (< x z)) (square_sum y z))
                                   ((and (< y x) (< y z)) (square_sum x z))
                                   (else (square_sum x y))))
```

1.4

```scheme
(define (a-plus-abs-b a b)
  ((if (b > 0) + -) a b))
```

`(if (b > 0) + -)` evaluates to `+` when `b > 0` and `-` otherwise, and then the evaluated operator is applied to `a b`. Hence when `b > 0`, the expression evaluates to `(+ a b)` and when `b <= 0`, it evaluates to `(- a b)`

1.5

```scheme
(define (p) (p))
(define (test x y)
  (if (= x 0) 
      0
      y))
(test 0 (p))
```

Applicative order: infinite recursion

Normal order: print 0



1.6 - 1.7

See [sqrt.scm](https://github.com/YangchenYe323/ScrapeScheme/chap1/sqrt.scm)



1.8

See [cuberoot.scm](https://github.com/YangchenYe323/ScrapeScheme/chap1/cuberoot.scm)

