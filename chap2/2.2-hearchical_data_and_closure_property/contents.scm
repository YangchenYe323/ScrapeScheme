; a sequence
(define nil ())
(cons 1
	  (cons 2 
	  	 	(cons 3
			      (cons 4 nil))))

(list 1 2 3 4) ;= (cons 1 (cons 2 (cons 3 (cons 4 nil))))

(car (list 1 2 3 4)) ; 1
(cdr (list 1 2 3 4)) ; (2 3 4)

(define (list-ref l n)
  (if (= n 0) 
        (car l)
		(list-ref (cdr l) (- n 1))))

(define squares (list 1 4 9 16 25))
(car squares)
(cdr squares)
(list-ref squares 3); 16

; null? primitive tests whether list is empty
(define (length l)
  (if (null? l) 0 (+ 1 (length (cdr l)))))

(length squares) ; 5

(define (length-iter l)
  (define (iter cur l)
    (if (null? l) cur (iter (+ 1 cur) (cdr l))))
  (iter 0 l))
(length-iter squares) ; 5

; append l2 at the end of l1
(define (append l1 l2)
  (if (null? l1)
    l2
	(cons (car l1) (append (cdr l1) l2))))

(append (list 1 4 9 16 25) (list 1 2 3 4)); 1 4 9 16 25 1 2 3 4

; exercise 2.17
(define (last-pair l)
  (if (null? (cdr l))
    l
	(last-pair (cdr l))))
(last-pair (list 23 72 149 34)); (34)

; exercsie 2.18
(define (reverse l)
  (define (reverse-iter newl l)
    (if (null? l) newl
				  (reverse-iter (cons (car l) newl) (cdr l))))
  (reverse-iter () l))

(reverse squares)

; exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc target list-of-coins)
  (cond ((< target 0) 0)
        ((= target 0) 1)
        ((null? list-of-coins) 0)
		(else (+ (cc (- target (car list-of-coins)) list-of-coins)
		         (cc target (cdr list-of-coins))))))

(cc 100 us-coins) ;291
(cc 10 uk-coins) ;50

; exercise 2.20
(define (same-parity target . candidates)
  (define (parity n) (remainder n 2))
  (define (parity-rec target candidates)
    (cond ((null? candidates) ())
          ((= (parity target) (parity (car candidates)))
		    (cons (car candidates) (parity-rec target (cdr candidates))))
		  (else
		    (parity-rec target (cdr candidates)))))
  
  (cons target (parity-rec target candidates)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

; map list
(define (scale-list items factor)
  (if (null? items) nil (cons (* (car items) factor)
  							  (scale-list (cdr items) factor))))
							
(scale-list (list 1 2 3 4 5) 10)

(define (map f items)
  (if (null? items) nil
                    (cons (f (car items))
					      (map f (cdr items)))))
(map (lambda (x) (* x 10)) (list 1 2 3 4 5))

; exercise 2.21
(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
    nil
	(cons (square (car items))
	      (square-list (cdr items)))))
(square-list (list 1 2 3 4))

(define (square-list items)
  (map square items))
(square-list (list 1 2 3 4))

; exercise 2.22
; because it appends to the front of the result list every time:
; (iter (1 2 3 4) nil)
; (iter (2 3 4) (cons (square 1) nil))
; (iter (2 4) (cons (sqare 2) (cons (square 1) nil)))
; ...
; (cons (square 4) (cons (square 3) (cons (sqare 2) (cons (square 1) nil))))
; (16 9 4 1)

; exercise 2.23
(define (for-each f items)
 (map f items) #t)

(for-each (lambda (x) (newline) (display x)) (list 57 321 99))

; trees
(define (count-leaves tree)
  (cond ((null? tree) 0)
		((not (pair? tree)) 1)
   		(else (+ (count-leaves (car tree)) (count-leaves (cdr tree))))))

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x); 4

; exercise 2.24
(list 1 (list 2 (list 3 4))); (1 (2 (3 4)))
; 	(1 (2 (3 4)))
; 	/			\
; 1				(2 (3 4))
;				/		\
;			   2		(3 4)
;						/	\
;						3    4

; exercise 2.25
(define (pick-7 tree)
  (cond ((null? tree) nil)
  		((not (pair? tree)) (if (= tree 7) (display tree) nil))
		(else 
		  (pick-7 (car tree))
		  (pick-7 (cdr tree)))))

(pick-7 (list 1 3 (list 5 7) 9))
(pick-7 (list (list 7)))
(pick-7 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 (list 7))))))))

; exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y); (1 2 3 4 5 6)
(cons x y); ((1 2 3) 4 5 6)
(list x y); ((1 2 3) (4 5 6))

; exercise 2.27
(define (deep-reverse items)
  (define (deep-reverse-iter res items)
    (cond ((null? items) res)
	      ((not (pair? (car items))) 
		    (deep-reverse-iter (cons (car items) res) (cdr items)))
		  (else
		    (deep-reverse-iter (cons (deep-reverse-iter () (car items)) res) (cdr items)))))
  (deep-reverse-iter () items))

(define x (list (list 1 2) (list 3 4)))
x; ((1 2) (3 4))
(reverse x); ((3 4) (2 1))
(deep-reverse x); ((4 3) (2 1))

; exercise 2.28
(define (fringe tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (list tree))
		(else (append (fringe (car tree)) (fringe (cdr tree))))))

(define x (list (list 1 2) (list 3 4)))
(fringe x); (1 2 3 4)
(fringe (list x x)); (1 2 3 4 1 2 3 4)

; exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;a.
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cdr branch))

;b. 

(define (total-weight-branch branch)
(if (pair? (branch-structure branch))
	(total-weight (branch-structure branch))
	(branch-structure branch)))

(define (total-weight mobile)
  (+ (total-weight-branch (left-branch mobile))
     (total-weight-branch (right-branch mobile))))
	
;c.
(define (balanced? mobile)
  (let ((left (left-branch mobile))
        (right (right-bench mobile)))
  	   (= (* (branch-length left) (total-weight-branch left))
		  (* (branch-length right) (total-weight-branch right)))))

;d.
; change definition of (left-branch), (branch-structure), etc

; map trees
(define (scale-tree tree factor)
  (map (lambda (sub-tree) 
  		 (if (pair? sub-tree) 
		   (scale-tree sub-tree factor)
		   (* factor sub-tree)))
		tree))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)

; exercise 2.30
(define (square-tree tree)
  (map (lambda (sub-tree)
  		 (if (pair? sub-tree)
		   (square-tree sub-tree)
		   (* sub-tree sub-tree)))
		tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
; (1 (4 (9 16) 25) (36 49))

;exercise 2.31
(define (tree-map f tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (f tree))
		(else 
		  (cons (tree-map f (car tree))
		        (tree-map f (cdr tree))))))

(define square-tree (lambda (tree) (tree-map square tree)))
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

; exercise 2.32
(define (subsets s)
  (if (null? s)
    (list nil)
	(let ((rest (subsets (cdr s))))
	     (append rest (map (lambda (l) (cons (car s) l)) rest)))))
(subsets (list 1 2 3))