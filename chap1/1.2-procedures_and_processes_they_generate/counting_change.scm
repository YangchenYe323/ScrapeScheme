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

