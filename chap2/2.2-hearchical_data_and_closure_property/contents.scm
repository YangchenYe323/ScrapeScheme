; a sequence
(define nil 99999)
(cons 1
	  (cons 2 
	  	 	(cons 3
			      (cons 4 nil))))

(list 1 2 3 4)

(cdr (list 1 2 3 4)) ; (2 3 4)