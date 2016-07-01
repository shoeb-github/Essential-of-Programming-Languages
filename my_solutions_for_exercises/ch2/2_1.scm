;;2.1
(define N 16)

(define iszero? null?)

(define zero '())

(define succ
  (lambda (bnum)
    (if (iszero? bnum)
	'(1)
	(if (= (+ 1 (car bnum)) N)
	    (cons 0 (succ (cdr bnum)))
	    (cons (+ 1 (car bnum)) (cdr bnum))))))

(define pred
  (lambda (bnum)
    (if (iszero? bnum)
	(eopl:error 'pred
		    "Cannot take predecessor for zero")
	(if (and (null? (cdr bnum)) (= 1 (car bnum)))
	    zero
	    (if (= 0 (car bnum))
		(cons (- N 1) (pred (cdr bnum)))
		(cons (- (car bnum) 1) (cdr bnum)))))))

(define factorial
  (lambda (bnum)
    (if (iszero? bnum)
	(succ zero)
	(prod bnum (factorial (pred bnum))))))

(define prod
  (lambda (bnum1 bnum2)
    (if (or (iszero? bnum1) (iszero? bnum2))
	    zero
	    (plus bnum2 (prod (pred bnum1) bnum2)))))

(define plus
  (lambda (bnum1 bnum2)
    (if (or (iszero? bnum1))
	bnum2
	(succ (plus (pred bnum1) bnum2)))))


;As the argument changes (additional bigit) execution time is exponential because the base case for recurions are the bigit zero
;and all operations do not go back the recursion tree until they reach zero.

;As the base changes is not affected because we have same number of calls to plus,prod,factorial,pred,succ



;;2.2
;We would like the user to construct any number they conceive of without having to go through sequence of succ,pred,sum,prod calls.
;None of these representations provide it.
