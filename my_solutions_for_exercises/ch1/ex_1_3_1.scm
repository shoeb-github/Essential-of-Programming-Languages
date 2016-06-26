;;as given on page 32 in the book
(define occurs-free?
  (lambda (var exp)
    (cond
     ((symbol? exp) (eqv? var exp))
     ((eqv? (car exp) 'lambda)
      (and (not (eqv? (caadr exp) var))
	   (occurs-free? var (caddr exp))))
     (else (or (occurs-free? var (car exp))
	       (occurs-free? var (cadr exp)))))))


;;as given on page 32 in the book
(define occurs-bound?
  (lambda (var exp)
    (cond
     ((symbol? exp) #f)
     ((eqv? (car exp) 'lambda)
      (or (occurs-bound? var (caddr exp))
	  (and (eqv? (caadr exp) var)
	       (occurs-free? var (caddr exp)))))
     (else (or (occurs-bound? var (car exp))
	       (occurs-bound? var (cadr exp)))))))


;;1.19
;;we represent sets as lists
(define free-vars
  (lambda (exp)
    (cond
     ((symbol? exp) (list exp))
     ((eqv? (car exp) 'lambda)
      (let ((fp (caadr exp)))
	(if (occurs-free? fp (caddr exp))
	    (remove-element fp (free-vars (caddr exp)))
	    (free-vars (caddr exp)))))
     (else (union (free-vars (car exp))
		  (free-vars (cadr exp)))))))

(define bound-vars
  (lambda (exp)
    (cond
     ((symbol? exp) '())
     ((eqv? (car exp) 'lambda)
      (if (occurs-bound? (caadr exp) exp)
	  (union (list (caadr exp))
		 (bound-vars (caddr exp)))
	  (bound-vars (caddr exp))))
     (else (union (bound-vars (car exp))
		  (bound-vars (cadr exp)))))))

(define remove-element
  (lambda (s set)
    (cond ((null? set) '())
	  ((eqv? s (car set)) (cdr set))
	  (else (cons (car set) (remove-element s (cdr set)))))))

(define element-in-set?
  (lambda (s set)
    (cond ((null? set) #f)
	  ((eqv? s (car set)) #t)
	  (else (element-in-set? s (cdr set))))))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
	  ((null? set2) set1)
	  ((element-in-set? (car set1) set2)
	   (union (cdr set1) set2))
	  (else
	   (cons (car set1) (union (cdr set1) set2))))))


;;1.20
(lambda (f)
  ((lambda (x) f) y))


;;1.21
((lambda (x) x) x)


;;1.22
(define occurs-free?
  (lambda (var exp)
    (cond
     ((symbol? exp) (eqv? var exp))
     ((eqv? (car exp) 'lambda)
      (and (not (member? var (caadr exp)))
	   (occurs-free? var (caddr exp))))
     (else (or (occurs-free? var (car exp))
	       (occurs-free? var (cadr exp)))))))

(define member?
  (lambda (s los)
    (cond ((null? los) #f)
	  ((eqv? s (car los)) #t)
	  (else (member? s (cdr los))))))

(define occurs-bound?
  (lambda (var exp)
    (cond
     ((symbol? exp) #f)
     ((eqv? (car exp) 'lambda)
      (or (occurs-bound? var (caddr exp))
	  (and (member? var (caadr exp))
	       (occurs-free? var (caddr exp)))))
     (else (or (occurs-bound? var (car exp))
	       (occurs-bound? var (cadr exp)))))))


;;1.23
(define occurs-free?
  (lambda (var exp)
    (cond
     ((symbol? exp) (eqv? var exp))
     ((eqv? (car exp) 'lambda)
      (and (not (member? var (caadr exp)))
	   (occurs-free? var (caddr exp))))
     ((eqv? (car exp) 'if)
      (or (occurs-free? (cadr exp))
	  (occurs-free? ((caddr exp)))
	  (occurs-free? ((cadddr exp)))))
     (else (or (occurs-free? var (car exp))
	       (occurs-free? var (cadr exp)))))))

(define occurs-bound?
  (lambda (var exp)
    (cond
     ((symbol? exp) #f)
     ((eqv? (car exp) 'lambda)
      (or (occurs-bound? var (caddr exp))
	  (and (member? var (caadr exp))
	       (occurs-free? var (caddr exp)))))
     ((eqv? (car exp) 'if)
      (or (occurs-bound? (cadr exp))
	  (occurs-bound? ((caddr exp)))
	  (occurs-bound? ((cadddr exp)))))
     (else (or (occurs-bound? var (car exp))
	       (occurs-bound? var (cadr exp)))))))


;;1.24
					;Given
					;(let ((<variable1> <value-exp1>)...)
					;  <body>)
					;A variable x is free if
					;(a) x is not in {<variable1>....} and x is free in <body>
					;or
					;(b) x is free in some of {<value-exp1>....}
					;
					;A variable x is bound if
					;(a) x is in {<variable1>...} and x is free in <body>
					;or
					;(b) x is bound in <body>
					;or
					;(c) x is bound in some {<value-exp1>...}


					;Given
					;(let* ((<variable1> <value-exp1>)...)
					;<body>)
					;this can be treated as
					;(let ((<variable1> <value-exp>))
					;  (let* ((<variable2> <value-exp2>)...)
					;   <body>))



;;1.25
					;Given
					;(quote <datum>)
					;x is neither bound nor free

;;1.26
					;Given
					;(set! <variable> <exp>)
					;A variable x is free if
					;(a) x is equal to <variable>
					;or
					;x is free in <exp>
					;
					;A variable x is bound if
					;(a) x is bound in <exp>
