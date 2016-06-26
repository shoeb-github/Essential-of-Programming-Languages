;
;1.27 and 1.28 are straightforward.


;1.29
;It should be (a : 0 0)


;1.30
(lambda (x)
  (lambda (y)
    x))

;1.31


(define lexical-address
  (lambda (exp)
    (lexical-address-denv exp '())))


(define lexical-address-denv
  (lambda (exp denv)
    (cond ((symbol? exp) (let ((pos (find-in-denv exp denv)))
			   (if (null? pos)
			       (list exp 'free)
			       (append (list exp ':) pos))))
	  ((eqv? (car exp) 'if) (append
				 '(if)
				 (map (lambda (e) (lexical-address-denv e denv)) (cdr exp))))
	  ((eqv? (car exp) 'lambda) (let ((new-denv (extend-denv (cadr exp) denv)))
				     (list
				      'lambda (cadr exp)
				      (lexical-address-denv (caddr exp) new-denv))))
	  (else (append
		 (list (lexical-address-denv (car exp) denv))
		 (map (lambda (e) (lexical-address-denv e denv)) (cdr exp)))))))

(define extend-denv
  (lambda (los denv)
    (cons los denv)))

(define find-in-denv
  (lambda (s denv)
    (if (null? denv)
	'()
	(let ((pos (find-in-los s (car denv))))
	  (if (number? pos)
	      (list 0 pos)
	      (let ((lex-add (find-in-denv s (cdr denv))))
		(if (null? lex-add)
		    '()
		    (list (+ 1 (car lex-add)) (cadr lex-add)))))))))

(define find-in-los
  (lambda (s los)
    (cond ((null? los) #f)
	  ((eqv? s (car los)) 0)
	  (else (let ((p (find-in-los s (cdr los))))
		  (if (number? p)
		      (+ 1 p)
		      #f))))))


	  
      
;;1.32
(define un-lexical-address
  (lambda (exp)
    (un-lexical-address-denv exp '())))

(define un-lexical-address-denv
  (lambda (exp denv)
    (cond ((eqv? (cadr exp) 'free) (let ((pos (find-in-denv (car exp) denv)))
				     (if (null? pos)
					 exp
					 #f)))
	  ((eqv? (car exp) ':) (let ((v (denv-ref (cdr exp) denv)))
				 (if (symbol? v)
				     v
				     #f)))
	  ((eqv? (car exp) 'if) (let ((bexps (map (lambda (e) (un-lexical-address-denv e denv)) (cdr exp))))
				  (if (and (car bexps) (cadr bexps) (caddr bexps))
				      (append '(if) bexps)
				      #f)))
	  ((eqv? (car exp) 'lambda) (let ((new-denv (extend-denv (cadr exp) denv)))
				      (let ((bexp (un-lexical-address-denv (caddr exp) new-denv)))
					(if bexp
					    (list 'lambda (cadr exp) bexp)
					    #f))))
	  (else (let ((rand-exps (map (lambda (e) (un-lexical-address-denv e denv)) (cdr exp)))
		      (rator-exp (un-lexical-address-denv (car exp) denv)))
		  (let ((un-lexical-exp (append (list rator-exp) rand-exps)))
		    (let ((pos-of-f (find-in-los '#f un-lexical-exp)))
		      (if (number? pos-of-f)
			  #f
			  (append (list rator-exp) rand-exps)))))))))


(define denv-ref
  (lambda (pos denv)
    (cond ((null? denv) #f)
	  ((or (< (car pos) 0) (< (cadr pos) 0)) #f)
	  ((= 0 (car pos)) (los-ref (cadr pos) (car denv)))
	  (else (denv-ref (list (- (car pos) 1) (cadr pos)) (cdr denv))))))

(define los-ref
  (lambda (pos los)
    (cond ((null? los) #f)
	  ((< pos 0) #f)
	  ((= pos 0) (car los))
	  (else (los-ref (- pos 1) (cdr los))))))
	  
