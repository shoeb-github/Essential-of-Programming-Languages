;;2.6
					; Abstract syntax tree for ((lambda (a) (a b)) c)
					;
					;                                                     app-exp
					;                                                       /  \
					;                                                 rator/    \rand
					;                                                     /      \
					;                                               lambda-exp  var-exp
					;                                                 /  \         \
					;                                              id/    \body     \id
					;                                               /      \         \
					;                                              a     app-exp      c
					;                                                      /  \
					;                                                rator/    \rand
					;                                                    /      \
					;                                                 var-exp  var-exp
					;                                                  /          \
					;                                               id/            \id
					;                                                /              \
					;                                               a                b


;;2.7
(define-datatype expression expression?
  (lit-exp
   (datum number?))
  (var-exp
   (id symbol?))
  (if-exp
   (test-exp expression?)
   (true-exp expression?)
   (false-exp expression?))
  (lambda-exp
   (ids (list-of symbol?))
   (body expression?))
  (app-exp
   (rator expression?)
   (rands (list-of expression?)))
  (lex-info
   (id symbol?)
   (depth number?)
   (position number?))
  (free-info
   (id symbol?)))


(define list-of
  (lambda (pred)
    (lambda (v)
      (if (null? v)
	  #t
	  (and (pred (car v))
	       ((list-of pred) (cdr v)))))))


(define unparse-expression
  (lambda (exp)
    (cases expression exp
	   (lit-exp (datum)
		    datum)
	   (var-exp (id)
		    id)
	   (if-exp (test-exp true-exp false-exp)
		   (list 'if (unparse-expression test-exp)
			 (unparse-expression true-exp)
			 (unparse-expression false-exp)))
	   (lambda-exp (ids body)
		       (list 'lambda
			     ids
			     (unparse-expression body)))
	   (app-exp (rator rands)
		    (append (list (unparse-expression rator))
			  (map (lambda (e) (unparse-expression e)) rands)))
	   (lex-info (id depth position)
		     (list id ': depth position))
	   (free-info (id)
		      (list id 'free)))))


(define parse-expression
  (lambda (datum)
    (cond
     ((number? datum) (lit-exp datum))
     ((symbol? datum) (var-exp datum))
     ((pair? datum)
      (cond ((eqv? (car datum) 'if)
	      (if-exp (parse-expression (cadr datum)) (parse-expression (caddr datum)) (parse-expression (cadddr datum))))
	     ((eqv? (car datum) 'lambda)
	      (lambda-exp (cadr datum) (parse-expression (caddr datum))))
	     (else
	      (app-exp (parse-expression (car datum)) (map (lambda (e) (parse-expression e)) (cdr datum))))))
     (else
      (eopl:error 'parse-expression
		  "Invalid concrete syntax ~s" datum)))))

			   
(define abstract-lexical-address-denv
  (lambda (exp denv)
    (cases expression exp
    (lit-exp (datum)
	     (lit-exp datum))
    (var-exp (id)
	     (let ((d-p (find-in-denv id denv)))
	       (if (null? d-p)
		   (free-info id)
		   (lex-info id (car d-p) (cadr d-p)))))
    (free-info (id)
	       (let ((d-p (find-in-denv id denv)))
		 (if (null? d-p)
		     (free-info id)
		     (eopl:error 'abstract-lexical-address-denv
				 "Variable ~s not free in environment ~s" id denv))))
    (lex-info (id depth position)
	      (let ((d-p (find-in-denv id denv)))
		(cond ((null? d-p) (eopl:error "Variable ~s not bound in environment ~s" id denv))
		      ((and (= depth (car d-p)) (= position (cadr d-p)))
		       (lex-info id depth position))
		      (else
		       (eopl:error 'abstract-lexical-address-denv
			            "Variable ~s not bound correctly in denv ~s. 
                                    Correct depth ~s, position ~s. But given depth ~s position ~s"
                                    id denv (car d-p) (cadr d-p) depth position)))))
    (if-exp (test-exp true-exp false-exp)
	    (if-exp (abstract-lexical-address-denv test-exp denv)
		    (abstract-lexical-address-denv true-exp denv)
		    (abstract-lexical-address-denv false-exp denv)))
    (lambda-exp (ids body)
		(let ((new-denv (extend-denv ids denv)))
		  (lambda-exp ids
			      (abstract-lexical-address-denv body new-denv))))
    (app-exp (rator rands)
	     (app-exp
	      (abstract-lexical-address-denv rator denv)
	      (map (lambda (e) (abstract-lexical-address-denv e denv)) rands))))))

(define abstract-lexical-address
  (lambda (exp)
    (abstract-lexical-address-denv exp (init-denv))))


(define lexical-address
  (lambda (datum)
    (unparse-expression (abstract-lexical-address (parse-expression datum)))))


(define init-denv
  (lambda ()
    '()))

(define extend-denv
  (lambda (ids denv)
    (cons ids denv)))


(define find-in-denv
  (lambda (id denv)
    (if (null? denv)
	'()
	(let ((p (find-in-list id (car denv))))
	  (if (number? p)
	      (list 0 p)
	      (let ((d-p (find-in-denv id (cdr denv))))
		(if (null? d-p)
		    d-p
		    (list (+ 1 (car d-p)) (cadr d-p)))))))))

(define find-in-list
  (lambda (v list)
    (cond ((null? list) #f)
	  ((eqv? v (car list)) 0)
	  (else (let ((p (find-in-list v (cdr list))))
		  (if (number? p)
		      (+ 1 p)
		      #f))))))


;;2.8
(define free-vars-exp
  (lambda (exp)
      (cases expression exp
	     (lit-exp (datum)
		      '())
	     (var-exp (id)
		      '())
	     (lex-info (id depth position)
		       '())
	     (free-info (id)
			(list id))
	     (if-exp (test-exp true-exp false-exp)
		     (union (free-vars-exp test-exp) (union (free-vars-exp true-exp) (free-vars-exp false-exp))))
	     (lambda-exp (ids body)
			 (let ((free-vars-in-body (free-vars-exp body)))
			   (diff free-vars-in-body ids)))
	     (app-exp (rator rands)
		      (union (free-vars-exp rator)
			     (accumulate union '()
					 (map (lambda (e) (free-vars-exp e)) rands)))))))

(define free-vars
  (lambda (datum)
    (free-vars-exp (abstract-lexical-address (parse-expression datum)))))


(define union
  (lambda (s1 s2)
    (cond ((null? s1) s2)
	  ((null? s2) s1)
	  (else (let ((diff-s1-s2 (diff s1 s2)))
		  (append diff-s1-s2 s2))))))

(define diff
  (lambda (s1 s2)
    (cond ((null? s1) '())
	  ((member? (car s1) s2) (diff (cdr s1) s2))
	  (else (cons (car s1) (diff (cdr s1) s2))))))

(define intersection
  (lambda (s1 s2)
    (diff s1 (diff s1 s2))))

(define member?
  (lambda (s set)
    (cond ((null? set) #f)
	  ((eqv? s (car set)) #t)
	  (else (member? s (cdr set))))))

(define accumulate
  (lambda (op init list)
    (if (null? list)
	init
	(op (car list) (accumulate op init (cdr list))))))

(define bound-vars-exp
  (lambda (exp)
      (cases expression exp
	     (lit-exp (datum)
		      '())
	     (var-exp (id)
		      '())
	     (lex-info (id depth position)
		       (list id))
	     (free-info (id)
			'())
	     (if-exp (test-exp true-exp false-exp)
		     (union (bound-vars-exp test-exp) (union (bound-vars-exp true-exp) (bound-vars-exp false-exp))))
	     (lambda-exp (ids body)
			 (let ((bound-vars-in-body (bound-vars-exp body))
			       (free-vars-in-body (free-vars-exp body)))
			   (union bound-vars-in-body
				  (intersection ids free-vars-in-body))))
	     (app-exp (rator rands)
		      (union (bound-vars-exp rator)
			     (accumulate union '()
					 (map (lambda (e) (bound-vars-exp e)) rands)))))))


(define bound-vars
  (lambda (datum)
    (bound-vars-exp (abstract-lexical-address (parse-expression datum)))))


;;2.9
(define-datatype expression expression?
  (var-exp
   (id symbol?))
  (lambda-exp
   (id symbol?)
   (body expression?))
  (app-exp
   (rator expression?)
   (rand  expression?)))


(define parse-expression
  (lambda (datum)
    (cond ((symbol? datum) (var-exp datum))
	  ((pair? datum)
	   (cond ((eqv? (car datum) 'lambda)
		  (if (pair? (cdr datum))
		      (if (and (pair? (cadr datum)) (pair? (cddr datum)))
			  (if (and (null? (cdadr datum)) (null? (cdddr datum)))
			      (if (symbol? (caadr datum))
				  (lambda-exp (caadr datum) (parse-expression (caddr datum)))
				  (eopl:error 'parse-expression
					      "Invalid concrete syntax for lambda expression ~s. Formal parameter ~s not an id" datum (caadr datum)))
			      (eopl:error 'parse-expression
					  "Invalid concrete syntax for lambda expression ~s. Formal parameter list ~s has more than one variable 
                                           or body has more than one expression"
					  datum (cadr datum) (cddr datum)))
			  (eopl:error 'parse-expression
				      "Invalid concrete syntax for lambda expression ~s. Formal parameter list:~s and body: ~s not correct syntax"
				      datum (cadr datum) (cddr datum)))
		      (eopl:error 'parse-expression
				  "Invalid concrete syntax for lambda expression ~s" datum)))
		 ((null? (cddr datum)) (app-exp (parse-expression (car datum))
						(parse-expression (cadr datum))))
		 (else
		  (eopl:error 'parse-expression
			      "Invalid concrete syntax ~s" datum))))
	  (else
	   (eopl:error 'parse-expression
		       "Invalid concrete syntax ~s" datum)))))


;;2.10
(define-datatype expression expression?
  (var-exp
   (id symbol?))
  (lambda-exp
   (id symbol?)
   (body expression?))
  (app-exp
   (rator expression?)
   (rand  expression?)))

(define fresh-id
  (lambda (exp s)
    (let ((syms (all-ids exp)))
      (letrec
	  ((loop (lambda (n)
		   (let ((sym (string->symbol
			       (string-append s
					      (number->string n)))))
		     (if (memv sym syms) (loop (+ n 1)) sym)))))
	(loop 0)))))

(define all-ids
  (lambda (exp)
    (cases expression exp
	   (var-exp (id)
		    (list id))
	   (lambda-exp (id body)
		       (let ((all-ids-in-body (all-ids body)))
			 (if (memv id all-ids-in-body)
			     all-ids-in-body
			     (cons id all-ids-in-body))))
	   (app-exp (rator rand)
		    (let ((all-ids-in-rator (all-ids rator))
			  (all-ids-in-rand (all-ids rand)))
		      (union all-ids-in-rator all-ids-in-rand))))))



;;2.11
;assume rator is not a lit-exp. Otherwise modify the predicate for rator in app-exp
(define-datatype expression expression?
  (lit-exp
   (datum (lambda (datum) (and (integer? datum) (> datum 0)))))
  (var-exp
   (id symbol?))
  (lambda-exp
   (id symbol?)
   (body expression?))
  (app-exp
   (rator expression?)
   (rand  expression?))
  (primapp-exp
   (rator prim-rator?)
   (rand1 expression?)
   (rand2 expression?)))

(define prim-rator?
  (lambda (datum)
    (and (symbol? datum) (or (eqv? datum '+) (eqv? datum '*)))))

(define unparse-expression
  (lambda (exp)
    (cases expression exp
	   (lit-exp (datum)
		    datum)
	   (var-exp (id)
		    id)
	   (lambda-exp (id body)
		       (list 'lambda
			     (list id)
			     (unparse-expression body)))
	   (app-exp (rator rand)
		    (list (unparse-expression rator)
			  (unparse-expression rand)))
	   (primapp-exp (rator rand1 rand2)
			 (list rator
			       (unparse-expression rand1)
			       (unparse-expression rand2))))))


(define parse-expression
  (lambda (datum)
    (cond
     ((number? datum) (lit-exp datum))
     ((symbol? datum) (var-exp datum))
     ((pair? datum)
      (cond ((eqv? (car datum) 'lambda)
	     (lambda-exp (caadr datum) (parse-expression (caddr datum))))
	    ((prim-rator? (car datum)) (primapp-exp (car datum)
						     (parse-expression (cadr datum))
						     (parse-expression (caddr datum))))
	    (else
	     (app-exp (parse-expression (car datum)) (parse-expression (cadr datum))))))
     (else
      (eopl:error 'parse-expression
		  "Invalid concrete syntax ~s" datum)))))


(define all-ids
  (lambda (exp)
    (cases expression exp
	   (var-exp (id)
		    (list id))
	   (lambda-exp (id body)
		       (let ((all-ids-in-body (all-ids body)))
			 (if (memv id all-ids-in-body)
			     all-ids-in-body
			     (cons id all-ids-in-body))))
	   (app-exp (rator rand)
		    (let ((all-ids-in-rator (all-ids rator))
			  (all-ids-in-rand (all-ids rand)))
		      (union all-ids-in-rator all-ids-in-rand)))
	   (lit-exp (datum)
		    '())
	   (primapp-exp (prim rand1 rand2)
			(union (all-ids rand1) (all-ids rand2))))))

					;reuse fresh-id from previous exercise


(define lambda-calculus-subst
  (lambda (exp subst-exp subst-id)
    (letrec
	((subst
	  (lambda (exp)
	    (cases expression exp
		   (var-exp (id)
			    (if (eqv? id subst-id) subst-exp exp))
		   (lambda-exp (id body)
			       (if (eqv? id subst-id)
				   exp
				   (let ((new-id (fresh-id body (symbol->string id))))
				     (let ((new-body (lambda-calculus-subst body (var-exp new-id) id)))
				       (lambda-exp new-id (subst new-body))))))
		   (app-exp (rator rand)
			    (app-exp (subst rator) (subst rand)))
		   (lit-exp (datum)
			    (lit-exp datum))
		   (primapp-exp (prim rand1 rand2)
				(primapp-exp prim (subst rand1) (subst rand2)))
		   ))))
      (subst exp))))



;;2.12
;;Yes they use recursion explicitly.
(define occurs-free?
  (lambda (var exp)
    (cases expression exp
	   (var-exp (id)
		    (eqv? id var))
	   (lit-exp (datum)
		    #f)
	   (lambda-exp (id body)
		       (and (not (eqv? id var))
			    (occurs-free? var body)))
	   (app-exp (rator rand)
		    (or (occurs-free? var rator) (occurs-free? var rand)))
	   (primapp-exp (prim rand1 rand2)
			(and (not (eqv? prim var))
			     (or (occurs-free? var rand1) (occurs-free? var rand2)))))))

			     
(define lambda-calculus-alpha
  (lambda (exp)
    (cases expression exp
	   (var-exp (id)
		    (eopl:error 'lambda-calculus-alpha
				"Operator defined for lambda expression.Provided expression is a var-exp ~s"
				exp))
	   (lit-exp (datum)
		    (eopl:error 'lambda-calculus-alpha
				"Operator defined for lambda expression.Provided expression is a lit-exp ~s"
				exp))
	   (app-exp (rator rand)
		    (eopl:error 'lambda-calculus-alpha
				"Operator defined for lambda expression.Provided expression is a app-exp ~s"
				exp))
	   (primapp-exp (rator rand1 rand2)
		    (eopl:error 'lambda-calculus-alpha
				"Operator defined for lambda expression.Provided expression is a primapp-exp ~s"
				exp))
	   (lambda-exp (id body)
		       (let ((fv (free-vars body))
			     (aids (all-ids body)))
			 (let ((not-free (diff aids fv)))
			   (if (null? not-free)
			       exp
			       (lambda-exp (car not-free) (lambda-calculus-subst body (var-exp (car not-free)) id))))
			 ))
	   )))


(define lambda-calculus-beta
  (lambda (exp)
    (cases expression exp
	   (var-exp (id)
		    (eopl:error 'lambda-calculus-beta
				"Operator defined for application expression.Provided expression is a var-exp ~s"
				exp))
	   (lit-exp (datum)
		    (eopl:error 'lambda-calculus-beta
				"Operator defined for application expression.Provided expression is a lit-exp ~s"
				exp))
	   (lambda-exp (id body)
		    (eopl:error 'lambda-calculus-beta
				"Operator defined for application expression.Provided expression is a lambda-exp ~s"
				exp))
	   (primapp-exp (rator rand1 rand2)
		    (eopl:error 'lambda-calculus-beta
				"Operator defined for application expression.Provided expression is a primapp-exp ~s"
				exp))
	   (app-exp (rator rand)
		    (let ((rator-exp rator))
		      (cases expression rator-exp
			   (var-exp (id)
				    (eopl:error 'lambda-calculus-beta
						"Invalid operator expression in app-exp ~s, operator should be lambda-exp, and not a var-exp ~s"
						exp rator-exp))
			   (lit-exp (datum)
				    (eopl:error 'lambda-calculus-beta
						"Invalid operator expression in app-exp ~s, operator should be lambda-exp, and not a lit-exp ~s"
						exp rator-exp))
			   (lambda-exp (id body)
				       (lambda-calculus-subst body rand id))
			   (primapp-exp (rator rand1 rand2)
					(eopl:error 'lambda-calculus-beta
						    "Invalid operator expression in app-exp ~s, operator should be lambda-exp, and not a primapp-exp ~s"
						    exp rator-exp))
			   (app-exp (rator rand)
				    (eopl:error 'lambda-calculus-beta
						"Invalid operator expression in app-exp ~s, operator should be lambda-exp, and not a app-exp ~s"
						exp rator-exp))))))))

(define lambda-calculus-eta
  (lambda (exp)
    (cases expression exp
	   (var-exp (id)
		    (eopl:error 'lambda-calculus-eta
				"Operator defined for lambda expression.Provided expression is a var-exp ~s"
				exp))
	   (lit-exp (datum)
		    (eopl:error 'lambda-calculus-eta
				"Operator defined for lambda expression.Provided expression is a lit-exp ~s"
				exp))
	   (app-exp (rator rand)
		    (eopl:error 'lambda-calculus-eta
				"Operator defined for lambda expression.Provided expression is a app-exp ~s"
				exp))
	   (primapp-exp (rator rand1 rand2)
		    (eopl:error 'lambda-calculus-eta
				"Operator defined for lambda expression.Provided expression is a primapp-exp ~s"
				exp))
	   (lambda-exp (id body)
		       (let ((body-exp body)
			     (sid id))
			 (cases expression body-exp
				(var-exp (id)
					 (eopl:error 'lambda-calculus-eta
						     "Invalid body expression in lambda-exp ~s, body should be app-exp, and not a var-exp ~s"
						     exp body-exp))
				(lit-exp (datum)
					 (eopl:error 'lambda-calculus-eta
						     "Invalid body expression in lambda-exp ~s, body should be app-exp, and not a lit-exp ~s"
						     exp body-exp))
				(lambda-exp (id body)
					 (eopl:error 'lambda-calculus-eta
						     "Invalid body expression in lambda-exp ~s, body should be app-exp, and not a lambda-exp ~s"
						     exp body-exp))
				(primapp-exp (rator rand1 rand2)
					     (eopl:error 'lambda-calculus-eta
						    "Invalid body expression in lambda-exp ~s, body should be app-exp, and not a primapp-exp ~s"
						    exp body-exp))
				(app-exp (rator rand)
					 (let ((rand-exp rand))
					   (cases expression rand-exp
						  (var-exp (id)
							   (eopl:error 'lambda-calculus-eta
								       (if (not (eqv? id sid))
									   (eopl:error 'lambda-calculus-eta
										       "Formal parameter id ~s and operand id ~s in body of lambda-exp ~s, should match"
										       sid id body-exp)
									   (if (not (occurs-free? id rator)) rator exp))))
						  (lit-exp (datum)
							   (eopl:error 'lambda-calculus-eta
								       "Invalid operand expression in body of lambda-exp ~s, operand should be var-exp, and not a lit-exp ~s"
								       exp rand-exp))
						  (lambda-exp (id body)
							      (eopl:error 'lambda-calculus-eta
								       "Invalid operand expression in body of lambda-exp ~s, operand should be var-exp, and not a lambda-exp ~s"
								       exp rand-exp))
						  (primapp-exp (rator rand1 rand2)
							       (eopl:error 'lambda-calculus-eta
									   "Invalid operand expression in body of lambda-exp ~s, operand should be var-exp, and not a primapp-exp ~s"
									   exp rand-exp))
						  (app-exp (rator rand)
							   (eopl:error 'lambda-calculus-eta
								       "Invalid operand expression in body of lambda-exp ~s, operand should be var-exp, and not a app-exp ~s"
								       exp rand-exp))))))))
	   )))



;;2.13




				
