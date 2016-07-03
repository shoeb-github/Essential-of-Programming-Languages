;;2.19
(define environment-to-list
  (lambda (env)
    (cases environment env
	   (empty-env-record ()
			     '(empty-env-record))
	   (extended-env-record (syms vals env)
				(list 'extended-env-record
				      syms
				      vals
				      (environment-to-list env))))))


;below procedures from book
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-values?))
   (env environment?)))

(define scheme-values? (lambda (v) #t))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define apply-env
  (lambda (env sym)
    (cases environment env
	   (empty-env-record ()
			     (eopl:error 'apply-env "No binding for ~s" sym))
	   (extended-env-record (syms vals env)
				(let ((pos (list-find-position sym syms)))
				  (if (number? pos)
				      (list-ref vals pos)
				      (apply-env env sym)))))))
(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
	  (and (pair? val)
	       (pred (car val))
	       ((list-of pred) (cdr val)))))))
	  
(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ list-index-r 1)
		 #f))))))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))



;;2.20
(define-datatype stack stack?
  (empty-stack-record)
  (extended-stack-record
   (top-of-stack scheme-value?)
   (stk stack?)))

(define scheme-value? (lambda (v) #t))

(define empty-stack
  (lambda ()
    (empty-stack-record)))

(define push
  (lambda (e stack)
    (extended-stack-record e stack)))

(define pop
  (lambda (S)
    (cases stack S
	   (empty-stack-record ()
			       (eopl:error 'pop
					   "Attempt to pop from an empty stack"))
	   (extended-stack-recrod (top-of-stack stk)
				  stk))))

(define top
  (lambda (S)
    (cases stack S
	   (empty-stack-record ()
			       (eopl:error 'top
					   "Attempt to rea from an empty stack"))
	   (extended-stack-record (top-of-stack stk)
				  top-of-stack))))

(define empty-stack?
  (lambda (S)
    (cases stack S
	   (empty-stack-record ()
			       #t)
	   (extended-stack-record (top-of-stack stk)
				  #f))))

;;2.20

(define has-association?
  (lambda (env sym)
    (cases environment env
	   (empty-env-record ()
			     (eopl:error 'has-association? "No binding for ~s" sym))
	   (extended-env-record (syms vals env)
				(let ((pos (list-find-position sym syms)))
				  (if (number? pos)
				      #t
				      (has-association? env sym)))))))


