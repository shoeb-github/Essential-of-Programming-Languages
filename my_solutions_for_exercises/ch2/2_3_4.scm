;;2.21
					;
					; (extend-env '() '() (empty-env)) produces ((() ()) ())
					;

;;2.22
(define-datatype rib rib?
  (a-rib
   (car-rib (list-of symbol?))
   (cdr-rib (list-of scheme-values?))))

(define scheme-values? (lambda (v) #t))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ list-index-r 1)
		 #f))))))

(define rib-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (syms vals env)
    (cons (a-rib syms vals) env)))

(define apply-env
  (lambda (env sym)
    (if (null? env)
	(eopl:error 'apply-env
		    "No binding for ~s" sym)
	(cases rib (car env)
	       (a-rib (car-rib cdr-rib)
		      (let ((pos (rib-find-position sym car-rib)))
			(if (number? pos)
			    (list-ref cdr-rib pos)
			    (apply-env (cdr env) sym))))))))
(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
	  (and (pair? val)
	       (pred (car val))
	       ((list-of pred) (cdr val)))))))


;;2.23
(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (syms vals env)
    (if (null? env)
	(a-rib syms vals)
	(cases rib env
	       (a-rib (car-rib cdr-rib)
		      (a-rib (append syms car-rib) (append vals cdr-rib)))))))


(define apply-env
  (lambda (env sym)
    (if (null? env)
	(eopl:error 'apply-env
		    "No binding for ~s" sym)
	(cases rib env
	       (a-rib (car-rib cdr-rib)
		      (let ((pos (rib-find-position sym car-rib)))
			(if (number? pos)
			    (list-ref cdr-rib pos)
			    (eopl:error 'apply-env
					"No binding for ~s" sym))))))))


;;2.24
;abstract syntax representation
(define-datatype substitution substitution?
  (empty-substitution-record)
  (extended-substitution-record (sym trm subst)
			 (sym symbol?)
			 (trm term?)
			 (subst substitution?)))

(define empty-subst
  (lambda ()
    (empty-substitution-record)))

(define extend-subst
  (lambda (i t s)
    (cases substitution s
	   (empty-substitution-record ()
				      (extended-substitution-record i t s))
	   (extended-substitution-record (sym trm subst)
					 (if (eqv? i sym)
					     (extended-substitution-record i t subst)
					     (extended-substitution-record sym trm (extend-subst i t subst)))))))

(define apply-subst
  (lambda (s i)
    (cases substitution s
	   (empty-substitution-record ()
				      (var-term i))
	   (extended-substitution-record (sym trm subst)
					 (if (eqv? i sym)
					     trm
					     (apply-subst subst i))))))

;Procedural representation
(define empty-subst
  (lambda ()
    (lambda (sym)
      (var-term sym))))

(define extend-subst
  (lambda (i t s)
    (lambda (sym)
      (if (eqv? sym i)
	  t
	  (apply-subst s sym)))))

(define apply-subst
  (lambda (s i)
    (s i)))

;subst-in-term
(define subst-in-term
  (lambda (t s)
    (cases term t
	   (var-term (id)
		     (apply-subst s id))
	   (constant-term (datum)
			  t)
	   (app-term (terms)
		     (app-term (map (lambda (e) (subst-in-term e s)) terms))))))


;subst-in-terms
(define subst-in-terms
  (lambda (lot s)
    (map (lambda (t) (subst-in-term t s)) lot)))



;repeat from exercise 2.13
(define-datatype term term?
  (var-term
   (id symbol?))
  (constant-term
   (datum constant?))
  (app-term
   (terms (list-of term?))))

(define constant?
  (lambda (datum)
    (or (string? datum) (number? datum) (boolean? datum) (null? datum))))


(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
	  (and (pair? val)
	       (pred (car val))
	       ((list-of pred) (cdr val)))))))


;;2.25
					;
					;we need the memv test in unify term because consider
					;t = 'a  --> this is var-term
					;and u = ('a) --> this is app-term
					;
					;for any unifier s we have
					;{(subs-in-term t s) = s(a)} != {(subst-in-term u s) = (s(a))}
					;


(define unit-subst
  (lambda (tid u)
    (extend-subst tid u (empty-subst))))



(define compose-substs
  (lambda (s1 s2)
    (cases substitution s1
	   (empty-substitution-record ()
				      s2)
	   (extended-sustitution-record (s1-sym s1-trm s1-subst)
					(extend-subst s1-sym (subst-in-term s1-term s2) (compose-substs s1-subst s2))))))


							 
				      
