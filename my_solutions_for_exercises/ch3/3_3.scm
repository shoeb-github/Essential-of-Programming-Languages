;;interpreter code from book
(define eval-program
  (lambda (pgm)
    (cases program pgm
           (a-program (body)
                      (eval-expression body (init-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
           (lit-exp (datum) datum)
           (var-exp (id) (apply-env env id))
           (primapp-exp (prim rands)
                        (let ((args (eval-rands rands env)))
                          (apply-primitive prim args))))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
           (add-prim () (+ (car args) (cadr args)))
           (subtract-prim () (- (car args) (cadr args)))
           (mult-prim () (* (car args) (cadr args)))
           (incr-prim () (+ (car args) 1))
           (decr-prim () (- (car args) 1))
           )))

(define init-env
  (lambda ()
    (extend-env
     '(i v x)
     '(1 5 10)
     (empty-env))))

;;parser generator fromm book
(define scanner-spec-3-1
  '((white-sp
     (whitespace) skip)
    (comment
     ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "?"))) symbol)
    (number
     (digit (arbno digit)) number)))


(define grammar-3-1
  '((program
     (expression)
     a-program)
    (expression
     (number)
     lit-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     (primitive "(" (separated-list expression ",") ")" )
     primapp-exp)
    (primitive ("+")
               add-prim)
    (primitive ("*")
               mult-prim)
    (primitive ("-")
               subtract-prim)
    (primitive ("add1")
               incr-prim)
    (primitive ("sub1")
               decr-prim)))


(define scan&parse
  (sllgen:make-string-parser
   scanner-spec-3-1
   grammar-3-1))

(sllgen:make-define-datatypes scanner-spec 3-1 grammar-3-1)


(define run
  (lambda (string)
    (eval-program
     (scan&parse string))))


(define read-eval-print
  (sllgen:make-rep-loop "--->" eval-program
                        (sllgen:make-stream-parser
                         scanner-spec-3-1
                         grammar-3-1)))

;;environments from book
;;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vec vector?)              ; can use this for anything.
    (env environment?))
  )

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((position (rib-find-position sym syms)))
          (if (number? position)
              (vector-ref vals position)
              (apply-env env sym)))))))

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))


;;3.10

(define true-value?
  (lambda (x)
    (not (zero? x))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
           (lit-exp (datum) datum)
           (var-exp (id) (apply-env env id))
	   (if-exp (test-exp true-exp false-exp)
		   (if (true-value? (eval-expression test-exp env))
		       (eval-expression true-exp env)
		       (eval-expression false-exp env)))
           (primapp-exp (prim rands)
                        (let ((args (eval-rands rands env)))
                          (apply-primitive prim args))))))

(define grammar-3-3
  '((program
     (expression)
     a-program)
    (expression
     (number)
     lit-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     (primitive "(" (separated-list expression ",") ")" )
     primapp-exp)
    (expression
     ("if" expression "then" expression "else" expression)
      if-exp)
    (primitive ("+")
               add-prim)
    (primitive ("*")
               mult-prim)
    (primitive ("-")
               subtract-prim)
    (primitive ("add1")
               incr-prim)
    (primitive ("sub1")
               decr-prim)))

(define scan&parse
  (sllgen:make-string-parser
   scanner-spec-3-1
   grammar-3-3))

(sllgen:make-define-datatypes scanner-spec-3-1 grammar-3-3)


(define run
  (lambda (string)
    (eval-program
     (scan&parse string))))


;;3.11

(define grammar-3-3b
  '((program
     (expression)
     a-program)
    (expression
     (number)
     lit-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     (primitive "(" (separated-list expression ",") ")" )
     primapp-exp)
    (expression
     ("if" expression "then" expression "else" expression)
      if-exp)
    (primitive ("+")
               add-prim)
    (primitive ("*")
               mult-prim)
    (primitive ("-")
               subtract-prim)
    (primitive ("add1")
               incr-prim)
    (primitive ("sub1")
               decr-prim)
    (primitive ("equal?")
	       equal?-prim)
    (primitive ("zero?")
	       zero?-prim)
    (primitive ("greater?")
	       greater?-prim)
    (primitive ("less?")
	       less?-prim)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
           (add-prim () (+ (car args) (cadr args)))
           (subtract-prim () (- (car args) (cadr args)))
           (mult-prim () (* (car args) (cadr args)))
           (incr-prim () (+ (car args) 1))
           (decr-prim () (- (car args) 1))
	   (equal?-prim () (if (= (car args) (cadr args)) 1 0))
	   (zero?-prim () (if (zero? (car args)) 1 0))
	   (greater?-prim () (if (> (car args) (cadr args)) 1 0))
	   (less?-prim () (if (< (car args) (cadr args)) 1 0))
           )))

(define scan&parse
  (sllgen:make-string-parser
   scanner-spec-3-1
   grammar-3-3b))

(sllgen:make-define-datatypes scanner-spec-3-1 grammar-3-3b)



;;3.12
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
           (add-prim () (+ (car args) (cadr args)))
           (subtract-prim () (- (car args) (cadr args)))
           (mult-prim () (* (car args) (cadr args)))
           (incr-prim () (+ (car args) 1))
           (decr-prim () (- (car args) 1))
	   (equal?-prim () (if (= (car args) (cadr args)) 1 0))
	   (zero?-prim () (if (zero? (car args)) 1 0))
	   (greater?-prim () (if (> (car args) (cadr args)) 1 0))
	   (less?-prim () (if (< (car args) (cadr args)) 1 0))
	   (null?-prim () (if (null? (car args)) 1 0))
           (list-prim () args)
           (cons-prim () (cons (car args) (cadr args)))
           (car-prim () (caar args))
           (cdr-prim () (cdar args))
           )))


(define grammar-3-3c
  '((program
     (expression)
     a-program)
    (expression
     (number)
     lit-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     (primitive "(" (separated-list expression ",") ")" )
     primapp-exp)
    (expression
     ("if" expression "then" expression "else" expression)
      if-exp)    
    (primitive ("+")
               add-prim)
    (primitive ("*")
               mult-prim)
    (primitive ("-")
               subtract-prim)
    (primitive ("add1")
               incr-prim)
    (primitive ("sub1")
               decr-prim)
    (primitive ("minus")
               minus-prim)
    (primitive ("print")
               print-prim)
    (primitive ("list")
               list-prim)
    (primitive ("cons")
               cons-prim)
    (primitive ("car")
               car-prim)
    (primitive ("cdr")
               cdr-prim)
    (primitive ("equal?")
	       equal?-prim)
    (primitive ("zero?")
	       zero?-prim)
    (primitive ("greater?")
	       greater?-prim)
    (primitive ("less?")
	       less?-prim)
    ))

(define scan&parse
  (sllgen:make-string-parser
   scanner-spec-3-1
   grammar-3-3c))

(sllgen:make-define-datatypes scanner-spec-3-1 grammar-3-3c)



;;3.13
(define grammar-3-3d
  '((program
     (expression)
     a-program)
    (expression
     (number)
     lit-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     (primitive "(" (separated-list expression ",") ")" )
     primapp-exp)
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    (expression
     ("cond" (arbno expression "==>" expression) "end")
     cond-exp)
    (primitive ("+")
               add-prim)
    (primitive ("*")
               mult-prim)
    (primitive ("-")
               subtract-prim)
    (primitive ("add1")
               incr-prim)
    (primitive ("sub1")
               decr-prim)
    (primitive ("minus")
               minus-prim)
    (primitive ("print")
               print-prim)
    (primitive ("list")
               list-prim)
    (primitive ("cons")
               cons-prim)
    (primitive ("car")
               car-prim)
    (primitive ("cdr")
               cdr-prim)
    (primitive ("equal?")
	       equal?-prim)
    (primitive ("zero?")
	       zero?-prim)
    (primitive ("greater?")
	       greater?-prim)
    (primitive ("less?")
	       less?-prim)
    ))

(define scan&parse
  (sllgen:make-string-parser
   scanner-spec-3-1
   grammar-3-3d))

(sllgen:make-define-datatypes scanner-spec-3-1 grammar-3-3d)
    
(define eval-expression
  (lambda (exp env)
    (cases expression exp
           (lit-exp (datum) datum)
           (var-exp (id) (apply-env env id))
	   (if-exp (test-exp true-exp false-exp)
		   (if (true-value? (eval-expression test-exp env))
		       (eval-expression true-exp env)
		       (eval-expression false-exp env)))
	   (cond-exp (test-exps conseq-exps)
		     (if (null? test-exps)
			 0
			 (if (true-value? (eval-expression (car test-exps)))
			     (eval-expression (car conseq-exps))
			     (eval-expression (cond-exp (cdr test-exps) (cdr conseq-exps))))))
           (primapp-exp (prim rands)
                        (let ((args (eval-rands rands env)))
                          (apply-primitive prim args))))))



;;3.14
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
           (add-prim () (+ (car args) (cadr args)))
           (subtract-prim () (- (car args) (cadr args)))
           (mult-prim () (* (car args) (cadr args)))
           (incr-prim () (+ (car args) 1))
           (decr-prim () (- (car args) 1))
	   (equal?-prim () (if (= (car args) (cadr args)) #t #f))
	   (zero?-prim () (if (zero? (car args)) #t #f))
	   (greater?-prim () (if (> (car args) (cadr args)) #t #f))
	   (less?-prim () (if (< (car args) (cadr args)) #t #f))
	   (null?-prim () (if (null? (car args)) #t #f))
           (list-prim () args)
           (cons-prim () (cons (car args) (cadr args)))
           (car-prim () (caar args))
           (cdr-prim () (cdar args))
           )))



(define eval-expression
  (lambda (exp env)
    (cases expression exp
           (lit-exp (datum) datum)
           (var-exp (id) (apply-env env id))
	   (if-exp (test-exp true-exp false-exp)
		   (if (true-value? (eval-expression test-exp env))
		       (eval-expression true-exp env)
		       (eval-expression false-exp env)))
	   (cond-exp (test-exps conseq-exps)
		     (if (null? test-exps)
			 0
			 (if (true-value? (eval-expression (car test-exps)))
			     (eval-expression (car conseq-exps))
			     (eval-expression (cond-exp (cdr test-exps) (cdr conseq-exps))))))
           (primapp-exp (prim rands)
                        (let ((args (eval-rands rands env)))
                          (apply-primitive prim args))))))

(define true-value?
  (lambda (x)
    (if (boolean? x)
	x
	(eopl:error 'true-value?
		    "Test expression does not return a boolean"))))



;;3.15
					;boolean exprresions return 1 for true and 0 for false
					;<bool-exp> ::= equal? ({<expression>})*(,)
					;<bool-exp> ::= greater? ({<expression>})*(,)
					;<bool-exp> ::= less? ({<expression>})*(,)
					;<bool-exp> ::= zero? ({<expression>})
					;
					;These predicates end up being boolean-exprresions are no longer primitive-app expressions
				       

(define eval-bool-exp
  (lambda (exp env)
    (cases boolean-expression exp
	   (equal?-exp (rands)
		       (if (equal? (eval-expression (car rands) env) (eval-expression (cadr rands) env))
			   1
			   0))
	   (greater?-exp (rands)
			 (if (> (eval-expression (car rands) env) (eval-expression (cadr rands) env))
			     1
			     0))
	   (less?-exp (rands)
		      (if (< (eval-expression (car rands) env) (eval-expression (cadr rands) env))
			  1
			  0))
	   (zero?-exp (rands)
		      (if (zero? (eval-expression (car rands) env))
			  1
			  0))
	   (else 'eval-bool-exp
		 "Expreesion is not a boolean-expression ~s"
		 exp))))



