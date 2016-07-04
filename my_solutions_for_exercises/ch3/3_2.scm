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


;;repeat solution to ex3.1
(define program-to-list
  (lambda (prg)
    (cases program prg
	   (a-program (exp)
		      (list
		       'a-program
		       (expression-to-list exp))))))

(define expression-to-list
  (lambda (exp)
    (cases expression exp
	   (lit-exp (datum)
		    (list 'lit-exp datum))
	   (var-exp (id)
		    (list 'var-exp id))
	   (primapp-exp (prim rands)
			(let ((p (cases primitive prim
					(add-prim ()
						  '(add-prim))
					(subtract-prim ()
						       '(subtrac-prim))
					(mult-prim ()
						   '(mult-prim))
					(incr-prim ()
						   '(incr-prim))
					(decr-prim ()
						   '(decr-prim)))))
			  (list 'primapp-exp
				p
				(map (lambda (e) (expression-to-list e)) rands)))))))




;;3.3
(define parse-program
  (lambda (datum)
    (a-program (parse-expression datum))))

(define parse-expression
  (lambda (datum)
    (cond ((symbol? datum) (var-exp datum))
	  ((number? datum) (lit-exp datum))
	  ((pair? datum) (let ((rands (map (lambda (x) (parse-expression x)) (cdr datum)))
			       (prim (cond ((eqv? (car datum) '+) (add-prim))
					   ((eqv? (car datum) '-) (subtract-prim))
					   ((eqv? (car datum) '*) (mult-prim))
					   ((eqv? (car datum) 'add1) (incr-prim))
					   ((eqv? (car datum) 'sub1) (decr-prim)))))
			   (primapp-exp prim rands))))))

;from book
(define run
  (lambda (x)
    (eval-program (parse-program x))))

(define read-eval-print
  (lambda ()
    (begin
      (display "---> ")
      (write (eval-program (parse-program (read))))
      (newline)
      (read-eval-print))))




;;3.4
					;test the cases in the book


;;3.5
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
	   (add-prim () (+ (car args) (cadr args)))
	   (subtract-prim () (- (car args) (cadr args)))
	   (mult-prim () (* (car args) (cadr args)))
	   (incr-prim () (+ (car args) 1))
	   (decr-prim () (- (car args) 1))
	   (print-prim () (begin (display (car args))
				 1))
	   )))


(define grammar-3-1b
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
	       decr-prim)
    (primitive ("print")
	       print-prim)))

(define scan&parse_b
  (sllgen:make-string-parser
   scanner-spec-3-1
   grammar-3-1b))

(sllgen:make-define-datatypes scanner-spec-3-1 grammar-3-1b)

(define run-b
  (lambda (string)
    (eval-program
     (scan&parse_b string))))



;;3.5
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
	   (add-prim () (+ (car args) (cadr args)))
	   (subtract-prim () (- (car args) (cadr args)))
	   (mult-prim () (* (car args) (cadr args)))
	   (incr-prim () (+ (car args) 1))
	   (decr-prim () (- (car args) 1))
	   (minus-prim () (* (car args) -1))
	   (print-prim () (begin (display (car args))
				 1))
	   )))


(define grammar-3-1c
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
	       decr-prim)
    (primitive ("minus")
	       minus-prim)
    (primitive ("print")
	       print-prim)))

(define scan&parse_c
  (sllgen:make-string-parser
   scanner-spec-3-1
   grammar-3-1c))

(sllgen:make-define-datatypes scanner-spec-3-1 grammar-3-1c)(define grammar-3-1c
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
	       decr-prim)
    (primitive ("minus")
	       minus-prim)
    (primitive ("print")
	       print-prim)))

(define scan&parse_c
  (sllgen:make-string-parser
   scanner-spec-3-1
   grammar-3-1c))

(sllgen:make-define-datatypes scanner-spec-3-1 grammar-3-1c)

(define run-c
  (lambda (string)
    (eval-program
     (scan&parse_c string))))


;;3.7

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
	   (add-prim () (+ (car args) (cadr args)))
	   (subtract-prim () (- (car args) (cadr args)))
	   (mult-prim () (* (car args) (cadr args)))
	   (incr-prim () (+ (car args) 1))
	   (decr-prim () (- (car args) 1))
	   (minus-prim () (* (car args) -1))
	   (print-prim () (begin (display (car args))
				 1))
	   (list-prim () args)
	   (cons-prim () (cons (car args) (cadr args)))
	   (car-prim () (caar args))
	   (cdr-prim () (cdar args))
	   )))


(define grammar-3-1d
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
    ))

(define scan&parse_d
  (sllgen:make-string-parser
   scanner-spec-3-1
   grammar-3-1d))

(sllgen:make-define-datatypes scanner-spec-3-1 grammar-3-1d)

(define run-d
  (lambda (string)
    (eval-program
     (scan&parse_d string))))


(define init-env
  (lambda ()
    (extend-env
     '(i v x emptylist)
     '(1 5 10 ())
     (empty-env))))


					;expressed values = numbers | emptylist | pairs of expressed values
					;denoted values = numbers | emptylist


;;3.8

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
	   (add-prim () (+ (car args) (cadr args)))
	   (subtract-prim () (- (car args) (cadr args)))
	   (mult-prim () (* (car args) (cadr args)))
	   (incr-prim () (+ (car args) 1))
	   (decr-prim () (- (car args) 1))
	   (minus-prim () (* (car args) -1))
	   (print-prim () (begin (display (car args))
				 1))
	   (list-prim () args)
	   (cons-prim () (cons (car args) (cadr args)))
	   (car-prim () (caar args))
	   (cdr-prim () (cdar args))
	   (setcar-prim () (set-car! (car args) (cadr args)))
	   )))


(define grammar-3-1e
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
    (primitive ("setcar")
	       setcar-prim)
    ))

(define scan&parse_e
  (sllgen:make-string-parser
   scanner-spec-3-1
   grammar-3-1e))

(sllgen:make-define-datatypes scanner-spec-3-1 grammar-3-1e)

(define run-e
  (lambda (string)
    (eval-program
     (scan&parse_e string))))


(define init-env
  (lambda ()
    (extend-env
     '(i v x emptylist)
     '(1 5 10 ())
     (empty-env))))


					;expressed values = numbers | emptylist | pairs of expressed values | value returned by set-car! of scheme (or we can make it
					; a number by using begin as in the print primitive
					;denoted values = numbers | emptylist





;;3.9
(define grammar-3-1e
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
    (primitive ("setcar")
	       setcar-prim)
    ))



;;interpreter
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
			(cond ((and (or (add-prim? prim)
					(mult-prim? prim)
					(subtract-prim? prim)
					(cons-prim? prim)
					(setcar-prim? prim))
				    (or (null? (cdr rands))
					(not (null? (cddr rands)))))
			       (eopl:error 'eval-expression
					   "Wroung number of arguments for prim ~s. Must provide two arguments" prim))
			      ((and (or (incr-prim? prim)
					(decr-prim? prim)
					(minus-prim? prim)
					(print-prim? prim)
					(car-prim? prim)
					(cdr-prim? prim))
				    (not (null? (cddr rands))))
			       (eopl:error 'eval-expression
					   "Wroung number of arguments for prim ~s. Must provide one argument" prim))
			      (else
			       (let ((args (eval-rands rands env)))
				 (apply-primitive prim args))))))))


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
	   (minus-prim () (* (car args) -1))
	   (print-prim () (begin (display (car args))
				 1))
	   (list-prim () args)
	   (cons-prim () (cons (car args) (cadr args)))
	   (car-prim () (caar args))
	   (cdr-prim () (cdar args))
	   (setcar-prim () (set-car! (car args) (cadr args)))
	   )))


(define add-prim?
  (lambda (prim)
    (cases primitive prim
	   (add-prim ()
		     #t)
	   (else #f))))

(define mult-prim?
  (lambda (prim)
    (cases primitive prim
	   (mult-prim ()
		     #t)
	   (else #f))))

(define subtract-prim?
  (lambda (prim)
    (cases primitive prim
	   (subtract-prim ()
		     #t)
	   (else #f))))

(define incr-prim?
  (lambda (prim)
    (cases primitive prim
	   (incr-prim ()
		     #t)
	   (else #f))))

(define decr-prim?
  (lambda (prim)
    (cases primitive prim
	   (decr?-prim ()
		     #t)
	   (else #f))))

(define cons-prim?
  (lambda (prim)
    (cases primitive prim
	   (cons-prim ()
		     #t)
	   (else #f))))

(define cdr-prim?
  (lambda (prim)
    (cases primitive prim
	   (cdr-prim ()
		     #t)
	   (else #f))))

(define car-prim?
  (lambda (prim)
    (cases primitive prim
	   (car-prim ()
		     #t)
	   (else #f))))


(define minus-prim?
  (lambda (prim)
    (cases primitive prim
	   (minus-prim ()
		     #t)
	   (else #f))))

(define list-prim?
  (lambda (prim)
    (cases primitive prim
	   (list-prim ()
		     #t)
	   (else #f))))

(define setcar-prim?
  (lambda (prim)
    (cases primitive prim
	   (setcar-prim ()
		     #t)
	   (else #f))))

(define init-env
  (lambda ()
    (extend-env
     '(i v x emptylist)
     '(1 5 10 ())
     (empty-env))))
