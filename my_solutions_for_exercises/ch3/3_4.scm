;;;;;FROM BOOK;;;;;

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
      (letter (arbno (or letter digit "_" "-" "?")))
      symbol)
    (number (digit (arbno digit)) number)))

(define the-grammar
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)   
    (expression
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp)
    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)
    (expression
      ("let" (arbno  identifier "=" expression) "in" expression)
      let-exp)
    (expression
     ("proc" "(" (separated-list identifier ",") ")" expression)
     proc-exp)
    (expression
     ("(" expression (arbno expression) ")")
     app-exp)
    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("zero?") zero-test-prim)
    
    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

(define read-eval-print 
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program pgm))   ; wrapped to avoid load
                                        ; dependency 
    (sllgen:make-stream-parser 
      the-lexical-spec
      the-grammar)))


;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

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
          (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp) ;\new4
        (if (true-value? (eval-expression test-exp env))
          (eval-expression true-exp env)
          (eval-expression false-exp env)))
      (let-exp (ids rands body)  ;\new3
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args env))))
      (proc-exp (ids body) (closure ids body env))
      (app-exp (rator rands)
	       (let ((proc (eval-expression rator env))
		     (args (eval-rands rands env)))
		 (if (procval? proc)
		     (apply-procval proc args)
		     (eopl:error 'eval-expression
				 "Attempt to apply non-procedure ~s" proc))))
      (else (eopl:error 'eval-expression "Not here:~s" exp))
      )))


(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim  () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim  () (* (car args) (cadr args)))
      (incr-prim  () (+ (car args) 1))
      (decr-prim  () (- (car args) 1))
      (zero-test-prim () (if (zero? (car args)) 1 0))
      )))

(define init-env 
  (lambda ()
    (extend-env
      '(i v x)
      '(1 5 10)
      (empty-env))))


;;;;;;;;;;;;;;;; closure ;;;;;;;;;;;;;;;;

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

(define apply-procval
  (lambda (proc args)
    (cases procval proc
	   (closure (ids body env)
		    (eval-expression body (extend-env (ids args env)))))))


;;;;;;;;;;;;;;;; booleans ;;;;;;;;;;;;;;;;

(define true-value?
  (lambda (x)
    (not (zero? x))))


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


(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

(define read-eval-print 
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program pgm))   ; wrapped to avoid load
                                        ; dependency 
    (sllgen:make-stream-parser 
      the-lexical-spec
      the-grammar)))



;;3.19
					;just run the test from the book using interpreter above


;;3.20

(define eval-expression 
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp) ;\new4
        (if (true-value? (eval-expression test-exp env))
          (eval-expression true-exp env)
          (eval-expression false-exp env)))
      (let-exp (ids rands body)  ;\new3
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args env))))
      (proc-exp (ids body) (closure ids body env))
      (app-exp (rator rands)
	       (let ((proc (eval-expression rator env)))
		 (if (procval? proc)		 
		     (cases procval proc
			    (closure (proc-ids proc-body proc-env)
				     (if (not (= (length proc-ids) (length rands)))
					 (eopl:error 'app-exp
						     "Number of formal arguments ~s don't match the number of actual arguments ~s for procedure application ~s"
						     (length proc-ids) (length rands) proc)
					 (let ((args (eval-rands rands env)))
					   (apply-procval proc args)))))
		     (eopl:error 'eval-expression
				 "Attempt to apply non-procedure ~s" proc))))
      (else (eopl:error 'eval-expression "Not here:~s" exp))
      )))




;;3.21
let fac-maker = proc (maker , n)
if n
then *(n, (maker maker -(n,1)))
else 1

in proc (n)
(fac-maker fac-maker n)



;;3.22
let even-maker = proc(maker , n)
if n
then (maker maker -(n,1))
else
1
in
let odd-maker = proc(maker, n)
if -(n,1)
then (maker even-maker -(n,1))
else
1
in proc(n)
(even-maker odd-maker n)

;;similarly for odd



;;3.23

 
