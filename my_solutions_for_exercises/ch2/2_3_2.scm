;;2.15

(define empty-stack
  (lambda ()
    (let ((s '()))
      (lambda () s))))

(define push
  (lambda (stack e)
    (lambda ()
      (cons e (stack)))))
      
      


(define pop
  (lambda (stack)
    (if (empty-stack? stack)
	stack
	(lambda ()
	  (cdr (stack))))))

(define top
  (lambda (stack)
    (if (empty-stack? stack)
	(eopl:error 'top
		    "Attempt to retrieve top element from an empty stack ~s")
	(car (stack)))))


(define empty-stack?
  (lambda (stack)
    (if (null? (stack))
	#t
	#f)))



;;2.16
(define list-find-position-last
  (lambda (sym los)
    (list-index-last (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index-last
  (lambda (pred lst)
    (cond ((null? lst) #f)
	  ((number? (list-index-last pred (cdr lst))) (+ 1 (list-index-last pred (cdr lst))))
	  ((pred (car lst)) 0)
	  (else #f))))


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

;;list-find-position can be used whenever the sym is not repeated.



;;2.17
(define empty-env
  (lambda ()
    (lambda (sym message)
      (cond ((eqv? message 'apply) (eopl:error 'apply-env "No binding for ~s" sym))
	    ((eqv? message 'look) (eopl:error 'has-association "No binding for ~s" sym))))))

(define extend-env
  (lambda (syms vals env)
    (lambda (sym message)
      (let ((pos (list-find-position sym syms)))
	(cond ((eqv? message 'apply) (if (number? pos)
					 (list-ref vals pos)
					 (apply-env env sym)))
	      ((eqv? message 'look)  (if (number? pos)
					 #t
					 (has-association? env sym))))))))

(define apply-env
  (lambda (env sym)
    (env sym 'apply)))

	     
(define has-association?
  (lambda (env sym)
    (env sym 'look)))

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




