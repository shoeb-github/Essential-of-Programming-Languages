;;;Exercise 1.15

;;1
(define duple
  (lambda (n x)
    (if (= n 0) 
	'()
	(cons x (duple (- n 1) x)))))

;;2
(define invert
  (lambda (lst)
    (if (null? lst)
	'()
	(cons (invert-two-list (car lst)) (invert (cdr lst))))))

(define invert-two-list
  (lambda (two-lst)
    (lst (cadr two-lst) (car two-lst))))

;;3
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
	'()
	(if (pred (car lst))
	    (cons (car lst) (filter-in pred (cdr lst)))
	    (filter-in pred (cdr lst))))))

;;4
(define every?
  (lambda (pred lst)
    (if (null? lst)
	#t
	(and (pred (car lst))
	     (every? pred (cdr lst))))))


;;5
(define exists?
  (lambda (pred lst)
    (if (null? lst)
	#f
	(or (pred (car lst))
	    (exists? pred (cdr lst))))))

;;6
(define vector-index-from-n
  (lambda (pred v n)
    (let ((len (vector-length v)))
      (if (> n len)
	  #f
	  (if (pred (vector-ref v n))
	      n
	      (vector-index-from-n pred v (+ n 1)))))))

(define vector-index 
  (lambda (pred v)
    (vector-index-from-n pred v 0)))

;;7
(define list-set
  (lambda (lst n x)
    (if (null? lst)
	'()
	(if (= n 0)
	    (cons x (cdr lst))
	    (cons (car lst) (list-set (cdr lst) (- n 1) x))))))

;;8
(define accumulate
    (lambda (op init lst)
      (if (null? lst)
          init
          (op (car lst) (accumulate op init (cdr lst))))))



(define product
  (lambda (los1 los2)
    (accumulate append '()
		(map (lambda (s2)
		       (map (lambda (s1)
			      (list s1 s2))
			    los1))
		     los2))))

;;9
(define down
  (lambda (lst)
    (if (null? lst)
	'()
	(cons (list (car lst))
	      (down (cdr lst))))))


;;10
(define vector-append-list
  (lambda (v lst)
    (let ((lst-len (length lst))
	  (v-len (vector-length v)))
      (if (= lst-len 0)
	  (vector-copy v)
	  (let ((new-v (extend-vector v lst-len)))
	    (vector-copy-list new-v lst v-len))))))

(define vector-copy-list
  (lambda (v lst ind)
    (if (null? lst)
	v
	(begin
	  (vector-set! v ind (car lst))
	  (vector-copy-list v (cdr lst) (+ 1 ind))))))

(define extend-vector
  (lambda (v len)
    (let ((new-len (+ len (vector-length v))))
      (let ((new-v (make-vector new-len)))
	(begin
	  (vector-2-vector-copy new-v v (- (vector-length v) 1))
	  new-v)))))


(define vector-2-vector-copy
  (lambda (new-v v n)
    (if (= n 0)
	(vector-set! new-v 0 (vector-ref v 0))
	(begin
	  (vector-set! new-v n (vector-ref v n))
	  (vector-2-vector-copy new-v v (- n 1))))))



;;;Exercise 1.16

;;1
(define up
  (lambda (lst)
    (if (null? lst)
	lst
	(cond ((symbol? (car lst)) (cons (car lst) (up (cdr lst))))
	      ((number? (car lst)) (cons (car lst) (up (cdr lst))))
	      ((boolean? (car lst)) (cons (car lst) (up (cdr lst))))
	      ((vector? (car lst)) (cons (car lst) (up (cdr lst))))
	      (else (append (car lst) (up (cdr lst))))))))


;;2
;;easy way is subst all s1 with 's2-swapper symbol. Likewise for s1
;;then subst 's2-swapper with 's2 and likewise for 's1-swapper.
;;But this assumes the tag '-swapper is not already in s-list

(define swapper-se
  (lambda (s1 s2 se)
    (if (symbol? se)
	(if (eq? s1 se)
	    s2
	    (if (eq? s2 se)
		s1
		se))
	(swapper s1 s2 se))))

(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
	'()
	(cons (swapper-se s1 s2 (car slist))
	      (swapper s1 s2 (cdr slist))))))

;;3
(define count-occurences 
  (lambda (s slist)
    (if (null? slist)
	0
	(+ (count-occurences-se s (car slist))
	   (count-occurences s (cdr slist))))))

(define count-occurences-se 
  (lambda (s se)
    (if (symbol? se)
	(if (eqv? s se)
	    1
	    0)
	(count-occurences s se))))

;;4
(define flatten
  (lambda (slist)
    (if (null? slist)
	'()
	(append (flatten-se (car slist))
		(flatten (cdr slist))))))

(define flatten-se
  (lambda (se)
    (if (symbol? se)
	(list se)
	(flatten se))))
;;5
(define merge
  (lambda (lon1 lon2)
    (cond ((null? lon1) lon2)
	  ((null? lon2) lon1)
	  ((< (car lon1) (car lon2)) (cons (car lon1) (merge (cdr lon1) lon2)))
	  ((< (car lon2) (car lon1)) (cons (car lon2) (merge lon1 (cdr lon2))))
	  (else (append (list (car lon1) (car lon2)) (merge (cdr lon1) (cdr lon2)))))))


;;;Exercise 1.17
;;1
(define path
  (lambda (n bst)
    (let ((root-key (car bst)))
      (if (= n root-key)
	  '()
	  (if (< n root-key)
	      (cons 'left (path n (cadr bst)))
	      (cons 'right (path n (caddr bst))))))))


;;2
(define sort
  (lambda (lon)
    (if (null? lon)
	lon
	(merge (list (car lon)) (sort (cdr lon))))))


;;3
(define sort-predicate
  (lambda (predicate lon)
    (if (null? lon)
	lon
	(merge-predicate predicate (list (car lon)) (sort-predicate predicate (cdr lon))))))

(define merge-predicate
  (lambda (predicate lon1 lon2)
    (cond ((null? lon1) lon2)
	  ((null? lon2) lon1)
	  ((predicate (car lon1) (car lon2)) (cons (car lon1) (merge-predicate predicate (cdr lon1) lon2)))
	  ((predicate (car lon2) (car lon1)) (cons (car lon2) (merge-predicate predicate lon1 (cdr lon2))))
	  (else (append (list (car lon1) (car lon2)) (merge-predicate predicate (cdr lon1) (cdr lon2)))))))


;;;Exercise 1.18
;;1
(define compose
  (lambda (p1 p2)
    (lambda (x)
      (p1 (p2 x)))))


;;2
(define car&cdr
  (lambda (s slist errvalue)
    (if (null? slist)
	errvalue
	(let ((exp1 (car&cdr-se s (car slist) errvalue)))
	  (cond ((null? exp1)
		 'car)
		((eq? exp1 errvalue)
		 (let ((exp2 (car&cdr s (cdr slist) errvalue)))
		   (if (eq? exp2 errvalue)
		       errvalue
		       (list 'compose exp2 'cdr))))
		(else (list 'compose exp1 'car)))))))

(define car&cdr-se
  (lambda (s se errvalue)
    (if (symbol? se)
	(if (eq? s se)
	    '()
	    errvalue)
	(car&cdr s se errvalue))))



;;3
;;just need to inline compose
(define car&cdr2
  (lambda (s slist errvalue)
    (if (null? slist)
	errvalue
	(let ((exp1 (car&cdr-se2 s (car slist) errvalue)))
	  (cond ((null? exp1)
		 'car)
		((eq? exp1 errvalue)
		 (let ((exp2 (car&cdr2 s (cdr slist) errvalue)))
		   (if (eq? exp2 errvalue)
		       errvalue
		       (list 'lambda '(slist) (list exp2 '(cdr slist))))))
		(else (list 'lambda '(slist) (list exp1 '(car slist)))))))))






(define car&cdr-se2
  (lambda (s se errvalue)
    (if (symbol? se)
	(if (eq? s se)
	    '()
	    errvalue)
	(car&cdr2 s se errvalue))))

