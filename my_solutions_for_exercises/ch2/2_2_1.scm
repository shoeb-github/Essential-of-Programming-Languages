;;2.3
(define vector-of
  (lambda (pred)
    (lambda (v)
      (vector-of-partial pred v (vector-length v)))))

(define vector-of-partial
  (lambda (pred v n)
    (if (= 0 n)
	(pred (vector-ref v 0))
	(and (pred (vector-ref v (- n 1)))
	    (vector-of-partial pred v (- n 1))))))


;another way

(define vector-of-partial
  (lambda (pred n)
    (lambda (v)
      (if (= 0 n)
	  (pred (vector-ref v n))
	  (and (pred (vector-ref v (- n 1)))
	       ((vector-of-partial pred (- n 1)) v))))))

(define vector-of
  (lambda (pred)
    (vector-of-partial pred (vector-length v))))


;;2.4
(define-datatype bintree bintree?
  (leaf-node
   (datum number?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))


(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
	   (leaf-node (datum) (list 'leaf-node datum))
	   (interior-node (key left right)
			  (list 'interior-node key (bintree-to-list left) (bintree-to-list right))))))

;;2.5
(define leaf-sum
  (lambda (tree)
    (cases bintree tree
	   (leaf-node (datum) datum)
	   (interior-node (key left right)
			  (+ (leaf-sum left) (leaf-sum right))))))


(define max-interior
  (lambda (tree)
    (cases bintree tree
	   (leaf-node (datum)
		      (eopl:error 'max-interior
				  "Tree has no interior-node"))
	   (interior-node (key left right)
			  (let ((my-sum (leaf-sum tree))
				(left-sum (leaf-sum left))
				(right-sum (leaf-sum right)))
			    (cond ((and (> my-sum left-sum) (> my-sum right-sum))
				   key)
				  ((and (> left-sum my-sum) (> left-sum right-sum))
				   (max-interior left))
				  (else
				   (max-interior right))))))))

