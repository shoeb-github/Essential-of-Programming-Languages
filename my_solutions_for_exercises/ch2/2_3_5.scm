;;2.26

(define-datatype reference reference?
  (a-ref
   (position integer?)
   (vec vector?)))

(define cell?
  (lambda (c)
    (reference? c)))

(define cell
  (lambda (datum)
    (a-ref 0 (make-vector 1 datum))))

(define contents
  (lambda (c)
    (cases reference c
	   (a-ref (position vec)
		  (vector-ref position vec)))))

(define setcell
  (lambda (c datum)
    (cases reference c
	   (a-ref (position vec)
		  (vector-set! vec position datum)))))


(define create-cell
  (lambda ()
    (let ((init-cell '()))
      (let
	  ((cell?
	    (lambda ()
	      (reference? init-cell)))
	   (cell
	    (lambda (datum)
	      (if (reference? init-cell)
		  (cases reference init-cell
			 (a-ref (position vec)
				(vector-set! vec position datum)))
		  (set! init-cell (a-ref 0 (make-vector 1 datum))))))
	   (contents
	    (lambda ()
	      (if (reference? init-cell)
		  (cases reference init-cell
			 (a-ref (position vec)
				(vecto-ref position vec)))
		  (eopl:error 'contents
			      "Attempt to read contents from un-initialized cell")))))
	(vector cell? cell constents)))))

(define get-cell?-operation
  (lambda (c)
    (vector-ref 0 c)))

(define get-cell-operation
  (lambda (c)
    (vector-ref 1 c)))

(define get-contents-operation
  (lambda (c)
    (vector-ref 2 c)))

(define get-setcell-operation get-cell-operation)
