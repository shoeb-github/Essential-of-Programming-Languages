;;3.1
(define-datatype program program?
  (a-program
   (exp expression?)))

(define-datatype expression expression?
  (lit-exp
   (datum number?))
  (var-exp
   (id symbol?))
  (primapp-exp
   (prim primitive?)
   (rands (list-of expression?)))
  )

(define-datatype primitive primitive?
  (add-prim)
  (subtract-prim)
  (mult-prim)
  (incr-prim)
  (decr-prim))


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
			(let ((p (cases prim primitive
					add-prim ()
					'(add-prim)
					subtract-prim ()
					'(subtrac-prim)
					mult-prim ()
					'(mult-prim)
					incr-prim ()
					'(incr-prim)
					decr-prim ()
					'(decr-prim))))
			  (list primapp-exp
				p
				(map (lambda (e) (expresseion-to-list e)) rands)))))))


;;need to write parse program to test above procedures.
;;parsing is not simple as in chapter2 because the language concrete syntax is not a list structure.
;;



;;3.2
					;
					;The order of evaluation is unspecified. It depends on the particular Scheme implementation.
					;(map ...) eventually calls (cons . (map ..)). There is no specification for order of evaluation in cons.
					;
					;Since expressions in our language have no side effects and state (no assignments), the order of evaluation does not matter.
					;
					;It may be possible to determine order of evaluation empirically.
					;Consider the expression (cons A B). We serialize access to a state for each of A,B.
					;For example, they can update the state with their id. Then watch how the state changes.
					;But this probably needs many trials.
					;

