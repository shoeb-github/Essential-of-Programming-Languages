;;2.14
					;                
					;(empty-stack) = r(empty-list); where r(.) is representation of an entity. This is a constructor.
					;
					;(push r(S) e) = r(S') such that (top r(S')) = e. This is a costructor.
					;
					;(pop r(S')) =  r(S')                                           if r(S') = r(empty-list)
					;            =  r(S) such that (push r(S) (top r(S')) ) = r(S') otherwise
					;               This is a constructor.
					;
					;(top r(S)) = output an error message if r(S) = r(empty-list)
					;           = e such tat (push (pop r(s)) (top r(S))) = r(S)
					;
					;            This is an observer.
					;
					;(empty-stack? r(S)) = true if r(S) == r(empty-list)
					;                    = false otherwise
					;                    This is an observer.
					;
					;
