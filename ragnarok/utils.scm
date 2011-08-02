(define-module (ragnarok utils)
  #:export (make-counter
	    make-cursor-for
	    exchange)
  )

(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test) (begin result1 result2 ...)))))

(define make-iterator
  (lambda (ll)
    (let ((rest ll))
      (lambda ()
	(let ((now (car rest)))
	  (set! rest (cdr rest))
	  now
	  )))))

(define make-counter
  (lambda ()
    (let ((old 0))
      (lambda op
	(if (null? op)
	    (set! old (1+ old))
	    (case (car op)
	      ((init) (set! old 0))
	      ((now) old)
	      (else
	       (error make-counter "invalid op ~a!~%" (car op))
	       )))))))

(define-syntax exchange
  (syntax-rules ()
    ((_ a b)
     (call-with-values 
	 (lambda () (values b a)) 
       (lambda (x y) (set! a x) (set! b y))
       ))))
