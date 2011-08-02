(define-module (ragnarok threads)
  #:use-module (ice-9 threads)
  #:export (threads:enqueue
	    threads:new)
  )

;; we use *futures* in Guile to implement thread pool
;; maybe we need a brand new thread pool later... 

(define-syntax threads:enqueue
  (syntax-rules ()
    ((_ proc . args)
     (touch 
      (future (proc . args))))))

(define-syntax threads:new
  (syntax-rules (&)
    ((_ proc . args)
     (make-thread proc . args))
    ((_ & proc . args)
     (threads:enqueue proc . args)
     )))

