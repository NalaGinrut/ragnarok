(define-module (ragnarok config)
  #:use-module (ice-9 rdelim)
  #:use-module (ragnarok utils)
  #:export (gen-conf-table)
  )

(define *max-conf-len* 32)
(define *conf-table* (make-hash-table *max-conf-len*))
(define *conf-path* "/etc/regnarok")
(define *core-conf-file* "server.conf")

;; one may add new conf item to this list for verifying.
(define *valid-conf-list*
  `((root-path ,string?)
    (protocol ,symbol?)
    (status-show ,symbol?)
    (max-request ,integer?)
    ))

(define verify-key-val
  (lambda (key val)
    (let ([vp (car (assoc-ref *valid-conf-list* key))])
      (if vp
	  (or (vp val) (error verify-key-val "invalid ~a type!~%" key))
	  (error verify-key-val "invalid key ~a!~%" key))
      )))
	   
(define verify-and-add-key-val
  (lambda (key val)
    (if (verify-key-val key val)
	(hash-set! *conf-table* key val)
	)))

(define gen-conf-table
  (lambda ()
    (let ([conf-list (get-config)])
      (for-each add-to-conf-table conf-list)
      *conf-table*
      )))

(define add-to-conf-table
  (lambda (kv-pair)
    (call-with-values
	(lambda ()
	  (values (car kv-pair)
		  (cdr kv-pair)))
      verify-then-add-key-val)))
      
    
(define verify-conf-list
  (lambda (conf-list)
    (for-each verify-key-val conf-list))) 
     
(define get-conf-from-path
  (lambda (path file)
    (let ([f (string-append path "/" file)])
      (open-input-file f))))

(define get-config
  (lambda ()
    (let ([conf-port (get-conf-from-path
		       *conf-path*
		       *core-conf-file*)]
	  )
      (call/cc
       (lambda (return)
	 (let read-loop ([cl '()])
	   (let ([conf-line (read-line conf-port)])
	     (cond
	      ((eof-object? conf-line)
	       (return cl))  
	      ((string-null? conf-line)
	       (read-loop cl))
	      (else
	       (read-loop (cons (cons
				 (string->symbol (car conf-line))
				 (cdr conf-line))
				cl)))
	      ))))))))
      