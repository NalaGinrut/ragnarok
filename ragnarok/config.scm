(define-module (ragnarok config)
  #:use-module (ice-9 rdelim)
  #:use-module (ragnarok utils)
  #:export (gen-conf-table
	    print-conf-table
	    *conf-table*)
  )

;; config will generate a global config hash table
;; so we need to start each server following config table
;; TODO: each environment should match one config file.
;;       And each env contains many servers,
;;       but we haven't implement environment yet. 
(define *max-conf-len* 32)
(define *conf-table* (make-hash-table *max-conf-len*))
(define *conf-path* "/etc/ragnarok")
(define *core-conf-file* "server.conf")

(define (print-conf-table)
  (hash-for-each
   (lambda (x y) (format #t "~a : ~a~%" x y))
   *conf-table*))


;; one may add new conf item to this list for verifying.
(define (type:string x) x)
(define (type:symbol x) (string->symbol x))
(define (type:integer x) (string->number x))

(define *valid-conf-list*
  `((root-path ,type:string)
    (protocol ,type:symbol)
    (status-show ,type:symbol)
    (max-request ,type:integer)
    ))

(define verify-key-val
  (lambda (key val)
    (let ([vp (car (assoc-ref *valid-conf-list* key))])
      (if vp
	  (or (vp val) (error verify-key-val "invalid ~a type!~%" key))
	  (error verify-key-val "invalid key ~a!~%" key))
      )))
	   
(define verify-then-add-key-val
  (lambda (key val)
    (let ([vv (verify-key-val key val)])
      (hash-set! *conf-table* key vv)
      )))

(define gen-conf-table
  (lambda ()
    (let ([conf-list (get-config-list)])
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

(define get-config-list
  (lambda ()
    (let ([conf-port (get-conf-from-path
		       *conf-path*
		       *core-conf-file*)]
	  )
      (let read-loop ([cl '()])
	(let ([conf-line (read-line conf-port)])
	  (cond
	   ((eof-object? conf-line)
	    cl)  
	   ((string-null? conf-line)
	    (read-loop cl))
	   (else
	    (let* ([kvl (string-split conf-line #\:)]
		   [key 
		    (string->symbol 
		     (string-trim-both (car kvl)))]
		   [val (string-trim-both (cadr kvl))]
		   )
	      (read-loop (cons (cons key val) cl))
	      ))
	   ))))))
      