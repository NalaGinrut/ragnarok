
(define-module (ragnarok server)
  #:use-module (oop goops)
  #:use-module (
  )


(define-class <server> ()
  (socket #:init-value #f #:accessor server:socket)
  (config #:init-value #f #:accessor server:config)
  (protocol #:init-value 'http #:accessor server:protocol)
  (handler #:init-value http-request-handler 
	   #:accessor server:handler)
  
  )

(define-method (initialize (self <server>) initargs)
  (let* ([config (make-hash-table max-conf-len)]
	 
	 )
    ;; TODO: get config from a config file. 
    ;; we defined config handler in a new module.
    

(define-method (server:get-config (self <server>) var)
  (let ((conf (server:config self)))
    ;; FIXME: I need an exeption catch
    (hash-ref conf var)))

(define-method (server:run (self <server>))
  (let* ([s (server:socket self)]
	 [root-path (server:get-config self 'root-path)]
	 [proto (server:get-config self 'protocol)]
	 [request-handler (server:get-handler self proto)]
	 )
    ;; FIXME: don't get config from config list each time
    ;; I need a record to get each config we need.
    (let active-loop ()
      (let* ([client-connection (accept s)]
	     [client-details (cdr client-connection)]
	     [client (car client-connection)]
	     )
	;; FIXME: checkout the validity
	(server:print-status self 'client-info client-details)
	(request-handler client-connection)
	(close client))
      (active-loop)
      )))

;; for more generilzation, we pass info as non-type arg
;; and one may handle it with a custom printer
(define-method (server:print-status 
		(self <server>) (type <symbol>) info)
  (let* ([status-show (server:get-config self 'status-show)]
	 [

(define-method (server:listen-port (self <server>) port)
  (let* ([s (socket PF_INET SOCK_STREAM 0)]
	 [max-req (server:get-config self 'max-request)]
	 )
    (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
    (bind s AF_INET INADDR_ANY port)
    (listen s max-req)
    ))