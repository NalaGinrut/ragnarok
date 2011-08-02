(define-module (ragnarok http)
  #:export (http-static-page-req-handler
	    http-request-handler)
  )

(define http-static-page-req-handler
  (lambda (file)
    
    ;; TODO: search static file then return content
    )
  )

(define http-request-handler
  (lambda (client-connection)
    (let* ([client-details (cdr client-connection)]
	   [client (car client-connection)]
	   )
      )))

