;;  Copyright (C) 2011  
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (ragnarok protocol http http)
  #:use-module (ragnarok protocol http status)
  #:use-module (ragnarok protocol http log)
  #:use-module (ragnarok protocol http response)
  #:use-module (ragnarok log)
  #:use-module (ragnarok utils)
  #:use-module (ragnarok msg)
  #:use-module (ragnarok info)
  #:use-module (web uri)
  #:use-module (web request)
  #:export (http-handler)
  )

(define http-method-handler-get 
  (@ (ragnarok protocol http method) http-method-handler-get)) 
(define init-mime (@ (ragnarok protocol http mime) init-mime))
(define *regular-headers* (@ (ragnarok protocol http header) *regular-headers*))
(define fold (@ (srfi srfi-1) fold))
;; We use guile native http header parser here.
;; Maybe I'll write a new one later, or I should post a patch to guile
;; to support more MIME.

  

;; FIXME: I need to wrap handler template into a macro.
;;        I believe users don't want to write some meta info by themselves.
(define http-handler 
  (lambda (config logger client-connection subserver-info)
    (let* ([conn-socket (car client-connection)]
	   [conn-detail (cdr client-connection)]
	   [request (get-request logger conn-socket)]
	   [remote-host (car (request-host request))]
	   [remote-addr (inet-ntoa 
			 (sockaddr:addr (cdr conn-detail)))]
	   [remote-ident #f] ;; doesn't support
	   [remote-user (request-user-agent request)]
	   [request-method (request-method request)]
	   [query-string 
	    (uri-query (request-uri request))]
	   [auth-type (request-authorization request)]
	   [content-length (request-content-length request)]
	   [content-type (request-content-type request)]
	   [target (uri-path (request-uri rq))]
	   [remote-info 
	    (make-remote-info remote-host remote-addr remote-ident
			      remote-user request-method query-string
			      auth-type content-length content-type
			      target)]
	   [server-info 
	    (make-server-info conn-detail
			      subserver-info
			      request-info)]
	   )
      (http-request-log logger request)
      (http-response config logger server-info conn-socket)
      )))

(define http-response
  (lambda (config logger server-info conn-socket)
    (let* ([client-details (cdr client-connection)]
	   [charset (get-config config 'charset)]
	   [connet-info (server-info:connect-info server-info)]
	   [subserver-info (server-info:subserver-info server-info)]
	   [remote-info (server-info:remote-info server-info)]
	   [method (remote-info:request-method remote-info)]
	   [r-handler (http-method-handler-get method)]
	   )

      (call-with-values
	  (lambda ()
	    (r-handler config logger server-info))
	    ;;(generate-http-response-content logger file))
	(lambda (bv bv-len status type etag mtime)
	  (let* ([reason (or (http-get-reason-from-status status)
			     "Invalid Status")]
		 [mt (strftime "%c" (gmtime mtime))] ;; return to client as GMT.
		 [response (build-response
			    #:version 1.1
			    #:code status
			    #:reason reason
			    #:headers `(,@*regular-headers*
					(last-modified . ,mt)
					(etag . ,etag)
					
					;; NOTE: keep these two lines last!
					(content-length . ,bv-len)
					(content-type . ,type)
					)
			    #:charset charset
			    )]
		 )
	    (write-response response conn-socket)
	    (and bv (write-response-body bv conn-socket))
	    ;;(http-response-log logger status)
	    )))
      )))

(define get-request
  (lambda (logger conn-socket)
    (let* ([request (read-request conn-socket)]

	   ;; FIXME: we should have a more pretty info print...
	   [request-info (fold 
			  (lambda (x y) 
			    (string-append y (format #f "~a : ~a~%" 
						     (object->string (car x))
						     (object->string (cdr x)))))
			  ""
			  (request-headers request))]
	   )
      
      ;; print request information
      (logger:printer logger 
		      (make-log-msg (msg-time-stamp)
				    'request-info 
				    request-info))
      request
      )))


