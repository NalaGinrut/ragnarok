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
  #:use-module (ragnarok protocol http mime)
  #:use-module (ragnarok server)
  #:use-module (ragnarok log)
  #:use-module (ragnarok utils)
  #:use-module (ragnarok msg)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (http-handler)
  )

(define get-bytevector-all (@ (rnrs io ports) get-bytevector-all))
(define fold (@ (srfi srfi-1) fold))

;; We use guile native http header parser here.
;; Maybe I'll write a new one later, or I should post a patch to guile
;; to support more MIME.


;; FIXME: I need to wrap handler template into a macro.
;;        I believe users don't want to write some meta info by themselves.
(define http-handler 
  (lambda (server conn-socket)
    (let* ([logger (server:logger server)]
	   [root-path (server:get-config server 'root-path)]
	   [request (get-request logger conn-socket)] 
	   )
      (http-request-log logger request)
      (http-response server request conn-socket)
      (close conn-socket)      
      )))

	  
(define http-response
  (lambda (server request conn-socket)
    (let* ([logger (server:logger server)]
	   [path (uri-path (request-uri request))]
	   [root-path (server:get-config server 'root-path)]
	   [file (string-append root-path path)]
	   )

      (call-with-values
	  (lambda ()
	    (generate-http-response-content logger file))
	(lambda (bv status)
	  (let* ([code (http-get-num-from-status status)]
		 [response (build-response
			    #:version '(1 . 1)
			    #:code code
			    #:port conn-socket)]
		 )
	    (write-response response conn-socket)
	    (and bv (write-response-body response bv))
	    ;;(http-response-log logger status)
	    )))
      )))

(define http-response-log
  (lambda (logger status)
    (let ([info (http-get-info-from-status status)])
      (logger:printer logger
		      (make-log-msg (msg-time-stamp)
				    status
				    info))
      )))

(define http-request-log
  (lambda (logger request)
    (let* ([path (uri-path (request-uri request))]
	   [info (format #f "Client request ~a" path)]
	   )
      (logger:printer logger
		      (make-log-msg (msg-time-stamp)
				    'request-info
				    info))
      )))
      

(define generate-http-response-content
  (lambda (logger file)
    (let ([mime (get-request-mime file)])

    ;; TODO: I need a MIME module, and a mime-list to get MIME
    ;;       handler. But here, I just used a simple dispatch.
    (case mime
      ((html) (http-static-page-serv-handler logger file))
      ((gl) (http-dynamic-page-serv-handler logger file))
      (else
       ;; unknown mime always return as a static page
       (http-static-page-serv-handler logger file)
       ))
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

(define http-error-page-serv-handler
  (lambda (logger status)
    (let* ([info (http-get-info-from-status status)]
	   [stat-file (http-get-stat-file-from-status status)]
	   [stat-html (string-append "/etc/ragnarok/stat_html/"
				     stat-file)]
	   [err-bv (get-bytevector-all
		    (open-input-file stat-html))]
	   )
      err-bv
      )))
      
(define http-static-bin-serv-handler
  (lambda (logger filename)
    (call-with-values
	(lambda ()
	  (if (file-exists? filename)
	      (values (get-bytevector-all 
		       (open-file filename "r"))
		      'OK)
	      (values #vu8(0)
		      'Not-Found)
	      ))
      (lambda (bv status)
	(http-response-log logger status)
	(values bv status)))
    ))
    
(define http-static-page-serv-handler
  (lambda (logger filename)
    (call-with-values
	(lambda ()
	  (if (file-exists? filename)
	      (values (get-bytevector-all 
		       (open-file filename "r"))
		      'OK)
	      (values (http-error-page-serv-handler logger 'Not-Found)
		      'Not-Found)
	      ))
      (lambda (bv status)
	(http-response-log logger status)
	(values bv status)))
    ))

(define http-dynamic-page-serv-handler
  (lambda (logger filename)
    #t
    ;; TODO: search file and call templete handler to render cgi script
    ))

