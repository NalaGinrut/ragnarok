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

(define-module (ragnarok protocol http method)
  #:use-module (ragnarok protocol http log)
  #:use-module (ragnarok protocol http status)
  #:use-module (ragnarok protocol http handler)
  #:use-module (ragnarok protocol http error)
  #:use-module (ragnarok info)
  #:use-module (ragnarok utils)
  #:export ( http-method-handler-get
	     http-method-POST-handler
	     http-method-DELETE-handler
	     http-method-HEAD-handler
	     http-method-PUT-handler
	     http-method-CONNECT-handler
	     http-method-OPTIONS-handler
	     http-method-TRACE-handler
	     http-method-PATCH-handler
	     http-method-GET-handler
	     )
  )

(define get-type-from-mime (@ (ragnarok protocol http mime) get-type-from-mime))
(define get-mime-handler (@ (ragnarok protocol http mime) get-mime-handler))
(define gcrypt:sha1 (@ (ragnarok gcrypt mda) gcrypt:sha1))
(define uri-path (@ (web uri) uri-path))
(define request-uri (@ (web request) request-uri))

(define http-method-handler-get
  (lambda (method)
    (get-arg *method-handler-list* method)))

(define run-with-CGI?
  (lambda (ext config)
    (let ([ext-list (get-config config 'with-cgi)])
      (list-has? ext-list ext)
      )))

(define http-method-POST-handler
  (lambda (config logger server-info)
    #t
    ))

(define http-method-DELETE-handler
  (lambda (config logger server-info)
    #t
    ))

;; returned as GET but without content
(define http-method-HEAD-handler
  (lambda (config logger server-info)
    ;; NOTE: We must read out the content of file, because we need to
    ;;       generate ETAG.
    (call-with-values
	(lambda ()
	  (http-method-GET-handler config logger server-info))
      (lambda (bv bv-len status type etag mtime)
       	(values #f bv-len status type etag mtime)))
    ))

(define http-method-PUT-handler
  (lambda (config logger server-info)
    #t
    ))
(define http-method-CONNECT-handler
  (lambda (config logger server-info)
    #t
    ))
(define http-method-OPTIONS-handler
  (lambda (config logger server-info)
    #t
    ))
(define http-method-TRACE-handler
  (lambda (config logger server-info)
    #t
    ))
(define http-method-PATCH-handler
  (lambda (config logger server-info)
    #t
    ))

(define http-method-GET-handler
  (lambda (config logger server-info)
    (let* ([remote-info (server-info:remote-info server-info)]
	   [target (remote-info:target remote-info)]
	   [root-path (get-config config 'root-path)]
	   [file (string-append root-path target)]
	   [use-cgi (get-config config 'cgi)]
	  	   
	   ;; FIXME: I need this m-handler later
	   [mime (get-request-mime file)]
	   [script-run-with-CGI (run-with-CGI? mime config)]
	   [m-handler (get-mime-handler mime)]
	   )

      ;;(m-handler logger file) 
      ;; TODO: I need a MIME module, and a mime-list to get MIME
      ;;       handler. But here, I just used a simple dispatch.
      (call-with-values
	  (lambda ()
	    (cond
	      ((eqv? mime '*directory*) 
	       (http-directory-serv-handler logger file server-info))
	      ((eqv? mime '*no-such-file*) 
	       (http-error-page-serv-handler logger *Not-Found* server-info))
	      ((not m-handler)
	       (if (and script-run-with-CGI
			use-cgi)
		   (call-with-values
		       (lambda ()
			 (http-regular-cgi-handler logger file server-info))
		     (lambda (bv status fst)
		       (if (not bv)
			   (http-error-page-serv-handler logger
							 status
							 server-info)
			   (values bv status fst))))

		   ;; if no handler and not a CGI allowed file,
		   ;; return it as static page
		   (http-static-page-serv-handler logger 
						  file
						  server-info)))
	      (else
	       ;; deal with files
	       (m-handler logger file server-info)
	       )))
	(lambda (bv status fst etag *not-html*)
	  (let* ([type (or 
			(and *not-html* (get-type-from-mime mime))
			"text/html")]
		 [bv-len (if fst 
			     (stat:size fst)
			     (if bv
				 (bytevector-length bv)
				 #f))] ;; do as dir handle.
		 [mtime (if fst 
			    (stat:mtime fst)
			    (stat:mtime (stat file)))] ;; return dir's mtime
		 )
	    (values bv bv-len status type etag mtime))
	  ))
      )))

(define *method-handler-list*
  `(("GET" ,http-method-GET-handler)
    ("POST" ,http-method-POST-handler)
    ("DELETE" ,http-method-DELETE-handler)
    ("HEAD" ,http-method-HEAD-handler)
    ("PUT" ,http-method-PUT-handler)
    ("CONNECT" ,http-method-CONNECT-handler)
    ("OPTIONS" ,http-method-OPTIONS-handler)
    ("TRACE" ,http-method-TRACE-handler)
    ("PATCH" ,http-method-PATCH-handler)
    ))


