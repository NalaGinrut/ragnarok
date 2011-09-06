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
  #:use-module (ragnarok utils)
  #:export ( http-get-method-handler
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

(define get-mime-handler (@ (ragnarok protocol http mime) get-mime-handler))
(define get-type-from-mime (@ (ragnarok protocol http mime) get-type-from-mime))
(define gcrypt:sha1 (@ (gcrypt mda) gcrypt:sha1))
(define uri-path (@ (web uri) uri-path))
(define request-uri (@ (web request) request-uri))
(define bytevector-length (@ (rnrs bytevectors) bytevector-length))

(define http-get-method-handler
  (lambda (method)
    (get-arg *method-handler-list* method)))

(define http-method-POST-handler
  (lambda (config logger request)
    #t
    ))

(define http-method-DELETE-handler
  (lambda (config logger request)
    #t
    ))

;; returned as GET but without content
(define http-method-HEAD-handler
  (lambda (config logger request)
    ;; NOTE: We must read out the content of file, because we need to
    ;;       generate ETAG.
    (call-with-values
	(lambda ()
	  (http-method-GET-handler config logger request))
      (lambda (bv bv-len status type etag mtime)
       	(values #f bv-len status type etag mtime)))
    ))

(define http-method-PUT-handler
  (lambda (config logger request)
    #t
    ))
(define http-method-CONNECT-handler
  (lambda (config logger request)
    #t
    ))
(define http-method-OPTIONS-handler
  (lambda (config logger request)
    #t
    ))
(define http-method-TRACE-handler
  (lambda (config logger request)
    #t
    ))
(define http-method-PATCH-handler
  (lambda (config logger request)
    #t
    ))

(define http-method-GET-handler
  (lambda (config logger request)
    (let* ([path (uri-path (request-uri request))]
	   [root-path (get-config config 'root-path)]
	   [file (string-append root-path path)]
	   	   
	   ;; FIXME: I need this m-handler later
	   [mime (get-request-mime file)]
	   [m-handler (get-mime-handler mime)]
	   )

      ;;(m-handler logger file) 
      ;; TODO: I need a MIME module, and a mime-list to get MIME
      ;;       handler. But here, I just used a simple dispatch.
      (call-with-values
	  (lambda ()
	    (case mime
	      ((*directory*) 
	       (http-directory-serv-handler logger file))
	      ((*no-such-file*) 
	       (http-error-page-serv-handler logger *Not-Found*))
	      (else
	       ;; deal with files
	       (m-handler logger file)
	       )))
	(lambda (bv status fst)
	  (let* ([type (get-type-from-mime mime)]
		 [bv-len (if fst 
			     (stat:size fst)
			     (bytevector-length bv))] ;; do as dir handle.
		 [mtime (if fst 
			    (stat:mtime fst)
			    (stat:mtime (stat file)))] ;; return dir's mtime
		 [etag (generate-etag bv mtime)]
		 )
	    (values bv bv-len status type etag mtime))
	  ))
      )))

(define *method-handler-list*
  `((GET ,http-method-GET-handler)
    (POST ,http-method-POST-handler)
    (DELETE ,http-method-DELETE-handler)
    (HEAD ,http-method-HEAD-handler)
    (PUT ,http-method-PUT-handler)
    (CONNECT ,http-method-CONNECT-handler)
    (OPTIONS ,http-method-OPTIONS-handler)
    (TRACE ,http-method-TRACE-handler)
    (PATCH ,http-method-PATCH-handler)
    ))

(define generate-etag
  (lambda (bv mtime)
    ;; TODO: generate etag, now it just return a null string.
    ""
    ))

