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
  #:use-module (ragnarok protocol http mime)
  #:use-module (ragnarok server)
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

(define uri-path (@ (web uri) uri-path))
(define request-uri (@ (web request) request-uri))
(define get-bytevector-all (@ (rnrs io ports) get-bytevector-all))

(define *status-page-dir* "/etc/ragnarok/stat_html/")

(define http-get-method-handler
  (lambda (method)
    (get-arg *method-handler-list* method)))

(define http-method-POST-handler
  (lambda (server request)
    #t
    ))

(define http-method-DELETE-handler
  (lambda (server request)
    #t
    ))
(define http-method-HEAD-handler
  (lambda (server request)
    #t
    ))
(define http-method-PUT-handler
  (lambda (server request)
    #t
    ))
(define http-method-CONNECT-handler
  (lambda (server request)
    #t
    ))
(define http-method-OPTIONS-handler
  (lambda (server request)
    #t
    ))
(define http-method-TRACE-handler
  (lambda (server request)
    #t
    ))
(define http-method-PATCH-handler
  (lambda (server request)
    #t
    ))

(define http-method-GET-handler
  (lambda (server request)
    (let* ([path (uri-path (request-uri request))]
	   [root-path (server:get-config server 'root-path)]
	   [file (string-append root-path path)]
	   [logger (server:logger server)]
	   
	   ;; FIXME: I need this m-handler later
	   [mime (get-request-mime file)]
	   ;;[m-handler (get-mime-handler mime)]
	   )

    ;; TODO: I need a MIME module, and a mime-list to get MIME
    ;;       handler. But here, I just used a simple dispatch.
    (case mime
      ((html) (http-static-page-serv-handler logger file))
      ((gl) (http-dynamic-page-serv-handler logger file))
      ((*directory*) (http-directory-serv-handler logger file))
      ((*no-such-file*) 
       (values (http-error-page-serv-handler logger 'Not-Found)
	       'Not-Found))
      (else
       ;; unknown mime always return as a static page
       (http-static-page-serv-handler logger file)
       ))
    )))

(define http-directory-serv-handler
  (lambda (logger file)
    #t
    ;; TODO: 
    ;; 1. check the access permission;
    ;; 2. print direcory content as a html; -use ftw
    ;; 3. return (values content status).
    ))
    
(define http-error-page-serv-handler
  (lambda (logger status)
    (let* ([info (http-get-info-from-status status)]
	   [stat-file (http-get-stat-file-from-status status)]
	   [stat-html (string-append *status-page-dir*
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
	      ;;Don't remove this exception handle, in case the file is deleted
	      ;;but it passed the first check
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
	      ;;Don't remove this exception handle, in case the file is deleted
	      ;;but it passed the first check
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


