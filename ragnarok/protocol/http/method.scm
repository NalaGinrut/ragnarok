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
	     make-serv-handler
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
(define bytevector-lengh (@ (rnrs io ports) bytevector-lengh))

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

;; returned as GET but without content
(define http-method-HEAD-handler
  (lambda (server request)
    (call-with-values
	(lambda ()
	  (http-method-GET-handler server request))
      (lambda (bv bv-len status type)
	(values #vu8(0) bv-len status type)))
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

      ;; (m-handler logger file) 
      ;; TODO: I need a MIME module, and a mime-list to get MIME
      ;;       handler. But here, I just used a simple dispatch.
      (call-with-values
	  (lambda ()
	    (case mime
	      ((html) (http-static-page-serv-handler logger file))
	      ((gl) (http-dynamic-page-serv-handler logger file))
	      ((*directory*) (http-directory-serv-handler logger file))
	      ((*no-such-file*) 
	       (values (http-error-page-serv-handler logger 'Not-Found)
		       'Not-Found))
	      (else
	       ;; unknown mime always be returned as a static page
	       (http-static-page-serv-handler logger file)
	       )))
	(lambda (bv status)
	  (let* ([type (get-type-from-mime mime)]
		 [bv-len (bytevector-lengh bv)]
		 )
	    (values bv bv-len status type))))
      )))

(define get-directory-in-html
  (lambda (logger dir)
    (let* ([perms (stat:perms (stat dir))]
	   [ok (check-stat-perms perms '(u+r g+r o+r))]
		   )
      ;; TODO: 
      ;; 1. check the access permission;
      ;; 2. print direcory content as a html; -use ftw
      ;; 3. return content.
  
      (if ok
	  ;; print directory in html
	  ;; FIXME: Guile haven't implement "scandir" yet ,we use pipe here.
	  ;;        Maybe I should submit a patch to guile-dev for this.
	  (let* ([cmd (string-append "ls -a " dir)] 
		 [dpipe (open-pipe cmd OPEN_READ)]
		 )
	    (call-with-output-string  
	     (lambda (port) 
	       (let lp ((d (read-line dpipe))) 
		 (if (not (eof-object? d)) 
		     (let ([f (string-append "tmp/" d)]) 
		       (if (file-is-directory? f) 
			   (format port "~a - directory~%" f) 
			   (format port "~a - file~%" f)) 
		       (lp (readdir dir))))))))
	  
	  ;; return Forbidden page
	  (http-error-page-serv-handler logger 'Forbidden)
	  ))))

(define http-directory-serv-handler
  (lambda (logger dir)
    (call-with-values
	(lambda ()
	  (if (file-exists? dir)
	      (values (get-directory-in-html logger dir)
		      'OK)
	      (values (http-error-page-serv-handler logger 'Not-Found)
		      'Not-Found)
	      ))
      (lambda (bv status)
	(http-response-log logger status)
	(values bv status)))
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
  (lambda (logger target)
    ((make-serv-handler logger target) get-static-page)
    ))

(define http-dynamic-page-serv-handler
  #t
  ;;(make-serv-handler logger filename get-dynamic-page)
  ;; TODO: search file and call templete handler to render cgi script
  )


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


(define make-serv-handler
  (lambda (logger target)
     (lambda (get-content-handler)
       (call-with-values
	   (lambda ()
	     (if (file-exists? target)
		 (values (get-content-handler logger target)
			 'OK)
		 (values (http-error-page-serv-handler logger 'Not-Found)
			 'Not-Found)
		 ;;Don't remove this exception handle, in case the file is deleted
		 ;;but it passed the first check
		 ))
	 (lambda (bv status)
	   (http-response-log logger status)
	   (values bv status))))
       ))

(define get-static-page
  (lambda (logger target)
    (let* ([perms (stat:perms (stat target))]
	   [ok (check-stat-perms perms '(u+r g+r o+r))]
	   )
      
      (if ok
	  (get-bytevector-all 
	   (open-file target "r"))
	  (http-error-page-serv-handler logger 'Forbidden))
     )))
