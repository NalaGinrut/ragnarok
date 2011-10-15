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

(define-module (ragnarok protocol http handler)
  #:use-module (ragnarok protocol http status)
  #:use-module (ragnarok protocol http log)
  #:use-module (ragnarok cgi cgi)
  #:use-module (ragnarok info)
  #:use-module (ragnarok version)
  #:use-module (ragnarok utils)
  )

(module-export-all! (current-module))

(define open-pipe (@ (ice-9 popen) open-pipe))
(define close-pipe (@ (ice-9 popen) close-pipe))
(define read-line (@ (ice-9 rdelim) read-line))

;; NOTE: each serv-handler returns 3 values, bv&status&file-stat
;;-------serv handler-----------------
(define http-directory-serv-handler
  (lambda (logger dir)
    (call-with-values
	(lambda ()
	  (if (file-exists? dir)
	      (values (string->utf8 (get-directory-in-html logger dir))
		      *OK*)
	      (values (http-error-page-serv-handler logger *Not-Found*)
		      *Not-Found*)
	      ))
      (lambda (bv status)
	(http-response-log logger status)
	(values bv status #f))) ;; return fst as #f, then we could deal with dir.
    ))
    
(define http-regular-cgi-handler
  (lambda (logger filename server-info)
    (call-with-values
	(lambda ()
	  (if (file-exists? filename)
	      (let* ([conn-socket (server-info:connect-socket server-info)]
		     [remote-info (server-info:remote-info server-info)]
		     [auth-type (remote-info:auth-type remote-info)]
		     [content-length (remote-info:content-length remote-info)]
		     [content-type (remote-info:content-type remote-info)]
		     ;;[gateway-interface #f]
		     ;;[path-info #f]
		     ;;[path-translated #f]
		     [query-string (remote-info:query-string remote-info)]
		     [remote-addr (remote-info:remote-addr remote-info)]
		     [remote-host (remote-info:remote-host remote-info)]
		     ;;[remote-ident #f]
		     [remote-user (remote-info:remote-user remote-info)]
		     [request-method (remote-info:request-method remote-info)]
		     ;;[script-name #f]
		     [subserver-info (server-info:subserver-info server-info)]
		     [server-name (subserver-info:server-name subserver-info)]
		     [server-port (subserver-info:server-port subserver-info)]
		     [server-protocol 
		      (subserver-info:server-protocol subserver-info)]
		     [server-software 
		      (subserver-info:server-software subserver-info)] 
		     [env-table (make-hash-table 17)]
		     )

		;; init CGI env vars
		(cgi:auth-type! env-table auth-type)
		(cgi:content-type! env-table content-type)
		(cgi:content-length! env-table content-length)
		;;(cgi:gateway-interface! env-table gateway-interface)
		;;(cgi:path-info! env-table path-info)
		;;(cgi:path-translated! env-table path-translated)
		(cgi:query-string! env-table query-string)
		(cgi:remote-addr! env-table remote-addr)
		(cgi:remote-host! env-table remote-host)
		;;(cgi:remote-ident! env-table remote-ident)
		(cgi:remote-user! env-table remote-user)
		(cgi:request-method! env-table request-method)
		;;(cgi:script-name! env-table script-name)
		(cgi:server-name! env-table server-name)
		(cgi:server-port! env-table server-port)
		(cgi:server-protocol! env-table server-protocol)
		(cgi:server-software! env-table server-software)

		(ragnarok-regular-cgi-handler
		 (make-cgi-record filename env-table conn-socket))
		);; end let*
	      ;; file doesn't exist ,throw *Not-Found*
	      (http-error-page-serv-handler logger *Not-Found* server-info)
	      );; end if
	  );; end lambda()
      (lambda (bv status fst)
	(http-response-log logger status)
	(values bv status fst))
      );; end call-with-values
    ))

(define http-error-page-serv-handler
  (lambda (logger status server-info)
    (let* ([stat-file (http-get-stat-file-from-status status)]
	   [stat-html (string-append *status-page-dir*
				     stat-file)]
	   [err-bv (get-bytevector-all
		    (open-input-file stat-html))]
	   [fst (stat stat-html)]
	   )
      (values err-bv status fst)
      )))
      
(define http-static-page-serv-handler
  (lambda (logger filename server-info)
    (call-with-values
	(lambda ()
	  (if (file-exists? filename)
	      (get-static-page logger filename)
	      (http-error-page-serv-handler logger *Not-Found*)
	      ;;Don't remove this exception handle, in case the file is deleted
	      ;;but it passed the first check
	      ))
      (lambda (bv status fst)
	(http-response-log logger status)
	(values bv status fst)))
    ))
    
(define http-dynamic-page-serv-handler
  (lambda (logger filename server-info)
    (call-with-values
	(lambda ()
	  (cond
	   ((not(file-exists? filename))
	    (http-error-page-serv-handler logger *Not-Found*))
	   ((file-is-exec-script? filename)
	    (get-dynamic-page logger filename)
	    ;;Don't remove this exception handle, in case the file is deleted
	    ;;but it passed the first check
	    ))
      (lambda (bv status fst)
	(http-response-log logger status)
	(values bv status fst)))

  ;;(make-serv-handler logger filename get-dynamic-page)
  ;; TODO: search file and call templete handler to render cgi script
  )))
;;-------serv handler end-----------------


;;-------method handler-----------------


(define get-static-page
  (lambda (logger target)
    (let* ([fst (stat target)]
	   [perms (stat:perms fst)]
	   [ok (check-stat-perms perms '(u+r g+r o+r))]
	   )
      
      (if ok
	  (values (get-bytevector-all 
		   (open-file target "r"))
		  *OK*
		  fst)
	  (http-error-page-serv-handler logger *Forbidden*))
     )))

(define get-directory-in-html
  (lambda (logger dir)
    (let* ([fst (stat dir)]
	   [perms (stat:perms fst)]
	   [ok (logand perms #o555)]
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
	    (values
	     (call-with-output-string  
	      (lambda (port) 
		(let lp ((d (read-line dpipe))) 
		  (if (not (eof-object? d)) 
		      (if (file-is-directory? d) 
			  (format port "~a - directory~%" d) 
			  (format port "~a - file~%" d)) 
		      (lp (readdir dir)))
		  (close-pipe dpipe)
		     )))
	     *OK*
	     fst))
	  
	  ;; return Forbidden page
	  (http-error-page-serv-handler logger *Forbidden*)
	  ))))
;;-------method handler end-----------------

