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
  #:use-module (ragnarok utils)
  )

(module-export-all! (current-module))

(define open-pipe (@ (ice-9 popen) open-pipe))
(define close-pipe (@ (ice-9 popen) close-pipe))
(define read-line (@ (ice-9 rdelim) read-line))
(define get-bytevector-all (@ (rnrs io ports) get-bytevector-all))
(define string->utf8 (@ (rnrs bytevectors) string->utf8))

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
     
(define http-error-page-serv-handler
  (lambda (logger status)
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
  (lambda (logger filename)
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
  #t
  ;;(make-serv-handler logger filename get-dynamic-page)
  ;; TODO: search file and call templete handler to render cgi script
  )
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

