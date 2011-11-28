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
  #:use-module (ragnarok protocol http error)
  #:use-module (ragnarok protocol http status)
  #:use-module (ragnarok protocol http log)
  #:use-module (ragnarok cgi cgi)
  #:use-module (ragnarok info)
  #:use-module (ragnarok version)
  #:use-module (ragnarok utils)
  #:export (http-directory-serv-handler
	    http-static-page-serv-handler
	    http-regular-cgi-handler
	    )
  )

(define open-pipe (@ (ice-9 popen) open-pipe))
(define close-pipe (@ (ice-9 popen) close-pipe))
(define read-line (@ (ice-9 rdelim) read-line))

(define *no-ETag* #f) ;; only static has ETag
(define *dynamic* #f) ;; dynamic page should be "text/html"
(define *static* #t) ;; static page should be its own mime type

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
	(values bv status *no-ETag *dynamic*))) 
    ;; return fst as #f, then we could deal with dir.
    ))

(define http-regular-cgi-handler
  (lambda (logger filename server-info)
    (let* ([subserver-info (server-info:subserver-info server-info)]
	   [charset (subserver-info:server-charset subserver-info)]
	   )
      (call-with-values
	  (lambda ()
	    (if (file-exists? filename)
		(ragnarok-regular-cgi-handler 
		 (http-make-cgi-type filename server-info)
		 charset)
		;; file doesn't exist ,throw *Not-Found*
		(http-error-page-serv-handler logger *Not-Found* server-info)
		)
	    );; end lambda()
	(lambda (bv status fst)
	  (http-response-log logger status)
	  (values bv status fst *no-ETag* *dynamic*))
	);; end call-with-values
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
	(let* ([mtime (if fst 
			  (stat:mtime fst)
			  (stat:mtime (stat file)))] ;; return dir's mtime
	       [etag (generate-etag bv mtime)]
	       )
	  (values bv status fst etag *static*)))
      ) ;; end call-with-values
    ))
    
;;-------serv handler end-----------------


;;-------method handler-----------------

(define get-static-page
  (lambda (logger target)
    (let* ([fst (stat target)]
	   [ok (check-file-perms target #o444)]
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
	   [ok (check-file-perms dir #o555)]
	   )
      ;; TODO: 
      ;; 1. check the access permission;
      ;; 2. print direcory content as a html; -use ftw
      ;; 3. return content.
  
      (if ok
	  ;; print directory in html
	  ;; FIXME: Guile haven't implement "scandir" yet ,we use pipe here.
	  ;;        Maybe I should submit a patch to guile-dev for this.
	  (let ([dir-list (scandir dir)])
	    (values
	     (call-with-output-string  
	      (lambda (port) 
		(for-each 
		 (lambda (d)
		   (if (file-is-directory? d) 
		       (format port "~a - directory~%" d) 
		       (format port "~a - file~%" d))) ;; end lambda (d)
		 dir-list
		 )))
	      *OK*
	      fst))
	  
	  ;; return Forbidden page
	  (http-error-page-serv-handler logger *Forbidden*)
	  ))))
;;-------method handler end-----------------

(define generate-etag
  (lambda (bv mtime)
    ;; TODO: generate etag, now it just return a null string.
    (format #f "")
    ))

