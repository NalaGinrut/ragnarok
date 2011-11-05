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

(define-module (ragnarok cgi guile)
  #:use-module (ragnarok protocol http status)
  #:use-module (ragnarok protocol http log)
  #:use-module (ragnarok cgi cgi)
  #:use-module (ragnarok cgi guile-fluid)
  #:use-module (ragnarok utils)
  #:export (cgi-guile-serv-handler
	    )
  )


(define *no-ETag* #f)
(define *dynamic* #f)
(define start-sign "<% ")
(define startd-sign "<%= ")
(define end-sign " %>")

(define ss-len (string-length start-sign))
(define sd-len (string-length startd-sign))
(define es-len (string-length end-sign))

(define create-environ-hash-table
  (lambda (envion-list)
    (let ([ht (make-hash-table)])
      (for-each (lambda (x) 
		  (hash-set! ht (car x) (cadr x)))
		(map (lambda (x) 
		       (string-split x #\=))
		     envion-list))
      ht
      )))

(define create-query-table
  (lambda (q-str)
    (if (or (not q-str) 
	    (string-null? q-str))
	(make-hash-table)
	(let* ([q-list (string-split q-str #\&)]
	       [qv-list (map (lambda (x)
			       (string-split x #\=))
			     q-list)]
	       [qv-ht (make-hash-table)]
	       )
	  (for-each (lambda (v)
		      (hash-set! qv-ht (car v) (cadr v)))
		    qv-list)
	  qv-ht)
	)))

(define guile-cgi-render
  (lambda (in-buf)
    (call-with-output-string
     (lambda (out-buf)
       (letrec*
	;; template parser
	([tpl-parser
	  (lambda args
	    (let* ([str-in (apply string-copy `(,in-buf ,@args))]
		   [pos (car args)]
		   [get-position
		    (lambda (sign)
		      (let ([p (string-contains str-in sign)])
			(if p
			    (+ p pos)
			    p)))]
		   [s (get-position start-sign)]
		   [e (get-position end-sign)]
		   [sd (get-position startd-sign)]
		   [in-len (string-length str-in)]
		   )
	      (cond
	       ((= in-len 0) #t) ;; recursive exit
	       ((and (or s sd) 
		     (not e))
		(error tpl-parser "can't find ending!"))
	       ((and (and (not s) (not sd)) 
		     e)
		(error tpl-parser "invalid template file!"))
	       ((and (and (not s) (not sd))
		     (not e))
		(write-html-to-out-buf pos))
	       ((and sd 
		     (or (not s) (< sd s))) ;; <%= situation FIXME: I didn't consider sd-s-e, say, no sd-e 
		(write-html-to-out-buf pos sd)
		(write-script-display-to-out-buf (+ sd sd-len) e)
		(tpl-parser (+ e es-len)))
	       (else
		(write-html-to-out-buf pos s)
		(write-script-to-out-buf (+ s ss-len) e)
		(tpl-parser (+ e es-len)))
	       )))]
	 ;; handle script display part
	 [write-script-display-to-out-buf
	  (lambda args
	    (let ([script-in (apply string-copy `(,in-buf ,@args))])
	      (format out-buf "~a" 
		      (string-append 
		       " (format *ragnarok-guile-cgi-outport* \"~a\" "
		       script-in
		       " ) "
		       )
		      )))]
	 ;; handle script part
	 [write-script-to-out-buf
	  (lambda args
	    (let ([script-in (apply string-copy `(,in-buf ,@args))])
	      (format out-buf "~a" script-in)
	      ))]
	 ;; handle html part
	 [write-html-to-out-buf
	  (lambda args
	    (let ([html-str (apply string-copy `(,in-buf ,@args))])
	      (format out-buf "~a"
		      (string-append 
		       " (format *ragnarok-guile-cgi-outport* \"~a\" " 
		       (object->string html-str)
		       " ) "
		       )
		      )))]
	 );; end let-rec*
	(tpl-parser 0)
	);; end lambda 
       );; end call-with-output-string
     )))

(define-syntax ->
  (syntax-rules (@query @post @global)
    ((_ @query target)
     (string->symbol (string-append "query-table-" target)))
    ((_ @post target)
     (string->symbol (string->symbol "post-table-" target)))
    ((_ @global target)
     (string->symbol (string->symbol "global-table-" target)))
    ))

(define guile-cgi-pre-head
  "(use-modules (ragnarok cgi guile-fluid))
   (let* ([*ragnarok-guile-cgi-outport* 
           (fluid-ref guile-cgi-outport-fluid)]
          [*ragnarok-guile-cgi-query-table* 
           (fluid-ref query-table-fluid)]
          [*ragnarok-guile-cgi-post-table*
           (fluid-ref post-table-fluid)]
          [*ragnarok-guile-cgi-env-table*
           (fluid-ref env-table-fluid)]
          )
         (define-syntax $env
          (syntax-rules (@query @post @global
               @query! @post! @global!)
          ((_ @query key)
           (hash-ref *ragnarok-guile-cgi-query-table* key))
          ((_ @query! key val)
           (hash-set! *ragnarok-guile-cgi-query-table* key val))
          ((_ @post! key val)
           (hash-set! *ragnarok-guile-cgi-post-table* key val))
          ((_ @post key)
           (hash-ref *ragnarok-guile-cgi-post-table* key))
          ((_ @global! key val)
           (hash-set! *ragnarok-guile-cgi-env-table* key val))
          ((_ @global key)
           (hash-ref *ragnarok-guile-cgi-env-table* key))
          ))"
  )

(define run-guile-cgi
  (lambda (cgi)
    (if (not (cgi-record? cgi))
	(error regular-cgi-run "Not cgi-record-type!" cgi))
    (let* ([env-table (cgi:env-table cgi)]
	   [conn-socket (cgi:conn-socket cgi)]
	   [target (cgi:target cgi)]
	   [method (hash-ref env-table "REQUEST_METHOD")]
	   [query-table 
	    (create-query-table (hash-ref env-table "QUERY-STRING"))]
	   [post-table #f] 
	    ;(create-query-table (read-line conn-socket))]
	   [cgi-content (get-string-all
			 (open-input-file target))]
	   [render-result (guile-cgi-render cgi-content)]
	   [pp (pipe)]
	   [r (car pp)]
	   [w (cdr pp)]
	   )
      
      (with-fluids
       ([guile-cgi-outport-fluid w]
	[query-table-fluid query-table]
	[post-table-fluid post-table]
	[env-table-fluid env-table]
	)
       (eval-string (string-append 
		     guile-cgi-pre-head
		     render-result
		     " ) ")
		    )
       );; end with-fluids

      (close w)
      
      (let* ([bv (get-bytevector-all r)]
	     [bv-len (bytevector-length bv)]
	     [fst (stat target)]
	     )
	(values bv
		*OK*
		(record-real-bv-size fst bv-len))
	);; end let*
      )))

(define cgi-guile-serv-handler
  (lambda (logger filename server-info)
    (call-with-values
	(lambda ()
	  (if (file-exists? filename)
	      (run-guile-cgi
	       (http-make-cgi-type filename server-info))
	      ;; doesn't exists
	      (values (http-error-page-serv-handler logger *Not-Found*)
		      *Not-Found*))
	  )
      (lambda (bv status fst)
	(http-response-log logger status)
	(values bv status fst #f #f)
	))))

