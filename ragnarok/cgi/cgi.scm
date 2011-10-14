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

(define-module (ragnarok cgi cgi)
  #:use-module (ragnarok protocol http status)
  #:use-module (ragnarok utils)
  #:use-module (ragnarok version)
  #:use-module (srfi srfi-9)
  #:export (make-cgi-record
	    cgi-record?
	    cgi-env-set!
	    create-cgi
	    regular-cgi-run
	    )
  )

;; NOTE: We must return file-stat as one of values, but the stat:size is not
;;       the real size of dynamic page. So we modify the stat:size.
(define file-stat:size 7)
(define record-real-bv-size
  (lambda (st size)
    (vector-set! st file-stat:size size)
    st
    ))

;; WARN: DO NOT change the order of *cgi-env-vars-list*.
(define *cgi-env-vars-list*
  '("AUTH_TYPE"
    "CONTENT_LENGTH"
    "CONTENT_TYPE"
    "GATEWAY_INTERFACE"
    "PATH_INFO"
    "PATH_TRANSLATED"
    "QUERY_STRING"
    "REMOTE_ADDR"
    "REMOTE_HOST"
    "REMOTE_IDENT"
    "REMOTE_USER"
    "REQUEST_METHOD"
    "SCRIPT_NAME"
    "SERVER_NAME"
    "SERVER_PORT"
    "SERVER_PROTOCOL"
    "SERVER_SOFTWARE"))


(define-record-type cgi-record
  (make-cgi-record target env-table conn-socket)
  cgi-record?
  (target cgi:target)
  (env-table cgi:env-table) ;; a hash table to put envion vars
  (conn-socket cgi:conn-socket)
  )

(define create-cgi
  (lambda* 
   (target conn-socket 
	   #:key 
	   (QUERY_STRING "")
	   (REQUEST_METHOD "GET")
	   (AUTH_TYPE "Basic")
	   (CONTENT_LENGTH "")
	   (CONTENT_TYPE "")
	   (GATEWAY_INTERFACE "CGI")
	   (PATH_INFO "")
	   (PATH_TRANSLATED "")
	   (REMOTE_ADDR "")
	   (REMOTE_HOST "")
	   (REMOTE_IDENT "")
	   (REMOTE_USER "")
	   (SCRIPT_NAME "")
	   (SERVER_NAME "")
	   (SERVER_PORT "80")
	   (SERVER_PROTOCOL "http")
	   (SERVER_SOFTWARE *ragnarok-version*)
	   )
  ;; WARN: DO NOT change this order.
  (let* ([in `(,AUTH_TYPE
	       ,CONTENT_LENGTH
	       ,CONTENT_TYPE
	       ,GATEWAY_INTERFACE
	       ,PATH_INFO
	       ,PATH_TRANSLATED
	       ,QUERY_STRING
	       ,REMOTE_ADDR
	       ,REMOTE_HOST
	       ,REMOTE_IDENT
	       ,REMOTE_USER
	       ,REQUEST_METHOD
	       ,SCRIPT_NAME
	       ,SERVER_NAME
	       ,SERVER_PORT
	       ,SERVER_PROTOCOL
	       ,SERVER_SOFTWARE)]
	 [env-table (make-hash-table 17)]
	 )
     
    (for-each (lambda (e i)
		(hash-set! env-table e i))
	      *cgi-env-vars-list*
	      in)
    
    (make-cgi-record target
		     env-table
		     conn-socket)
    )))

(define cgi-env-get
  (lambda (key cgi)
    (let ([env-table (cgi:env-table cgi)])
      (hash-ref env-table key)
      )))

(define cgi-env-set!
  (lambda (cgi key value)
    (let ([env-table (cgi:env-table cgi)])
      (hash-set! env-table key value)
      )))

(define regular-cgi-run
  (lambda (cgi)
    (if (not (cgi-record? cgi))
	(error regular-cgi-run "Not cgi-record-type!" cgi))
    (let* ([p-buf (pipe)]
	   [r (car p-buf)]
	   [w (cdr p-buf)]
	   [i (primitive-fork)]
	   )
      (cond 
       ((< i 0)
	(values #f *Fork-Error* #f))
       ((= i 0)
	(let* ([target (cgi:target cgi)]
	       [QUERY_STRING (cgi-env-get 'QUERY_STRING cgi)]
	       [conn-socket (cgi:conn-socket cgi)]
	       )
	  (setvbuf w _IONBF) ;; set to block buffer
	  (redirect-port w (current-output-port))
	  
	  ;; if QUERY_STRING is not #f ,that means method is POST
	  ;; FIXME: we should use REQUEST_METHOD to decide.
	  (if QUERY_STRING 
	      (setenv "QUERY_STRING" QUERY_STRING)
	      (redirect-port conn-socket (current-input-port)))
	  
	  (execle target (environ)) ;; run cgi script
	  (close (current-output-port))
	  )))
       
      ;; NOTE: parent must wait child terminate, 
      ;;       or get-bytevector-all will be blocked.
      (waitpid i)
      
      ;; NOTE: we must close input pipe ,or get-bytevector-all will be blocked.
      ;; I wonder if this is a bug.
      (close w) 
      (let* ([bv (get-bytevector-all r)]
	     [size (bytevector-length bv)]
	     [fst (stat (cgi:target cgi))]
	     )
	(values bv
		*OK*
		(record-real-bv-size fst size))
	))))
		    
(define ragnarok-regular-cgi-handler
  (lambda (cgi)
    (if (not (cgi-record? cgi))
	(error ragnarok-regular-cgi-handler "Not cgi-record-type!" cgi))
    (if (not (check-file-perms (cgi:target cgi) #o555)) ;; DON'T use "access?"
	(values #f *Forbidden* #f) ;; no excute perms
	(regular-cgi-run cgi)
	)))

(define *cgi-env-vars-list* 17)
(define (create-cgi-env-table)
  (make-hash-table *cgi-envs-numbers*))

(define (cgi:auth-type! cgi-env-table v)
  (hash-set! cgi-env-table "AUTH_TYPE" v))

(define (cgi:content-length! cgi-env-table v)
  (hash-set! cgi-env-table "CONTENT_LENGTH" v))

(define (cgi:content-type! cgi-env-table v)
  (hash-set! cgi-env-table "CONTENT_TYPE" v))

(define (cgi:gateway-interface! cgi-env-table v)
  (hash-set! cgi-env-table "GATEWAY_INTERFACE" v))

(define (cgi:path-info! cgi-env-table v)
  (hash-set! cgi-env-table "PATH_INFO" v))

(define (cgi:path-translated! cgi-env-table v)
  (hash-set! cgi-env-table "PATH_TRANSLATED" v))

(define (cgi:query-string! cgi-env-table v)
  (hash-set! cgi-env-table "QUERY_STRING" v))

(define (cgi:remote-addr! cgi-env-table v)
  (hash-set! cgi-env-table "REMOTE_ADDR" v))

(define (cgi:remote-host! cgi-env-table v)
  (hash-set! cgi-env-table "REMOTE_HOST" v))

(define (cgi:remote-ident! cgi-env-table v)
  (hash-set! cgi-env-table "REMOTE_IDENT" v))

(define (cgi:remote-user! cgi-env-table v)
  (hash-set! cgi-env-table "REMOTE_USER" v))

(define (cgi:request-method! cgi-env-table v)
  (hash-set! cgi-env-table "REQUEST_METHOD" v))

(define (cgi:script-name! cgi-env-table v)
  (hash-set! cgi-env-table "SCRIPT_NAME" v))

(define (cgi:server-name! cgi-env-table v)
  (hash-set! cgi-env-table "SERVER_NAME" v))

(define (cgi:server-port! cgi-env-table v)
  (hash-set! cgi-env-table "SERVER_PORT" v))

(define (cgi:server-protocol! cgi-env-table v)
  (hash-set! cgi-env-table "SERVER_PROTOCOL" v))

(define (cgi:server-software! cgi-env-table v)
  (hash-set! cgi-env-table "SERVER_SOFTWARE" v))

	   
