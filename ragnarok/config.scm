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

(define-module (ragnarok config)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ragnarok utils)
  #:export (gen-conf-table
	    print-conf-table
	    *conf-table*
	    get-sub-server-conf-table
	    get-sub-server-name-list)
  )

;; config will generate a global config hash table
;; so we need to start each server following config table
;; TODO: each environment should match one config file.
;;       And each env contains many servers.
(define *start-sign* "+[")
(define *stop-sign* "-[")
(define *split-sign* #\:)
(define *conf-table* (make-hash-table))
(define *conf-path* "/etc/ragnarok")
(define *core-conf-file* "server.conf")
(define *ragnarok-conf-file*
  (string-append *conf-path* "/" *core-conf-file*))

(define print-conf-table 
  (lambda (table)
    (hash-for-each
     (lambda (x y) (format #t "~a : ~a~%" x y))
     table)))

;; one may add new conf item to this list for verifying.
(define (type:string x) x)
(define (type:symbol x) (string->symbol x))
(define (type:integer x) (string->number x))
(define (type:bool x) (string=? x "yes"))
(define (type:sym-list x)
  (let ([l (string-split x #\,)])
    (map (lambda (e)
	   (string->symbol (string-trim-both e)))
	 l)
    ))

(define *valid-conf-list*
  `((root-path ,type:string)
    (protocol ,type:symbol)
    (status-show ,type:symbol)
    (max-request ,type:integer)
    (listen ,type:integer)
    (charset ,type:string)
    (cgi ,type:bool)
    (with-cgi ,type:sym-list)
    ))

(define verify-key-val
  (lambda (key val)
    (let ([vp (car (assoc-ref *valid-conf-list* key))])
      (or (vp val) 
	  (error verify-key-val "invalid key!" key))
      )))
	   
(define gen-conf-table
  (lambda ()
    (let ([conf-list (get-config-list)])
      (for-each add-each-sub-server-conf-to-table conf-list)
      *conf-table*
      )))

(define add-each-sub-server-conf-to-table
  (lambda (sc-pair)
    (let* ([sub-server-conf-table (make-hash-table)]
	   [sname (car sc-pair)]
	   [sconf (cdr sc-pair)]
	   )
      (for-each 
       (lambda (kv-pair) 
	 (let* ([k (car kv-pair)]
		[v (cdr kv-pair)]
		[vv (verify-key-val k v)]
		)
	   (hash-set! sub-server-conf-table k vv)
	   )) ;; end lambda
       sconf) ;; end for-each
      
      ;; add sub-server conf table to table
      (hash-set! *conf-table* sname sub-server-conf-table)
      )))

(define verify-conf-list
  (lambda (conf-list)
    (for-each verify-key-val conf-list))) 
     
(define get-conf-from-path
  (lambda (path file)
    (let ([f (string-append path "/" file)])
      (open-input-file f))))

;; NOTE: subserver name only contains upper/lower case char and numbers
(define *subserver-name-pattern* "\\+\\[([A-Za-z0-9 ]+)\\]")

(define (get-sub-server-name-list)
  (hash-map->list 
   (lambda (k v)
     k)
   *conf-table*)
  )
      
(define get-sub-server-conf-table
  (lambda (sname)
    (hash-ref *conf-table* sname)
    ))

(define get-sub-server-name
  (lambda (conf-line)
    (let* ([match (string-match *subserver-name-pattern*
				conf-line)]
	   [sname (and match (match:substring match 1))]
	   )
      (and sname
	   (string-trim-both sname))
      )))

(define fix-sconf-str
  (lambda (str)
    (let ([i (string-contains "{" str)])
      (if i
	  (string-copy str (1+ i))
	  str
	  ))))

(define get-sub-server-conf
  (lambda (fp)
    (let* ([sconf-str-0 (read-delimited "}" fp)]
	   [sconf-str (fix-sconf-str sconf-str-0)]
	   )
      (call-with-input-string 
       sconf-str
       (lambda (port)
	 (let read-loop ([scl '()])
	   (let ([conf-line (read-line port)])
	     (cond
	      ((eof-object? conf-line)
	       scl)
	      ((or (string-null? conf-line)
		   (char=? (string-ref conf-line 0) #\#)) ;; comment
	       (read-loop scl))
	      (else
	       (let* ([kvl (string-split conf-line *split-sign*)]
		      [key 
		       (string->symbol 
			(string-trim-both (car kvl)))]
		      [val (string-trim-both (cadr kvl))]
		      )
		 (read-loop (cons (cons key val) scl))
		 ))) ;; end cond
	     )))) ;; end call-with-input-string
      )))
	   
(define get-config-list
  (lambda ()
    (let ([conf-port (open-input-file *ragnarok-conf-file*)])
      (let read-loop ([cl '()])
	(let ([conf-line (read-line conf-port)])
	  (cond
	   ((eof-object? conf-line)
	    cl)  
	   ((or (string-null? conf-line)
		(char=? (string-ref conf-line 0) #\#) ;; comment
		(string-contains conf-line *stop-sign*)) ;; sub-server who won't start
	    (read-loop cl))
	   ((string-contains conf-line *start-sign*)
	    (let* ([sname (get-sub-server-name conf-line)]
		   [sconf (get-sub-server-conf conf-port)]
		   )
	      (read-loop (cons (cons sname sconf) cl))
	      ))
	   );; end cond
	  )))))

