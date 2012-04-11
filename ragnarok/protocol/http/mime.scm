;;  Copyright (C) 2011-2012
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  Ragnarok is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  Ragnarok is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (ragnarok protocol http mime)
  #:use-module (ragnarok utils)
  #:export (get-mime-handler
	    *mime-types-table*
	    get-type-from-mime
	    init-mime))

(define *mime-list-file* "/etc/ragnarok/mime.list")

(define get-mime-handler
  (lambda (mime)
    (hash-ref *mime-handlers-table* mime)
    ;;(get-arg *mime-handler-list* mime)
    ))

(define get-type-from-mime
  (lambda (mime)
    (hash-ref *mime-types-table* mime)))

;; TODO: generate this table on the env init time, and save a copy in env.
(define *mime-types-table* (make-hash-table 100))
(define *mime-handlers-table* (make-hash-table 10))

;; TODO: generated mime-types table
(define (get-mime-types-list)
  (load *mime-list-file*))

(define (init-mime)
  (let ([mtl (get-mime-types-list)])
    ;; init mime type table
    (for-each (lambda (x)
		(let* ([mtype (car x)]
		       [mimes (cadr x)]
		       )
		  (for-each (lambda (m)
			      (hash-set! *mime-types-table*
					 m
					 mtype))
			    mimes)))
	      mtl)

    ;; init mime handler table
    (for-each (lambda (x)
		(hash-set! *mime-handlers-table*
			   (car x)
			   (cadr x)))
	      *mime-handler-list*)))

;; TODO: MIME handler should be dynamically registered.
(define *mime-handler-list*
  `((html ,(@ (ragnarok protocol http handler) http-static-page-serv-handler))
    (gl ,(@ (ragnarok cgi guile) cgi-guile-serv-handler))))


