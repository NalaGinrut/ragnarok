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
  #:use-module (srfi srfi-9)
  #:export (make-cgi-record
	    cgi-record?
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
 
(define-record-type cgi-record
  (make-cgi-record target q-str conn-socket)
  cgi-record?
  (target cgi:target)
  (q-str cgi:q-str)
  (conn-socket cgi:conn-socket)
  )

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
	       [q-str (cgi:q-str cgi)]
	       [conn-socket (cgi:conn-socket cgi)]
	       [hole (open "/dev/null" O_RDWR)]
	       )
	  ;;(setvbuf w _IONBF) ;; set to block buffer
	  ;;(redirect-port hole (current-error-port))
	  ;;(redirect-port hole (current-input-port))
	  (redirect-port w (current-output-port))
	  ;;(redirect-port (current-output-port) w)

	  ;; if q-str is not #f ,that means method is POST
	  (if q-str 
	      (setenv "QUERY_STRING" q-str)
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
