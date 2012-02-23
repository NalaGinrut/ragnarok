;;  Copyright (C) 2011  
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

(define-module (ragnarok aio)
  #:use-module (ragnarok utils)
  #:use-module (ragnarok error)
  #:use-module (ice-9 rw)
  #:export (async-write async-read)
  )

(define async-read
  (lambda* (in-port #:key (size 4096))
	   (call-with-output-string 
	    (lambda (out-port) 
	      (ragnarok-try
	       (let ([buf (make-string size)])
		 (let lp ()
		   (if (and (port-is-not-end fp) 
			    (> (read-string!/partial buf fp)))
		       (begin
			 (write buf out-port)
			 (lp)))))
	       catch 'system-error
	       do (lambda (k . e)
		    (let ([E (system-error-errno e)])
		      (cond
		       ((= E EAGAIN) 
			(yield)
			(write buf out-port) (lp)) 
		       (else
			(error "aio encountered a unknown error!" 
			       (system-error-errno e)))))
		    ))))))

(define async-write
  (lambda* (str port #:key (size 4096))
	   (let ([buf (make-string size)]
		 [str-len (string-length str)]
		 )
	     (let lp ([rest str])
	       (if (port-is-not-end port)
		   (ragnarok-try
		    (let ([read-len (write-string!/partial buf port)])
		      (if (> read-len 0)
			  (lp (substring/shared str read-len))))
		    catch 'system-error
		    do (lambda (k . e)
			 (let ([E (system-error-errno e)])
			   (cond
			    ((= E EAGAIN) 
			     (yield) 
			     (lp (substring/shared str (read-len))))
			    (else
			     (error "aio encountered a unknown error!" 
				    (system-error-errno e)))))
			 )))))))

