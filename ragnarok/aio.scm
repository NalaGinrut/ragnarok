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

;; NOTE: tail-call is not safe in the catch-context, so we use loop.
(define async-read
  (lambda* (in-port #:key (block 4096))
	   (call-with-output-string 
	    (lambda (out-port) 
	      (let ([buf (make-string block)])
		(while 
		 (and (port-is-not-end in-port) 
		      (> (read-string!/partial buf in-port) 0))
		 (ragnarok-try
		  (lambda ()
		    ;; FIXME: is necessary to force-output?
		    (format out-port "~a~!" buf))
		  catch #t
		  do (lambda e
		       (let ([E (get-errno e)])
			 (cond
			  ((= E EAGAIN) 
			   (yield))
			  ((= E EINTR)
			   (error "aio read was interrupted by user!"))
			  (else
			   (error "aio encountered a unknown error!" 
				  (system-error-errno e))))))
		  )))))))

(define async-write
  (lambda* (str port #:key (block 4096))
	   (let ([buf (make-string block)]
		 [str-len (string-length str)]
		 )
	     (do ([rest str (substring/shared str read-len)] 
		  [read-len (write-string!/partial buf port)
			    (write-string!/partial buf port)])
		  ((and (port-is-end port) (= read-len 0)) #t)
	       (ragnarok-try
		(lambda ()
		  #t)
		catch #t
		do (lambda e
		     (let ([E (get-errno e)])
		       (cond
			((= E EAGAIN)
			 (force-output port)
			 (yield)) 
			((= E EINTR)
			 (error "aio write was interrupted by user!"))
			(else
			 (error "aio encountered a unknown error!" 
				(system-error-errno e))))))
		)))))

