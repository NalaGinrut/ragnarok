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
  #:autoload (rnrs io ports) (put-bytevector
			      get-bytevector-n
			      call-with-bytevector-output-port)
  #:export (async-write async-read async-read-n)
  )

;; FIXME: A simple implemation to get n bytes from a port.
;;        But inefficient, since it'll get all bytes then cut.
(define (async-read-n read-port size)
  (let* ([bv (async-read read-port)]
	 [len (bytevector-length bv)])
    (cond
     ((= len size)
      bv)
     ((> len size)
      (let ([buf (make-bytevector size)])
	(bytevector-copy! bv 0 buf 0 size)
	buf))
     (else
      (ragnarok-throw "async-read-n get wrong bytevector" len size)))))

;; NOTE: tail-call is not safe in the catch-context, so we use loop.
(define (async-read read-port)
  (call-with-bytevector-output-port
   (lambda (out-port) 
     (while
      (port-is-not-end read-port) 
      (ragnarok-try
       (lambda ()
	 (put-bytevector out-port (get-bytevector-all read-port))
	 (force-output out-port))
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
		       (system-error-errno e)))))))))))

;; FIXME: There should be a counterpart of (ice-9 rw) for bytevector.
;;        I believe utf8->string is too heavy for a big bytevector.
(define* (async-write bv write-port #:key (block 4096))
  (let ([str (utf8->string bv)]
	[len (string-length str)]
	[begin 0]
	[end 0]
	[write-len 1] ;; set it to 1 to avoid being 0, but it won't do any harm.
	[buf (make-string block)])
    (lambda (port)
      (while
       (< begin len)
       (ragnarok-try
	(lambda ()
	  (let ([rest (if (<= end len)
			  (substring/shared str begin end)
			  (ragnarok-throw "async-write: impossible write-len" 
					  write-len begin end len))])
	    (if (> write-len 0)
		(begin
		  (set! write-len (write-string!/partial rest write-port))
		  (set! end (+ begin write-len))
		  (set! begin end)))))
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
			(system-error-errno e)))))))))))

