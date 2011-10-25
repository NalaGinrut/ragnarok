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

(define-module (ragnarok threads)
  #:use-module (ice-9 threads)
  #:use-module (ragnarok utils)
  #:export (threads:enqueue
	    threads:new
	    ragnarok-exclusive-try
	    ragnarok-call-with-new-fork
	    ragnarok-call-with-new-thread)
  )

;; we use *futures* in Guile to implement thread pool
;; maybe we need a brand new thread pool later... 

(define-syntax threads:enqueue
  (syntax-rules ()
    ((_ proc . args)
     (touch 
      (future (proc . args))))))

(define-syntax threads:new
  (syntax-rules (&)
    ((_ proc . args)
     (make-thread proc . args))
    ((_ & proc . args)
     (threads:enqueue proc . args)
     )))

(define-syntax ragnarok-exclusive-try 
  (syntax-rules ()
    ((_ body ...)
     (monitor body ...)
     )))

(define-syntax ragnarok-call-with-new-thread
  (syntax-rules ()
    ((_ thunk ...)
     (call-with-new-thread thunk ...)
     )))

(define ragnarok-call-with-new-fork
  (lambda (thunk)
    (if (not (thunk? thunk))
	(error ragnarok-call-with-new-fork
	       "Not a thunk!~%"
	       thunk))
    (let ([i (ragnarok-fork)])
      (cond
       ((< i 0)
	(error ragnarok-call-with-new-fork
	       "Fork error!~%"))
       ((= i 0)
	(apply thunk '()))
       ))
    ))
	
	       
	