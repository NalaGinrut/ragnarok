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

(define-module (ragnarok handler)
  #:use-module (ragnarok utils)
  #:export (get-handler 
	    handler-register!
	    handler-unregister!
	    )
  )

(define *handler-list* '())

;; NOTE: proto here doesn't have to be symbol, just write it down!
(define-syntax handler-register! 
  (syntax-rules () 
    ((_ proto handler)
     (add-to-list! *handler-list*
		   proto
		   (@ (ragnarok protocol proto) handler))
     )))

;; NOTE: proto here must be a symbol!
(define-syntax handler-unregister! 
  (syntax-rules () 
    ((_ proto handler)
     (add-to-list! *handler-list*
		   proto)
     )))

(define get-handler
  (lambda (protocol)
    (cond
     ((not (symbol? protocol))
      (error get-handler "invalid type, should be symbol:" protocol))
     (protocol
      (assoc-ref *handler-list* protocol))
     (else
      (error get-handler "protocol isn't specified!" protocol)
      ))))

(define (show-supported-protocol)
  (format #t "Ragnarok supports these protocols:~%")
  (for-each (lambda (x)
	      (format #t "~a~%" (car x)))
	    *handler-list*)
  )




