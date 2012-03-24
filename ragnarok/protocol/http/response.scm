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

(define-module (ragnarok protocol http response)
  #:use-module (ragnarok protocol http header)
  #:use-module (srfi srfi-9)
  #:export (build-response
	    write-response
	    write-response-body))

(define put-bytevector (@ (rnrs io ports) put-bytevector))
(define-record-type response-type
  (make-response-type version code reason headers charset)
  response-type?
  (version response:version)
  (code response:code)
  (reason response:reason)
  (headers response:headers)
  (charset response:charset))

(define gen-header-str
  (lambda (headers)
    (call-with-output-string
     (lambda (p)
       (for-each (lambda (h)
		   (let ([head-str (http-header (car h)
						(cdr h))])
		     (if head-str
			 (format p "~%~a" head-str)
			 )))
		 headers)
       ))))

;; Rewrite from (web response), this version is smarter (I think, at least).
;; NOTE: We don't need to verify the headers, because we can make sure of it.
(define* (build-response
	  #:key
	  (version 1.1) 
	  (code 200)
	  (headers '())
	  reason
	  (charset "iso-8859-1")
	  )
  (make-response-type version code reason headers charset)
  )

(define write-response
  (lambda (response port)
    (let* ([version (object->string (response:version response))]
	   [code (response:code response)]
	   [reason (response:reason response)]
	   [headers (gen-header-str (response:headers response))]
	   [charset (response:charset response)]
	   )
      (format port "HTTP/~a ~a ~a ~a; charset=~a~%~%"
	      version code reason headers charset)
      )))
       
(define-syntax write-response-body
  (syntax-rules ()
    ((_ bv port)
     (put-bytevector port bv)
     )))

	   