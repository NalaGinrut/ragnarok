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

(define-module (ragnarok protocol http response)
  #:use-module (srfi srfi-9)
  #:export (build-response
	    write-response
	    )
  )

(define http-header (@ (ragnarok protocol http header) http-header))

(define-record-type response-type
  (make-response-type version code reason headers charset)
  response-type?
  (version response:version)
  (code response:code)
  (reason response:reason)
  (headers response:headers)
  (charset response:charset)
  )

(define gen-code-str
  (lambda (code)
    (if (list? code)
	(call-with-output-string
	 (lambda (p)
	   (for-each (lambda (x) (format p "~a " x)) code)))
	(object->string code)
	)))

(define gen-reason-str
  (lambda (reason)
    (cond
     ((symbol? reason)
      (let* ([str (symbol->string reason)]
	     [i (string-contains str "-")]
	     [str2 (string-replace str " " i (1+ i))]
	     )
	(string-capitalize str2)))
     ((string? reason)
      reason)
     (else
      (error gen-reason-str "Invalid reason-phrase!" reason)))
    ))

(define gen-header-str
  (lambda (headers)
    (call-with-output-string
     (lambda (p)
       (for-each (lambda (f v)
		   (format p "~%~a"
			   (http-header f (object->string v))))
		 headers)
       ))))

;; Rewrite from (web response), this version is smarter (I think, at least).
;; NOTE: We don't need to verify the headers, because we can make sure of it.
(define* (build-response
	  (#:key
	   (version 1.1) 
	   (code 200)
	   reason ;; In general, we just pass "status" to this "reason-phrase".
	   (headers '())
	   (charset "iso-8859-1")
	   ))
  (make-response-type version code reason headers)
  )

(define write-response
  (lambda (response port)
    (let* ([version (response:version response)]
	   [code (response:code response)]
	   [reason (response:reason response)]
	   [headers (response:headers response)]
	   [response-str
	    (call-with-output-string
	     (lambda (p)
	       (let* ([ver-str (object->string version)]
		      [code-str (gen-code-str code)]
		      [reason-str (gen-reason-str reason)]
		      [h-str (gen-header-str headers)]
		      )
		 (format p "~a ~a ~a ~a;charset=~a~%~%"
			 ver-str code-str reason-str h-str))))]
	   )
      (write response-str port)
      )))
       
(define write-response-body
  (lambda (bv type char-set port)
    (let* ([len (bytevector-lengh bv)]
	   