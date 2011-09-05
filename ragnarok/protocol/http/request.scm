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

(define-module (ragnarok protocol http request)
  #:use-module (ragnarok utils)
  #:use-module (srfi srfi-9)
  #:export (request-read
	    )
  )

(define read-line (@ (ice-9 rdelim) read-line))

(define request-read
  (lambda (conn-socket)
    (call-with-values 
	(lambda ()
	  (parse-first-line conn-socket))
      (lambda (method file qstr version)
	(let ([lln
	       (let lp ((lns '())) ;; parse left lines
		 (let ([ln (read-line conn-socket)])
		   (if (eof-object? ln)
		       lns
		       (let ([ll (map string-trim-both 
				      (string-split ll #\:))])
			 (lp (cons ll lns))
			 ))))]
	      )
	  (make-request-type method file qstr version lln))
	))))

(define-record-type http-request
  (make-request-type method file qstr version)
  http-request?
  (method request:method)
  (file request:file)
  (qstr request:query-string) 
  (version request:version)
  (headers request:headers)
  )
  
(define parse-first-line
  (lambda (port)
    (let* ([fln (read-line port)]
	   [ll (get-word-list fln)]
	   [method (list-ref ll 0)]
	   [target (list-ref ll 1)]
	   [proto (list-ref ll 2)]
	   [tl (string-split target #\?)]
	   [file (car tl)]
	   [qstr (cadr tl)]
	   [version (cadr (string-split proto #\/))] 
	   )
      (values method file qstr version)
      )))


    
