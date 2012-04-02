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

(define-module (ragnarok protocol http error)
  #:use-module (ragnarok protocol http status)
  #:use-module (ragnarok protocol http log)
  #:use-module (ragnarok version)
  #:use-module (ragnarok error)
  #:use-module (ragnarok utils)
  #:autoload (rnrs io ports) (get-bytevector-all)
  #:export (http-error-page-serv-handler
	    http-inner-error-handler
	    http-connection-error-handler)
  #:re-export (ragnarok-try))

(define *http-error-msg-list*
  '(*Continue*
    *Switch-Proto*
    
    ;; Successful 2xx
    *OK*
    *Created*
    *Accepted*
    *Non-Auth*
    *No-Content*
    *Reset*
    *Partial-Content*

    ;; Redirection 3xx
    *Multi-Choices*
    *Moved-Permanently*
    *Found*
    *See-Other*
    *Not-Modified*
    *Use-Proxy*
    ;; 306 unused
    *Temp-Redirect*

    ;; Client Error 4xx
    *Bad-Request*
    *Unauth*
    *Payment-Required*
    *Forbidden*
    *Not-Found*
    *Not-Allow*
    *Not-Accept*
    *Proxy-Auth-Required*
    *Request-Timeout*
    *Conflict*
    *Gone*
    *Length-Required*
    *Precondition-Failed*
    *Req-Too-Large*
    *URI-Too-Long*
    *Unsupported-Media-Type*
    *Range-Not-Satisfiable*
    *Expectation-Failed*


    ;; Server Error 5xx
    *Internal-Error*
    *Not-Implemented*
    *Bad-Gateway*
    *Service-Unavailable*
    *Gateway-Timeout*
    *Version-Not-Supported*

    ;; Server Extended (only ragnarok)
    *Fork-Error*
    *CGI-Not-Allowed*
    ))

(define http-error-page-serv-handler
  (lambda (logger status server-info)
    (let* ([stat-file (http-get-stat-file-from-status status)]
	   [stat-html (string-append *status-page-dir*
				     stat-file)]
	   [err-bv (get-bytevector-all
		    (open-input-file stat-html))]
	   [fst (stat stat-html)]
	   )
      (values err-bv status fst)
      )))

(define http-inner-error-handler
  (lambda (key . parameters)
    (let* ([logger (car parameters)]
	   [server-info (cadr parameters)]
	   [inner-handler 
	    (lambda (logger server-info)
	      (lambda (status)
		(http-error-page-serv-handler logger status server-info)))])
      (case key
	((*Bad-Request*)
	 (inner-handler *Bad-Request*))
	((*Forbidden*)
	 (inner-handler *Forbidden*))
	((*Not-Found*)
	 (inner-handler *Not-Found*))
	((*Not-Allow*)
	 (inner-handler *Not-Allow*))
	((*Not-Accept*)
	 (inner-handler *Not-Accept*))
	((*Proxy-Auth-Required*)
	 (inner-handler *Proxy-Auth-Required*))
	((*Request-Timeout*)
	 (inner-handler  *Request-Timeout*))
	;; TODO: finish the rest error handler
	))))

(define http-connection-error-handler
  (lambda e
    (case (car e)
      ((system-error)
       (let ([E (get-errno e)])
	 (cond
	  ((or (= E ECONNRESET) (= E ENOTCONN))
	   #f))))
      ((bad-header)
       #f)))) ;; return #f if any connection problem occurs
