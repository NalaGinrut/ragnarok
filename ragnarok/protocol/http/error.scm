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

(define-module (ragnarok protocol http error)
  #:use-module (ragnarok protocol http status)
  #:use-module (ragnarok protocol http log)
  #:use-module (ragnarok version)
  #:use-module (ragnarok utils)
  #:export (http-error-page-serv-handler
	    http-inner-error-handler)
  )

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
	   )
      (case key
	((*Bad-Request*)
	 (http-error-page-serv-handler logger *Bad-Request* server-info))
	((*Forbidden*)
	 (http-error-page-serv-handler logger *Forbidden* server-info))
	((*Not-Found*)
	 (http-error-page-serv-handler logger *Not-Found* server-info))
	((*Not-Allow*)
	 (http-error-page-serv-handler logger *Not-Allow* server-info))
	((*Not-Accept*)
	 (http-error-page-serv-handler logger *Not-Accept* server-info))
	((*Proxy-Auth-Required*)
	 (http-error-page-serv-handler logger *Proxy-Auth-Required* server-info))
	((*Request-Timeout*)
	 (http-error-page-serv-handler logger *Request-Timeout* server-info))
	;; TODO: finish the rest error handler
	))))

