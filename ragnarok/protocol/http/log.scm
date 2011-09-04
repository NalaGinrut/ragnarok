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

(define-module (ragnarok protocol http log)
  #:use-module (ragnarok protocol http status)
  #:use-module (ragnarok log)
  #:use-module (ragnarok msg)
  #:export (http-request-log
	    http-response-log
	    )
  )

(define uri-path (@ (web uri) uri-path))
(define request-uri (@ (web request) request-uri))

(define http-response-log
  (lambda (logger status)
    (let ([reason (http-get-reason-from-status status)])
      (logger:printer logger
		      (make-log-msg (msg-time-stamp)
				    status
				    reason))
      )))

(define http-request-log
  (lambda (logger request)
    (let* ([path (uri-path (request-uri request))]
	   [info (format #f "Client request ~a" path)]
	   )
      (logger:printer logger
		      (make-log-msg (msg-time-stamp)
				    'request-info
				    info))
      )))
