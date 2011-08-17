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

(define-module (ragnarok protocol http)
  #:use-module (ragnarok handler)
  #:export (http-handler
	    http-static-page-req-handler
	    http-request-handler)
  )

(handler-register! http http-handler)
(define http-handler
  (lambda ()
    (format #t "ok~%")))

(define http-static-page-req-handler
  (lambda (file)
    #t
    ;; TODO: search static file then return content
    )
  )

(define http-request-handler
  (lambda (client-connection)
    (let* ([client-details (cdr client-connection)]
	   [client (car client-connection)]
	   )
      #t
      )))

