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
  #:use-module (ragnarok server)
  #:use-module (ragnarok log)
  #:use-module (ragnarok utils)
  #:export (http-handler
	    )
  )

(handler-register! http http-handler)

;; FIXME: I need to wrap handler template into a macro.
;;        I believe users don't want to write some meta info by themselves.
(define http-handler
  (lambda (server conn-socket)
    (let* ([logger (server:logger server)]
	   [root-path (server:get-config server 'root-path)]
	   
	   ;; TPL[1]
	   [request (get-request conn-socket)] 
	   )

      
	   
    (format #t "ok~%"))))

(define get-request
  (lambda (conn-socket)
    ;; TODO: parse the request then return a request type
    #t
    ))

(define http-static-page-serv-handler
  (lambda (file)
    #t
    ;; TODO: search static file then return content
    )
  )

(define http-dynamic-page-serv-handler
  (lambda (file)
    #t
    ;; TODO: search file and call templete handler to render cgi script
    ))

(define http-request-handler
  (lambda (client-connection)
    (let* ([client-details (cdr client-connection)]
	   [client (car client-connection)]
	   )
      #t
      )))

