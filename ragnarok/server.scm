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

(define-module (ragnarok server)
  #:use-module (oop goops)
  #:use-module (ragnarok utils)
  #:use-module (ragnarok config)
  #:use-module (ragnarok msg)
  #:use-module (ragnarok handler)
  #:export (<server>
	    server:socket server:config server:handler
	    server:logger server:run server:show-config
	    )
  )

(define-class <server> ()
  (listen-socket #:init-value #f #:accessor server:listen-socket)
  (config #:init-value #f #:accessor server:config)
  (handler #:init-value #f #:accessor server:handler)
  (logger #:init-value #f #:accessor server:logger)
  )

(define-method (initialize (self <server>) initargs)
  (next-method) ;; call regular routine
  (let* ([config (get-conf-table)]
	 [status-show (hash-ref config 'status-show)]
	 [logger (make <logger> `(status-show ,status-show))]
	 [port (open-proper-port status-show)]
	 [protocol (hash-ref config 'protocol)]
	 [handler (get-handler protocol)]
	 )
    (set! (server:config self) config)
    (set! (server:logger self) logger)
    (set! (server:port self) port)
    
    (if handler
	(set! (server:handler self) handler)
	(error initialize "<server>: protocol hasn't been implemented yet!" protocol))
    ))

(define-method (server:get-config (self <server>) var)
  (let ((conf (server:config self)))
    ;; FIXME: I need an exception catch
    ;; TODO: each server should have one config hash table
    ;;       And all of them covered by one hash table.
    (hash-ref conf var)))

(define-method (server:show-config (self <server>))
  (let ([config (server:config self)])
    (print-conf-table config)))
	    
(define-method (server:run (self <server>))
  (let* ([port (server:get-config self 'port)]
	 [s (server:listen-port self port)]
	 [root-path (server:get-config self 'root-path)]
	 [proto (server:get-config self 'protocol)]
	 [request-handler (server:get-handler self proto)]
	 )
    ;; response loop
    (let active-loop ()
      (let* ([client-connection (accept s)]
	     [client-details (cdr client-connection)]
	     [client (car client-connection)]
	     )
	;; FIXME: checkout the validity
	(server:print-status self 
			     'client-info 
			     (get-client-info client-details))
	;; FIXME: I need to spawn new thread for a request-handler
	(request-handler self client-connection)
	(close client))
      (active-loop)
      )))

(define get-client-info
  (lambda (client-details)
    (let* ([fam (sockaddr:fam client-details)]
	   [ip (inet-ntop fam (sockaddr:addr client-details))]
	   [port (ntohs (sockaddr:port client-details))]
	   )
      (format #f "Get request from ~a, client port: ~a~%" ip port)
      )))

;; for more generilzation, we pass info as non-type arg
;; and one may handle it with a custom printer
(define-method (server:print-status 
		(self <server>) (type <symbol>) (info <string>))
  (let* ([logger (server:logger self)]
	 [time (msg-time-stamp)]
	 [msg (make-log-msg time type info)]
	 )
    (logger:printer logger msg)))
    
;; listen in the port then return the socket
(define-method (server:listen-port (self <server>) port)
  (let* ([s (socket PF_INET SOCK_STREAM 0)]
	 [max-req (server:get-config self 'max-request)]
	 )
    (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
    (bind s AF_INET INADDR_ANY port)
    (listen s max-req)
    (set! (server:listen-socket self) s)
    s
    ))