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
  #:use-module (ragnarok env)
  #:use-module (ragnarok config)
  #:use-module (ragnarok msg)
  #:use-module (ragnarok handler)
  #:use-module (ragnarok log)
  #:use-module (ragnarok utils)
  #:use-module (ragnarok info)
  #:use-module (ragnarok version)
  #:export (<server>
	    server:socket server:config server:handler
	    server:logger server:run server:show-config
	    server:get-config server:down
	    server:print-start-info
	    )
  )

(define-class <server> (<env>)
  ;; FIXME: support server name later. 
  ;;        And logger name should accord to server name.
  (name #:init-value "http0" #:accessor server:name 
	#:init-keyword #:name)
  (listen-socket #:accessor server:listen-socket)
  (config #:accessor server:config)
  (handler #:accessor server:handler)
  (logger #:accessor server:logger)
  )

(define-method (initialize (self <server>) initargs)
  (next-method) ;; call regular routine
  (let* ([handler-list (load-handler)]
	 [name (server:name self)]
	 [config (get-sub-server-conf-table name)]
	 [status-show (hash-ref config 'status-show)]
	 [logger (make <logger> `(status-show ,status-show) '())]
	 [protocol (hash-ref config 'protocol)]
	 [handler (get-handler handler-list protocol)]
	 )
    (set! (server:config self) config)
    (set! (server:logger self) logger)
    (set! (server:handler self) handler)
    (set! (env:handler-list self) handler-list)

    ;; update env's servers list
    (add-to-list! (env:server-list self) 
		  (string->symbol name)
		  self)

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

(define-method (server:print-start-info (self <server>))
  (let* ([sname (server:name self)]
	 [proto (server:get-config self 'protocol)]
	 [port (server:get-config self 'listen)]
	 )
    (format #t "*Starting [~a] ...~%" sname)
    (format #t "  [~a] is ~a server which's listenning in port ~a~%"
	    sname proto port)
    ))

(define-method (server:show-config (self <server>))
  (let ([config (server:config self)])
    (print-conf-table config)))

(define-method (server:down (self <server>))
  ;; TODO: log server down infomation
  (server:print-status self
		       'server-down
		       (format #f "Sub-server - ~s is down!~%"
			       (server:name self)))
    ;; NOTE: DO NOT close listen socket here. Because Guile will
    ;;       deal with this automatically. Or it'll cause error.
  )

(define-method (server:run (self <server>))
  (let* ([server-port (server:get-config self 'listen)]
	 [server-protocol (server:get-config self 'proto)]
	 [server-name (server:name self)]
	 [server-software *ragnarok-version*]
	 [subserver-info 
	  (make-subserver-info server-port server-protocol
			       server-name server-software)]
	 [s (server:listen-port self server-port)]
	 [request-handler (server:handler self)]
	 [config (server:config self)]
	 [logger (server:logger self)]
	 	 )
    ;; response loop
    (let active-loop ()
      (if (not (port-closed? s))
	  (let* ([client-connection (accept s)]
		 [conn-socket (car client-connection)]
		 [client-details (cdr client-connection)]
		 )
	    ;; FIXME: checkout the validity
	    (server:print-status self 
				 'client-info 
				 (get-client-info client-details))
	    ;; FIXME: I need to spawn new thread for a request-handler
	    (request-handler config logger client-connection subserver-info)
	    (shutdown conn-socket 2) ;; can be closed after trans finished.
	    (close-port conn-socket)      
	    (logger:sync logger)
	    (active-loop)
	    )
	  ))))

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
    ;; TODO: It MUST BE an exclusive logger file accession.
    (logger:printer logger msg)))
    
;; listen in the port then return the socket
(define-method (server:listen-port (self <server>) port)
  (if (not port)
      (error "Listen port isn't specified!" (server:name self)))
  (let* ([s (socket PF_INET SOCK_STREAM 0)]
	 [max-req (server:get-config self 'max-request)]
	 )
    (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
    (bind s AF_INET INADDR_ANY port)
    (listen s max-req)
    (set! (server:listen-socket self) s)
    s
    ))
