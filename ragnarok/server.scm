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

(define-module (ragnarok server)
  #:use-module (oop goops)
  #:use-module (ragnarok env)
  #:use-module (ragnarok config)
  #:use-module (ragnarok msg)
  #:use-module (ragnarok handler)
  #:use-module (ragnarok log)
  #:use-module (ragnarok utils)
  #:use-module (ragnarok info)
  #:use-module (ragnarok threads)
  #:use-module (ragnarok event)
  #:use-module (ragnarok posix)
  #:use-module (ragnarok error)
  #:use-module (ragnarok version)
  #:export (<server>
	    server:socket server:config server:handler
	    server:logger server:run server:show-config
	    server:get-config server:down
	    server:print-start-info
	    server:init-event-system
	    server:add-to-env
	    )
  )

(define *default-max-events* 32)

(define-class <server> (<env>)
  ;; FIXME: support server name later. 
  ;;        And logger name should accord to server name.
  (name #:init-value "http0" #:accessor server:name 
	#:init-keyword #:name)
  (listen-socket #:accessor server:listen-socket)
  (config #:accessor server:config)
  (handler #:accessor server:handler)
  (logger #:accessor server:logger)
  (read-set #:accessor server:read-set)
  (write-set #:accessor server:write-set)
  (except-set #:accessor server:except-set)
  (timeout #:init-value #f #:accessor server:timeout)
  (ready-list #:init-value #f #:accessor server:ready-list)
  )

(define-method (initialize (self <server>) initargs)
  (next-method) ;; call regular routine
  (let* ([handler-list (load-handler)]
	 [name (server:name self)]
	 [config (get-sub-server-conf-table name)]
	 [status-show (hash-ref config 'status-show)]
	 [timeout (hash-ref config 'timeout)]
	 [logger (make <logger> `(status-show ,status-show) '())]
	 [protocol (hash-ref config 'protocol)]
	 [max-events (hash-ref config 'max-events)]
	 [handler (get-handler handler-list protocol)]
	 )
    ;; NOTE: init order is important!
    ;;     1. init all properties
    ;;     2. init event system
    ;;     3. init handler
    ;;     4. add subserver to env finally

    (set! (server:config self) config)
    (set! (server:logger self) logger)
    (set! (server:handler self) handler)
    (set! (env:handler-list self) handler-list)

    ;; init max-events
    (if (not max-events)
	(hash-set! config 'max-events *default-max-events*))

    (if timeout
	(set! (server:timeout self) (format-timeout timeout)))

    ;; init subserver event system
    (server:init-event-system self)
     
    (if handler
	(set! (server:handler self) handler)
	(error initialize "<server>: protocol hasn't been implemented yet!" protocol))

    ;; update env's servers list
    (server:add-to-env self)

    ))

(define-method (server:get-config (self <server>) var)
  (let ((conf (server:config self)))
    ;; FIXME: I need an exception catch
    ;; TODO: each server should have one config hash table
    ;;       And all of them covered by one hash table.
    (hash-ref conf var)))

(define-method (server:print-start-info (self <server>))
  (let ([sname (server:name self)]
	[proto (server:get-config self 'protocol)]
	[port (server:get-config self 'listen)]
	)
    (ragnarok-exclusive-try
     (format #t "*Starting [~a] ...~%" sname)
     (format #t "  [~a] is ~a server which's listenning in port ~a~%"
	     sname proto port)
     ) ;; end ragnarok-exclusive-try
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
	 [server-charset (server:get-config self 'charset)]
	 [server-root (server:get-config self 'root-path)]
	 [subserver-info 
	  (make-subserver-info server-port server-protocol
			       server-name server-software
			       server-charset server-root)]
	 [s (server:listen-in-port self server-port)]
	 [request-handler (server:handler self)]
	 [logger (server:logger self)]
	 )

    ;; set listen-port to non-block
    (set-port-non-block! s)

    ;; store listen-socket for later use
    ;; FIXME: This step can provide 'changing listen port on the fly' feature.
    ;;        But one should handle this listen-socket properly after it's closed.
    ;;        I think set! listen-socket to #f immediatly after it was closed 
    ;;        would be a good operation.
    (set! (server:listen-socket self) s)

    ;; response loop
    (let active-loop ()
      (if (not (port-closed? s))
	  (if (not (server:wait-for-listen-port-ready self))
	      (yield) ;; if no request then yield this thread 
	      (let* ([client-connection (ragnarok-accept s)]
		     [conn-socket (car client-connection)]
		     [client-details (cdr client-connection)]
		     )
		;; FIXME: checkout the validity
		(server:print-status self 
				     'client-info 
				     (get-client-info client-details))

		;; TODO: 1. add conn-socket into write-set
		;;       2. deal with aio
		;;       3. del conn-socket from write-set after closed
		;; (server:add-event self (port->fdes conn-socket))

		;; deal with request in new thread
		;; FIXME: we need thread pool! I'll do it later.
		(ragnarok-call-with-new-thread
		 (lambda ()
		   (request-handler logger client-connection subserver-info)
		   (shutdown conn-socket 2) ;; can be closed after trans finished.
		   (close-port conn-socket)      
		   (logger:sync logger)
		   ))
		
		(active-loop) ;; do loop in normal situation
		) ;; end (let* ([client-connection
	      ) ;; end (if (not (server:wait-for-listen-port-ready
	  
	  ;; if socket is closed ,quit loop
	  ))))

(define get-client-info
  (lambda (client-details)
    (let* ([fam (sockaddr:fam client-details)]
	   [ip (inet-ntop fam (sockaddr:addr client-details))]
	   [port (ntohs (sockaddr:port client-details))]
	   )
      (format #f "Get request from ~a, client port: ~a~%" ip port)
      )))

;; TODO:
;; for more general, we pass info as non-type arg
;; and one may handle it with a custom printer
(define-method (server:print-status 
		(self <server>) (type <symbol>) (info <string>))
  (let* ([logger (server:logger self)]
	 [time (msg-time-stamp)]
	 [msg (make-log-msg time type info)]
	 )
    (logger:printer logger msg)))
    
;; listen in the port then return the socket
(define-method (server:listen-in-port (self <server>) (port-fd <integer>))
  (ragnarok-try
   (lambda ()
     (let ([s (socket PF_INET SOCK_STREAM 0)]
	   [max-req (server:get-config self 'max-request)]
	   )
       (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
       (ragnarok-bind s AF_INET INADDR_ANY port-fd)
       (ragnarok-listen s max-req)
       (set! (server:listen-socket self) s)
       s ;; return listen-socket
       ))))

(define-method (server:init-event-system (self <server>))
  (let ([max-events (server:get-config self 'max-events)])
    (ragnarok-try
     (lambda ()
       (call-with-values
	   (lambda ()
	     (if (<= max-events 0)
		 (ragnarok-throw "invalid max-events: ~a~%" max-events)
		 (ragnarok-event-init max-events)))
	 (lambda (rs ws es)
	   (set! (server:read-set self) rs)
	   (set! (server:write-set self) ws)
	   (set! (server:except-set self) es)))))))

(define-method (server:wait-for-listen-port-ready (self <server>))
  (let ([ready-list (server:update-ready-list self)]
	[socket (server:socket self)]
	)
    (if (null? ready-list)
	#f ;; if no event then return #f
	(call/cc
	 (lambda (return)
	   (for-each 
	    (lambda (fd)
	      (if (= (port->fdes socket) fd)
		  (return #t)))
	    ready-list)
	   #f ;; if listen socket isn't ready then return #f 
	   )) ;; end call/cc
	)))

(define-method (server:update-ready-list (self <server>))
  (let ([ready-list (server:get-request-events self)])
    (set! (server:ready-list self) ready-list)
    ready-list
    ))
	
(define-method (server:get-request-events (self <server>))
  (let ([read-set (server:read-set self)]
	[write-set (server:write-set self)]
	[except-set (server:except-set self)]
	[timeout (server:timeout self)]
	)
    ;; NOTE: the order is important ,read&write&except
    (ragnarok-try
     (lambda ()
       (if timeout
	   (ragnarok-event-handler `(,read-set ,write-set ,except-set)
				   (timeout:second timeout)
				   (timeout:msecond timeout))
	   (ragnarok-event-handler `(,read-set ,write-set ,except-set)))
       ))))

(define-method (server:get-event-set (self <server>) (type <symbol>))
  (ragnarok-try
   (lambda ()
     (case type
       ((read) (server:read-set self))
       ((write) (server:write-set self))
       ((except) (server:except-set self))
       (else
	(ragnarok-throw "invalid event-set type: ~a~%" type))))))

(define-method (server:add-event (self <server>) (fd <integer>) (type <symbol>))
  (let ([set (server:get-event-set self type)]
	[event (ragnarok-event-create #:type type #:status 'ready #:fd fd)]
	) 
    (ragnarok-event-add event set)))

(define-method (server:add-to-env (self <server>))
  (add-to-list! (env:server-list self) 
		(string->symbol (server:name self))
		self))
