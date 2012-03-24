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
	    server:listen-socket server:config server:handler
	    server:logger server:run server:show-config
	    server:get-config server:down
	    server:print-start-info
	    server:init-event-system
	    server:add-to-env))

(define *default-max-events* 32)
(define *default-event-triger* 'level-triger)

;; FIXME: actually epoll module don't need a default timeout.
(define *default-server-timeout* 5000) ;; default timeout 5s

(define-class <server> (<env>)
  ;; FIXME: support server name later. 
  ;;        And logger name should accord to server name.
  (name #:init-value "http0" #:accessor server:name 
	#:init-keyword #:name)
  (listen-socket #:accessor server:listen-socket)
  (config #:accessor server:config)
  (handler #:accessor server:handler)
  (logger #:accessor server:logger)
  (event-set #:accessor server:event-set)
  ;; these three lists for statistics purpose 
  (read-list #:init-value '() #:accessor server:read-list) 
  (write-list #:init-value '() #:accessor server:write-list)
  (except-list #:init-value '() #:accessor server:except-list)
  (timeout #:init-value #f #:accessor server:timeout)
  (ready-list #:init-value '() #:accessor server:ready-list)
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
	 [triger (hash-ref config 'triger)]
	 [handler (get-handler handler-list protocol)]
	 )
    ;; NOTE: init order is important!
    ;;     1. init all properties
    ;;     2. init event system
    ;;     3. init handler
    ;;     4. finally add subserver to env

    (set! (server:config self) config)
    (set! (server:logger self) logger)
    (set! (server:handler self) handler)
    (set! (env:handler-list self) handler-list)

    ;; init max-events
    (or max-events (hash-set! config 'max-events *default-max-events*)) 
    
    ;; init server event triger
    (or triger (hash-set! config 'triger *default-event-triger*))

    ;; init server timeout
    (or timeout	(set! timeout *default-server-timeout*))
    (set! (server:timeout self) (format-timeout timeout))

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
	[port (server:get-config self 'listen)])
    (ragnarok-exclusive-try
     (format #t "*Starting [~a] ...~%" sname)
     (format #t "  [~a] is ~a server which's listenning in port ~a~%"
	     sname proto port))))

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
    ;;       deal with this at exit automatically. Or it'll cause error.
  )

(define-method (server:run (self <server>))
  (let* ([server-port (server:get-config self 'listen)]
	 [server-protocol (server:get-config self 'proto)]
	 [server-triger (server:get-config self 'triger)]
	 [server-name (server:name self)]
	 [server-software *ragnarok-version*]
	 [server-charset (server:get-config self 'charset)]
	 [server-root (server:get-config self 'root-path)]
	 [server-conf (server:config self)]
	 [subserver-info 
	  (make-subserver-info server-port server-protocol
			       server-name server-software
			       server-charset server-root server-conf)]
	 [s (server:listen-in-port self server-port)]
	 [request-handler (server:handler self)]
	 [logger (server:logger self)])
    ;; store listen-socket for later use
    ;; FIXME: This step can provide 'changing listen port on the fly' feature.
    ;;        But one should handle this listen-socket properly after it's closed.
    ;;        I think set! listen-socket to #f immediatly after it was closed 
    ;;        would be a good solution.
    (set! (server:listen-socket self) s)

    ;; add listen-socket into read-set
    (server:add-event self s 'read)

    ;; response loop
    (let active-loop()
      (cond
       ((port-closed? s)
	(error "listen-port is closed"))
       ((server:wait-for-listen-port-ready self)
	(let* ([client-connection (ragnarok-accept s)]
	       [conn-socket (car client-connection)]
	       [client-details (cdr client-connection)])
	  ;; set conn-socket to non-block if edge-triger
	  ;; TODO: use a read/write handler register mechanism
	  (and (eqv? server-triger 'edge-triger) (set-port-non-block! conn-socket))

	  ;; FIXME: checkout the validity
	  (server:print-status self 
			       'client-info 
			       (get-client-info client-details))

	  ;; TODO: 1. add conn-socket into write-set
	  ;;       2. deal with aio
	  ;;       3. del conn-socket from write-set after closed
	  ;;(server:register-request self conn-socket)

	  ;; deal with request in new thread
	  ;; FIXME: we need thread pool! I'll do it later.
	  (ragnarok-call-with-new-thread
	   (lambda ()
	     (request-handler logger client-connection subserver-info)
	     (shutdown conn-socket 2) ;; can be closed after trans finished.
	     (close-port conn-socket)      
	     (logger:sync logger)))
	  (active-loop)))
       (else
	(yield) ;; if no request then yield this thread
	(active-loop))))))

;; NOTE: A request event is read/write usually.
;;       So we just add this conn-socket as write type which means both read/write. 
(define-method (server:register-request self (conn-socket <port>))
  (server:add-event self conn-socket 'write))

(define get-client-info
  (lambda (client-details)
    (let* ([fam (sockaddr:fam client-details)]
	   [ip (inet-ntop fam (sockaddr:addr client-details))]
	   [port (ntohs (sockaddr:port client-details))])
      (format #f "Get request from ~a, client port: ~a~%" ip port))))

;; TODO:
;; for more general, we pass info as non-type arg
;; and one may handle it with a custom printer
(define-method (server:print-status 
		(self <server>) (type <symbol>) (info <string>))
  (let* ([logger (server:logger self)]
	 [time (msg-time-stamp)]
	 [msg (make-log-msg time type info)])
    (logger:printer logger msg)))
    
;; listen in the port then return the socket
(define-method (server:listen-in-port (self <server>) (port-fd <integer>))
  (ragnarok-try
   (let ([s (socket PF_INET SOCK_STREAM 0)]
	 [max-req (server:get-config self 'max-request)])
     (setsockopt s SOL_SOCKET SO_REUSEADDR 1)
     (ragnarok-bind s AF_INET INADDR_ANY port-fd)
     (ragnarok-listen s max-req)
     (set! (server:listen-socket self) s)
     s ;; return listen-socket
     )))

(define-method (server:init-event-system (self <server>))
  (let ([max-events (server:get-config self 'max-events)])
    (ragnarok-try
     (begin
       (if (or (not max-events) 
	       (not (integer? max-events)) 
	       (<= max-events 0))
	   (ragnarok-throw "invalid max-events: ~a~%" max-events)
	   (set! (server:event-set self) (ragnarok-event-init max-events)))))))
	
;; Check if there's new request on ready-list.
;; Or try to update ready-list then check.
(define-method (server:wait-for-listen-port-ready (self <server>))
  (let ([ready-list (server:ready-list self)]
	[listen-fd (port->fdes (server:listen-socket self))])
    ;; FIXME: This linear search maybe inefficient when lots of conn-sockets.
    ;;        I'll find another proper search algorithms instead.
    ;; FIXME: Actually we shouldn't use this linear search to check if 
    ;;        listen-socket is ready. The better solution would be a thread pool
    ;;        with a work-queue.
    (cond
     ((not ready-list)
      ;; if no event then return #f
      (error "invalid ready-list type, it should be a list!")) 
     ((assoc listen-fd ready-list)
      #t)
     (else
      (assoc listen-fd (server:update-ready-list self))))))

(define-method (server:update-ready-list (self <server>))
  (let ([ready-list (server:get-request-events self)])
    (set! (server:ready-list self) ready-list)
    ready-list))
	
(define-method (server:get-request-events (self <server>))
  (let ([event-set (server:event-set self)]
	[timeout (server:timeout self)])
    (ragnarok-try
     (if timeout
	 (ragnarok-event-handler event-set
				 (timeout:second timeout)
				 (timeout:msecond timeout))
	 (ragnarok-event-handler event-set)))))

(define-method (server:get-record-list (self <server>) (type <symbol>))
  (ragnarok-try
   (begin
     (case type
       ((read) (server:read-list self))
       ((write) (server:write-list self))
       ((except) (server:except-list self))
       (else
	(ragnarok-throw "invalid record-list type: ~a~%" type))))))

(define-method (server:del-event (self <server>) (type <symbol>) (socket <port>))
  (ragnarok-try
   (begin
     (case type
       ((read) 
	(set! (server:read-list self)
	      (assoc-remove! (server:read-list self) (port->fdes socket))))	
       ((write) 
	(set! (server:write-list self)
	      (assoc-remove! (server:write-list self) (port->fdes socket))))	
       ((except) 
	(set! (server:except-list self)
	      (assoc-remove! (server:except-list self) (port->fdes socket))))	
       (else
	(ragnarok-throw "invalid record-list type: ~a~%" type)))
     (ragnarok-event-del (port->fdes socket) (server:event-set self))
     )))
   
(define-method (server:update-record-list (self <server>) (type <symbol>) 
					  (socket <port>) event)
  (ragnarok-try
   (begin
     (case type
       ((read) 
	(set! (server:read-list self) 
	      (cons (cons (port->fdes socket) event) (server:read-list self))))
       ((write) 
	(set! (server:write-list self) 
	      (cons (cons (port->fdes socket) event) (server:write-list self))))
       ((except) 
	(set! (server:except-list self) 
	      (cons (cons (port->fdes socket) event) (server:except-list self))))
       (else
	(ragnarok-throw "invalid record-list type: ~a~%" type))))))

(define-method (server:add-event (self <server>) (socket <port>) (type <symbol>))
  (let* ([lst (server:get-record-list self type)]
	 [triger (server:get-config self 'triger)]
	 [event (ragnarok-make-event-from-socket socket type triger)]
	 [set (server:event-set self)]) 
    (ragnarok-try
     (ragnarok-event-add event set)
     (server:update-record-list self type socket event))))

(define-method (server:add-event (self <server>) (fd <integer>) 
				 (triger <symbol>) (type <symbol>))
  (let ([set (server:get-event-set self type)]
	[event (ragnarok-event-create #:type type #:status 'ready
				      #:fd fd #:triger triger)]) 
    (ragnarok-try
     (ragnarok-event-add event set)
     (server:update-record-list self type event))))

(define-method (server:add-event (self <server>) (fd <integer>) (triger <symbol>) 
				 (type <symbol>) (oneshot <boolean>))
  (let ([set (server:get-event-set self type)]
	[event (ragnarok-event-create #:type type #:status 'ready 
				      #:fd fd #:triger triger
				      #:oneshot onshot)]) 
    (ragnarok-try
     (ragnarok-event-add event set)
     (server:update-record-list self type event))))

(define-method (server:add-to-env (self <server>))
  (set! (env:server-list self)
	(cons 
	 (cons (string->symbol (server:name self)) self)
	 (env:server-list self))))

