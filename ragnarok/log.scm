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

(define-module (ragnarok log)
  #:use-module (ragnarok conf)
  #:use-module (ragnarok utils)
  #:use-module (oop goops)
  #:export (<logger>
	    logger:printer
	    logger:name logger:path
	    logger:port logger:filename)
  )

(define-class <logger> ()
  (name #:init-value "http0" #:accessor logger:name
	#:init-keyword #:name)
  (path #:init-value "/var/log/ragnarok" #:accessor logger:path
	#:init-keyword #:path)
  (filename #:accessor logger:filename
	    #:allocation #:virtual
	    #:slot-ref (lambda (o)
			 (let* ([name (logger:name o)]
				[path (logger:path o)]
				)
			   (string-append path "/" "ragnarok." name ".log")))
	    #:slot-set! #f)

  ;; port must be a symbol
  (port #:init-value #f #:accessor logger:port #:init-keyword #:port)
  )
  
(define *log-ports*
  '((console-port (current-output-port))
    (err-port (current-error-port))
    ))
	 
(define-method (logger:printer (self <logger>) msg)
  (let ([port (logger:port self)])
	(if port
	    (log-printer msg port)
	    (print-to-all-ports 
	     msg 
	     (car `(log-port ,port)
		  *log-ports*))
	    )))
   
(define-method (initialize (self <logger>) initargs)
  (let* ([now (msg-time-stamp)]
	 [status-show (get-arg initargs 'status-show)]
	 [port (get-proper-port status-show)]
	 [path (logger:path self)]
	 )
    (or (file-exists? *path*)
	(mkdir *path*))
    (set! (logger:port self) port)
    (log-printer port "**%a**: ~%Logger init!~%" now)
    ))

(define log-printer
  (lambda (msg port)
    (or (log-msg? msg)
	(error log-printer "invalid msg format:" msg))

    (let* ([time (msg:time msg)]
	   [type (symbol->string (msg:type msg))]
	   [info (msg:info msg)]
	   )
      (format port "~a:~% [~a] ~a~%" time type info)
      
      ;; FIXME: will this sync drag down server's speed?
      (force-output port)
      )))

(define print-to-all-ports
  (lambda (msg port-list)
    (for-each (lambda (x)
		(let ([port (car x)])
		  (log-printer msg port)))
	      port-list
	      )
    ))

(define get-proper-port
  (lambda (which . filename)
    (or (symbol? which)
	(error get-proper-port "invalid status-show:" which))
    (case which
      ((all-ports) #f)
      ((console-port) (current-output-port))
      ((err-port) (current-error-port))
      ((log-port) (open-output-port (car filename)))
      (else 
       (error get-proper-port "invalid port request:" (car filename))
       ))))
    


