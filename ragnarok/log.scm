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

(define-module (ragnarok log)
  #:use-module (ragnarok config)
  #:use-module (ragnarok utils)
  #:use-module (ragnarok msg)
  #:use-module (ragnarok threads)
  #:use-module (oop goops)
  #:export (<logger>
	    logger:open-proper-port
	    logger:printer
	    logger:sync
	    logger:name logger:path
	    logger:port logger:filename))

(define-class <logger> ()
  ;; FIXME: Each server should a name, and each logger get this name,
  ;;        we use this name to name the log. But we haven't do this yet.
  ;;        There're 3 steps to go:
  ;;        1. config file should have name{ conf... } pattern;
  ;;        2. server/logger should init name in the beginning;
  ;;        3. name could be dynamically changed.
  (name #:init-value "http0" #:accessor logger:name #:init-keyword #:name)
  (path #:init-value "/var/log/ragnarok" #:accessor logger:path)
  (filename #:accessor logger:filename
	    #:allocation #:virtual
	    #:slot-ref (lambda (o)
			 (let* ([name (logger:name o)]
				[path (logger:path o)]
				)
			   (string-append path "/" "ragnarok." name ".log")))
	    ;; why do this? If server name changed, 
	    ;; log name also changed on the fly~
	    #:slot-set! (lambda (o v) #f))

  ;; port must be a symbol
  (port #:init-value #f #:accessor logger:port))
  
	      
(define *log-ports*
  '((console-port (current-output-port))
    (err-port (current-error-port))))
	
(define-method (logger:sync (self <logger>))
  (force-output 
   (logger:port self)))

(define-method (logger:printer (self <logger>) msg)
  (let ([port (logger:port self)])
    (if port
	(log-printer msg port)
	(print-to-all-ports 
	 msg 
	 (cons `(log-port ,port)
	       *log-ports*)))))
   
(define-method (initialize (self <logger>) initargs)
  (next-method) ;; call regular routine
  (let* ([now (msg-time-stamp)]
	 [status-show (get-arg initargs 'status-show)]
	 [log-file (logger:filename self)]
	 [port (logger:open-proper-port status-show log-file)]
	 [path (logger:path self)]
	 [msg (make-log-msg now 'init-logger "Logger init!")]
	 )
    (or (file-exists? path)
	(mkdir path))
    (set! (logger:port self) port)
    (log-printer msg port)
    ))

(define log-printer
  (lambda (msg port)
    (or (log-msg? msg)
	(error log-printer "invalid msg format:" msg))

    (let* ([time (msg:time msg)]
	   [type (object->string (msg:type msg))]
	   [info (msg:info msg)]
	   )
      (ragnarok-exclusive-try
       (format port "~a:~% [~a] ~a~%~!" time type info))
      
      ;; FIXME: Could this sync-process drag down server's speed?
      ;;(force-output port)
      )))

(define print-to-all-ports
  (lambda (msg port-list)
    (for-each (lambda (x)
		(let ([port (car x)])
		  (log-printer msg port)))
	      port-list)))

(define logger:open-proper-port
  (lambda (which filename)
    (or (symbol? which)
	(error logger:open-proper-port "invalid status-show:" which))
    (case which
      ;; FIXME: "all" needs to open all ports, but now we haven't done it yet.
      ((all) #f)
      ((console) (current-output-port))
      ((error) (current-error-port))
      ((log) 
       (let ([port (open-file filename "w+")])
	 (if port
	     port
	     (error logger:open-proper-port
		    "open file failed!" 
		    filename))))
      (else 
       (error logger:open-proper-port "invalid port request:" which)
       ))))
    
;; NOTE: use this procedure to generate log file suffix.
;; TODO: we need a periodical log archive procedure.
(define (gen-log-file-archive-suffix)
  (strftime "%y-%m-%d" (localtime (current-time))))

