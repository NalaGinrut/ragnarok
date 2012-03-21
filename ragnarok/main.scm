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

(define-module (ragnarok main)
  #:use-module (ragnarok env)
  #:use-module (ragnarok server)
  #:use-module (ragnarok config)
  #:use-module (ragnarok version)
  #:use-module (ragnarok threads)
  #:use-module (ragnarok utils)
  #:use-module (oop goops)
  #:use-module (ice-9 getopt-long)
  #:export (main)
  )

(define ragnarok-env (make <env>))

(define *ragnarok-running-dir* "/var/log/ragnarok")
(define make-ragnarok-sys-file
  (lambda (filename)
    (string-append *ragnarok-running-dir* "/" filename)))
(define *ragnarok-lock-file* 
  (make-ragnarok-sys-file "ragnarok.lock"))
(define *ragnarok-log-file* 
  (make-ragnarok-sys-file "ragnarok.log"))
(define *ragnarok-err-log-file* 
  (make-ragnarok-sys-file "ragnarok.err"))

(define (ragnarok-unlock)
  (let ([lfp (open *ragnarok-lock-file* O_RDWR)])
    (flock lfp LOCK_UN)
    (close lfp))
  (delete-file *ragnarok-lock-file*))

(define option-spec
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (config (single-char #\c) (value #f)) ;; specify config file
    (server (single-char #\s) (value #f)) ;; specify sub-servers to start
    ))

(define help-str
  "
Ragnarok is a generic server written with GNU/Guile and C.
Ragnarok supports http/1.1 originally now. You may define your own protocol to Ragnarok by protobuf-r6rs(coming soon).

Usage: ragnarok [OPTIONS]...

--help -h: Show this screen.
--version -v: Show version.
--config -c: Specify config file.
--server -s: Specify sub-servers to start which delimited by ','.

Any bug/improve report will be appreciated.
Author: NalaGinrut@gmail.com
God bless hacking.\n
")

(define version-str
  (format #f 
	  "
~a. 

Copyright (C) 2011-2012 Mu Lei known as \"NalaGinrut\" <NalaGinrut@gmail.com>
License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
Ragnarok is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

God bless hacking.~%"
	  *ragnarok-version*))

(define (ragnarok-terminate)
  (kill (getpid) SIGTERM))

(define (show-help)
  (display help-str)
  (exit)
  )

(define (show-version)
  (display version-str)
  (exit)
  )

(define ragnarok-log-message
  (lambda (message)
    (let* ([lf (open-file *ragnarok-log-file* "a")]
	   [cgt (get-global-current-time)]
	   )
      (if lf
	  (format lf "~a at ~a~%" message cgt))
      (close lf)
      )))

(define (ragnarok-kill-all-servers)
  (let ([server-list (env:server-list ragnarok-env)])
    (for-each (lambda (s-pair)
		(server:down (cdr s-pair)))
	      server-list)))

(define (ragnarok-terminate-environ)
  ;; TODO: terminate environ
  (ragnarok-kill-all-servers)
  )

(define ragnarok-SIGHUP-handler
  (lambda (msg)
    (ragnarok-log-message "Ragnarok hangup!")
    ;; TODO: deal with hangup
    ))

(define ragnarok-SIGTERM-handler
  (lambda (msg)
    (ragnarok-log-message "Ragnarok exit!");
    (ragnarok-terminate-environ)
    (ragnarok-unlock)
    (sync)
    ;;(format #t "well~quit")
    (exit)
    ))

(define (signal-register)
  (sigaction SIGCHLD SIG_IGN) ;; ignore child
  (sigaction SIGTSTP SIG_IGN) ;; ignore tty signals
  (sigaction SIGTTOU SIG_IGN) ;; 
  (sigaction SIGTTIN SIG_IGN) ;;
  (sigaction SIGHUP ragnarok-SIGHUP-handler) ;; catch hangup signal
  (sigaction SIGTERM ragnarok-SIGTERM-handler) ;; catch kill signal
  )

(define (ragnarok-server-start)
  (let* ([snl (get-sub-server-name-list)]
	 [cnt (length snl)]
	 )
    (let lp ([server-list '()] [rest snl])
      (if (null? rest)
	  (begin
	    (set! (env:server-list ragnarok-env) server-list)
	    (let ([n (length server-list)])
	      (cond
	       ((> n 1)
		(format #t "~a sub-servers activated!~%" n))
	       ((= n 1)
		(format #t "~a sub-server activated!~%" n))
	       ((= n 0)
		(format #t "No sub-server activated!~%")))))
	  (let* ([sname (car rest)]
		 [server (make <server> #:name sname)])
	    (server:print-start-info server)
	    (ragnarok-call-with-new-thread
	     (lambda ()
	       (server:run server)))
	    (lp (cons (cons sname server) server-list) (cdr rest)))))))

(define (display-startup-message)
  (format #t "~a~%" version-str)
  (newline)
  (display "===================")
  (newline)
  (format #t "Ragnarok starting...~%")
  (format #t "If you want to check the log, type ragnarok-show-[err/log]~%")
  )

(define (show-subserver-info)
  (let* ([snl (get-sub-server-name-list)]
	 [cnt (length snl)]
	 )
    (format #t "Find ~a sub-servers from you machine:~%" cnt)
    (for-each (lambda (sname)
		(format #t "[~a] " sname))
	      snl)
    (newline)
    ))

(define main
  (lambda (args)
    (let* ((options 
            (getopt-long args option-spec))
           (need-help?
            (option-ref options 'help #f))
           (need-version?
            (option-ref options 'version #f))
	   (config-file
	    (option-ref options 'config "/etc/ragnarok/server.conf"))
	   (server-list
	    (option-ref options 'server #f))
	   )

      (cond
       (need-help? (show-help))
       (need-version? (show-version)))
      
      ;; daemonize
      (let ([i (ragnarok-fork)])
	(cond
	 ((> i 0) (primitive-exit)) ;; exit parent
	 ((< i 0) (error "Ragnarok: fork error!")))
	)

      ;; print greeting message
      (display-startup-message)

      ;; child(daemon) continue
      (setsid)

      (if (not (file-exists? *ragnarok-running-dir*))
	  (mkdir *ragnarok-running-dir*))
      (chdir *ragnarok-running-dir*)

      ;; delete old err log file ,or it will mess up with old-old err log
      (if (file-exists? *ragnarok-err-log-file*)
	  (delete-file *ragnarok-err-log-file*))
      (if (file-exists? *ragnarok-log-file*)
	  (delete-file *ragnarok-log-file*))

      (let* ([i (open "/dev/null" O_RDWR)]
	     [e (open *ragnarok-err-log-file* (logior O_CREAT O_RDWR))] 
	     [log (open *ragnarok-log-file* (logior O_CREAT O_RDWR))]
	     [lfp (open *ragnarok-lock-file* 
			(logior O_RDWR O_CREAT)
			#o640)])
	
	;;(for-each close (iota 3)) ;; close all ports
	(redirect-port i (current-input-port)) ;; stdin
	(redirect-port log (current-output-port))
	(redirect-port e (current-error-port)) ;; stderr
	(umask 022)
	
	(if (< (port->fdes lfp) 0)
	    (begin
	      (display "Ragnarok: can not open/create lock file!\n")
	      (exit 2)))

	(flock lfp LOCK_EX)
	
	(write (getpid) lfp)
	(close lfp))

      ;; TODO: signal handler register
      (signal-register)

      ;; TODO: overload cmd parameters to default parameters
      ;;       #f for default ,otherwise overload it.
        
      ;; show the active subserver information
      (show-subserver-info)
      
      (ragnarok-server-start)

      ;; never quit
      (eternal-loop)
      )))

(define (eternal-loop)
  (sleep 1000)
  (eternal-loop))

