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

(define-module (ragnarok main)
  #:use-module (ragnarok server)
  #:use-module (ragnarok version)
  #:use-module (ragnarok utils)
  #:use-module (oop goops)
  #:use-module (ice-9 getopt-long)
  #:export (main)
  )

(define *ragnarok-running-dir* "/var/log/ragnarok")
(define *ragnarok-lock-file* "ragnarok.lock")
(define *ragnarok-log-file* "ragnarok.log")
(define *ragnarok-err-log-file* "ragnarok.err")

(define (ragnarok-unlock)
  (unlink (string-append *ragnarok-running-dir* 
			 *ragnarok-err-log-file*)))

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
God bless hacking. 
")

(define version-str
  (format #f 
	  "
~a. 

Copyright (C) 2011 Mu Lei known as \"NalaGinrut\" <NalaGinrut@gmail.com>
License LGPLv3+: GNU LGPL 3 or later <http://gnu.org/licenses/lgpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

God bless hacking."
	  ragnarok-version))

(define (ragnarok-terminate)
  (kill (getpid) SIGTERM))

(define (show-help)
  (display help-str)
  (primitive-exit 0)
  )

(define (show-version)
  (display ragnarok-version)
  (primitive-exit 0)
  )

(define ragnarok_log_message
  (lambda (message)
    (let ([lf (open-file *ragnarok-log-file* "a")])
      (if (not lf)
	  (format lf "~a~%" message))
      (close lf)
      )))
	  
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

      (cond
       ((> (fork) 0) (primitive-exit 0)) ;; exit parent
       ((< (fork) 0) (error "Ragnarok: fork error!")))

      ;; child(daemon) continue
      (setsid)
      (chdir *ragnarok-running-dir*)

      (let* ([i (open "/dev/null" O_RDWR)]
	     [e (open *ragnarok-err-log-file* O_RDWR)] 
	     [lfp (open *ragnarok-lock-file* 
			(or O_RDWR O_CREAT)
			#o640)]
	     )

	(dup2 0 i) ;; stdin
	(dup2 2 e) ;; stderr
	(umask 022)
	
	(cond
	 ((< lfp 0)
	  (display "Ragnarok: can not open/create lock file!\n")
	  (primitive-exit 2))
	 ((< (lockf lfp F_TLOCK) 0)
	  (display "Ragnarok: can not lock!\n")
	  (primitive-exit 3)))

	(write lfp (getpid))
	(close lfp)
	)

      ;; TODO: signal handler register
 
      ;; TODO: overload cmd parameters to default parameters
      ;;       #f for default ,otherwise overload it.

      ;;(let ((server (make <server>)))
      ;; (server:run server))
    )))
