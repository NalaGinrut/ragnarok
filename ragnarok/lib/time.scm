;;  Copyright (C) 2012  
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

(define-module (ragnarok lib time)
  #:use-module (oop goops)
  #:use-module (srfi srfi-19))

(module-export-all! (current-module))

(define write-date (@@ (web http) write-date))

(define-class <timer> ())

(define-method (install (self <timer>) (proc <procedure>))
  (sigaction SIGALRM proc SA_RESTART))

;; TODO: we need timer run on msec.
;; FIXME: we need infinite timer run loop.
(define-method (run (self <timer>) (sec <integer>))
  (alarm sec))

(define (time:now)
  (local-now))

(define (local-now)
  (call-with-output-string 
   (lambda (port)
     ;; NOTE: (time-utc->data t) to get local time.
     (write-date (time-utc->date (current-time)) port))))

(define (global-now)
  (call-with-output-string 
   (lambda (port)
     ;; NOTE: (time-utc->data t 0) to get global time.
     (write-date (time-utc->date (current-time) 0) port))))

(define ->global-time
  (lambda (t)
    ;; The old way to call strftime is obsolete for not using locale purpose.
    ;;(strftime "%a, %d %b %Y %T %Z" (gmtime t))    
    (call-with-output-string 
     (lambda (port)
       ;; NOTE: (time-utc->data t 0) to get global UTC time.
       (write-date (time-utc->date (make-time time-utc 0 t) 0) port)))))

(define ->local-time
  (lambda (t)
    ;; The old way to call strftime is obsolete for not using locale purpose.
    ;;(strftime "%a, %d %b %Y %T %Z" (localtime t))
    (call-with-output-string 
     (lambda (port)
       ;; NOTE: (time-utc->data t) to get local time.
       (write-date (time-utc->date (make-time time-utc 0 t)) port)))))

(define (get-global-current-time)
  ;; The old way to call strftime is obsolete for not using locale purpose.
  ;;(strftime "%a, %d %b %Y %T %Z" (gmtime (current-time ))))
  (call-with-output-string 
   (lambda (port)
     ;; NOTE: (time-utc->data t 0) to get global time.
     (write-date (time-utc->date (current-time) 0) port))))

(define get-modified-time-str
  (lambda (mt)
    ;; The old way to call strftime is obsolete for not using locale purpose.
    ;;(strftime "%Y-%b-%d %H:%I" (localtime mt))))
    (->local-time mt)))
