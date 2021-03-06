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

(define-module (ragnarok error)
  #:use-module (ragnarok utils)
  #:export (ragnarok-try 
	    ragnarok-throw
	    ragnarok-print-error-msg))

(define *ragnarok-error-symbol* 'ragnarok-error)

(define ragnarok-throw
  (lambda (msg . info)
     (apply throw *ragnarok-error-symbol* msg info)))

(define-syntax ragnarok-try
  (syntax-rules (catch throw final)
    ((_ thunk catch exception do handler)
     (catch exception (lambda () thunk) handler))
    ((_ thunk1 catch exception do handler final thunk2)
     (catch exception (lambda () thunk1) (lambda (k . e)
					   (handler k e)
					   (thunk2))))
    ((_ thunk)
     (catch *ragnarok-error-symbol*
	    (lambda () thunk)
	    ragnarok-print-error-msg))
    ((_ thunk handler)
     (catch *ragnarok-error-symbol*
	    (lambda () thunk)
	    handler))))

(define-syntax format-error-msg
  (syntax-rules ()
    ((_ key fmt)
     (format #f "[~a] ~a~%~!" key fmt))))

;; NOTE: print-error-msg shouldn't exit, or we can hardly debug
;; FIXME: this proc doesn't print out other msgs except first error arg.
;;        say, only err-proc name.
(define ragnarok-print-error-msg
  (lambda (k . e)
    (apply format #t `(,(format-error-msg k (car e)) ,@(cdr e)))))

;; TODO: but we really need a critical-error-handler to throw errmsg then exit

 
