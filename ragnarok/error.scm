;;  Copyright (C) 2011-2012
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

(define-module (ragnarok error)
  #:use-module (ragnarok utils)
  #:export (ragnarok-try ragnarok-throw)
  )

(define *ragnarok-error-symbol* 'ragnarok-error)

(define-syntax ragnarok-throw
  (syntax-rules ()
    ((_ msg)
     (throw *ragnarok-error-symbol* msg))
    ))

(define-syntax ragnarok-try
  (syntax-rules (catch throw final)
    ((_ *thunk* catch *exception* do *handler*)
     (catch *exception* *thunk* *handler*))
    ((_ *thunk1* catch *exception* do *handler* final *thunk2*)
     (catch *exception* *thunk1* (lambda (k . e)
				   (*handler* k e)
				   (*thunk2*))))
    ))

(define-syntax format-error-msg
  (syntax-rules ()
    ((_ fmt)
     (string-append "[error] " fmt))))

(define ragnarok-print-error-msg
  (lambda (k . e)
    (apply format #t `(,(format-error-msg (car e)) ,@(cdr e)))))
 
(define ragnarok-error-converter
  (lambda (msg . info)
    (lambda (k . e)
      (ragnarok-throw msg info))))
