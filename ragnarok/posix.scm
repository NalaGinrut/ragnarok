;;  Copyright (C) 2012  
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

(define-module (ragnarok posix)
  #:use-module (ragnarok utils)
  #:export (ragnarok-catch-context
	    ragnarok-bind
	    ragnarok-listen)
  )

(define-syntax ragnarok-catch-context
  (syntax-rules ()
    ((_ thunk error-handler)
     (ragnarok-try
      (lambda ()
	(catch 'system-error
	       thunk
	       ragnarok-error-converter))
      catch 'ragnarok-error
      do
      error-handler))))

(define* (ragnarok-bind bind sock fam_or_sockaddr #:optional address . args)
  (ragnarok-catch-context
   (lambda ()
     (bind bind sock fam_or_sockaddr address args))
   ragnarok-print-error-msg))
   
(define ragnarok-listen 
  (lambda (sock backlog)
    (ragnarok-catch-context
     (lambda ()
       (listen sock backlog)))
    ragnarok-print-error-msg))


