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

(define-module (ragnarok posix)
  #:use-module (ragnarok utils)
  #:use-module (ragnarok error)
  #:export (ragnarok-posix-context
	    ragnarok-bind
	    ragnarok-listen
	    ragnarok-accept))

(define* (ragnarok-bind sock fam_or_sockaddr #:optional address . args)
  (catch
   'system-error
   (lambda ()
     (apply bind sock fam_or_sockaddr address args))
   ragnarok-print-error-msg))
   
(define ragnarok-listen 
  (lambda (sock backlog)
    (catch
     'system-error
     (lambda ()
       (listen sock backlog))
     ragnarok-print-error-msg)))

(define (ragnarok-accept socket)
  (catch 
   'system-error
   (lambda ()
     (accept socket))
   ragnarok-print-error-msg))


