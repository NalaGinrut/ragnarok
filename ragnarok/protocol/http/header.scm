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

(define-module (ragnarok protocol http header)
  #:export (http-header)
  )

(define *ragnarok-version* (@ (ragnarok env) *ragnarok-version*))
(define *OS-info* (vector-ref (uname) 0))
(define *server-info*
  (format #f "~a (~a)" *ragnarok-version* *OS-info*))
(define *accept-ranges* "bytes")
(define *age* 3600) ;; This should be configured

(define http-header
  (lambda (field value)
    (format #f "~a: ~a"
	    (string-capitalize (symbol->string field))
	    value)
    ;; NOTE: one must convert "value" to a proper string before call "http-header".
    ))

(define *regular-headers*
  `((server . ,*server-info*)
    (accept-ranges . ,*accept-ranges*)
    (age . ,*age*)
    ))