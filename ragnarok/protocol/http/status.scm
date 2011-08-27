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

(define-module (ragnarok protocol http status)
  #:use-module (ragnarok utils)
  #:export (*status-list*
	    http-get-info-from-status
	    http-get-num-from-status
	    http-get-stat-file-from-status
	    http-gen-status-phrase
	    )
  )

;; TODO: complete this *status-list*
(define *status-list*
  '((OK (200 "*Status-200* Request's OK!" #f))
    (No-Content (204 "*Status-204* No content!" #f))
    ;; 4xx
    (Bad-Request (400 "*Status-400* Bad Request!" #f))
    (Forbidden (403 "*Status-403* Forbidden" "403.html"))
    (Not-Found (404 "*Status-404* Not Found!" "404.html"))
    ))

(define http-gen-status-phrase
  (lambda (status)
    (let* ([str (symbol->string value)]
	   [i (string-contains str "-")]
	   [str2 (string-replace str " " i (1+ i))]
	   )
      (string-capitalize str2))
    ))

(define http-get-info-from-status
  (lambda (status)
    (cadr (get-arg *status-list* status))))

(define http-get-num-from-status
  (lambda (status)
    (car (get-arg *status-list* status))))

(define http-get-stat-file-from-status
  (lambda (status)
    (caddr (get-arg *status-list* status))))