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
  )

(module-export-all! (current-module))

(define *OK* 200)
(define *No-Content* 204)
(define *Bad-Request* 400)
(define *Forbidden* 403)
(define *Not-Found* 404)

;; TODO: complete this *status-list*
(define *status-list*
  '((200 ("OK" "*Status-200* Request's OK!" #f))
    (204 ("No Content" "*Status-204* No content!" #f))
    ;; 4xx
    (400 ("Bad Request" "*Status-400* Bad Request!" #f))
    (403 ("Forbidden" "*Status-403* Forbidden" "403.html"))
    (404 ("Not Found" "*Status-404* Not Found!" "404.html"))
    ))

(define http-get-reason-from-status
  (lambda (status)
    (car (get-arg *status-list* status))))

(define http-get-stat-file-from-status
  (lambda (status)
    (caddr (get-arg *status-list* status))))

