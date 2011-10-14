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

(define *status-page-dir* "/etc/ragnarok/stat_html/")

;;-------Status Code-----------

;; Informational 1xx
(define *Continue* 100)
(define *Switch-Proto* 101)

;; Successful 2xx
(define *OK* 200)
(define *Created* 201)
(define *Accepted* 202)
(define *Non-Auth* 203)
(define *No-Content* 204)
(define *Reset* 205)
(define *Partial-Content* 206)

;; Redirection 3xx
(define *Multi-Choices* 300)
(define *Moved-Permanently* 301)
(define *Found* 302)
(define *See-Other* 303)
(define *Not-Modified* 304)
(define *Use-Proxy* 305)
;; 306 unused
(define *Temp-Redirect* 307)

;; Client Error 4xx
(define *Bad-Request* 400)
(define *Unauth* 401)
(define *Payment-Required* 402)
(define *Forbidden* 403)
(define *Not-Found* 404)
(define *Not-Allow* 405)
(define *Not-Accept* 406)
(define *Proxy-Auth-Required* 407)
(define *Request-Timeout* 408)
(define *Conflict* 409)
(define *Gone* 410)
(define *Length-Required* 411)
(define *Precondition-Failed* 412)
(define *Req-Too-Large* 413)
(define *URI-Too-Long* 414)
(define *Unsupported-Media-Type* 415)
(define *Range-Not-Satisfiable* 416)
(define *Expectation-Failed* 417)


;; Server Error 5xx
(define *Internal-Error* 500)
(define *Not-Implemented* 501)
(define *Bad-Gateway* 502)
(define *Service-Unavailable* 503)
(define *Gateway-Timeout* 504)
(define *Version-Not-Supported* 505)

;; Server Extended (only ragnarok)
(define *Fork-Error* 1500)
(define *CGI-Not-Allowed 1501)

;;----------Status Code End-------------

(define *status-list*
  '(;; 1xx
    (100 ("Continue" "*Status-100* Request Continue!" #f))
    (101 ("Switching Protocols" "*Status-101* Switching Protocol!" #f))
    ;; 2xx
    (200 ("OK" "*Status-200* Request's OK!" #f))
    (201 ("Created" "*Status-201* Created!" #f))
    (202 ("Accepted" "*Status-202* Accepted" #f))
    (203 ("Non-Authoritative Information" 
	  "*Status-203* Non-Authoritative Information" #f))
    (204 ("No Content" "*Status-204* No content!" #f))
    (205 ("Reset Content" "*Status-205* Reset Content!" "205.html"))
    (206 ("Partial Content" "*Status-206* Partial Content!" #f))
    ;; 3xx
    (300 ("Multiple Choices" "Status-300* Multiple Choices!" #f))
    (301 ("Moved Permanently" "Status-301* Moved Permanently!" "301.html"))
    (302 ("Found" "Status-302* Found!" #f))
    (303 ("See Other" "Status-303* See Other!" #f))
    (304 ("Not Modified" "Status-304* Not Modified!" #f))
    (305 ("Use Proxy" "Status-305* Use Proxy!" #f))
    ;; 306 unused
    (307 ("Temporary Redirect" "Status-307* Temporary Redirect!" #f))
    ;; 4xx
    (400 ("Bad Request" "*Status-400* Bad Request!" #f))
    (401 ("Unauthorized" "*Status-401* Unauthorized!" "401.html"))
    (402 ("Payment Required" "*Status-402* Payment Required!" #f))
    (403 ("Forbidden" "*Status-403* Forbidden" "403.html"))
    (404 ("Not Found" "*Status-404* Not Found!" "404.html"))
    (405 ("Method Not Allowed" "*Status-405* Method Not Allowed!" "405.html"))
    (406 ("Not Acceptable" "*Status-406* Not Acceptable!"  #f))
    (407 ("Proxy Authentication Required"
	  "*Status-407* Proxy Authentication Required!" #f))
    (408 ("Request Timeout" "*Status-408* Request Timeout!" "408.html"))
    (409 ("Conflict" "*Status-409* Conflict!" #f))
    (410 ("Gone" "*Status-410* Gone!" #f))
    (411 ("Length Required" "*Status-411* Length Required!" #f))
    (412 ("Precondition Failed" "*Status-412* Precondition Failed!" #f))
    (413 ("Request Entity Too Large" "*Status-413* Request Entity Too Large!" #f))
    (414 ("Request-URI Too Long" "*Status-414* Request-URI Too Long!" #f))
    (415 ("Unsupported Media Type" "*Status-415* Unsupported Media Type!" #f))
    (416 ("Requested Range Not Satisfiable" 
	  "*Status-416* Requested Range Not Satisfiable!" #f))
    (417 ("Expectation Failed" "*Status-417* Expectation Failed!" #f))
    ;; 5xx
    (500 ("Internal Server Error" "*Status-500* Internal Server Error!" "500.html"))
    (501 ("Not Implemented" "*Status-501* Not Implemented!" #f))
    (502 ("Bad Gateway" "*Status-502* Bad Gateway!" "502.html"))
    (503 ("Service Unavailable" "*Status-503* Service Unavailable!" "503.html"))
    (504 ("Gateway Timeout" "*Status-504* Gateway Timeout!" "504.html"))
    (505 ("HTTP Version Not Supported"
	  "*Status-505* HTTP Version Not Supported!" #f))
    ;; 15xx
    (1500 ("Fork Error" "*Status-1500* Fork Error!" #f))
    (1501 ("CGI-Not-Allowed" "*Status-1501* CGI Not Allowed" #f))
    ))

(define http-get-reason-from-status
  (lambda (status)
    (car (get-arg *status-list* status))))

(define http-get-stat-file-from-status
  (lambda (status)
    (caddr (get-arg *status-list* status))))

