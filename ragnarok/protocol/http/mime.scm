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

(define-module (ragnarok protocol http mime)
  #:use-module (ragnarok utils)
  #:export (get-mime-handler
	    *mime-list*
	    )
  )

(define get-mime-handler
  (lambda (mime)
    (get-arg *mime-list* mime)))

;; FIXME: Well~this is tmp, I know it's urgly, but don't worry, I'll fix it.
(define html-serv-handler #f) 
  ;;(@@ (ragnarok protocol http http) http-static-page-serv-handler))

(define guile-serv-handler #f)


;; TODO: MIME handler should be dynamic registered.
(define *mime-list*
  `((html ,html-serv-handler)
    (gl ,guile-serv-handler)
    ))
