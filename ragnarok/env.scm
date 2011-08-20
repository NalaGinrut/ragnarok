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

(define-module (ragnarok env)
  #:use-module (oop goops)
  #:use-module (ragnarok handler)
  #:export (<env>
	    env:handler-list env:reload-handler-list
	    env:server-list)
  )

(define-class <env> ()

  ;; NOTE: Don't load handler-list in the <env> init. Do it when each
  ;;       <server> init. Then we have dynamic handler loader.
  (handler-list #:accessor env:handler-list #:allocation #:class)
  (server-list #:init-value '() #:accessor env:server-list
	       #:allocation #:class)
  )

(define-method (env:reload-handler-list (self <env>))
  (let ([handler-list (load-handler)])
    (set! (env:handler-list self) handler-list)))
