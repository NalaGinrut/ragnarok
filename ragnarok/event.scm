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

(define-module (ragnarok event)
  #:use-module (ragnarok utils)
  #:use-module (srfi srfi-9)
  #:export ()
  )

(define ragnarok-kickout-events
  (lambda (event-list event-set)
    ;; TODO: del every event of event-list from event-set
    #t
    ))

(define ragnarok-follow-events
  (lambda (event-list event-set)
    ;; TODO: add every event of event-list from event-set
    #t
    ))

