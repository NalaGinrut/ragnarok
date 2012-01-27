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

(define-module (ragnarok event)
  #:use-module (ragnarok utils)
  #:use-module (ragnarok error)
  #:use-module (srfi srfi-9)
  )

(module-export-all! (current-module))

(dynamic-call "init_event_module" (dynamic-link "libragnarok-event"))

;; TODO: 1. three lists for read/write/except 
;;       2. add read/write/except event into correspding list
;;       3. remember ragnarok-event-init returns values
;;       4. ragnarok-event-handler returns lists for ready fd
;;       5. enumerate read/write list and check if fd of the event elem
;;          equal to the ready-fd-list.
;; FIXME: how to determine a fd has both read and write type from ready-fd-list?
;;        that maybe cause a read-fd be written if it can be written.
;;        I think the IO operation would return EWOULDBREAK or something else.
;;        Then I could igore this operation. Anyway, it's a thread, and it'll be 
;;        over soon. (Consider the thread creating overhead, this must optimze later)

(define ragnarok-do-with-events
  (lambda (event-list event-set op)
    (for-each (lambda (e)
		(op e event-set))
	      event-list)))

(define ragnarok-kickout-events
  (lambda (del-list event-set)
    "del every event of del-list from event-set"
    (ragnarok-do-with-events del-list 
			     event-set
			     ragnarok-event-del)))

(define ragnarok-follow-events
  (lambda (add-list event-set)
    "add every event of event-list from event-set"
    (ragnarok-do-with-events add-list
			     event-set
			     ragnarok-event-add)))

(define make-event-enum-indexer
  (lambda (sl)
    (enum-set-indexer (make-enumeration sl))))

(define make-event-status-enum-indexer
  (lambda (sl)
    (make-event-status-enum-indexer sl)))

(define *event-status-list*
  '(wait block sleep dead ready clear unknown))

(define event-status-index
  (make-event-status-enum-indexer *event-status-list*))
  
(define make-event-type-enum-indexer
  (lambda (tl)
    (make-event-enum-indexer tl)))
  
(define *event-type-list*
  '(read write except unknown))

(define event-type-index
  (make-event-type-enum-indexer *event-type-list*))

(define* (ragnarok-event-create #:key
				(type 'unknown)
				(status 'unknown)
				(fd #f))
  (if (or (not fd) (< fd 0))
      (ragnarok-throw "invalid fd:~a~%" fd)
      (ragnarok-make-meta-event (event-type-index type)
				(event-status-index status)
				fd)))

(define-syntax-rule (ragnarok-event-from-socket socket type)
  (ragnarok-event-create #:type type #:status 'ready #:fd (port->fdes socket)))

