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

(define-module (ragnarok actors)
  #:use-module (ragnarok utils)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 control)
  #:export ())

(define (make-sleeper ctx thread seconds)
  (let ((waketime (+ (get-internal-real-time)
		     (inexact->exact
		      (round (* seconds internal-time-units-per-second))))))
    (let lp ((head '()) (tail (econtext-sleepers ctx)))
      (if (and (pair? tail) (> waketime (cdar tail)))
	  (lp (cons (car tail) head) (cdr tail))
	  (set-econtext-sleepers!
	   ctx
	   (append-reverse! head (acons thread waketime tail)))))))

(define (actor-sleep seconds)
  (lambda (director actor)
    (let ((s (make-sleeper seconds)))
      (! director <= actor 'stun-me s)))))

(define make-mail-box
  (lambda (capacity)
    (if capacity
	(make <mmr-queue>)
	(make <mmr-queue>) ;; FIXME: implement a limited queue
	)))

(define-syntax !
  (syntax-rules (<= :)
    ((_ actor2 <= actor1 : msg)
     ((actor1) self msg actor1))))

(define* (make-actor director #:key (name (gensym "actor-")) (capacity #f))
  (let ((mail-box (make-mail-box capacity)))
    (lambda (self msg from)
      (reset
       (let lp()
	 (case (mail-box-out! mail-box)
	   ((run) (shift k (k (! director <= self 'wake-up))))
	   ((sleep) (shift k (k (! director <= self 'stun-me))))
	   ((send) (shift k (k (mail-box-in! (cons from msg)))))
	   ;; TODO: other msg handler
	   (else (error "wrong msg"))))))))
		   

(define* (make-scenario GOD #:key (name (gensym "scenario-")))
  (lambda (script)
    (reset
     (let lp()
       (match script
	 (`(,actor *RUN*) (shift k (k (! actor <= GOD 'run))))
	 (`(,actor *SLEEP* ,time) (shift k (k (! actor <= GOD : 'sleep))))
	 (`(actor ,*DEAD*) #! do dead, just send a msg to add itself to the GOD-actor to add it to its free list !#)
	 ;; do other scenario
	 (else (act script)))
       (lp)))))


