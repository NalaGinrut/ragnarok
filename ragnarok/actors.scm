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
	(make <mmr-limit-queue> #:limit capacity))))

(define mail-box-in!
  (lambda (mbox msg)
    (in mbox msg)))

(define mail-box-out!
  (lambda (mbox)
    (out mbox)))

(define-syntax !
  (syntax-rules (<= :)
    ((_ to <= from : msg)
     (begin 
       (to from msg)
       (abort)))))

;; 1. Not all actors need work/sleep queue, so we just make it when we need
;; 2. But consider the recycling of actors, we still provide 'add' methods for these queues
(define* (make-actor director #:key (name (gensym "actor-")) 
		     (box-room 100) (work-queue #f) (sleep-queue #f)
		     (boss #f))
  (let ((mail-box (make-mail-box box-room)))
    (lambda (self from . msg)
      (let lp((m (cadr (mail-box-out! mail-box))))
	(cond
	 ((not m) (! director <= self 'stun-me 1)) ;; no msg, sleep 1s
	 (boss
	  (case m
	    ((sleep block) (error "boss never sleep/block!" m))
	    ((run) (error "boss is working, go back to work!" m))
	    ((yield) (lp (mail-box-out! mail-box))))) ;; boss never yield, keep working
	 (else
	  (match m
	    (run (! director <= self 'wake-up))
	    (sleep (! director <= self 'stun-me))
	    (block (! director <= self 'block-me))
	    (send (mail-box-in! (cons from msg)))
	    (yield (and work-queue (work-queue-in! from))) 
	    (`(add-work-queue ,wq) (set! work-queue wq))
	    (`(add-sleep-queue ,sq) (set! sleep-queue sq))
	    ;; TODO: other msg handler
	    (else (error "wrong msg")))
	    (! director <= self 'yield) ;; yield
	    (lp (mail-box-out! mail-box)))))))))))
		   

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


