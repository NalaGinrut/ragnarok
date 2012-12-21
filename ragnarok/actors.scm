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
  #:use-module (ice-9 control)
  #:export ())

(define make-queue
  (lambda (capacity)
    (if capacity
	(make <mmr-queue>)
	(make <mmr-limit-queue> #:limit capacity))))

(define queue-in! in)
(define queue-out! out)

(define (make-mail-box capacity) (make-queue capacity))

(define mail-box-in! 
  (lambda (mbox msg)
    (queue-in! mbox msg)))

(define mail-box-out!
  (lambda (mbox)
    (queue-out! mbox)))
      
(define-syntax !
  (syntax-rules (<= :)
    ((_ to <= from : msg)
     (% (begin 
	  (to from msg)
	  (abort))
	(lambda (k) k)))))

;; 1. Not all actors need work/sleep queue, so we just make it when we need
;; 2. But consider the recycling of actors, we still provide 'add' methods for these queues
(define* (make-actor director #:key (name (gensym "actor-")) 
		     (box-room 100) (work-queue #f) (sleep-queue #f) (block-queue #f)
		     (boss #f) (script #f) (status 'ready))
  (let ((mail-box (make-mail-box box-room)))
    (lambda (self from . new-msg)
      (let lp((msg (mail-box-out! mail-box)))
	(cond
	 ((not msg)
	  (if boss
	      (sleep 10) ;; OK, I lied, boss can sleep when there's no work, but it won't response for any sleep message.
	      (! director <= self 'stun-me 1))) ;; no msg, sleep 1s
	 (boss
	  (case (cadr msg) ;; boss never response to these messages
	    ((free-me) (and fq (queue-in! fq from)))
	    ((sleep block) (error "boss never sleep/block!" m))
	    ((run) (error "boss is working, go back to work!" m))
	    ((yield) (lp (mail-box-out! mail-box))) ;; boss never yield, keep working
	    ;; TODO: add others?
	    )) 
	 (else
	  (let ((sender (car msg)) (m (cadr msg)))
	    (match m
	      (send (mail-box-in! (cons from new-msg)))
	      (run (set! status 'running) (! director <= self 'wake-up))
	      (sleep (set! status 'sleep) (! director <= self 'stun-me))
	      (block (set! status 'block) (! director <= self 'block-me))
	      (yield (set! status 'ready) (! director <= self 'need-rest))
	      (release (set! status 'free) (clear! mail-box) (! director <= self 'free-me))
	      (`(to-work-queue ,actor) (and wq (queue-in! wq actor)))
	      (out-work-queue (and wq (queue-out! wq)))
	      (`(to-sleep-queue ,actor) (and sq (queue-in! sq actor)))
	      (out-sleep-queue (and sq (queue-out! sq)))
	      (`(to-block-queue ,actor) (and bq (queue-in! bq actor)))
	      (out-block-queue (and bq (queue-out! bq)))
	      (`(set-work-queue ,wq) (set! work-queue wq))
	      (`(set-sleep-queue ,sq) (set! sleep-queue sq))
	      (`(take-over ,actor) (set! director actor))	      
	      ;; TODO: other msg handler
	      (else 
	       (cond
		(script (script director msg))
		(else ((fluid-ref *the-manager-script*) director msg))))))))
	(and (not boss) (! director <= self 'yield)) ;; yield if not boss
	(lp (mail-box-out! mail-box))))))

(define new-boss-actor
  (lambda (name script)
    (let ((boss (make-actor #f ;; boss' director is himself, we'll do it later
			    #:box-room #f ;; infinite actors it will control
			    #:work-queue (make-work-queue #f) ;; infinite workers
			    #:sleep-queue (make-sleep-queue #f) ;; infinite sleepers
			    #:block-queue (make-queue #f) ;; infinite blocked actors
			    #:boss #t ;; yes, he's the boss
			    #:script script ;; boss got the script
			    #:status 'running ;; boss' always working
			    )))
      (! boss <= 'take-over boss) ;; boss has to take over himself, poor man...
      boss ;; return the poor boss...
      )))

(define new-actor 
  (lambda (name script director workers)
    (let ((actor (make-actor director
			     #:box-room #f ;; infinite actors it will control
			     #:work-queue (make-work-queue workers) ;; infinite workers
			     #:sleep-queue (make-sleep-queue workers) ;; infinite sleepers
			     #:block-queue (make-queue workers) ;; infinite blocked actors
			     #:boss #f ;; no, just a worker :-(
			     #:script script ;; worker got the script
			     #:status 'ready ;; worker's ready
			     )))
      actor ;; return the new worker
      )))

(define *the-manager-script*
  (lambda (director msg)
    (let ((from (car msg)) (m (cadr msg)))
      (match m
	(wake-up (! director <= 'out-sleep-queue))
	(stun-me (! director <= 'to-sleep-queue from))
	(block-me (! director <= 'to-block-queue from))
	(need-rest (! director <= 'to-work-queue from))
	;; TODO: other msg handler for manager
	(else (error "wrong msg" m))))))
       
(define* (spawn #:key (name (gensym "actor-")) (script #f) (director #f) (workers 10))
  (if director 
      (new-actor name script director) ;; a new worker 
      (new-boss-actor name script workers)))
      



