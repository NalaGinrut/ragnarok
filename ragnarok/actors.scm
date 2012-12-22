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
	(make <mmr-limit-queue> #:limit capacity)
	(make <mmr-queue>))))
	
(define queue-in! in)
(define queue-out! out)

(define (make-mail-box capacity) (make-queue capacity))

(define mail-box-in! 
  (lambda (mbox msg)
    (queue-in! mbox msg)))

(define mail-box-out!
  (lambda (mbox)
    (queue-out! mbox)))
      
;; message engine
(define-syntax !
  (syntax-rules (<= :)
    ((_ to <= from : msg)
     (let ((actor (to to from msg)))
       (! actor)))
    ((_ to <= from : msg ? condition) ;; send and wait for 'condition'
     (let ((actor (to to from `(wait-for ,condition))))
       (! actor)))
    ((_ who) ;; schedule 'who'
     (! who <= who : `(schedule ,who))
     who)))

;; 1. Not all actors need work/sleep queue, so we just make it when we need
;; 2. But consider the recycling of actors, we still provide 'add' methods for these queues
(define* (make-actor director #:key (name (gensym "actor-")) 
		     (box-room 100) (boss #f) (script #f) (status 'ready)
		     (wq (make-queue #f)) (sq (make-queue #f)) 
		     (bq (make-queue #f)) (fq (make-queue #f)))
  (let ((mail-box (make-mail-box box-room)))
    (lambda (self from . new-msg)
      (let lp((msg (mail-box-out! mail-box)))
	(cond
	 ((not msg)
	  (if boss
	      (sleep 10) ;; OK, I lied, boss can sleep when there's no work, but it won't response for any sleep command.
	      (! director <= self `(schedule ,self)))) ;; no msg, re-schedule
	 ((eq? status 'blocked) (! from <= self `(resend ,m)))
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
	      (`(spawn ,name ,script ,director) 
	       (let ((actor (if fq (queue-out! fq) (error name "no a boss"))))
		 (if actor
		     (! actor <= director : `(attr-set! ,(list (name ,name) (script ,script) (director ,director))))
		     (new-actor name script director))))
	      (`(schedule ,actor) (queue-in! wq actor)) 
	      (run (set! status 'running) (! director <= self : 'wake-up))
	      (sleep (set! status 'sleep) (! director <= self : 'stun-me))
	      (block (set! status 'block) (! director <= self : 'block-me))
	      (yield (set! status 'ready) (queue-in! wq self))
	      (release (set! status 'free) (for-each clear! (list mail-box wq sq bq)) 
		       (! director <= self : 'free-me))
	      (`(attr-set! ,kv-pairs)
	       (for-each (lambda (a) 
			   (module-set! (current-module) (car k) (cadr v)))
			 kv-pairs))	      
	      (`(take-over ,actor) (set! director actor) (! self))	      
	      ;; TODO: other msg handler
	      (else 
	       (cond
		(script (script director msg))
		(else ((fluid-ref *the-manager-script*) director msg))))))))
	;; ENHANCEME: the policy is naive: schedule me after 'one msg' is handled
	(if (not boss)
	     (begin
	      (and (eq? status running) (set! status 'ready)) ;; need rest
	      (! director <= self : 'yield)) ;; yield if a running worker
	     (lp (mail-box-out! mail-box))))))) ;; boss keep working

(define new-boss-actor
  (lambda (name script)
    (let ((boss (make-actor #f ;; boss' director is himself, we'll do it later
			    #:name name
			    #:box-room #f ;; infinite actors it will control
			    #:wq (make-queue #f) ;; infinite workers
			    #:sq (make-queue #f) ;; infinite sleepers
			    #:bq (make-queue #f) ;; infinite blocked actors
			    #:fq (make-queue #f) ;; only boss has free-queue
			    #:boss #t ;; yes, he's the boss
			    #:script script ;; boss got the script
			    #:status 'running ;; boss' always working
			    )))
      ;; boss has to take over himself, poor man...
      ;; and this operation will return the boss as an actor
      (! boss <= 'take-over boss))))

(define new-actor 
  (lambda (name script director workers)
    (let ((actor (make-actor director #:name name
			     #:box-room #f ;; infinite actors it will control
			     #:wq (make-queue workers) ;; infinite workers
			     #:sq (make-queue workers) ;; infinite sleepers
			     #:bq (make-queue workers) ;; infinite blocked actors
			     #:fq #f ;; workers don't manage free-queue
			     #:boss #f ;; no, just a worker :-(
			     #:script script ;; worker got the script
			     #:status 'ready ;; worker's ready
			     )))
      (! director <= actor : 'yield) ;; return the new worker
      )))

(define *the-manager-script*
  (lambda (director msg)
    (let ((from (car msg)) (m (cadr msg)))
      (match m
	(wake-up (! director <= director : 'out-sleep-queue))
	(stun-me (! director <= director : 'to-sleep-queue from))
	(block-me (! director <= director : 'to-block-queue from))
	;; TODO: other msg handler for manager
	(else (error "wrong msg" m))))))
       
(define* (spawn #:key (name (gensym "actor-")) (script #f) (director #f) (workers 10))
  (if director 
      (new-actor name script director) ;; a new worker 
      (new-boss-actor name script workers)))
      



