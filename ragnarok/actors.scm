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
  #:use-module (ragnarok error)
  #:use-module (ice-9 control)
  #:export ())

(define (table-in! tb k v)
  (hash-set! tb k v))

(define (table-out! tb k)
  (let ((v (hash-ref tb k)))
    (hash-set! tb k #f)
    v))

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

;; stored-message-format := (original-actor msg-format)
;; msg-format            := (msg-name msg-content)      

(define from-who car)
(define msg-content caddr)

;; message engine
(define-syntax !
  (syntax-rules (<= :)
    ((_ to <= from : msg)
     (let ((actor (% (to to from msg)
		     (lambda (k) k))))
       (if (thunk? actor) ;; if #t, then it's a delimited-continuation
	   (to to to 'would-block actor) ;; add itself to boss' bt
	   (! actor))))
    ((_ who) ;; schedule 'who'
     (! who <= who : 'yield))))

;; 1. Not all actors need work/sleep queue, so we just make it when we need
;; 2. But consider the recycling of actors, we still provide 'add' methods for these queues
;; 3. Status:  ready/sleep/blocked/free
;; 4. NOTE: we don't have explicit 'running' status, if it's working in progress,
;;          it's runing, but we don't have a status valued 'running', only 'ready'
;;          actor could be a 'running' one.
(define* (make-actor director #:key (name (gensym "actor-")) (wait-for #f)
		     (box-room 100) (boss #f) (script #f) (status 'ready)
		     (wq (make-queue #f)) (fq (make-queue #f)) (bt (make-hash-table)))
  (let ((mail-box (make-mail-box box-room))
	(start-sleep 0)	(sleep-time 0))
    (lambda (self from . new-msg)
      (let lp((msg (mail-box-out! mail-box)))
	(cond
	 ((not msg)
	  (if boss
	      (sleep 10) ;; OK, I lied, boss can sleep when there's no work, but it won't response for any sleep command.
	      (! director <= self `(schedule ,self)))) ;; no msg, re-schedule
	 ((eq? status 'blocked)
	  (if wait-for
	      (catch #t (lambda () (peek-char wait-for))
		(lambda e
		  (let ((E (get-errno e)))
		    (cond
		     ((or (= E EAGAIN) (= E EWOULDBLOCK)) 
		      (! from <= self `(resend ,m))) ;; resend if still blocked
		     (else (set! wait-for #f) (! director <= self 'raise-me))))))))
	 ((eq? status 'sleep)
	  (cond
	   ((>= (- (current-time) start-sleep) sleep-time)
	    (set! sleep-time 0) (set! start-sleep 0)
	    (! self <= director : 'run))))
	 ((eq? status 'free) (! from <= self 'dead)) ;; dead must handle in script
	 (boss
	  (match (msg-content msg) ;; boss never response to these messages
	    ('wake-up (queue-in! wq (from-who msg)))
	    ('stun-me (queue-in! sq (from-who msg)))
	    (`(block-me ,dc) (table-in! bt (from-who msg) dc))
	    ('raise-me ((table-out! bt (from-who msg)))) ;; raise the dc from bt then run it
	    ('free-me (and fq (queue-in! fq (from-who msg))))
	    ('schedule-me (queue-in! wq (from-who msg)))
	    ('sleep (error "boss never sleep!" m))
	    ('block (error "boss never block!" m))
	    ('run (error "boss is working, go back to work!" m))
	    ('yield (lp (mail-box-out! mail-box))) ;; boss never yield, keep working
	    ;; TODO: add others?
	    ))	 
	 (else
	  (let ((sender (car msg)) (m (cadr msg)))
	    (match m
	      ('send (mail-box-in! (cons from new-msg)))
	      (`(spawn ,name ,script ,director) 
	       (let ((actor (if fq (queue-out! fq) (error name "no a boss"))))
		 (if actor
		     (! actor <= director : `(attr-set! ,(list (name ,name) (script ,script) (director ,director))))
		     (new-actor name script director))))
	      ('run (set! status 'ready) (! director <= self : 'wake-up))
	      (`(sleep ,time) 
	       (set! status 'sleep)
	       (set! sleep-time time)
	       (set! start-sleep (current-time))
	       (! director <= self : 'stun-me))
	      (`(would-block ,dc) (set! status 'blocked) (! director <= self : 'block-me dc))
	      ('yield (set! status 'ready) (! director <= self : 'schedule-me))
	      ('release (set! status 'free) 
		       (for-each clear! (list mail-box wq sq bq)) 
		       (! director <= self : 'free-me))
	      (`(attr-set! ,kv-pairs)
	       (for-each (lambda (a) 
			   (module-set! (current-module) (car k) (cadr v)))
			 kv-pairs))	      
	      ('take-over (set! director sender) (! self))	      
	      ;; TODO: other msg handler
	      (else (and script (script self director from msg)))))))
	;; ENHANCEME: the policy is naive: schedule me after 'one msg' is handled
	(if (not boss) ;; boss don't yield
	    (! director <= self : 'yield) ;; yield if a running worker
	    (lp (mail-box-out! mail-box))))))) ;; boss keep working

(define new-boss-actor
  (lambda (name script)
    (let ((boss (make-actor #f ;; boss' director is himself, we'll do it later
			    #:name name
			    #:wait-for #f ;; never used by boss
			    #:box-room #f ;; infinite actors it will control
			    #:wq (make-queue #f) ;; infinite workers
			    #:bq (make-queue #f) ;; infinite blocked actors
			    #:fq (make-queue #f) ;; only boss has free-queue
			    #:boss #t ;; yes, he's the boss
			    #:script script ;; boss got the script
			    #:status 'running ;; boss' always working
			    )))
      ;; boss has to take over himself, poor man...
      ;; and this operation will return the boss as an actor
      (! boss <= boss : 'take-over))))

(define new-actor 
  (lambda (name script director workers)
    (let ((actor (make-actor director #:name name
			     #:wait-for #f ;; the buffered port which is blocked, and need break
			     #:box-room #f ;; infinite actors it will control
			     #:wq (make-queue workers) ;; infinite workers
			     #:bq (make-queue workers) ;; infinite blocked actors
			     #:fq #f ;; workers don't manage free-queue
			     #:boss #f ;; no, just a worker :-(
			     #:script script ;; worker got the script
			     #:status 'ready ;; worker's ready
			     )))
      (! director <= actor : 'yield) ;; return the new worker
      )))

(define-syntax script-wrapper
  (syntax-rules (<=)
    ((_ <= pattern patterns* ...)
     (lambda (script self director from msg)
       (cond
	((not msg) (error "invalid message" msg))
	(else
	 (match (msg-content m)
	   pattern
	   patterns*
	   ...
	   ('resend 'ignore) ;; default resend handler
	   ('dead 'ignore) ;; default dead handler
	   (else (error "invalid pattern" m)))))))))

(define* (spawn #:key (name (gensym "actor-")) (script #f) (director #f) (workers 10))
  (if director 
      (new-actor name (script-wrapper script) director) ;; a new worker 
      (new-boss-actor name (script-wrapper script) workers)))
      



