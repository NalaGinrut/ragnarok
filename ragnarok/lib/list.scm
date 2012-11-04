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

(define-module (ragnarok lib list)
  #:use-module (ice-9 q)
  #:use-module (oop goops))

(module-export-all! (current-module))

(define (list->q l) (cons l (last-pair l)))
(define (q->list q) (car q))

(define-class <mmr-list> ()
  (priv #:init-thunk make-q #:getter priv #:setter priv!) 
  (count #:init-value 0 #:getter count #:setter count!)
  (mutex #:init-thunk make-mutex #:getter mutex))

(define-method (initialize (self <mmr-list>) args)
  (next-method)
  (display args)
  (priv! self (list->q args)) 
  (count! self (q-length (priv self))))

(define-generic map)
(define-method (map (proc <procedure>) (self <mmr-list>) . args)
  (let* ((ll (if (null? args) args (map (lambda (x) (car (priv x))) args)))
	 (p (car (priv self)))
	 (v (list->q (apply map proc p ll))))
    (make <mmr-stack> #:value v)))

(define-method (lock-tree! (self <mmr-list>))
  (lock-mutex (mutex self)))

(define-method (unlock-tree! (self <mmr-list>))
  (unlock-mutex (mutex self)))

(define-method (call-with-exclusivly (self <mmr-list>) (thunk <procedure>))
  (with-mutex (mutex self) (thunk)))

(define-method (count++ (self <mmr-list>))
  (count! self (1+ (count self))))

(define-method (count-- (self <mmr-list>))
  (and (<= (count self) 0) (error "mmr-list: count <= 0"))
  (count! self (1- (count self))))

(define-method (empty? (self <mmr-list>))
  (or (and (q-empty? (priv self)) (= (count self) 0))
      (error "mmr-list: invalid empty list")))

(define-method (to-head! (self <mmr-list>) elem)
  (q-push! (priv self) elem)
  (count++ self))

;; NOTE: enq! won't traverse the whole list. It actually stored the pointer to the last cell.
;; ((1 2 3) 3), in this example, cdr 3 is not a simple number, it's the pointer of last cell of '(1 2 3)
;;       ^  |   And the 'magic' is last-pair, it can get the ponter of the last cell.
;;       |==|
(define-method (to-tail! (self <mmr-list>) elem)
  (enq! (priv self) elem)
  (count++ self))

(define-method (head-out! (self <mmr-list>))
  (if (empty? self)
      #f
      (begin
	(deq! (priv self))
	(count-- self))))

(define-method (head (self <mmr-list>))
  (q-front (priv self)))

(define-method (tail (self <mmr-list>))
  (q-rear (priv self)))

;;-------- queue 
(define-class <mmr-queue> (<mmr-list>))

(define-method (top (self <mmr-queue>))
  (tail self))

(define-method (in (self <mmr-queue>) elem)
  (to-tail! self elem))

(define-method (out (self <mmr-queue>))
  (head-out! self))

;;-------- stack
(define-class <mmr-stack> (<mmr-list>))

(define-method (top (self <mmr-stack>))
  (head self))

(define-method (push (self <mmr-stack>) elem)
  (to-head! self elem))

(define-method (pop (self <mmr-stack>))
  (head-out! self))



