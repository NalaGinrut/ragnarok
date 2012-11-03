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

(define-module (ragnarok lib tree)
  #:use-module (srfi srfi-9)
  #:use-module (oop goops))

(module-export-all! (current-module))

(define-class <mmr-node> ()
  (parent #:init-value #f #:getter parent #:setter parent!)
  (left #:init-value #f #:getter left #:setter left!)
  (right #:init-value #f #:getter right #:setter right!)
  (key #:init-value #f #:getter key #:setter key!)
  (value #:init-value #f #:init-keyword #:value #:getter value #:setter value!))

(define-method (grand-parent (self <mmr-node>))
  (parent (or (parent self) (error "Invalid parent! Maybe root node?"))))

(define-method (grand-parent! (self <mmr-node>) (node <mmr-node>))
  (parent! (or (parent self) (error "Invalid parent! Maybe root node?")) node))

(define-class <mmr-tree> (<mmr-node>)
  (count #:init-value 0 #:getter count #:setter count!)
  (mutex #:init-thunk make-mutex #:getter mutex))

(define-method (count++ (self <mmr-tree>))
  (count! self (1+ (count self))))

(define-method (count-- (self <mmr-tree>))
  (and (<= (count self) 0) (error "rb-tree: count <= 0"))
  (count! self (1- (count self))))

(define-method (empty? (self <mmr-tree>))
  (and (not (right self)) (not (left self)))

(define-method (call-with-exclusivly (self <mmr-tree>) (thunk <procedure>))
  (with-mutex (mutex self) (thunk)))

(define-method (lock-tree! (self <mmr-tree>))
  (lock-mutex (mutex self)))

(define-method (unlock-tree! (self <mmr-tree>))
  (unlock-mutex (mutex self)))


;; ------- Red&Black tree
(define RED 0)
(define BLACK 1)

(define-class <mmr-rbnode> (<mmr-node>)
  (color #:init-value RED #:init-keyword #:color #:getter color #:setter color!))

(define-method (black? (self <mmr-rbnode>))
  (= (color self) BLACK))

(define-method (red? (self <mmr-rbnode>))
  (= (color self) RED))

(define-method (root? (self <mmr-rbnode>))
  (parent self))

(define-method (leaf? (self <mmr-rbnode>))
  (and (not (right self)) 
       (not (left self))
       (= (color self) BLACK)))

(define-class <mmr-rbtree> (<mmr-rbnode>))

(define-class (initialize (self <mmr-rbtree>) . args)
  (next-method)
  (color! self BLACK))  ;; root should be BLACK.

(define-method (empty? (self <mmr-rbtree>))
  (leaf? self))  ;; if root is leaf, it means an empty tree.

(define-method (insert (self <mmr-rbtree>) elem)
  (let ((node (make #:value elem)))
    (if (empty? self)
	(value! self elem)
	(begin
	  (rb-tree-insert-node self node)
	  (color! self BLACK))
	(count++ self)
	node)))

(define rb-tree-insert-node
  (lambda (self node)
    #t))

;; TODO: finish rbtree insert/search

