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
  #:use-module (ragnarok utils)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 threads)
  #:use-module (oop goops))

(module-export-all! (current-module))

;; naive default compare function, only support integers.
;; you must set your own compare function after class made.
(define *default-compare*
  (lambda (x y)
    (- x y)))

(define-class <mmr-node> ()
  (compare #:init-value *default-compare* #:init-keyword #:compare #:getter compare #:setter compare!)
  (parent #:init-value #f #:getter parent #:setter parent!)
  (left #:init-value #f #:getter left #:setter left!)
  (right #:init-value #f #:getter right #:setter right!)
  (key #:init-value #f #:init-keyword #:key #:getter key #:setter key!)
  (value #:init-value #f #:init-keyword #:value #:getter value #:setter value!))

(define-method (grand-parent (self <mmr-node>))
  (parent (or (parent self) (error "Invalid parent! Maybe root node?"))))

(define-method (grand-parent! (self <mmr-node>) (node <mmr-node>))
  (parent! (or (parent self) (error "Invalid parent! Maybe root node?")) node))

(define-class <mmr-tree> (<mmr-node>)
  (count #:init-value 0 #:getter count #:setter count!)
  (mutex #:init-thunk make-mutex #:getter mutex))

(define-method (count++! (self <mmr-tree>))
  (count! self (1+ (count self))))

(define-method (count--! (self <mmr-tree>))
  (and (<= (count self) 0) (error "rb-tree: count <= 0"))
  (count! self (1- (count self))))

(define-method (root? (self <mmr-tree>))
  (not (parent self)))

(define-method (empty? (self <mmr-tree>))
  (and (root? self) (not (value self)) 
       (not (right self)) (not (left self))))

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
  (color #:init-value BLACK #:init-keyword #:color #:getter color #:setter color!))

(define-method (black? (self <mmr-rbnode>))
  (= (color self) BLACK))

(define-method (red? (self <mmr-rbnode>))
  (= (color self) RED))

(define-method (root? (self <mmr-rbnode>))
  (and (not (parent self)) 
       (or (black? self) (error "bad root node!" self))))

(define-method (leaf? (self <mmr-rbnode>))
  (and (not (right self)) 
       (not (left self))
       (= (color self) BLACK)))

(define new-black-node
  (lambda (k v)
    (make <mmr-rbnode> #:color BLACK #:key k #:value v)))

(define new-red-node
  (lambda (k v)
    (make <mmr-rbnode> #:color RED #:key k #:value v)))

(define new-rb-root-node new-black-node)
(define new-rb-node new-red-node)

;;--- rb tree ---
(define-class <mmr-rbtree> (<mmr-tree> <mmr-rbnode>))

(define-method (empty? (self <mmr-rbtree>))
  (leaf? self))  ;; if root is leaf, it means an empty tree.

(define-method (insert! (self <mmr-rbtree>) k v)
  (if (empty? self)
      (begin
	(value! self v)
	(key! self k)
	(color! self BLACK))
      (insert-node! self (make #:key k #:value v)))
  (count++! self))

(define insert-node! 
  (lambda (rbt node)
    (let ((it (inner-insert! rbt (key node) (value node))))
      (inner-fixup it)
      (color! rbt BLACK))))
  
(define-method (search (self <mmr-rbtree>) key)
  (if (empty? self) 
      #f
      (let ((r ((compare self) key (key self))))
	(cond
	 ((> r 0) (search (right self) key))
	 ((< r 0) (search (left self) key))
	 (else (value self))))))

(define insert-left!
  (lambda (rbt k v)
    (left! rbt (new-rb-node k v))
    (parent! (left rbt) rbt)))

(define insert-right!
  (lambda (rbt k v)
    (right! rbt (new-rb-node k v))
    (parent! (right rbt) rbt)))

(define inner-insert!
  (lambda (rbt k v)
    (let ((r ((compare rbt) k v)))
    (cond
     ((< r 0) 
      (if (leaf? (left rbt))
	  (begin
	    (insert-left! rbt k v)
	    rbt)
	  (inner-insert! (left rbt) k v)))
     ((> r 0) 
      (if (leaf? (right rbt))
	  (begin
	    (insert-right! rbt k v)
	    rbt)
	  (inner-insert! (right rbt) k v)))
     (else (value! rbt v) rbt)))))

(define inner-fixup
  (lambda (rbt)
    (cond 
     ((and (red? (right rbt))
           (red? (left rbt)))
      (rb-tree-flip-color rbt)
      (inner-fixup rbt))
     ((and (red? (right rbt))
           (black? (left rbt)))
      (inner-fixup (rb-tree-rotate-left-1 rbt)))
     ((and (red? (left rbt))
           (red? (left (left rbt))))
      (inner-fixup (rb-tree-rotate-right-1 rbt)))
     ((root? rbt) rbt)
     (else (inner-fixup (parent rbt))))))

(define color-flip!
  (lambda (n)
    (unless (leaf? n)
      (let ([c (color n)])
	(color! n (flip c)))))) 

(define flip
  (lambda (x)
    (logxor x 1)))

(define rb-tree-flip-color
  (lambda (nn)
    (color-flip! nn)
    (color-flip! (left nn))
    (color-flip! (right nn))))

(define set-parent!
  (lambda (n p)
    (unless (leaf? n)
            (parent! n p))))

(define set-child-left!
  (lambda (n1 n2)
    (unless (leaf? n1)
            (left! n1 n2))))

(define set-child-right!
  (lambda (n1 n2)
    (unless (leaf? n1)
            (right! n1 n2))))

(define right-child?
  (lambda (n)
    (if (leaf? n)
        #f
        (let ([np (parent n)])
          (if (leaf? np)
              #f
              (eq? (right np) n))))))

(define set-red!
  (lambda (n)
    (unless (leaf? n)
            (color! n RED))))

(define set-black!
  (lambda (n)
    (unless (leaf? n)
            (color! n BLACK))))

;; n1->color = n2->color
(define get-color-from!
  (lambda (n1 n2)
    (unless (leaf? n1)
            (if (leaf? n2)
                (set-black! n1)
                (color! n1 (color n2))))))

(define rb-tree-rotate-left-1
  (lambda (rbt)
    (let ((x (right rbt)))
      (right! rbt (left x))  ;; rbt->right = x->left
      (set-parent! (right rbt) rbt)  ;; rbt->right->parent = rbt
      
      (left! x rbt)  ;; x->left = rbt
      
      (if (right-child? rbt)
	  (set-child-right! (parent rbt) x)
	  (set-child-left! (parent rbt) x))
      
      (set-parent! x (parent rbt))  ;; x->parent = rbt->parent
      (set-parent! rbt x)  ;; rbt->parent = x
      
      (get-color-from! x rbt)
      (set-red! rbt)
      ;;(format #t "rb-tree-rotate-left: en:~a~%" x)
      x)))

(define rb-tree-rotate-right-1
  (lambda (rbt)
    (let ((x (left rbt)))
      (left! rbt (right x))  ;; rbt->left = x->right
      (set-parent! (left rbt) rbt)  ;; rbt->left->parent = rbt

      (right! x rbt)  ;; x->right = rbt

      (if (right-child? rbt)
          (set-child-right! (parent rbt) x)
          (set-child-left! (parent rbt) x))

      (set-parent! x (parent rbt)) ;; x->parent = rbt->parent
      (set-parent! rbt x)  ;; rbt->parent = x
      
      (get-color-from! x rbt)
      (set-black! rbt)
      ;;(format #t "rb-tree-rotate-right: en:~a~%" x)
      x)))

(define move-red-left
  (lambda (rbt)
    (rb-tree-flip-color rbt)
    (if (red? (left (right rbt)))
	(begin
	  (right! rbt (rb-tree-rotate-right-1 (right rbt)))
	  (set! rbt (rb-tree-rotate-left-1 rbt))
	  (rb-tree-flip-color rbt))
	rbt)))

(define move-red-right
  (lambda (rbt)
    (rb-tree-flip-color rbt)
    (if (red? (left (left rbt)))
	(begin
	  (set! rbt (rb-tree-rotate-right-1 rbt))
	  (rb-tree-flip-color rbt))
	rbt)))

;; TODO: finish rbtree insert/search

