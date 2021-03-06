;;  Copyright (C) 2011-2012  
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

(define-module (ragnarok utils)
  #:autoload (srfi srfi-1) (iota)
  #:autoload (rnrs base) (div div-and-mod)
  #:autoload (rnrs bytevectors) (string->utf8 bytevector-length)
  #:autoload (rnrs io ports) (get-bytevector-all get-string-all)
  #:autoload (rnrs) (enum-set-indexer)
  #:autoload (rnrs base) (assert)
  #:use-module (ragnarok lib time)
  #:use-module (ragnarok lib list)
  #:use-module (ragnarok lib tree)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 regex))

(module-export-all! (current-module))

(dynamic-call "init_lib" (dynamic-link "libragnarok"))

(define-syntax get-errno
  (syntax-rules (jumpkey withkey)
    ((_ e)
     (car (list-ref e 4)))
    ((_ jumpkey e)
     (car (list-ref (cdr e) 3)))))
   
(define F_ULOCK 0)	;; Unlock a previously locked region.
(define F_LOCK  1)	;; a region for exclusive use.  
(define F_TLOCK 2)	;; and lock a region for exclusive use.
(define F_TEST  3)	;; a region for other processes locks. 

(define get-config hash-ref)

(define touch 
  (lambda (file)
    (close (open file O_CREAT))))

(define port-add-flags
  (lambda (port flags)
    (fcntl port F_SETFL (logior (fcntl port F_GETFL) flags))))

(define set-port-non-block!
  (lambda (port)
    (port-add-flags port O_NONBLOCK)))

(define search-from-list
  (lambda (l e)
       (call/cc
	(lambda (return)
	  (let lp ((ll l) (n 0))
	    (cond
	     ((null? ll)
	      (return #f))
	     ((equal? (car ll) e)
	      (return n))
	     (else
	      (lp (cdr ll) (1+ n)))))))))

(define list-has? search-from-list)

(define-syntax space-skip
  (syntax-rules ()
    ((_ str . opt)
     (string-skip str #\space . opt))))

(define-syntax get-word 
  (syntax-rules ()
    ((_ str . opt)
     (let ([i (space-skip str . opt)])
       (substring/shared str i (string-contains str " " (1+ i))))))) 

(define-syntax get-word-list
  (syntax-rules ()
    ((_ str)
     (map string-trim-both (string-split str #\space)))
    ((_ str ch)
     (map string-trim-both (string-split str ch)))))

(define make-iterator
  (lambda (ll)
    (let ((rest ll))
      (lambda ()
	(let ((now (car rest)))
	  (set! rest (cdr rest))
	  now)))))

(define make-counter
  (lambda ()
    (let ((old 0))
      (lambda op
	(if (null? op)
	    (set! old (1+ old))
	    (case (car op)
	      ((init) (set! old 0))
	      ((now) old)
	      (else
	       (error make-counter "invalid op ~a!~%" (car op)))))))))

(define-syntax exchange
  (syntax-rules ()
    ((_ a b)
     (call-with-values 
	 (lambda () (values b a)) 
       (lambda (x y) (set! a x) (set! b y))))))

(define-syntax get-arg
  (syntax-rules ()
    ((_ args which)
     (let ([al (assoc-ref args which)])
       (if al
	   (car al)
	   #f)))))

(define-syntax add-to-list! 
  (syntax-rules ()
    ((_ a k v)
     (set! a
	   (apply assoc-set! `(,a 
			       k 
			       ,v))))))

(define-syntax remove-from-list! 
  (syntax-rules ()
    ((_ a k)
     (set! a
	   (apply assoc-remove! a k)))))
	   
(define-syntax get-file-ext		  
  (syntax-rules ()
    ((_ filename)
     (substring/shared filename
		       (1+ (string-index-right filename #\.))))))

(define-syntax get-request-mime
  (syntax-rules ()
    ((_ filename)
     (if (file-exists? filename)
	 (if (file-is-directory? filename)
	     '*directory*
	     (string->symbol (get-file-ext filename)))
	 '*no-such-file*))))

;; Iff k and base has same sign, result is non-negative. Vice versa.
;; This function is twice faster than (string->number (object->string n) base)
(define num->xbase
  (lambda (k base)
    (if (not (and (integer? base) (> base 1)))
	(error num->xbase "Invalid base! Base can not be 0 or 1!" base))
    (let* ([neg0 (or (negative? k) (negative? base))] 
	   [neg1 (not (and (negative? k) (negative? base)))] 
	   [neg (and neg0 neg1)]
	   [k (abs k)]
	   [base (abs base)])

      (let lp ((n k) (result 0) (i 1))
	(call-with-values 
	    (lambda () (div-and-mod n base))
	  (lambda (x y)
	    (let ([ret (+ result (* y i))])
	      (if (> x 0) 
		  (lp x ret (* i 10))
		  (if neg 
		      (- ret) 
		      ret)))))))))

;; FIXME: it's very slow, enhance it!
(define check-permits
  (lambda (perms mode)
    (case mode
      ((r) (not (zero? (logand perms #o444))))
      ((w) (not(zero? (logand perms #o222))))
      ((x) (not(zero? (logand perms #o111))))
      ((o+r) (not (zero? (logand perms #o004))))
      ((o+w) (not (zero? (logand perms #o002))))
      ((o+x) (not (zero? (logand perms #o001))))
      ((g+r) (not (zero? (logand perms #o040))))
      ((g+w) (not (zero? (logand perms #o020))))
      ((g+x) (not (zero? (logand perms #o010))))
      ((u+r) (not (zero? (logand perms #o400))))
      ((u+w) (not (zero? (logand perms #o200))))
      ((u+x) (not (zero? (logand perms #o100))))
      (else
       (error check-permits "Invalid mode!" mode)))))

(define-syntax make-perms-seeds
  (syntax-rules ()
    ((_ pm)
     (lambda (mode)
       (check-permits pm mode)))))

(define-syntax check-stat-perms
  (syntax-rules ()
    ((_ pm mode)
     (let ([seeds (make-perms-seeds pm)])
	  (or-map seeds mode)))))

(define get-file-size-str
  (lambda (bytes)
    (let ([units '("bytes" "M" "G" "T" "P")])
      (let lp ((sz bytes) (i 0))
	(if (>= sz 1024)
	    (lp (/ sz 1024) (1+ i))
	    (format #f "~5f~a" sz (list-ref units i)))))))

(define-syntax ->string
  (syntax-rules ()
    ((_ obj)
     (object->string obj display))))

(define read-count 
  (lambda (port num) 
    (let lp ([n num] [ret '()])
      (if (or (= n 0) (eof-object? (peek-char port))) 
	  (apply string (reverse ret)) 
	  (lp (1- n) (cons (read-char port) ret))))))

(define* (regexp-split regex str #:optional (flags 0))
  (let ((ret (fold-matches 
	      regex str (list '() 0 str)
	      (lambda (m prev)
		(let* ((ll (car prev))
		       (start (cadr prev))
		       (tail (match:suffix m))
		       (end (match:start m))
		       (s (substring/shared str start end))
		       (groups (map (lambda (n) (match:substring m n))
				    (iota (1- (match:count m))))))
		  (list `(,@ll ,s ,@groups) (match:end m) tail)))
	      flags)))
    `(,@(car ret) ,(caddr ret))))

(define timeout:second car)
(define timeout:msecond cdr)
(define format-timeout
  (lambda (timeout)
    (call-with-values
	(lambda ()
	  (div-and-mod timeout 1000))
      (lambda (second msecond)
	(cons second msecond)))))

(define-syntax make-enum-indexer
  (syntax-rules ()
    ((_ name sl)
     (define name
       (enum-set-indexer (make-enumeration sl))))))

(define (port-is-end? p)
  (eof-object? (peek-char p)))

(define (port-is-not-end? p)
  (not (port-is-end? p)))

(define-syntax-rule (with-locale i c e0 e1 ...)
  (let ([old (setlocale i)])
    (monitor
     (dynamic-wind
	 (lambda () (setlocale i c))
	 (lambda () (begin e0 e1 ...))
	 (lambda () (setlocale i old))))))

(define (time-get-time)
  (let ((tv (gettimeofday)))
    (+ (* (- (car tv) 1000000000) 1000) (div (cdr tv) 1000))))

(define-macro (call-with-return thunk) 
  `(call/cc (lambda (return) (,thunk))))


