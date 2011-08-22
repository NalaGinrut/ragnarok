;;  Copyright (C) 2011  
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

(define-module (ragnarok utils)
  )

(module-export-all! (current-module))

(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test) (begin result1 result2 ...)))))

(define make-iterator
  (lambda (ll)
    (let ((rest ll))
      (lambda ()
	(let ((now (car rest)))
	  (set! rest (cdr rest))
	  now
	  )))))

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
	       (error make-counter "invalid op ~a!~%" (car op))
	       )))))))

(define-syntax exchange
  (syntax-rules ()
    ((_ a b)
     (call-with-values 
	 (lambda () (values b a)) 
       (lambda (x y) (set! a x) (set! b y))
       ))))

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
	   (apply assoc-remove! a k)))
    ))
	   
(define-syntax get-file-ext		  
  (syntax-rules ()
    ((_ filename)
     (string-copy filename
		  (1+ (string-index-right filename #\.)))
    )))

(define-syntax get-request-mime
  (syntax-rules ()
    ((_ filename)
     (if (file-exists? filename)
	 (if (file-is-directory? filename)
	     '*directory*
	     (string->symbol (get-file-ext filename)))
	 '*no-such-file*
	 ))))

(define-syntax check-if-dir
  (syntax-rules ()
    ((_ filename)
     (