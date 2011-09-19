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

(define-module (ragnarok cgi guile)
  #:export (cgi-guile-serv-handler)
  )

(define cgi-guile-serv-handler
  (lambda (logger file)
    #t))


(define get-string-all (@ (rnrs io ports) get-string-all))
(define start-sign "<?gl ")
(define startd-sign "<?gld ")
(define end-sign " ?>")

(define in-filename cadr)
(define out-filename "out.html")
;;=============================================================
(define ss-len (string-length start-sign))
(define sd-len (string-length startd-sign))
(define es-len (string-length end-sign))

(define in-buf #f)
(define out-buf (format #f "(define output-port (open-file ~s \"w\"))~%" out-filename))
(define (close-output)
  (set! out-buf (string-append out-buf "(close output-port)"))) 

(define tpl-parser
  (lambda args
    (let* ([str-in (apply string-copy `(,in-buf ,@args))]
	   [pos (car args)]
	   [get-position
	    (lambda (sign)
		(let ([p (string-contains str-in sign)])
		  (if p
		      (+ p pos)
		      p)))]
	   [s (get-position start-sign)]
	   [e (get-position end-sign)]
	   [sd (get-position startd-sign)]
	   [in-len (string-length str-in)]
	   )
    (cond
     ((= in-len 0) #t) ;; recursive exit
     ((and (or s sd) 
	   (not e))
      (error tpl-parser "can't find ending!"))
     ((and (and (not s) (not sd)) 
	   e)
      (error tpl-parser "invalid template file!"))
     ((and (and (not s) (not sd))
	   (not e))
      (write-html-to-out-buf pos))
     ((and sd 
	   (or (not s) (< sd s))) ;; <?gld situation FIXME: I didn't consider sd-s-e, say, no sd-e 
      (write-html-to-out-buf pos sd)
      (write-script-display-to-out-buf (+ sd sd-len) e)
      (tpl-parser (+ e es-len)))
     (else
      (write-html-to-out-buf pos s)
      (write-script-to-out-buf (+ s ss-len) e)
      (tpl-parser (+ e es-len)))
     ))))
      
(define (parse-begin)
  (tpl-parser 0)
  (close-output)
  (deal-with-script))

(define (deal-with-script)
  (eval-string out-buf (current-module)))

(define write-script-display-to-out-buf
  (lambda args
    (let ([script-in (apply string-copy `(,in-buf ,@args))])
      (set! out-buf (string-append out-buf
				   "(format output-port \"~a\" "
				   script-in
				   " )"
				   )))))

(define write-script-to-out-buf
  (lambda args
    (let ([script-in (apply string-copy `(,in-buf ,@args))])
      (set! out-buf (string-append out-buf script-in)))))

(define write-html-to-out-buf
  (lambda args
    (let ([html-str (apply string-copy `(,in-buf ,@args))])
      (set! out-buf (string-append out-buf 
				   "(format output-port \"~a\" " 
				   (object->string html-str)
				   " )"
				   ))
      )))
    
(define main
  (lambda (args)
    (let ([fin (in-filename args)])
      (set! in-buf (get-string-all (open-input-file fin)))
      (parse-begin)
      )))
    