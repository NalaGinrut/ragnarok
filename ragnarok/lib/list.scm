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
  #:use-module (oop goops)
  #:export (<mmr-list>
	    mmr-list:head! mmr-list:tail! mmr-list:head-out! mmr-list:tail-out!
	    <mmr-queue> 
	    <mmr-stack>)

(define-class <mmr-list> ()
  (priv #:init-thunk make-q #:accessor mmr-list:priv))

(define-method (mmr-list:head! (self <mmr-list>) elem)
  (q-push! (mmr-list:priv self) elem))

(define-method (mmr-list:tail! (self <mmr-list) elem)
  (enq! (mmr-list:priv self) elem))

(define-method (mmr-list:head-out! (self <mmr-list>))
  (deq! (mmr-list:priv self)))

(define-method (mmr-list:tail-out! (self <mmr-list>))
  (drop-right! (mmr-list:priv self) 1))


