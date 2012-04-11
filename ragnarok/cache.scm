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

(define-module (ragnarok cache)
  #:use-module (ragnarok utils)
  #:export (get-cached-file))

;; NOTE: This is primitive cache core. It's very low-level.
;;       Use it in your cache system of your sub-server.

(define create-cached-file-path
  (lambda (cached-file)
    (create-this-path (dirname cached-file))))

(define is-cached-path-exist?
  (lambda (cached-file)
    (let ([dir (dirname cached-file)])
      (file-exists? dir))))

(define build-this-cached-file
  (lambda (cached-file)
    (if (not (is-cached-path-exist? cached-file))
	(create-cached-file-path cached-file))
    (open cached-file O_CREAT)))

;; FIXME: this build operation must be exclusive.
(define build-cache-file
  (lambda (cached-file generator)
    (let* ([content (generator cached-file)]
	   [cfd (build-this-cached-file cached-file)])
      (write content cfd)
      (close cfd))))

(define is-cached-file-expired?
  (lambda (cached-file)
    (let* ([st (stat cached-file)]
	   [mtime (stat:mtime st)]
	   [atime (stat:atime st)])
      (> (- mtime atime) 0))))

(define have-cached-file? file-exists?) 

;; get cached file and return its file-port
(define get-cached-file
  (lambda (target path suffix generator)
    (let ([cached-file
	   (string-append cache-path "/" target "." suffix)])
      (if (or (not (have-cached-file? cached-file))
	      (is-cached-file-expired? cached-file))
	  (build-cache-file cached-file cache-path suffix generator))
      (open cached-file O_RDONLY))))

