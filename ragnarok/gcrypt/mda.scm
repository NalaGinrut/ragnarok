;;  Copyright (C) 2011  
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


(define-module (ragnarok gcrypt mda)
  #:use-module (ragnarok utils)
  #:export (
	    gcrypt:md5
	    gcrypt:sha1
;;	    gcrypt:rmd160
;;	    gcrypt:md2
;;	    gcrypt:tiger
;;	    gcrypt:haval
	    gcrypt:sha256
	    gcrypt:sha384
	    gcrypt:sha512
	    gcrypt:sha224
	    gcrypt:md4 
	    gcrypt:crc32
	    gcrypt:crc32-rfc1510
	    gcrypt:crc24-rfc2440
;;	    gcrypt:whirlpool 
;;	    gcrypt:tiger1
;;	    gcrypt:tiger2
	    )
  )

;; (gcrypt:algo str) ==> digest string
  
;; if you don't understand these, checkout the documentation of libgcrypt
(define GCRY_MD_NONE 0)
(define GCRY_MD_MD5 1)
(define GCRY_MD_SHA1    2)
(define GCRY_MD_RMD160  3) ;; no implementation yet
(define GCRY_MD_MD2     5) ;; no implementation yet
(define GCRY_MD_TIGER   6) ;; no implementation yet 
(define GCRY_MD_HAVAL   7) ;; no implementation yet 
(define GCRY_MD_SHA256  8)
(define GCRY_MD_SHA384  9)
(define GCRY_MD_SHA512  10)
(define GCRY_MD_SHA224  11)
(define GCRY_MD_MD4     301)
(define GCRY_MD_CRC32   302)
(define GCRY_MD_CRC32_RFC1510 303)
(define GCRY_MD_CRC24_RFC2440 304)
(define GCRY_MD_WHIRLPOOL 305) ;; no implementation yet
(define GCRY_MD_TIGER1  306) ;; no implementation yet
(define GCRY_MD_TIGER2  307) ;; no implementation yet


(define-syntax mda-dispatch
  (syntax-rules ()
    ((_ algo)
     (lambda (str)
       (gcrypt:mda str algo)))))
 
(define gcrypt:md5 (mda-dispatch GCRY_MD_MD5))
(define gcrypt:sha1 (mda-dispatch GCRY_MD_SHA1))
;;(define gcrypt:rmd160 (mda-dispatch GCRY_MD_RMD160))
;;(define gcrypt:md2 (mda-dispatch GCRY_MD_MD2))
;;(define gcrypt:tiger (mda-dispatch GCRY_MD_TIGER))
;;(define gcrypt:haval (mda-dispatch GCRY_MD_HAVAL))
(define gcrypt:sha256 (mda-dispatch GCRY_MD_SHA256))
(define gcrypt:sha384 (mda-dispatch GCRY_MD_SHA384))
(define gcrypt:sha512 (mda-dispatch GCRY_MD_SHA512))
(define gcrypt:sha224 (mda-dispatch GCRY_MD_SHA224))
(define gcrypt:md4 (mda-dispatch GCRY_MD_MD4))
(define gcrypt:crc32 (mda-dispatch GCRY_MD_CRC32))
(define gcrypt:crc32-rfc1510 (mda-dispatch GCRY_MD_CRC32_RFC1510))
(define gcrypt:crc24-rfc2440 (mda-dispatch GCRY_MD_CRC24_RFC2440))
;;(define gcrypt:whirlpool (mda-dispatch GCRY_MD_WHIRLPOOL))
;;(define gcrypt:tiger1 (mda-dispatch GCRY_MD_TIGER1))
;;(define gcrypt:tiger2 (mda-dispatch GCRY_MD_TIGER2))

