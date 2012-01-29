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

(define-module (ragnarok info)
  #:use-module (srfi srfi-9)
  #:use-module (ragnarok version)
  #:use-module (ragnarok utils)
  )

(module-export-all! (current-module))

(define-record-type subserver-info
  (make-subserver-info server-port server-protocol 
		       server-name server-software
		       server-charset server-root)
  subserver-info?
  (server-port subserver-info:server-port)
  (server-protocol subserver-info:server-protocol)
  (server-name subserver-info:server-name)
  (server-software subserver-info:server-software)
  (server-charset subserver-info:server-charset)
  (server-root subserver-info:server-root)
  )

(define-record-type remote-info
  (make-remote-info remote-host remote-addr remote-ident
		    remote-user request-method query-string
		    auth-type content-length content-type
		    target)
  remote-info?
  (remote-host remote-info:remote-host)
  (remote-addr remote-info:remote-addr)
  (remote-ident remote-info:remote-ident)
  (remote-user remote-info:remote-user)
  (request-method remote-info:request-method)
  (query-string remote-info:query-string)
  (auth-type remote-info:auth-type)
  (content-length remote-info:content-length)
  (content-type remote-info:content-type)
  (target remote-info:target)
  )

(define-record-type server-info
  (make-server-info connect-info connect-socket
		    subserver-info remote-info)
  server-info?
  (connect-info server-info:connect-info)
  (connect-socket server-info:connect-socket)
  (subserver-info server-info:subserver-info)
  (remote-info server-info:remote-info)
  )
		    
