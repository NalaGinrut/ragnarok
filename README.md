Ragnarok
========

Ragnarok is a generic server written with GNU Guile. 

The notable features listed here:
*  Unified epoll/select/kqueue interface
*  Concurrency based on threads
*  High performance concurrency with delimited-continuation based Actors-model (work in progress)
*  Object oriented 
   (yes, I'm the minority in Guile community who try to program with GOOPS, though FP features may cancel out most of OO features)
*  HTTP/1.1
*  Multi-protocols
*  Multi-languages
   (Guile is actually a dynamic compiler collection against the GCC who's the static one according to Andy's free speech. So multi-language must be the most fascinating feature.)
*  Guile/Scheme Template
*  Configurable
*  MIME
*  Logger
*  Standard CGI
*  Fast CGI (not sure whether worthy adding it since Guile has multi-language feature)
*  Static page and binary downloading service (of course) 

== Quick start

Note: now you have to try v0.0.3 since master is working on progress and may not run successfully.
1. git clone git@github.com:NalaGinrut/ragnarok.git
2. git checkout v0.0.3 
3. ./configure
4. make 
5. sudo make install
6. You may checkout /etc/ragnarok/server.conf to see the config file.

=== config file ===
+[http0] {
root-path : /var/www
protocol : http
status-show : log
max-request : 100000
listen : 8080
charset : utf-8
with-cgi : gl,scm
cgi : yes
}
===================

More details please review Wiki (coming soon).

Happy hacking!
