Ragnarok
========

Ragnarok is generic server which aimed to be a multi-protocols and high performance concurrency server framework.  
Its core written with C language, and use GNU Guile, the GNU official extension language for extensional part.

With Guile's multi-language features, it may support most of the main-stream languages in the long term, which means
you don't have to rewrite your program to get full performance of Guile.

Besides, Guile has the fine-intergrated delimited-continuation in its language core. It can be used for handling async-IO easily.
This feature (provided in language core) only supported by Scala/Ocaml/Haskell/Scheme45, and Guile (of course).

The last but not the least, Guile as Scheme is suitable for Actors-model since it was born by the idea to implement Actors.
And the most expectation is the Actors-model based on delimited-continuation could provide more performance with async-IO.

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
*  Guile/Scheme Template (it's practical and cool!)
*  Configurable
*  MIME
*  Logger
*  Standard CGI
*  Fast CGI (not sure whether worthy adding it since Guile has multi-language feature)
*  Static page and binary downloading service (of course) 

== Quick start

Note: now you have to try v0.0.3 since master is working on progress and may not run successfully.

1. git clone git://github.com/NalaGinrut/ragnarok.git
2. git checkout v0.0.3 
3. ./configure
4. make 
5. sudo make install
6. You may checkout /etc/ragnarok/server.conf to see the config file.

=== config file ===
``` js
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
```
=== template ===

And you may try the Guile template as well:
``` scheme
<html>
<% (if (= 1 1) (begin %>
<p>asdf: <%= (+ 1 1) %></p>
<% )) %>

<% (let ((test-me (expt 3 8))) %>
<p><%= test-me %></p>
<% ) %>
</html>
```

And you get the output as:
``` html
<html>
<p>asdf: 2</p>
<p>6561</p>
</html>
```

More details please review Wiki (coming soon).

Happy hacking!
