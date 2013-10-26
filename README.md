
F-XML-RPC
=========

An Xml/Rpc client (making use of drakma) and server (using hunchentoot).

The encoding/decoding support is taken from S-XML-RPC

The http calls (client/server) are handled instead by the libraries drakma/hunchentoot

Calls are made using
* (call-xml-rpc-server keywords method &rest args)

Servers are by 
* (start-xml-rpc-server ...)

* (stop-xml-rpc-server)

Define exported methods 
* (define-xml-rpc-export name parameters &body body)

Over https
-------------

* generate certificates and keys
openssl genrsa -des3 -out server.key 1024
-> use the password "frank"

openssl req -new -key server.key -out server.csr

openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt

* start the server
(start-xml-rpc-server :port 8000 :ssl-certificate-file "server.crt" :ssl-privatekey-file "server.key" :ssl-privatekey-password "frank")

* try calling it
(call-xml-rpc-server (list :port 8000 :ssl t) "myfunction")


Frank James
2013

