
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



Frank James
2013

