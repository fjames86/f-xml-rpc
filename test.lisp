

(defpackage :test
  (:use :cl :f-xml-rpc))

(in-package :test)

;; start a server on port 8000 using https, accepting only connections
;; with an authorization handler only accepting the user "frank" with password "james". 

(start-xml-rpc-server :port 8000
					  :auth-handler
					  (lambda (username password)
						(and (string= username "frank")
							 (string= password "james")))
					  :ssl-certificate-file "mycert.crt"
					  :ssl-privatekey-file "mykey.key"
					  :ssl-privatekey-password "frank")

;; define an exported function
(define-xml-rpc-export |test.myfun| (name)
  (format nil "Hello ~A!" name))

;; try calling it
(call-xml-rpc-server (list :port 8000
						   :authorization (list "frank" "james")
						   :ssl t)
					 "test.myfun")

;; define a calling function
(define-xml-rpc-call myfun (name)
  :port 8000 :authorization (list "frank" "james") :method "test.myfun" :ssl t)

;; call it
(myfun "frank")

;; try calling from another client, e.g. Python

;; stop the server
(stop-xml-rpc-server)


