

(in-package #:asdf)

(defsystem :f-xml-rpc
  :name "F-XML-RPC"
  :author "Frank James <frank.a.james@gmail.com>"
  :version "1"
  :maintainer "Frank James <frank.a.james@gmail.com>"
  :licence "Lisp Lesser General Public License (LLGPL)"
  :description "Common Lisp Histogram package"
  :long-description "F-XML-RPC is an XML/RPC client/server using drakma/hunchentoot"

  :components
  ((:file "f-xml-rpc"))
  
  :depends-on (:drakma :hunchentoot :s-xml :ntlm))



