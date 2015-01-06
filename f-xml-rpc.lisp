
(defpackage :f-xml-rpc
  (:use :cl :ntlm)
  (:nicknames :fxpc)
  (:export #:call-xml-rpc-server
		   #:encode-xml-rpc-call
		   #:xml-rpc-call
		   #:*authorization*
		   
		   #:make-xml-rpc-server 
		   #:start-xml-rpc-server
		   #:stop-xml-rpc-server
		   #:export-system-methods 
		   
		   #:xml-rpc-error
		   
		   #:make-xml-rpc-struct
		   #:xml-rpc-struct
		   #:xml-rpc-struct-p
		   #:xml-rpc-struct-member
		   #:xml-rpc-struct-alist 
		   
		   #:define-xml-rpc-export
		   #:define-xml-rpc-call

           #:struct-member
           #:call
           #:encode
           #:call*))

(defpackage :f-xml-rpc-exports)

(in-package :f-xml-rpc)

;;; conditions

(define-condition xml-rpc-condition (error)
  ()
  (:documentation "Parent condition for all conditions thrown by the XML-RPC package"))

(define-condition xml-rpc-fault (xml-rpc-condition)
  ((code :initarg :code :reader xml-rpc-fault-code)
   (string :initarg :string :reader xml-rpc-fault-string))
  (:report (lambda (condition stream)
			 (format stream
					 "XML-RPC fault with message '~a' and code ~d."
					 (xml-rpc-fault-string condition)
					 (xml-rpc-fault-code condition))))
  (:documentation "This condition is thrown when the XML-RPC server returns a fault"))

(setf (documentation 'xml-rpc-fault-code 'function) "Get the code from an XML-RPC fault")
(setf (documentation 'xml-rpc-fault-string 'function) "Get the string from an XML-RPC fault")

(define-condition xml-rpc-error (xml-rpc-condition)
  ((place :initarg :code :reader xml-rpc-error-place)
   (data :initarg :data :reader xml-rpc-error-data))
  (:report (lambda (condition stream)
			 (format stream
					 "XML-RPC error ~a at ~a."
					 (xml-rpc-error-data condition)
					 (xml-rpc-error-place condition))))
  (:documentation "This condition is thrown when an XML-RPC protocol error occurs"))

(setf (documentation 'xml-rpc-error-place 'function)
      "Get the place from an XML-RPC error"
      (documentation 'xml-rpc-error-data 'function)
      "Get the data from an XML-RPC error")

;;; iso8601 support (the xml-rpc variant)

(defun universal-time->iso8601 (time &optional (stream nil))
  "Convert a Common Lisp universal time to a string in the XML-RPC variant of ISO8601"
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format stream
			"~d~2,'0d~2,'0dT~2,'0d:~2,'0d:~2,'0d"
			year
			month
			date
			hour
			minute
			second)))

(defun iso8601->universal-time (string)
  "Convert string in the XML-RPC variant of ISO8601 to a Common Lisp universal time"
  (let (year month date (hour 0) (minute 0) (second 0))
    (when (< (length string) 9)
      (error "~s is to short to represent an iso8601" string))
    (setf year (parse-integer string :start 0 :end 4)
		  month (parse-integer string :start 4 :end 6)
		  date (parse-integer string :start 6 :end 8))
    (when (and (>= (length string) 17) (char= #\T (char string 8)))
      (setf hour (parse-integer string :start 9 :end 11)
			minute (parse-integer string :start 12 :end 14)
			second (parse-integer string :start 15 :end 17)))
    (encode-universal-time second minute hour date month year)))

(defstruct (xml-rpc-time (:print-function print-xml-rpc-time))
  "A wrapper around a Common Lisp universal time to be interpreted as an XML-RPC-TIME"
  universal-time)

(setf (documentation 'xml-rpc-time-p 'function)
      "Return T when the argument is an XML-RPC time"
      (documentation 'xml-rpc-time-universal-time 'function)
      "Return the universal time from an XML-RPC time")

(defun print-xml-rpc-time (xml-rpc-time stream depth)
  (declare (ignore depth))
  (format stream
		  "#<XML-RPC-TIME ~a>"
		  (universal-time->iso8601 (xml-rpc-time-universal-time xml-rpc-time))))

(defun xml-rpc-time (&optional (universal-time (get-universal-time)))
  "Create a new XML-RPC-TIME struct with the universal time specified, defaulting to now"
  (make-xml-rpc-time :universal-time universal-time))

;;; a wrapper for literal strings, where escaping #\< and #\& is not
;;; desired

(defstruct (xml-literal (:print-function print-xml-literal))
  "A wrapper around a Common Lisp string that will be sent over
  the wire unescaped"
  content)

(setf (documentation 'xml-literal-p 'function)
      "Return T when the argument is an unescaped xml string"
      (documentation 'xml-literal-content 'function)
      "Return the content of a literal xml string")

(defun print-xml-literal (xml-literal stream depth)
  (declare (ignore depth))
  (format stream
		  "#<XML-LITERAL \"~a\" >"
		  (xml-literal-content xml-literal)))

(defun xml-literal (content)
  "Create a new XML-LITERAL struct with the specified content."
  (make-xml-literal :content content))

;;; an extra datatype for xml-rpc structures (associative maps)

(defstruct (xml-rpc-struct (:print-function print-xml-rpc-struct))
  "An XML-RPC-STRUCT is an alist mapping member names to values"
  alist)

(setf (documentation 'xml-rpc-struct-p 'function)
      "Return T when the argument is an XML-RPC struct"
      (documentation 'xml-rpc-struct-alist 'function)
      "Return the alist of member names and values from an XML-RPC struct")

(defun print-xml-rpc-struct (xml-element stream depth)
  (declare (ignore depth))
  (format stream "#<XML-RPC-STRUCT :KEYS ~S>"
		  (mapcar #'car (xml-rpc-struct-alist xml-element))))

(defun (setf xml-rpc-struct-member) (value struct &rest keys)
  "Set the value of a specific member of an XML-RPC-STRUCT"
  (do ((keys keys (cdr keys))
       (obj struct (if (integerp (car keys))
		       (elt obj (car keys))
		       (cdr (assoc (car keys) (xml-rpc-struct-alist obj))))))
      ((null (cdr keys))
       (if (integerp (car keys))
	   (setf (elt obj (car keys)) value)
	   (setf (cdr (assoc (car keys) (xml-rpc-struct-alist obj))) value))))
  value)

(defun xml-rpc-struct-member (struct &rest keys)
  "Find a member of a structure."
  (do ((keys keys (cdr keys))
       (obj struct (let ((key (car keys)))
		     (if (integerp key)
			 (elt obj key)
			 (cdr (assoc key (xml-rpc-struct-alist obj) :test #'string-equal))))))
      ((null keys) obj)))

(defun xml-rpc-struct (&rest args)
  "Create a new XML-RPC-STRUCT from the arguments: alternating member names and values"
  (unless (evenp (length args))
    (error "~s must contain an even number of elements" args))
  (let (alist)
    (loop (if (null args)
			  (return)
			  (push (cons (pop args) (pop args)) alist)))
    (make-xml-rpc-struct :alist alist)))

(defun xml-rpc-struct-equal (struct1 struct2)
  "Compare two XML-RPC-STRUCTs for equality"
  (equal (xml-rpc-struct-alist struct1)
		 (xml-rpc-struct-alist struct2)))

;;; encoding support

(defun encode-xml-rpc-struct (struct stream)
  (princ "<struct>" stream)
  (mapcar (lambda (pair)
			(destructuring-bind (key . value) pair
			  (princ "<member>" stream)
			  (format stream "<name>~a</name>" key) ; assuming name contains no special characters
			  (encode-xml-rpc-value value stream)
			  (princ "</member>" stream)))
		  (xml-rpc-struct-alist struct))
  (princ "</struct>" stream))

(defun encode-xml-rpc-array (sequence stream)
  (princ "<array><data>" stream)
  (map 'nil #'(lambda (element) (encode-xml-rpc-value element stream)) sequence)
  (princ "</data></array>" stream))

(defun encode-xml-rpc-value (arg stream)
  (princ "<value>" stream)
  (cond ((or (null arg) (eq arg t))
		 (princ "<boolean>" stream)
		 (princ (if arg 1 0) stream)
		 (princ "</boolean>" stream))
		((or (stringp arg) (symbolp arg))
		 (princ "<string>" stream)
		 (s-xml:print-string-xml (string arg) stream)
		 (princ "</string>" stream))
		((integerp arg) (format stream "<int>~d</int>" arg))
		((floatp arg) (format stream "<double>~f</double>" arg))	
		((and (arrayp arg)
			  (= (array-rank arg) 1)
			  (subtypep (array-element-type arg)
						'(unsigned-byte 8)))
		 (princ "<base64>" stream)
		 (cl-base64:usb8-array-to-base64-stream arg stream)
		 (princ "</base64>" stream))
		((xml-rpc-time-p arg)
		 (princ "<dateTime.iso8601>" stream)
		 (universal-time->iso8601 (xml-rpc-time-universal-time arg) stream)
		 (princ "</dateTime.iso8601>" stream))
        ((xml-literal-p arg)
         (princ (xml-literal-content arg) stream))
		((or (listp arg) (vectorp arg)) (encode-xml-rpc-array arg stream))
		((xml-rpc-struct-p arg) (encode-xml-rpc-struct arg stream))
		;; add generic method call
		(t (error "cannot encode ~s" arg)))
  (princ "</value>" stream))

(defun encode-xml-rpc-args (args stream)
  (princ "<params>" stream)
  (dolist (arg args)
    (princ "<param>" stream)
    (encode-xml-rpc-value arg stream)
    (princ "</param>" stream))
  (princ "</params>" stream))

(defun encode-xml-rpc-call (name &rest args)
  "Encode an XML-RPC call with name and args as an XML string"
  (with-output-to-string (stream)
    (princ "<methodCall>" stream)
    ;; Spec says: The string may only contain identifier characters,
    ;; upper and lower-case A-Z, the numeric characters, 0-9,
    ;; underscore, dot, colon and slash.
    (format stream "<methodName>~a</methodName>" (string name)) ; assuming name contains no special characters
    (when args
      (encode-xml-rpc-args args stream))
    (princ "</methodCall>" stream)))

(defun encode-xml-rpc-result (value)
  (with-output-to-string (stream)
    (princ "<methodResponse>" stream)
    (encode-xml-rpc-args (list value) stream)
    (princ "</methodResponse>" stream)))

(defun encode-xml-rpc-fault-value (fault-string &optional (fault-code 0))
  ;; for system.multicall
  (with-output-to-string (stream)
    (princ "<struct>" stream)
    (format stream "<member><name>faultCode</name><value><int>~d</int></value></member>" fault-code)
    (princ "<member><name>faultString</name><value><string>" stream)
    (s-xml:print-string-xml fault-string stream)
    (princ "</string></value></member>" stream)
    (princ "</struct>" stream)))

(defun encode-xml-rpc-fault (fault-string &optional (fault-code 0))
  (with-output-to-string (stream)
    (princ "<methodResponse><fault><value>" stream)
    (princ (encode-xml-rpc-fault-value fault-string fault-code) stream)
    (princ "</value></fault></methodResponse>" stream)))

;;; decoding support

(defparameter *decode-value-types* nil)

(defun decode-xml-rpc-new-element (name attributes seed)
  (declare (ignore seed name attributes))
  '())

(defun decode-xml-rpc-finish-element (name attributes parent-seed seed)
  (declare (ignore attributes))
  (cons (ecase name
		  ((:|int| :|i4|)
		   (if *decode-value-types*
			   :int
			   (parse-integer seed)))
		  (:|double|
			(if *decode-value-types*
				:double
				(read-from-string seed)))
		  (:|boolean|
			(if *decode-value-types*
				:boolean
				(= 1 (parse-integer seed))))
		  (:|string|
			(if *decode-value-types*
				:string
				(if (null seed) "" seed)))
		  (:|dateTime.iso8601|
			(if *decode-value-types*
				:datetime
				(xml-rpc-time (iso8601->universal-time seed))))
		  (:|base64|
			(if (null seed)
				(make-array 0 :element-type '(unsigned-byte 8))
				(cl-base64:base64-string-to-usb8-array seed)))
		  (:|array|	(car seed))
		  (:|data| (nreverse seed))  ; potential problem with empty data i.e. <data>\n</data> parsed as "\n"
		  (:|value| (if (stringp seed) seed (car seed)))
		  (:|struct| (make-xml-rpc-struct :alist seed))
		  (:|member| (cons (cadr seed) (car seed)))
		  (:|name| (intern seed :keyword))
		  (:|params| (nreverse seed))  ; potential problem with empty params <params>\n</params> parsed as "\n"
		  (:|param| (car seed))
		  (:|fault| (make-condition 'xml-rpc-fault
									:string (xml-rpc-struct-member (car seed) :|faultString|)
									:code (xml-rpc-struct-member (car seed) :|faultCode|)))
		  (:|methodName| seed)
		  (:|methodCall| (let ((pair (nreverse seed)))
						   (cons (car pair) (cadr pair))))
		  (:|methodResponse| (car seed)))
		parent-seed))

;; fixes issue with empty params, data 
(defun decode-xml-rpc-text (string seed)
  (declare (ignore seed))
  (if (> (length (string-trim '(#\Space #\Newline #\Return)
							  string))
		 0)
      string
      nil))


(defun decode-xml-rpc (stream)
  (car (s-xml:start-parse-xml
		stream
		(make-instance 's-xml:xml-parser-state
					   :new-element-hook #'decode-xml-rpc-new-element
					   :finish-element-hook #'decode-xml-rpc-finish-element
					   :text-hook #'decode-xml-rpc-text))))

;;; networking basics

(defparameter *xml-rpc-host* "localhost"
  "String naming the default XML-RPC host to use")

(defparameter *xml-rpc-port* 8000
  "Integer specifying the default XML-RPC port to use")

(defparameter *xml-rpc-url* "/RPC2"
  "String specifying the default XML-RPC URL to use")

(defvar *xml-rpc-debug* nil
  "When not null is a stream to output debug info")

(defparameter *xml-rpc-package* (find-package :f-xml-rpc-exports)
  "Package for XML-RPC callable functions")

(defun format-debug (&rest args)
  (when *xml-rpc-debug*
    (apply #'format *xml-rpc-debug* args)))


;;; client API

(defun b64-usb8 (string)
  "Base64 string to (unsigned-byte 8) array"
  (cl-base64:base64-string-to-usb8-array string))

(defun usb8-b64 (usb8)
  "(unsigned-byte 8) array to Base64 string"
  (cl-base64:usb8-array-to-base64-string usb8))

(defun authorization-header (msg)
  "Format an AUTHORIZATION http header"
  (format nil "NTLM ~A" (cl-base64:usb8-array-to-base64-string msg)))

(defun authorization-msg (auth-header)  
  "Extract the binary message from the AUTHORIZATION header"
  (let ((matches (nth-value 1 (cl-ppcre:scan-to-strings "NTLM ([\\w=\\+/]+)" auth-header))))
    (when (and matches (> (length matches) 0))
      (b64-usb8 (elt matches 0)))))


;; http://msdn.microsoft.com/en-us/library/cc236676.aspx
(defun generate-reply (challenge username domain computer-name password-md4 version)
  "Generate an AUTHENTICATE message from the challenge"
  (format t "challenge: ~S~%" challenge)
  (let* ((lmowf (lmowf-v2 username domain password-md4))
         (ntowf (ntowf-v2 username domain password-md4))
         (server-challenge (cdr (assoc :server-challenge challenge)))
         (client-challenge (client-challenge))
         (time (cdr (assoc :timestamp (cdr (assoc :target-info challenge)))))
         (target-info-buffer (cdr (assoc :target-info-buffer challenge)))
         (temp (make-temp time client-challenge target-info-buffer))
         (lm-response (lm-response-v2 lmowf server-challenge client-challenge))
         (nt-response (nt-response-v2 ntowf
                                      server-challenge
                                      temp))
         (session-base-key (session-base-key-v2 ntowf 
                                                server-challenge
                                                temp))
         (key-exchange-key (key-exchange-key session-base-key
                                             lm-response
                                             server-challenge
                                             lmowf))
         (exported-session-key (exported-session-key :negotiate-key-exch t
                                                     :key-exchange-key key-exchange-key)))
    (pack-authenticate-message (cdr (assoc :flags challenge))
                               :lm-response lm-response
                               :nt-response nt-response
                               :domain domain
                               :username username 
                               :workstation computer-name
                               :encrypted-session-key
                               (encrypted-session-key key-exchange-key exported-session-key)
                               :version version)))

(defun ntlm-http-request (uri keyword-args &key username domain password-md4 workstation version)
  "Perform an HTTP request with NTLM authentication"
  (multiple-value-bind (content status-code headers ruri stream must-close reason)
      (apply #'drakma:http-request 
             uri
             :close nil
             :keep-alive t
             :additional-headers 
             `((:authorization . ,(authorization-header 
                                   (pack-negotiate-message '(:NEGOTIATE-UNICODE
                                                             :NEGOTIATE-OEM 
                                                             :REQUEST-TARGET
                                                             :NEGOTIATE-NTLM
                                                             :NEGOTIATE-OEM-DOMAIN-SUPPLIED
                                                             :NEGOTIATE-OEM-WORKSTATION-SUPPLIED
                                                             :NEGOTIATE-ALWAYS-SIGN
                                                             :NEGOTIATE-EXTENDED-SESSIONSECURITY
                                                             :NEGOTIATE-VERSION 
                                                             :NEGOTIATE-128
                                                             :NEGOTIATE-56)
                                                           :workstation workstation
                                                           :domain domain
                                                           :version version))))
             keyword-args)
    (declare (ignore content ruri must-close reason)) ;; status-code))
    (format t "status-code: ~S~%" status-code)
    (let ((msg (authorization-msg (cdr (assoc :www-authenticate headers)))))
      (hd msg)
      (if msg
	  (apply #'drakma:http-request 
		 uri 
		 :stream stream
		 :additional-headers 
		 `((:authorization . ,(authorization-header
				       (generate-reply (unpack-challenge-message msg) 
						       username domain workstation password-md4 version))))
		 keyword-args)
	  (error "No CHALLENGE response")))))

(defun xml-rpc-call (encoded 
                     &key
                       uri
                       (host *xml-rpc-host*) (port *xml-rpc-port*)
                       (url *xml-rpc-url*) 
                       ssl authorization ntlm-authorization
                       proxy proxy-basic-authorization (close t))
  "Call an xml/rpc server with the already encoded method call.

The HOST, PORT and URL of the server. 

Set SSL to T for communication over https.

Set AUTHORIZATION to a 2-element list of username and password to use basic authorization.

NTLM-AUTHORIZATION to a keyword list (&key username domain password workstation version).
Password can be either the md4 hash of the plaintext password, or a string (which gets automatically hashed
before use). Version should be either nil or the result of (make-ntlm-version).

See the drakma documentation for using proxy servers, PROXY PROXY-BASIC-AUTHORIZATION"

  (let ((uri (if uri 
                 uri
                 (format nil "~A://~A:~A~A"
                         (if ssl "https" "http")
                         host
                         port
                         url))))
	(multiple-value-bind (body status-code headers ruri stream
							   must-close reason-phrase)
        (if ntlm-authorization
            (destructuring-bind (&key username domain password workstation version) ntlm-authorization
              (ntlm-http-request uri 
                                 (list :content encoded
                                       :method :post
                                       :proxy proxy
                                       :proxy-basic-authorization proxy-basic-authorization)
                                 :username username
                                 :domain domain
                                 :password-md4 (if (stringp password)
                                                   (password-md4 password)
                                                   password)
                                 :version (if version version (make-ntlm-version 6 1 2600))
                                 :workstation workstation))
            (drakma:http-request uri
                                 :close close
                                 :method :post
                                 :content encoded
                                 :basic-authorization authorization
                                 :proxy proxy
                                 :proxy-basic-authorization proxy-basic-authorization))
	  (declare (ignore ruri stream headers must-close))
;;	  (format *standard-output* "~S~%" headers)
	  ;; if the response comes in as an array of char codes then 
	  ;; convert it to a string
	  (when (and (not (stringp body)) (arrayp body))
	    (setf body (map 'string (lambda (c)
								  (if (integerp c)
									  (code-char c)
									  c))
						body)))
	  (format-debug "body: ~S~%" body)
	  (if (= status-code 200)
		  (with-input-from-string (bstream body)
			(let ((decoded (decode-xml-rpc bstream)))
			  (if (typep decoded 'xml-rpc-fault)
				  (error decoded)
				  (car decoded))))
		  (error "http-error ~A: ~A" status-code reason-phrase)))))

(defun call-xml-rpc-server (server-keywords name &rest args)
  "Encode and execute an XML-RPC call with name and args, using the list of server-keywords"
  (apply #'xml-rpc-call
		 (apply #'encode-xml-rpc-call (cons name args))
		 server-keywords))

(defun describe-server (&key (host *xml-rpc-host*) (port *xml-rpc-port*) (url *xml-rpc-url*))
  "Tries to describe a remote server using system.* methods"
  (dolist (method (xml-rpc-call (encode-xml-rpc-call "system.listMethods")
								:host host
								:port port
								:url url))
    (format t
			"Method ~a ~a~%~a~%~%"
			method
			(xml-rpc-call (encode-xml-rpc-call "system.methodSignature" method)
						  :host host
						  :port port
						  :url url)
			(xml-rpc-call (encode-xml-rpc-call "system.methodHelp" method)
						  :host host
						  :port port
						  :url url))))


;;; server API

(defparameter +xml-rpc-method-characters+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.:/")

(defun valid-xml-rpc-method-name-p (method-name)
  (not (find-if-not (lambda (c) (find c +xml-rpc-method-characters+))
                    method-name)))

(defun find-xml-rpc-method (method-name package)
  "Looks for a method with the given name in *xml-rpc-package*,
  except that colons in the name get converted to hyphens."
  (let ((sym (find-symbol method-name package)))
    (if (fboundp sym) sym nil)))

(defun execute-xml-rpc-call (method-name package &rest arguments)
  "Execute method METHOD-NAME on ARGUMENTS, or raise an error if
  no such method exists in *XML-RPC-PACKAGE*"
  (let ((method (find-xml-rpc-method method-name package)))
    (if method
        (apply method arguments)
        ;; http://xmlrpc-epi.sourceforge.net/specs/rfc.fault_codes.php
        ;; -32601 ---> server error. requested method not found
        (error 'xml-rpc-fault :code -32601
               :string (format nil "Method ~A not found." method-name)))))

(defun handle-xml-rpc-call (encoded package)
  "Handle an actual call, reading XML from in and returning the
  XML-encoded result."
  ;; Try to conform to
  ;; http://xmlrpc-epi.sourceforge.net/specs/rfc.fault_codes.php
  (handler-bind ((s-xml:xml-parser-error
                  #'(lambda (c)
                      (format-debug "Request parsing failed with ~a~%" c)
                      (return-from handle-xml-rpc-call
                        ;; -32700 ---> parse error. not well formed
                        (encode-xml-rpc-fault (format nil "~a" c) -32700))))
                 (xml-rpc-fault
                  #'(lambda (c)
                      (format-debug "Call failed with ~a~%" c)
					  (return-from handle-xml-rpc-call
                        (encode-xml-rpc-fault (xml-rpc-fault-string c)
                                              (xml-rpc-fault-code c)))))
                 (error
                  #'(lambda (c)
                      (format-debug "Call failed with ~a~%" c)
                      (return-from handle-xml-rpc-call
                        ;; -32603 ---> server error. internal xml-rpc error
                        (encode-xml-rpc-fault (format nil "~a" c) -32603)))))
	(with-input-from-string (s encoded)
	  (let ((call (decode-xml-rpc s)))
		(format-debug "Received call ~s~%" call)
		(let ((result (apply #'execute-xml-rpc-call
							 (first call) package (rest call))))
		  (format-debug "Call result is ~s~%" result)
		  (encode-xml-rpc-result result))))))

;; use hunchentoot for the server

;; handle the incoming connection

(defclass xml-rpc-acceptor (hunchentoot:acceptor)
  ((auth-handler :initarg :auth-handler :initform nil)
   (package :initarg :package :initform *xml-rpc-package*))
  (:default-initargs :address nil))

(defclass xml-rpc-ssl-acceptor (hunchentoot:ssl-acceptor)
  ((auth-handler :initarg :auth-handler :initform nil)
   (package :initarg :package :initform *xml-rpc-package*))
  (:default-initargs :address nil))

(defmethod hunchentoot:reset-connection-stream ((acceptor xml-rpc-acceptor) stream)
  (setf hunchentoot::*close-hunchentoot-stream* t)
  (call-next-method))

(defmethod hunchentoot:reset-connection-stream ((acceptor xml-rpc-ssl-acceptor) stream)
  (setf hunchentoot::*close-hunchentoot-stream* t)
  (call-next-method))


(defvar *authorization* nil)

(defun acceptor-handler (a request)
  (format *standard-output* "headers in: ~S~%" (hunchentoot:headers-in* request))
  (let ((encoded (map 'string #'code-char
		      (hunchentoot:raw-post-data))))
    (setf (hunchentoot:content-type*) "text/xml")
    ;; check authorization
    (let ((ahandler (slot-value a 'auth-handler)))
      (multiple-value-bind (username password)
	  (hunchentoot:authorization request)
	(cond
	  ((not ahandler)
	   (handle-xml-rpc-call encoded (slot-value a 'package)))
	  ((and username (funcall ahandler username password))
	   (let ((*authorization* (list username password)))
	     (handle-xml-rpc-call encoded (slot-value a 'package))))
	  (t
	   (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
	   ""))))))


(defmethod hunchentoot:acceptor-dispatch-request
	((a xml-rpc-acceptor) request)
  (acceptor-handler a request))

(defmethod hunchentoot:acceptor-dispatch-request
	((a xml-rpc-ssl-acceptor) request)
  (acceptor-handler a request))

;; start the server
(defun make-xml-rpc-acceptor (&key (port *xml-rpc-port*)
							  ssl-certificate-file ssl-privatekey-file
							  ssl-privatekey-password
							  auth-handler (package *xml-rpc-package*))
  
  (if (and ssl-certificate-file ssl-privatekey-file)
	  ;; ssl
	  (make-instance
	   'xml-rpc-ssl-acceptor
	   :port port
	   :ssl-certificate-file ssl-certificate-file
	   :ssl-privatekey-file ssl-privatekey-file
	   :ssl-privatekey-password ssl-privatekey-password
	   :auth-handler auth-handler
	   :package package)	  
	  (make-instance
	   'xml-rpc-acceptor
	   :port port
	   :auth-handler auth-handler
	   :package package)))

(defvar *xml-rpc-acceptor* nil)

(defun start-xml-rpc-server (&key acceptor 
							 (port *xml-rpc-port*)
							 ssl-certificate-file ssl-privatekey-file
							 ssl-privatekey-password
							 auth-handler (package *xml-rpc-package*))
  (if acceptor
	  (hunchentoot:start acceptor)
	  (progn
		(setf *xml-rpc-acceptor* 
		      (make-xml-rpc-acceptor :port port
									 :ssl-certificate-file ssl-certificate-file
									 :ssl-privatekey-file ssl-privatekey-file
									 :ssl-privatekey-password ssl-privatekey-password
									 :auth-handler auth-handler 
									 :package package))
		(hunchentoot:start *xml-rpc-acceptor*))))

;; stop the server 

(defun stop-xml-rpc-server (&optional acceptor)
  "Stop the xml/rpc server"
  (if acceptor
	  (hunchentoot:stop acceptor)
	  (progn
		(hunchentoot:stop *xml-rpc-acceptor*)
		(setf *xml-rpc-acceptor* nil))))

;; --------- convenience macros --------------

(defmacro define-xml-rpc-export (name parameters &body body)
  "Define a function exported from the server"
  `(defun ,(intern (symbol-name name) *xml-rpc-package*)
	   ,parameters
	 ,@body))

(defmacro define-xml-rpc-call (name params &key
							   (host *xml-rpc-host*)
							   (url *xml-rpc-url*)
							   (port *xml-rpc-port*)
							   authorization method)
  "Define a call to a function exported by a server"
  `(defun ,name ,params
     (xml-rpc-call (encode-xml-rpc-call ,(if method method (symbol-name name))
										,@params)
				   :url ,url
				   :host ,host
				   :authorization ,authorization
				   :port ,port)))



(defun |system.listMethods| ()
  (let (exports)
    (do-symbols (var *xml-rpc-package*)
      (if (fboundp var)
		  (push (symbol-name var) exports)))
    exports))

(defun |system.methodSignature| (method)
  (let ((fn (find-xml-rpc-method method *xml-rpc-package*)))
    (if fn
		(third (function-lambda-expression (symbol-function fn)))
		(error 'xml-rpc-fault :code -32601
               :string (format nil "Method ~A not found." method)))))

(defun |system.methodHelp| (method)
  (let ((fn (find-xml-rpc-method method *xml-rpc-package*)))
    (if fn
		(documentation fn 'function)
		(error 'xml-rpc-fault :code -32601
               :string (format nil "Method ~A not found." method)))))

(defun export-system-methods (&optional (package *xml-rpc-package*))
  (loop for sym in '(|system.listMethods| |system.methodSignature| |system.methodHelp|)
	 do (handler-case (import sym package)
		  (package-error () 
			(format-debug "Unable to import ~S to package ~S" sym package)))))






;; -----------

;; some wrappers to make certain calls easier
(defun struct-member (struct &rest keys)
  (apply #'xml-rpc-struct-member struct keys))

(defun call (encoded &key uri authorization ntlm-authorization)
  (xml-rpc-call encoded 
                :uri uri
                :authorization authorization 
                :ntlm-authorization ntlm-authorization))

(defun encode (name &rest args)
  (apply #'encode-xml-rpc-call name args))

(defmacro call* ((&key uri authorization ntlm-authorization) name &rest args)
  "Sugar coating"
  `(call (encode ,name ,@args)
         :uri ,uri
         :authorization ,authorization 
         :ntlm-authorization ,ntlm-authorization))

  
