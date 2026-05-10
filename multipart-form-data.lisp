(defpackage #:io.github.cl-sdk.multipart-form-data
  (:use #:cl)
  (:export
   #:form-data
   #:form-item
   #:append-data
   #:create-form-data
   #:response-content
   #:response-submit
   #:parse))

(in-package :io.github.cl-sdk.multipart-form-data)

(defvar *crlf* (concatenate 'string (string #\return) (string #\linefeed)))
(defvar *content-disposition* "Content-Disposition")
(defvar *content-type* "Content-Type")

(defclass form-data ()
  ((boundary :initarg :boundary :initform (error "boundary must be defined"))
   (items :initarg :items :initform nil))
  (:documentation "Form data stores all data and process a response"))

(defclass form-item ()
  ((name :initarg :name :initform (error "name must be defined"))
   (type :initarg :type :initform (error "type must be defined"))
   (content-type :initarg :content-type :initform (error "content-type must be defined"))
   (filename :initarg :filename :initform nil)
   (stream :initarg :stream :initform (error "stream must be defined"))
   (length :initarg :length :initform 0))
  (:documentation "A form data item"))

(defun create-form-data (&key boundary items)
  (make-instance 'form-data :boundary boundary :items items))

(defgeneric parse (boundary content))
(defgeneric response-submit (obj))
(defgeneric response-header-content-type (obj))
(defgeneric response-content (obj))
(defgeneric append-data (type obj name stream &key length content-type &allow-other-keys))

(defmethod append-data ((type (eql :file))
			(obj form-data)
			name
			stream
			&key
			  (content-type "application/octet-stream")
			  (length 0)
			  (filename nil)
			&allow-other-keys)
  (push (make-instance 'form-item
		       :name name
		       :type type
		       :content-type content-type
		       :filename filename
		       :stream stream
		       :length length)
	(slot-value obj 'items)))

(defmethod append-data ((type (eql :field))
			(obj form-data)
			name
			stream
			&key (content-type "text/plain")
			     (length 0)
			&allow-other-keys)
  (push (make-instance 'form-item
		       :name name
		       :type type
		       :content-type content-type
		       :stream stream
		       :length length)
	(slot-value obj 'items)))

(defmethod response-header-content-type ((obj form-data))
  (format nil "multipart/form-data; boundary=~a" (slot-value obj 'boundary)))

(defmethod response-content ((obj form-data))
  (labels ((read-stream (s)
	     (let ((buf (make-string 4096)))
	       (with-output-to-string (out)
		 (loop for n = (read-sequence buf s)
		       while (plusp n)
		       do (write-string buf out :end n)))))
	   (section (boundary item)
	     (let* ((name (slot-value item 'name))
		    (type (slot-value item 'type))
		    (content-type (slot-value item 'content-type))
		    (filename (slot-value item 'filename))
		    (body (read-stream (slot-value item 'stream)))
		    (disposition
		     (if (and (eq type :file) filename)
			 (format nil "Content-Disposition: form-data; name=\"~a\"; filename=\"~a\""
				 name filename)
			 (format nil "Content-Disposition: form-data; name=\"~a\"" name))))
	       (concatenate 'string
			    "--" boundary *crlf*
			    disposition *crlf*
			    *content-type* ": " content-type *crlf*
			    *crlf*
			    body))))
    (let ((boundary (slot-value obj 'boundary)))
      (concatenate 'string
		   (reduce (lambda (acc item)
			     (concatenate 'string
					  acc
					  (section boundary item)
					  *crlf*))
			   (slot-value obj 'items)
			   :initial-value "")
		   (format nil "--~a--" boundary)))))

(defmethod response-submit ((obj form-data))
  (let* ((content-type (response-header-content-type obj))
	 (content (response-content obj)))
    (values content-type
	    (length content)
	    content)))

(defparameter *d*
  `(("text/plain" :text . identity)))

(defun parse-body (boundary content)
  (let ((parser (fast-http:make-ll-multipart-parser :boundary boundary))
	headers
	results)
    (fast-http:http-multipart-parse parser
				    (fast-http:make-callbacks
				     :header-field (lambda (parser data start end)
						     (declare (ignore parser))
						     (push
						      (cons (babel:octets-to-string data :start start :end end) nil)
						      headers))
				     :header-value (lambda (parser data start end)
						     (declare (ignore parser))
						     (setf (cdr (car headers))
							   (append (cdr (car headers))
								   (list (babel:octets-to-string data :start start :end end)))))
				     :body (lambda (parser data start end)
					     (declare (ignore parser))
					     (push
					      (list :headers (loop for (key . values) in (nreverse headers)
								   append (list key (apply #'concatenate 'string values)))
						    :body (babel:octets-to-string data :start start :end end))
					      results)
					     (setf headers nil)))
				    content)
    results))

(defun parse-content-disposition (str)
  (let ((result (cl-hash-util:hash-create nil))
	(start (position #\; str)))
    (when start
      (dolist (item (str:split ";" (subseq str (1+ start))))
	(let* ((trimmed (string-trim " " item))
	       (eq-pos (position #\= trimmed)))
	  (when eq-pos
	    (setf (gethash (string-trim " " (subseq trimmed 0 eq-pos)) result)
		  (str:replace-all "\"" "" (subseq trimmed (1+ eq-pos))))))))
    result))

(defmethod parse (boundary content)
  (reduce (lambda (acc field)
	    (let* ((headers (alexandria:plist-alist (getf field :headers)))
		   (body (getf field :body))
		   (info (parse-content-disposition
			  (alexandria:assoc-value headers *content-disposition*
						  :test 'string-equal)))
		   (mime (or (alexandria:assoc-value headers *content-type*
						    :test 'string-equal)
			    "text/plain"))
		   (spec (find-if (lambda (spec) (string-equal mime (car spec))) *d*))
		   (transformer (if spec
				    (destructuring-bind (mime-type sym . fn) spec
				      (declare (ignore mime-type sym))
				      fn)
				    #'identity)))
	      (push (list (gethash "name" info) (funcall transformer body)) acc)
	      acc))
	  (parse-body boundary content)
	  :initial-value nil))
