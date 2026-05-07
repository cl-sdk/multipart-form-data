(asdf:defsystem #:multipart-form-data
  :author "Bruno Dias"
  :serial t
  :depends-on (#:alexandria #:babel #:str #:cl-hash-util #:fast-http)
  :components ((:file "multipart-form-data"))
  :in-order-to ((test-op (test-op #:multipart-form-data.test))))
