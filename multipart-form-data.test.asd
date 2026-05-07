(asdf:defsystem #:multipart-form-data.test
  :depends-on (#:multipart-form-data #:fiveam)
  :serial t
  :components ((:module "t"
                :components ((:file "multipart-form-data.test"))))
  :perform (test-op (op c)
            (declare (ignore op c))
            (uiop:symbol-call :multipart-form-data.test :run-tests)))
