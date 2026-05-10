(defpackage #:io.github.cl-sdk.multipart-form-data.test
  (:use #:cl #:io.github.cl-sdk.multipart-form-data #:fiveam)
  (:export #:run-tests))

(in-package #:io.github.cl-sdk.multipart-form-data.test)

(defun string-contains-p (text needle)
  (not (null (search needle text :test #'char=))))

(def-suite multipart-form-data-suite)
(in-suite multipart-form-data-suite)

(test should-render-response-header-content-type
  (let ((form (create-form-data :boundary "boundary-1")))
    (append-data :field form "x" (make-string-input-stream "1"))
    (multiple-value-bind (content-type content-length content)
        (response-submit form)
      (declare (ignore content-length content))
      (is (string= "multipart/form-data; boundary=boundary-1" content-type)))))

(test should-build-response-content
  (let* ((boundary "boundary-2")
         (form (create-form-data :boundary boundary)))
    (append-data :field form "name" (make-string-input-stream "alice"))
    (append-data :file
                 form
                 "avatar"
                 (make-string-input-stream "PNGDATA")
                 :filename "avatar.png"
                 :content-type "image/png")
    (let ((content (response-content form))
          (final-boundary "--boundary-2--"))
      (is (string-contains-p content "--boundary-2"))
      (is (string-contains-p content "Content-Disposition: form-data; name=\"avatar\"; filename=\"avatar.png\""))
      (is (string-contains-p content "Content-Type: image/png"))
      (is (string-contains-p content "PNGDATA"))
      (is (string-contains-p content "Content-Disposition: form-data; name=\"name\""))
      (is (string-contains-p content "Content-Type: text/plain"))
      (is (string-contains-p content "alice"))
      (is (string= final-boundary
                   (subseq content (- (length content)
                                      (length final-boundary))))))))

(test should-build-the-correct-header-and-content
  (let ((form (create-form-data :boundary "boundary-3"))
        (content-stream (make-string-input-stream "123")))
    (append-data :field form "x" content-stream)
    (multiple-value-bind (content-type content-length content)
        (response-submit form)
      (is (string= "multipart/form-data; boundary=boundary-3" content-type))
      (is (= content-length (length content)))
      (progn
        (file-position content-stream 0)
        (is (string= content (response-content form)))))))

(test parse
  (let* ((boundary "boundary-4")
         (form (create-form-data :boundary boundary)))
    (append-data :field form "age" (make-string-input-stream "42"))
    (append-data :field form "name" (make-string-input-stream "alice"))
    (let* ((content (response-content form))
           (parsed (parse boundary (babel:string-to-octets content))))
      (is (equal "alice" (second (assoc "name" parsed :test #'string=))))
      (is (equal "42" (second (assoc "age" parsed :test #'string=)))))))
