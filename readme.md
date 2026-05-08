# multipart/form-data (RFC-7578)

Implementation of http's form data to parse and build forms to send over HTTP.

This library lets you both generate multipart/form-data payloads and parse incoming multipart bodies. The examples below show the basic workflow for each direction so you can quickly integrate it with your HTTP client or server code.

## Example: build a multipart/form-data request body

```lisp
(ql:quickload :multipart-form-data)

(let ((form (multipart-form-data:create-form-data :boundary "boundary-123")))
  (multipart-form-data:append-data
   :field form "name" (make-string-input-stream "alice"))
  (multipart-form-data:append-data
   :file form "avatar" (make-string-input-stream "PNGDATA")
   :filename "avatar.png"
   :content-type "image/png")
  (multiple-value-bind (content-type content-length content)
      (multipart-form-data:response-submit form)
    (format t "Content-Type: ~a~%" content-type)
    (format t "Content-Length: ~a~%" content-length)
    (format t "~a~%" content)))
```

## Example: parse a multipart/form-data body

```lisp
(ql:quickload '(:multipart-form-data :babel))

(let* ((boundary "boundary-123")
       (raw-body
         (concatenate
          'string
          "--boundary-123\r\n"
          "Content-Disposition: form-data; name=\"name\"\r\n"
          "Content-Type: text/plain\r\n\r\n"
          "alice\r\n"
          "--boundary-123\r\n"
          "Content-Disposition: form-data; name=\"age\"\r\n"
          "Content-Type: text/plain\r\n\r\n"
          "42\r\n"
          "--boundary-123--"))
       (parsed
         (multipart-form-data:parse
          boundary
          (babel:string-to-octets raw-body))))
  ;; => (("age" "42") ("name" "alice"))
  parsed)
```

## License

Unlicense.

See [license](https://github.com/cl-sdk/multipart-form-data/blob/development/license).
