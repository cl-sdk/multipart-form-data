(push *default-pathname-defaults* ql:*local-project-directories*)

(setf asdf/source-registry::*source-registry-file* #P"./.qlot/")

(asdf:initialize-source-registry)

(ql:quickload :io.github.cl-sdk.multipart-form-data.test)

(5am:run-all-tests)
