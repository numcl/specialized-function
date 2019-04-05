#|
  This file is a part of specialized-function project.
  Copyright (c) 2019 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage specialized-function.test-asd
  (:use :cl :asdf))
(in-package :specialized-function.test-asd)


(defsystem specialized-function.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of specialized-function"
  :license "LGPL"
  :depends-on (:specialized-function
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(5am:run! :specialized-function)"))
))
