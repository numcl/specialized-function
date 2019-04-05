
(defsystem specialized-function
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LGPL"
  :defsystem-depends-on ()
  :depends-on (:trivia :alexandria :iterate :lisp-namespace :type-r)
  :pathname "src"
  :components ((:file "0package")
               (:file "1common")
               #+sbcl
               (:file "2sbcl")
               #+ccl
               (:file "2ccl")
               #-(or sbcl ccl)
               (:file "2unsupported"))
  :description "Provides a Julia-like function that automatically compiles a type-specific version of the function from the same code"
  :in-order-to ((test-op (test-op :specialized-function.test)))
  ;; :defsystem-depends-on (:trivial-package-manager)
  ;; :perform
  #+(or)
  (load-op :before (op c)
           (uiop:symbol-call :trivial-package-manager
                             :ensure-program
                             "minisat"
                             :apt "minisat"
                             :dnf "minisat2"
                             :yum "minisat2"
                             :packman ""
                             :yaourt ""
                             :brew "minisat"
                             :choco ""
                             :from-source (format nil "make -C ~a"
                                                  (asdf:system-source-directory :specialized-function)))
           (uiop:symbol-call :trivial-package-manager
                             :ensure-library
                             "libfixposix"
                             :apt "libfixposix0"
                             :dnf ""
                             :yum ""
                             :packman ""
                             :yaourt ""
                             :brew "libfixposix"
                             :choco ""
                             :from-source (format nil "make -C ~a"
                                                  (asdf:system-source-directory :specialized-function)))))
