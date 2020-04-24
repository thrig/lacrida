(asdf:defsystem #:lacrida
  :description "a LISP 2020 spring game jam game"
  :author "Jeremy Mates <jeremy.mates@gmail.com>"
  :license "ISC"
  :version "0.0.4"
  :serial t
  :depends-on (#:alexandria #:cl-charms)
  :components ((:static-file "README")
               (:static-file "LICENSE")
               (:file "package")
               (:file "definitions" :depends-on ("package"))
               (:file "message" :depends-on ("package"))
               (:file "structures" :depends-on ("package" "message"))
               (:file "main" :depends-on ("package" "definitions"
                                          "structures" "message"))))
