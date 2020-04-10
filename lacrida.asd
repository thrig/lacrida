(asdf:defsystem #:lacrida
  :description "maybe a game"
  :author "Jeremy Mates <jeremy.mates@gmail.com>"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-minterm)
  :components ((:file "lacrida")
               (:static-file "README")
               (:static-file "LICENSE")))
