(asdf:defsystem #:alphabetical-asdf
  :description "ASDF extension that loads files and modules in alphabetical order."
  :author "Peter von Etter"
  :license "LLGPL"
  :version "0.0.1"
  :serial t
  :components ((:file "alphabetical-asdf"))
  :depends-on ("alexandria"))
