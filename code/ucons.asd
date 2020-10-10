(defsystem "ucons"
  :description "Unique conses and functions for working on them."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("alexandria"
   "named-readtables"
   "trivia")

  :serial t
  :components
  ((:file "packages")
   (:file "ucons")
   (:file "library")
   (:file "readtable")
   (:file "print-object")))
