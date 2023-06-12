(defsystem "ucons"
  :description "Unique conses and functions for working on them."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on
  ("alexandria"
   "atomics"
   "named-readtables"
   "trivia"
   "bordeaux-threads")

  :serial t
  :components
  ((:file "packages")
   (:file "concurrent-hash-table")
   (:file "cache")
   (:file "ucons")
   (:file "library")
   (:file "readtable")
   (:file "print-object")))
