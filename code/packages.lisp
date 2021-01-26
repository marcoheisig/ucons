(cl:in-package #:common-lisp-user)

(defpackage #:ucons
  (:use #:common-lisp)
  (:export
   ;; Core Functionality
   #:ucons
   #:uconsp
   #:ucar
   #:ucdr
   #:*root-table*
   #:make-root-table
   #:root-table

   ;; Readtable
   #:read-ulist
   #:ucons-readtable

   ;; Library
   #:upush
   #:upop
   #:do-ulist
   #:ulist
   #:ulist*
   #:ulength
   #:unth
   #:unthcdr
   #:ulist-from-vector
   #:ulist-from-list
   #:utree-from-tree
   #:vector-from-ulist
   #:list-from-ulist
   #:tree-from-utree
   #:umapcar))
