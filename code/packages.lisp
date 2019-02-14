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

   ;; Readtable
   #:read-ulist
   #:ucons-readtable

   ;; Library
   #:ulist
   #:ulist*
   #:do-ulist
   #:umapcar
   #:ulength
   #:list-from-ulist
   #:ulist-from-list
   #:tree-from-utree
   #:utree-from-tree))
