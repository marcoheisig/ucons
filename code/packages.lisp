(cl:in-package #:common-lisp-user)

(defpackage #:ucons
  (:use #:common-lisp)
  (:export
   ;; Cache
   #:make-cache
   #:cachep
   #:with-caching

   ;; Core Functionality
   #:ucons
   #:uconsp
   #:ucar
   #:ucdr
   #:*atom-cache*

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
