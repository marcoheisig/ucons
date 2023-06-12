(in-package #:ucons)

(declaim (cache *atom-cache*))
(defvar *atom-cache* (make-cache))

(defstruct (ucons
            (:constructor make-fresh-ucons (car cdr))
            (:copier nil)
            (:predicate uconsp)
            (:conc-name u))
  (cdr nil :type t :read-only t)
  (car nil :type t :read-only t)
  (cache (make-cache) :type cache :read-only t))

(deftype ulist () '(or ucons null))

(defun cdr-cache (cdr)
  (typecase cdr
    (ucons (ucache cdr))
    (otherwise
     (with-caching (*atom-cache* cdr :inline t)
       (make-cache)))))

(declaim (ftype (function (t t) (values ucons &optional)) ucons))

(defun ucons (car cdr)
  (declare (optimize (speed 3) (safety 0)))
  (the (values ucons &optional)
       (with-caching ((cdr-cache cdr) car :inline t)
         (make-fresh-ucons car cdr))))

(defmethod make-load-form ((ucons ucons) &optional env)
  (declare (ignore env))
  (values `(locally (declare (notinline ucons))
             (ucons ,(ucar ucons) ,(ucdr ucons)))
          `(values)))

#+sbcl
(sb-c:defknown ucons (t t) ucons
    (sb-c:foldable sb-c:movable sb-c:flushable)
  :overwrite-fndb-silently t)
