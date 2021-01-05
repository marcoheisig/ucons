(in-package #:ucons)

(defstruct (ucons
            (:constructor make-fresh-ucons (car cdr))
            (:copier nil) ; This is the whole point, isn't it?
            (:predicate uconsp)
            (:conc-name u))
  (cdr   nil :read-only t   :type (or structure-object null))
  (car   nil :read-only t   :type t )
  (table nil :read-only nil :type (or list chash)))

(deftype ulist ()
  "A list made of UCONSes, or NIL."
  '(or ucons null))

(defstruct (root-table
            (:constructor make-root-table ())
            (:copier nil)
            (:predicate root-table-p))
  (chash (make-chash)
   :type chash
   :read-only t)
  (small-integer-cache
   (let ((array (make-array 33)))
     (loop for index from -16 to 16 do
       (setf (aref array (+ index 16))
             (make-fresh-ucons index nil)))
     array)
   :type (simple-array ucons (33))
   :read-only t))

(declaim (root-table *root-table*))
(defvar *root-table* (make-root-table)
  "The table of all uconses whose cdr is NIL.")

(declaim
 (ftype (function (ucons)   t)     ucar)
 (ftype (function (ucons)   ulist) ucdr)
 (ftype (function (t ulist) ucons) ucons)
 (ftype (function (t)       ucons) ucons-leaf))

;; The number of entries in a table after which we change it from an alist
;; to a hash table.
(defconstant +ucons-hash-table-threshold+ 15)

(defun ucons (car cdr)
  "Given a suitable CAR and CDR, return a UCONS that is EQL to all future
and past invocation of this function with the same arguments."
  (declare (type ulist cdr))
  (if (null cdr)
      (ucons-leaf car)
      (loop do
        (let ((table (utable cdr)))
          (etypecase table
            (list
             (let ((length 0))
               (declare (fixnum length))
               ;; Search the alist for an existing ucons.
               (loop for entry of-type cons in table do
                 (incf length)
                 (when (eql (car entry) car)
                   (return-from ucons (cdr entry))))
               ;; Attempt to insert a fresh ucons into the table, either
               ;; via atomic pushing, or by upgrading the table to a
               ;; concurrent hash table first.
               (if (< length +ucons-hash-table-threshold+)
                   ;; Attempt to add to the existing alist.
                   (let ((ucons (make-fresh-ucons car cdr)))
                     (when (atomics:cas (utable cdr) table (cons (cons car ucons) table))
                       (return-from ucons ucons)))
                   ;; Upgrade to a concurrent hash table.
                   (let ((chash (chash-from-alist table)))
                     (when (atomics:cas (utable cdr) table chash)
                       (return-from ucons
                         (values
                          (ensure-getchash chash car (make-fresh-ucons car cdr)))))))))
            (chash
             (return-from ucons
               (values
                (ensure-getchash table car (make-fresh-ucons car cdr))))))))))

;;; Lookup of root nodes, i.e., uconses whose cdr is NIL.
(defun ucons-leaf (car)
  (let ((root-table *root-table*))
    (if (typep car '(integer -16 16))
        ;; Go to the small integer cache.
        (aref (root-table-small-integer-cache root-table)
              (+ car 16))
        ;; Else, go to the slightly slower general cache.
        (values
         (ensure-getchash
          (root-table-chash root-table)
          car
          (make-fresh-ucons car nil))))))
