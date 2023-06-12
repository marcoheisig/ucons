(in-package #:ucons)

(deftype concurrent-hash-table () 'hash-table)

(defmacro ensure-concurrent-hash-table-entry (cht key value-form)
  (alexandria:once-only (cht key)
    `(values
      (alexandria:ensure-gethash ,key ,cht ,value-form))))

(defun make-concurrent-hash-table ()
  (make-hash-table
   :size 40
   :rehash-size 4.0
   .
   #+sbcl (:synchronized t)
   #-sbcl ()))

(defun concurrent-hash-table-from-alist (alist)
  (let ((table (make-concurrent-hash-table)))
    (loop for (key . value) in alist do
      (ensure-concurrent-hash-table-entry table key value))
    table))


