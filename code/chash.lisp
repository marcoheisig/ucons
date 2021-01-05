(in-package #:ucons)

;;; This is a very simple and not so fast implementation of a concurrent
;;; hash table.  The plan is to switch to another library for this once
;;; such a thing is available.

(defstruct (chash
            (:constructor make-chash ())
            (:predicate chashp)
            (:copier nil))
  (hash-table (make-hash-table :size 40 :rehash-size 4.0)
   :type hash-table
   :read-only t)
  (lock (bordeaux-threads:make-lock)
   :type (bordeaux-threads:lock)
   :read-only t))

(defmacro ensure-getchash (chash key value-form)
  (alexandria:once-only (chash key)
    `(bordeaux-threads:with-lock-held ((chash-lock ,chash))
       (alexandria:ensure-gethash
        ,key
        (chash-hash-table ,chash)
        ,value-form))))

(defun chash-from-alist (alist)
  (let* ((chash (make-chash))
         (table (chash-hash-table chash)))
    (bordeaux-threads:with-lock-held ((chash-lock chash))
      (loop for (key . value) in alist do
        (setf (gethash key table) value)))
    chash))
