(in-package #:ucons)

(defstruct (ucons
            (:constructor make-fresh-ucons (car cdr))
            (:copier nil) ; This is the whole point, isn't it?
            (:predicate uconsp)
            (:conc-name u))
  (cdr   nil :read-only t   :type (or structure-object null))
  (car   nil :read-only t   :type t )
  (table nil :read-only nil :type (or list hash-table)))

(deftype ulist ()
  "A list made of UCONSes, or NIL."
  '(or ucons null))

(declaim (inline ucons)
         (notinline ucons-leaf ucons-hash ucons-list)
         (ftype (function (ucons)   t)     ucar)
         (ftype (function (ucons)   ulist) ucdr)
         (ftype (function (t ulist) ucons) ucons)
         (ftype (function (t)       ucons) ucons-leaf)
         (ftype (function (t ucons) ucons) ucons-hash ucons-list))

(defun ucons (car cdr)
  "Given a suitable CAR and CDR, return a UCONS that is EQL to all future
and past invocation of this function with the same arguments."
  (declare (type ulist cdr))
  (if (null cdr)
      (ucons-leaf car)
      (let ((table (utable cdr)))
        (declare (optimize (safety 0) (debug 0) (speed 3))
                 #+sbcl
                 (sb-ext:muffle-conditions sb-ext:compiler-note))
        (if (listp table)
            (loop for entry of-type cons in table do
              (when (eql (car entry) car)
                (return (cdr entry)))
                  finally (return (ucons-list car cdr)))
            (ucons-hash car cdr)))))

;;; Called if the UTABLE of CDR is a hash table
(defun ucons-hash (car cdr)
  (declare (ucons cdr))
  (values
   (alexandria:ensure-gethash car (utable cdr) (make-fresh-ucons car cdr))))

;;; Called if the UTABLE of CDR is an alist that does not contain CAR.
(defun ucons-list (car cdr)
  (declare (ucons cdr))
  (let ((ucons (make-fresh-ucons car cdr)))
    (prog1 ucons
      (if (< (length (utable cdr)) 16)
          (push (cons car ucons) (utable cdr))
          (let ((hash-table (alexandria:alist-hash-table (utable cdr) :size 32)))
            (setf (gethash car hash-table) ucons)
            (setf (utable cdr) hash-table))))))

;;; Lookup of root nodes, i.e., uconses whose cdr is NIL.

(defstruct (root-table (:copier nil)
                       (:predicate nil))
  (cache (make-hash-table) :read-only t :type hash-table)
  (small-integer-cache
   (let ((array (make-array 33)))
     (loop for index from -16 to 16 do
       (setf (aref array (+ index 16))
             (make-fresh-ucons index nil)))
     array)
   :read-only t
   :type (simple-array ucons (33))))

(declaim (root-table *root-table*))
(defvar *root-table* (make-root-table)
  "The table of all uconses whose cdr is NIL.")

(defun ucons-leaf (car)
  (if (typep car '(integer -16 16))
      ;; Go to the small integer cache.
      (aref (root-table-small-integer-cache *root-table*)
            (+ car 16))
      ;; Else, go to the slightly slower general cache.
      (let ((cache (root-table-cache *root-table*)))
        (values
         (alexandria:ensure-gethash car cache (make-fresh-ucons car nil))))))
