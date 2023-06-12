(in-package #:ucons)

(defconstant +cache-hash-threshold+ 15
  "How many entries are cached in a list with linear search before switching to a
hash table.")

(defstruct (cache
            (:predicate cachep)
            (:copier nil)
            (:constructor make-cache ()))
  #-ccl (data nil :type (or list hash-table))
  #+ccl (data (concurrent-hash-table-from-alist '()) :type hash-table))

(defmacro with-caching ((cache key &key inline) &body body)
  (let ((form `(call-with-caching ,cache ,key (lambda () ,@body))))
    (if inline
        `(locally (declare (inline call-with-caching)) ,form)
        form)))

(declaim (inline call-with-caching))
(defun call-with-caching (cache key thunk)
  (declare (cache cache))
  (declare (function thunk))
  #+ccl
  (ensure-concurrent-hash-table-entry (cache-data cache) key (funcall thunk))
  #-ccl
  (flet ((compute-value () (funcall thunk)))
    (tagbody retry
       (let ((data (cache-data cache)))
         (etypecase data
           (null
            (let ((value (compute-value)))
              (unless (atomics:cas (cache-data cache) nil `((,key . ,value)))
                (go retry))
              (return-from call-with-caching value)))
           (cons
            (do* ((tail data next)
                  (count 0 (1+ count))
                  (entry (first tail) (first tail))
                  (next (rest tail) (rest tail)))
                 ((= count +cache-hash-threshold+)
                  (let* ((value (compute-value))
                         (alist `((,key . ,value) ,@data))
                         (table (concurrent-hash-table-from-alist alist)))
                    (unless (atomics:cas (cache-data cache) data table)
                      (go retry))
                    (return-from call-with-caching value)))
              (declare (cons tail) (fixnum count))
              (cond ((eql (car entry) key)
                     (return-from call-with-caching (cdr entry)))
                    ((atom next)
                     (let ((value (compute-value)))
                       (unless (atomics:cas (cdr tail) next `((,key . ,value)))
                         (go retry))
                       (return-from call-with-caching value))))))
           (concurrent-hash-table
            (return-from call-with-caching
              (ensure-concurrent-hash-table-entry data key (compute-value)))))))))
;;; Store the inline expansion, but don't inline by default.
(declaim (notinline call-with-caching))

(defun cache-benchmark ()
  (let ((data '(("constant" 42)
                ("small" 1 2 3 4)
                ("up-down-5" 1 2 3 4 5 5 4 3 2 1)
                ("up-down-10" 1 2 3 4 5 6 7 8 9 0 0 9 8 7 6 5 4 3 2 1)
                ("iota" 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))))
    (loop for (name . values) in data do
      (let* ((vector (coerce values 'vector))
             (repetitions (ceiling (expt 10 8) (length vector)))
             (cache (make-cache))
             (time (get-internal-real-time)))
        (loop repeat repetitions do
          (loop for element across vector do
            (with-caching (cache element) element)))
        (format t "~&~A: ~,2E seconds per lookup~%"
                name
                (/ (float
                    (/ (- (get-internal-real-time) time)
                       internal-time-units-per-second))
                   (* repetitions (length vector))))))))
