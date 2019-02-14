(in-package #:ucons)

(defun read-ulist (stream char)
  (declare (ignore char))
  (ulist-from-list
   (read-delimited-list #\] stream t)))

(define-condition unmatched-closing-square-bracket
    (reader-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Unmatched closing square bracket on ~S."
             (stream-error-stream condition)))))

(defun read-right-square-bracket (stream char)
  (declare (ignore char))
  (error 'unmatched-closing-square-bracket
         :stream stream))

(named-readtables:defreadtable ucons-readtable
  (:merge :common-lisp)
  (:macro-char #\[ 'read-ulist)
  (:macro-char #\] 'read-right-square-bracket))
