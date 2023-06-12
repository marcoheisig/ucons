(in-package #:ucons)

(defmethod print-object ((ucons ucons) stream)
  (let ((list (list-from-ulist ucons)))
    (if *print-readably*
        (if *read-eval*
            (let* ((last (last list))
                   (properp (null (cdr last))))
              (print-list
               (if properp
                   `(ulist ,@list)
                   `(ulist* ,@(butlast list) ,(car last) ,(cdr last)))
               stream
               "#.(" ")"))
            ;; Each ucons tracks uconses that have it as its cdr, and is
            ;; therefore inherently circular.  So either *PRINT-CIRCLE*
            ;; must be true, or we signal an error.
            (if *print-circle*
                (call-next-method)
                (error 'print-not-readable :object ucons)))
        (print-list list stream "[" "]"))))

(defun print-list (list stream prefix suffix)
  (pprint-logical-block (stream list :prefix prefix :suffix suffix)
    (pprint-linear stream list nil)))
