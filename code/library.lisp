(in-package #:ucons)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ulist creation

(defun ulist (&rest args)
  "Return the ulist associated with the supplied arguments."
  (declare (dynamic-extent args))
  (reduce #'ucons args :from-end t :initial-value nil))

(define-compiler-macro ulist (&rest args)
  (flet ((symbolic-ucons (car cdr)
           `(ucons ,car ,cdr)))
    (reduce #'symbolic-ucons args :from-end t :initial-value nil)))

(defun ulist* (&rest args)
  "Return the ulist associated with the supplied arguments, but using the
   last argument as the tail of the constructed ulist."
  (declare (dynamic-extent args))
  (labels ((aux (first rest)
             (if (null rest)
                 (prog1 (the ulist first)
                   (check-type first (or ucons null)))
                 (ucons first (aux (car rest) (cdr rest))))))
    (aux (first args) (rest args))))

(define-compiler-macro ulist* (&rest arg-forms)
  (let* ((n (length arg-forms))
         (gensyms (loop repeat n collect (gensym "ARG"))))
    `(let* ,(mapcar #'list gensyms arg-forms)
       ,(let* ((rgensyms (reverse gensyms))
               (result-form
                 `(prog1 ,(car rgensyms)
                    (check-type ,(car rgensyms)
                                (or ucons null)))))
          (loop for gensym in (cdr rgensyms)
                do (setf result-form `(ucons ,gensym ,result-form)))
          result-form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ulist iteration

(defmacro do-ulist ((var ulist &optional result) &body body)
  (check-type var symbol)
  (multiple-value-bind (forms decls)
      (alexandria:parse-body body)
    (alexandria:once-only (ulist)
      (alexandria:with-gensyms (start)
        `(block nil
           (tagbody
              ,start
              (when ,ulist
                (let ((,var (ucar ,ulist)))
                  ,@decls
                  (tagbody ,@forms))
                (setf ,ulist (ucdr ,ulist))
                (go ,start)))
           (let ((,var nil))
             (declare (ignorable ,var))
             ,result))))))

(declaim (inline umapcar))
(defun umapcar (function &rest sequences)
  "Return an ulist containing the results of applying FUNCTION to
successive elements of the supplied sequences.  The resulting ulist is as
long as the shortest supplied sequence."
  (declare (dynamic-extent sequences)
           (function function))
  (let ((length (reduce #'min sequences :key #'length))
        (stack-allocation-threshold 11))
    (flet ((ulist-from-buffer (buffer)
             (let ((ulist '()))
               (loop for index from (1- length) downto 0 do
                 (setf ulist (ucons (aref buffer index) ulist)))
               ulist)))
      (if (<= length stack-allocation-threshold)
          (let ((buffer (make-array stack-allocation-threshold)))
            (declare (dynamic-extent buffer))
            (apply #'map-into buffer function sequences)
            (ulist-from-buffer buffer))
          (let ((buffer (make-array length)))
            (apply #'map-into buffer function sequences)
            (ulist-from-buffer buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; miscellaneous ulist utilities

(defun ulength (ulist)
  "Return the length of the given ulist."
  (declare (ulist ulist) (optimize speed))
  (loop for elt = ulist then (ucdr elt)
        while elt count t))

(defun list-from-ulist (ulist)
  "Return a list of the elements of ULIST."
  (declare (ulist ulist))
  (loop for elt = ulist then (ucdr elt)
        while elt collect (ucar elt)))

(defun ulist-from-list (list)
  (etypecase list
    (null '())
    (cons (ucons
           (car list)
           (ulist-from-list (cdr list))))))

(defun tree-from-utree (utree)
  "Return a tree of the same shape as UTREE, but where all occuring ulists
have been converted to lists."
  (if (not (uconsp utree))
      utree
      (loop for elt = utree then (ucdr elt) while elt
            for car = (ucar elt)
            collect (tree-from-utree car))))

(defun utree-from-tree (tree)
  (if (atom tree)
      tree
      (ucons (utree-from-tree (car tree))
             (utree-from-tree (cdr tree)))))
