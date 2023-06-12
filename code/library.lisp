(in-package #:ucons)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ulist macros

(defmacro upush (object place &environment env)
  (multiple-value-bind (vars vals new-vars setter getter)
      (get-setf-expansion place env)
    (alexandria:once-only (object)
      `(let* (,@(mapcar #'list vars vals)
              (,(car new-vars) (ucons ,object ,getter))
              ,@(cdr new-vars))
         ,setter))))

(defmacro upop (place &environment env)
  (multiple-value-bind (vars vals new-vars setter getter)
      (get-setf-expansion place env)
    (alexandria:with-gensyms (tmp)
      `(let* (,@(mapcar #'list vars vals)
              (,tmp ,getter)
              ,@(cdr new-vars))
         (let ((,(car new-vars) (ucdr ,tmp)))
           ,setter
           (ucar ,tmp))))))

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
                (upop ,ulist)
                (go ,start)))
           (let ((,var nil))
             (declare (ignorable ,var))
             ,result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ulist creation

(defun ulist (&rest args)
  "Return the ulist associated with the supplied arguments."
  (declare (dynamic-extent args))
  (reduce #'ucons args
          :from-end t
          :initial-value '()))

(define-compiler-macro ulist (&rest args)
  (flet ((symbolic-ucons (car cdr)
           `(ucons ,car ,cdr)))
    (reduce #'symbolic-ucons args
            :from-end t
            :initial-value '())))

(defun ulist* (&rest args)
  "Return the ulist associated with the supplied arguments, but using the
   last argument as the tail of the constructed ulist."
  (reduce #'ucons args
          :from-end t))

(define-compiler-macro ulist* (&rest arg-forms)
  (let* ((n (length arg-forms))
         (gensyms (loop repeat n collect (gensym "ARG"))))
    `(let* ,(mapcar #'list gensyms arg-forms)
       ,(let* ((rgensyms (reverse gensyms))
               (result-form (car rgensyms)))
          (loop for gensym in (cdr rgensyms)
                do (setf result-form `(ucons ,gensym ,result-form)))
          result-form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; pattern matching

(trivia:defpattern ucons (car cdr)
  (alexandria:with-gensyms (ucons)
    `(trivia:guard1 ,ucons (uconsp ,ucons)
                    (ucar ,ucons) ,car
                    (ucdr ,ucons) ,cdr)))

(trivia:defpattern ulist (&rest args)
  (if args
      `(ucons ,(first args) (ulist ,@(rest args)))
      `(null)))

(trivia:defpattern ulist* (&rest args)
  (if (rest args)
      `(ucons ,(first args) (ulist* ,@(rest args)))
      (first args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; miscellaneous ulist utilities

(defun ulength (ulist)
  "Return the length of the given ulist."
  (declare (ulist ulist) (optimize speed))
  (loop for rest = ulist then (ucdr rest)
        until (null rest) count t))

(defun unth (n ulist)
  (declare (type (and unsigned-byte fixnum) n)
           (ulist ulist))
  (loop repeat n do (upop ulist))
  (ucar ulist))

(defun unthcdr (n ulist)
  (declare (type (and unsigned-byte fixnum) n)
           (ulist ulist))
  (loop repeat n do (upop ulist))
  ulist)

(defun ulist-from-vector (vector)
  (declare (vector vector))
  (let ((ulist '()))
    (if (typep vector 'simple-vector)
        (loop for index from (1- (length vector)) downto 0 do
          (upush (svref vector index) ulist))
        (loop for index from (1- (length vector)) downto 0 do
          (upush (aref vector index) ulist)))
    ulist))

(defun vector-from-ulist (ulist)
  (declare (ulist ulist))
  (let ((vector (make-array (ulength ulist)))
        (index 0))
    (declare (fixnum index))
    (do-ulist (elt ulist vector)
      (setf (svref vector index) elt)
      (incf index))))

(defun list-from-ulist (ulist)
  "Return a (possibly dotted) list of the elements of the supplied ulist."
  (declare (ulist ulist))
  (let* ((head (cons nil nil))
         (tail head))
    (declare (cons head tail))
    (declare (dynamic-extent head))
    (loop for urest = ulist then (ucdr urest) do
      (etypecase urest
        (ucons
         (let ((new-tail (list (ucar urest))))
           (setf (cdr tail) new-tail)
           (setf tail new-tail)))
        (t
         (setf (cdr tail) urest)
         (return))))
    (cdr head)))

(defun ulist-from-list (list)
  (reduce #'ucons list
          :from-end t
          :initial-value '()))

(defun tree-from-utree (utree)
  "Return a tree of the same shape as UTREE, but where all occuring ulists
have been converted to lists."
  (if (not (uconsp utree))
      utree
      (loop for rest = utree then (ucdr rest)
            until (null rest)
            collect (tree-from-utree (ucar rest)))))

(defun utree-from-tree (tree)
  (if (atom tree)
      tree
      (ucons (utree-from-tree (car tree))
             (utree-from-tree (cdr tree)))))

(defun umapcar (function &rest sequences)
  "Return an ulist containing the results of applying FUNCTION to
successive elements of the supplied sequences.  The resulting ulist is as
long as the shortest supplied sequence."
  (declare (dynamic-extent sequences)
           (function function))
  (let* ((length (reduce #'min sequences :key #'length))
         (buffer (make-array length)))
    (apply #'map-into buffer function sequences)
    (ulist-from-vector buffer)))
