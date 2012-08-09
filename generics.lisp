; Code for dealing with generics

(defparameter *generics-table* (make-hash-table))

(defun get-generic-flist (generic)
  (gethash generic *generics-table*))

(define-condition undefined-generic-error (error)
  ((function-name
    :initarg :name
    :accessor function-name
    :initform nil
    :documentation "Name of the generic function")
   (type-list
    :initarg :types
    :accessor types
    :initform nil
    :documentation "The types of the arguments passed to the function")))

(defmethod print-object ((object undefined-generic-error) stream)
  (format stream "No generic for ~a with types ~a"
          (function-name object)
          (types object)))

(defun get-generic (generic type-list &optional no-error)
  (let* ((f-list (get-generic-flist generic))
         (f (find-if (lambda (x)
                     (equal type-list (car x))) f-list)))
    (if (and (null f) (not no-error))
        (error 'undefined-generic-error :name generic :types type-list)
        (cadr f))))

(defun put-generic (generic type-list f)
  (if (get-generic generic type-list t)
      (error "Generic ~a already defined for types ~a" generic type-list)
      (setf (gethash generic *generics-table*)
            (cons (list type-list f) (gethash generic *generics-table* '())))))

(defun attach-tag (type-tag contents)
  (cons type-tag contents))

(defun type-tag (datum)
  (if (listp datum)
      (car datum)
      (error "Untagged data ~a" datum)))

(defun contents (datum)
  (if (listp datum)
      (cdr datum)
      (error "Untagged data ~a" datum)))

(defun apply-generic (name &rest arguments)
  (let ((type-tags (mapcar #'type-tag arguments)))
    (apply (get-generic name type-tags)
           (mapcar #'contents arguments))))

; Far too lazy to repeatedly type out defuns ;D
(defmacro defgen (name (&rest arguments))
  `(defun ,name (,@arguments)
     (apply-generic ',name ,@arguments)))
