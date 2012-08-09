; Code for dealing with coercions

(defparameter *coercions-table* '())

(defun get-coercion (from to)
  (let* ((types (list from to))
         (coercion (find-if (lambda (coercion)
                              (equal types (car coercion))) *coercions-table*)))
    (when coercion
      (cadr coercion))))

(defun put-coercion (from to f)
  (cond ((eq from to) (error "You cannot coerce to the same type (~a)" from))
         ((get-coercion from to)
          (error "Coercion already exists for ~a -> ~a" from to))
         (t (setf *coercions-table* (cons (list (list from to)
                                                f)
                                          *coercions-table*)))))


