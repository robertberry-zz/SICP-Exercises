; Answers for 2-5-2

(defun lisp-number->complex (n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'lisp-number 'complex #'lisp-number->complex)

; Exercise 2.81

;; a) It gets stuck in an infinite loop. apply-generic tries to find the procedure
;;    for the types, it fails. The types then get coerced to themselves. apply-generic
;;    is called yet again ... etc.

;; b) apply-generic works correctly, but it does unneccessary work (trying to
;;    locate coercions that could not possibly exist).

;; c)

(defun apply-generic (op &rest args)
  (let* ((type-tags (mapcar #'type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (mapcar #'contents args))
        (if (and (= (length args) 2))
            (let ((type1 (car type-tags))
                  (type2 (cadr type-tags)))
              (if (eq type1 type2)
                  (error "No method for these types.")
                  (let ((a1 (car args))
                        (a2 (cadr args))
                        (t1->t2 (get-coercion type1 type2))
                        (t2->t1 (get-coercion type2 type1)))
                    (cond (t1->t2
                           (apply-generic op (funcall t1->t2 a1) a2))
                          (t2->t1
                           (apply-generic op a1 (funcall t1->t2 a2)))
                          (t
                           (error "No method for these types."))))))))))

; Exercise 2.82

;; TODO ... 