; Answers for 2-5-2

(load "ex251.lisp")
(load "coercions.lisp")

(install-lisp-number-package)
(install-rational-package)
(install-complex-package)
(install-rectangular-package)
(install-polar-package)

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
         (proc (get-generic op type-tags t)))
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

(defun available-coercions (types)
  (labels ((unable-to-coerce-all (types)
             (some #'null types))
           (coercions-for-type (coerce-to)
             (mapcar (lambda (coerce-from)
                       (if (eq coerce-from coerce-to)
                           #'identity
                           (get-coercion coerce-from coerce-to))) types)))
    (remove-if #'unable-to-coerce-all
               (mapcar #'coercions-for-type (remove-duplicates types)))))

(defun all-same-type (type-tags)
  (let ((x (car type-tags)))
    (every (lambda (y)
             (eq x y)) (cdr type-tags))))

(defun apply-generic (op &rest args)
  (labels ((iter (args)
             (let* ((type-tags (mapcar #'type-tag args))
                    (proc (get-generic op type-tags t)))
               (if proc
                   (apply proc (mapcar #'contents args))
                   (if (all-same-type type-tags)
                       nil
                       (some #'iter
                             (mapcar (lambda (coercions)
                                       (mapcar #'funcall
                                               coercions args))
                                     (available-coercions type-tags))))))))
    (or (iter args)
        (error "Unable to apply generic ~a for types ~a"
               op
               (mapcar #'type-tag args)))))

;; This performs all coercions to single types. You can edit
;; 'available-coercions' to iterate through every possible combination of
;; coercions. (TODO: implement this)

;; The strategy is not sufficiently general when there's a function for which
;; there are suitable mixed-type operations that depend on ordering, e.g. a
;; situation in which a version of the generic exists for (rational complex)
;; but not for (complex rational).

;; I can't think of any functions for which this is true at the moment,
;; however. (TODO: think of some ... )


; Exercise 2.83

(defgen raise (x))

(put-generic 'raise '(lisp-number) (lambda (x)
                               (make-rational x 1)))

(defun make-real (x)
  (attach-tag 'real x))

(put-generic 'raise '(rational) (lambda (x)
                                  (make-real (* 1.0 (/ (numer x) (denom x))))))

(put-generic 'raise '(real) (lambda (x)
                    (make-complex-from-real-imag x 0)))

; Exercise 2.84

(defun const (x)
  (lambda (y)
    x))

(defparameter *parent-class-table* (make-hash-table))

(defun set-parent (cls parent)
  (setf (gethash cls *parent-class-table*) parent))

(defun get-parent (cls)
  (gethash cls *parent-class-table* nil))

(defun parents (cls)
  (let ((parent (get-parent cls)))
    (if (null parent)
        '()
        (cons parent (parents parent)))))

(set-parent 'rational 'lisp-number)
(set-parent 'real 'rational)
(set-parent 'complex 'real)

;; STILL TO DO ... 

; Exercise 2.85

(defgen project (x))

(put-generic 'project '(complex) #'real-part)

(defun real-to-rational (x)
  "Performs a rough conversion between a real number and a rational one. (May
  be inaccurate; is only being used for projection.)"
  (multiple-value-bind (whole fractional) (floor (abs x))
    (if (/= 0 fractional)
        (let ((inverse-fractional (round (/ 1 fractional))))
          (make-rational (* (1+ (round (/ whole (/ 1 inverse-fractional))))
                            (if (plusp x) 1 -1))
                         inverse-fractional))
        (make-rational x 1))))

(put-generic 'project '(real) #'real-to-rational)

(put-generic 'project '(rational) #'numer)

;; TODO ... 