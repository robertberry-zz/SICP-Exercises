; Answers for 2-5-2

(load "ex251.lisp")
(load "coercions.lisp")

(install-lisp-number-package)
;(install-rational-package) ;< turning this off, redefining in ex253
;(install-complex-package)      These packages are redefined below.
;(install-rectangular-package)
;(install-polar-package)

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
                   (list (apply proc (mapcar #'contents args)))
                   (if (all-same-type type-tags)
                       nil
                       (some #'iter
                             (mapcar (lambda (coercions)
                                       (mapcar #'funcall
                                               coercions args))
                                     (available-coercions type-tags))))))))
    (let ((result (iter args)))
      (if (not (null result))
          (car result)
          (error "Unable to apply generic ~a for types ~a"
                 op
                 (mapcar #'type-tag args))))))

; I know in the above using the list to signal the iter worked is ugly - I
; wrote the function unthinkingly and had to patch it. todo: fix this

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

(defun parent-of (a b)
  (not (null (member a (parents b)))))

(set-parent 'rational 'lisp-number)
(set-parent 'real 'rational)
(set-parent 'complex 'real)

(defun lowest-type (type-list)
  (car (sort type-list #'parent-of)))

(defun apply-to-first (pred f seq)
  "Maps f to the first element of seq that matches pred."
  (if (null seq)
      '()
      (let ((x (car seq))
            (xs (cdr seq)))
        (if (funcall pred x)
            (cons (funcall f x) xs)
            (cons x (apply-to-first pred f xs))))))

(defun can-raise (type)
  "Whether the given type can be raised."
  (generic-exists 'raise (list type)))

(defun apply-generic (op &rest args)
  (labels ((iter (args)
             (let* ((type-tags (mapcar #'type-tag args))
                    (proc (get-generic op type-tags t)))
               (if proc
                   (list (apply proc (mapcar #'contents args)))
                   (let ((lowest (lowest-type type-tags)))
                     (if (can-raise lowest)
                         (iter (apply-to-first (lambda (x)
                                                 (eq (type-tag x) lowest))
                                               #'raise
                                               args))
                         nil))))))
    (let ((result (iter args)))
      (if (not (null result))
          (car result)
          (error "Unable to apply generic ~a for types ~a"
                 op
                 (mapcar #'type-tag args))))))

; Exercise 2.85

(defgen project (x))

(put-generic 'project '(complex) (lambda (x) (make-real (real-part x))))

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

(defun can-project (type)
  (generic-exists 'project (list type)))

(defun drop (x)
  "Drops x's type as far as possible."
  (let ((type-x (type-tag x)))
    (if (can-project type-x)
        (let ((project-x (project x)))
          (if (equ? (raise project-x) x)
              project-x
              x))
        x)))

(defun highest-type (type-list)
  (car (last (sort type-list #'parent-of))))

(defun apply-generic (op &rest args)
  (labels ((type-tags (args)
             (mapcar #'type-tag args))
           (get-proc (args)
             (get-generic op (type-tags args) t))
           (apply-proc (proc args)
             (apply proc (mapcar #'contents args)))
           (iter (args)
             (let ((type-tags (type-tags args))
                   (proc (get-proc args)))
               (if proc
                   (list (apply-proc proc args))
                   (let ((lowest (lowest-type type-tags)))
                     (if (can-raise lowest)
                         (iter (apply-to-first (lambda (x)
                                                 (eq (type-tag x) lowest))
                                               #'raise
                                               args))
                         nil))))))
    (let ((proc (get-proc args)))
      (if proc
          (apply-proc proc args) ; this is to stop drop infinitely recursing
          (let ((result (iter (mapcar #'drop args))))
            (if (not (null result))
                (car result)
                (error "Unable to apply generic ~a for types ~a"
                       op
                       (mapcar #'type-tag args))))))))

; Unsure whether it wants apply-generic to always 'simplify' or only simplify
; the arguments if a generic does not exist for their current types.

; todo: write a version that always simplifies


; Exercise 2.86

; The generic operations that operate on parts of the complex number
; (e.g. add) will need to be redefined so that they themselves use generic
; operations on the parts.

(defun install-rectangular-package ()
  (labels ((real-part (z)
             (car z))
           (imag-part (z)
             (cdr z))
           (make-from-real-imag (x y)
             (cons x y))
           (magnitude (z)
             (square-root (add (square (real-part z))
                        (square (imag-part z)))))
           (angle (z)
             (atangent (imag-part z) (real-part z)))
           (make-from-mag-ang (r a)
             (cons (* r (cosine a)) (* r (sine a)))))
    (install-operators 'rectangular
                       (list (list 'make-from-real-imag #'make-from-real-imag)
                             (list 'make-from-mag-ang #'make-from-mag-ang))
                       '())
    (put-generic 'real-part '(rectangular) #'real-part)
    (put-generic 'imag-part '(rectangular) #'imag-part)
    (put-generic 'magnitude '(rectangular) #'magnitude)
    (put-generic 'angle '(rectangular) #'angle)    
    'done))

(defun install-polar-package ()
  (labels ((magnitude (z)
             (car z))
           (angle (z)
             (cdr z))
           (make-from-mag-ang (r a) (cons r a))
           (real-part (z)
             (mul (magnitude z) (cosine (angle z))))
           (imag-part (z)
             (mul (magnitude z) (sine (angle z))))
           (make-from-real-imag (x y)
             (cons (square-root (+ (square x) (square y)))
                   (atangent y x))))
    (install-operators 'polar
                       (list (list 'make-from-real-imag #'make-from-real-imag)
                             (list 'make-from-mag-ang #'make-from-mag-ang))
                       '())
    (put-generic 'real-part '(polar) #'real-part)
    (put-generic 'imag-part '(polar) #'imag-part)
    (put-generic 'magnitude '(polar) #'magnitude)
    (put-generic 'angle '(polar) #'angle)
    'done))

(defun install-complex-package ()
  (labels ((make-from-real-imag (x y)
             (funcall (get-generic 'make-from-real-imag '(rectangular)) x y))
           (make-from-mag-ang (r a)
             (funcall (get-generic 'make-from-mag-ang '(polar)) r a))

           (add-complex (z1 z2)
             (make-from-real-imag (add (real-part z1) (real-part z2))
                                  (add (imag-part z1) (imag-part z2))))
           (sub-complex (z1 z2)
             (make-from-real-imag (sub (real-part z1) (real-part z2))
                                  (sub (imag-part z1) (imag-part z2))))
           (mul-complex (z1 z2)
             (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                                (add (angle z1) (angle z2))))
           (div-complex (z1 z2)
             (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                                (sub (angle z1) (angle z2)))))
    (install-operators 'complex
                       (list (list 'make-from-real-imag #'make-from-real-imag)
                             (list 'make-from-mag-ang #'make-from-mag-ang))
                       (list (list 'add #'add-complex)
                             (list 'sub #'sub-complex)
                             (list 'mul #'mul-complex)
                             (list 'div #'div-complex)))
    (put-generic 'real-part '(complex) #'real-part)
    (put-generic 'imag-part '(complex) #'imag-part)
    (put-generic 'angle '(complex) #'angle)
    (put-generic 'magnitude '(complex) #'magnitude)
    'done))

(defgen square-root (x))

(put-generic 'square-root '(lisp-number) #'sqrt)
(put-generic 'square-root '(real) #'sqrt)

(defgen atangent (x y))

(put-generic 'atangent '(lisp-number lisp-number) #'atan)
(put-generic 'atangent '(real real) #'atan)

(defgen cosine (x))

(put-generic 'cosine '(lisp-number) #'cos)
(put-generic 'cosine '(real) #'cos)

(defgen sine (x))

(put-generic 'sine '(lisp-number) #'sin)
(put-generic 'sine '(real) #'sin)

(defgen square (x))

(put-generic 'square '(lisp-number) (lambda (x) (* x x)))
(put-generic 'square '(real) (lambda (x) (* x x)))

;; This should actually be enough, due to the raising.


(install-complex-package)
(install-rectangular-package)
(install-polar-package)
