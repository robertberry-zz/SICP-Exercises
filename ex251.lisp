; Answers for 2-5-1

(load "generics.lisp")

(defgen add (x y))
(defgen sub (x y))
(defgen mul (x y))
(defgen div (x y))

(defun compose (f g)
  (lambda (&rest args)
    (funcall f (apply g args))))

; This hideous function will aid further laziness ... 
(defun install-operators (type unary-ops binary-ops)
  (labels ((tag (x)
             (attach-tag type x))
           (add-binary-op (op f)
             (put-generic op (list type type)
                          (compose #'tag f)))
           (add-binary-ops (ops-list)
             (mapcar (lambda (args)
                       (add-binary-op (car args)
                                      (cadr args)))
             ops-list))
           (add-unary-op (op f)
             (put-generic op (list type) (compose #'tag f)))
           (add-unary-ops (ops-list)
             (mapcar (lambda (args)
                       (add-unary-op (car args)
                                     (cadr args)))
                     ops-list)))
     (add-binary-ops binary-ops)
     (add-unary-ops unary-ops)
     'done))

(defun install-lisp-number-package ()
  (install-operators 'lisp-number
                     (list (list 'make #'identity))
                     (list (list 'add #'+)
                           (list 'sub #'-)
                           (list 'mul #'*)
                           (list 'div #'/))))

(defun make-lisp-number (n)
  (funcall (get-generic 'make 'lisp-number) n))

(defun numer (x)
  (car x))

(defun denom (x)
  (cdr x))

(defun install-rational-package ()
  (labels ((make-rat (n d)
             (let ((g (gcd n d)))
               (cons (/ n g) (/ d g))))
           (add-rat (x y)
             (make-rat (+ (* (numer x) (denom y))
                          (* (numer y) (denom x)))
                       (* (denom x) (denom y))))
           (sub-rat (x y)
             (make-rat (- (* (numer x) (denom y))
                          (* (numer y) (denom x)))
                       (* (denom x) (denom y))))
           (mul-rat (x y)
             (make-rat (* (numer x) (numer y))
                       (* (denom x) (denom y))))
           (div-rat (x y)
             (make-rat (* (numer x) (denom y))
                       (* (denom x) (numer y)))))
    (install-operators 'rational
                       (list (list 'make #'make-rat))
                       (list (list 'add #'add-rat)
                             (list 'sub #'sub-rat)
                             (list 'mul #'mul-rat)
                             (list 'div #'div-rat)))
    'done))

(defun make-rational (n d)
  (funcall (get-generic 'make '(rational)) n d))

(defun square (x)
  (* x x))

(defgen real-part (z))
(defgen imag-part (z))
(defgen magnitude (z))
(defgen angle (z))

(defun install-rectangular-package ()
  (labels ((real-part (z)
             (car z))
           (imag-part (z)
             (cdr z))
           (make-from-real-imag (x y)
             (cons x y))
           (magnitude (z)
             (sqrt (+ (square (real-part z))
                      (square (imag-part z)))))
           (angle (z)
             (atan (imag-part z) (real-part z)))
           (make-from-mag-ang (r a)
             (cons (* r (cos a)) (* r (sin a)))))
    (install-operators 'rectangular
                       (list (list 'make-from-real-imag #'make-from-real-imag
                                   'make-from-mag-ang #'make-from-mag-ang))
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
             (* (magnitude z) (cos (angle z))))
           (imag-part (z)
             (* (magnitude z) (sin (angle z))))
           (make-from-real-imag (x y)
             (cons (sqrt (+ (square x) (square y)))
                   (atan y x))))
    (install-operators 'polar
                       (list (list 'make-from-real-imag #'make-from-real-imag
                                   'make-from-mag-ang #'make-from-mag-ang))
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
             (make-from-real-imag (+ (real-part z1) (real-part z2))
                                  (+ (imag-part z1) (imag-part z2))))
           (sub-complex (z1 z2)
             (make-from-real-imag (- (real-part z1) (real-part z2))
                                  (- (imag-part z1) (imag-part z2))))
           (mul-complex (z1 z2)
             (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                                (+ (angle z1) (angle z2))))
           (div-complex (z1 z2)
             (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                                (- (angle z1) (angle z2)))))
    (install-operators 'complex
                       (list (list 'make-from-real-imag #'make-from-real-imag)
                             (list 'make-from-mag-ang #'make-from-mag-ang))
                       (list (list 'add #'add-complex)
                             (list 'sub #'sub-complex)
                             (list 'mul #'mul-complex)
                             (list 'div #'div-complex)))
    'done))

;; All the above could be made even more concise and clean with a macro ...!

(defun make-complex-from-real-imag (x y)
  (funcall (get-generic 'make-from-real-imag '(complex)) x y))

(defun make-complex-from-mag-ang (r a)
  (funcall (get-generic 'make-from-mag-ang '(complex)) r a))


; Exercise 2.77

;; First it's dispatched to the 'real-part for complex - this strips off the
;; complex tag, passing the data to the newly defined operators. This then
;; dispatches this data again to 'real-part, now for either 'polar or
;; 'rectangular. The original function returns the correct data.

; Exercise 2.78

(defun type-tag (x)
  (cond ((numberp x) 'lisp-number)
        ((listp x) (car x))
        (t (error "Unrecognized type ~a" x))))

(defun contents (x)
  (cond ((numberp x) x)
        ((listp x) (cdr x))
        (t (error "Untyped data ~a" x))))

(defun attach-tag (tag data)
  (if (eq tag 'lisp-number)
      data
      (cons tag data)))

; Exercise 2.79

(defgen equ? (x y))

(put-generic 'equ? '(lisp-number lisp-number) #'=)

(put-generic 'equ? '(rational rational) (lambda (x y)
                                  (and (= (numer x) (numer y))
                                       (= (denom x) (denom y)))))

(put-generic 'equ? '(complex complex) (lambda (x y)
                                (and (= (real-part x) (real-part y))
                                     (= (imag-part x) (imag-part y)))))

;; some extra ones for fun ;D

(put-generic 'equ? '(lisp-number rational) (lambda (x y)
                                     (= x (/ (numer y) (denom y)))))

(put-generic 'equ? '(lisp-number complex) (lambda (x y)
                                    (and (= (imag-part y) 0)
                                         (= (real-part y) x))))

(put-generic 'equ? '(rational complex) (lambda (x y)
                                 (and (= (imag-part y) 0)
                                      (= (real-part y) (/ (numer x)
                                                          (denom x))))))

; Exercise 2.80

(defgen =zero? (x))

(put-generic '=zero? '(lisp-number) 'zerop)

(put-generic '=zero? '(rational) (lambda (x)
                           (= (numer x) 0)))

(put-generic '=zero? '(complex) (lambda (x)
                          (and (= (real-part x) 0)
                               (= (imag-part x) 0))))

