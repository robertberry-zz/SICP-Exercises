; Answers for 2-5-1

; Far too lazy to repeatedly type out defuns ;D
(defmacro defgen (name (&rest arguments))
  `(defun ,name (,@arguments)
     (apply-generic ',name ,@arguments)))
       
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
             (put op (list type type)
                  (compose #'tag f)))
           (add-binary-ops (ops-list)
             (mapcar (lambda (args)
                       (add-binary-op (car args)
                                      (cadr args))))
             ops-list))
           (add-unary-op (op f)
             (put op type (compose #'tag f)))
           (add-unary-ops (ops-list)
             (mapcar (lambda (args)
                       (add-unary-op (car args)
                                     (cadr args)))
                     ops-list)))
  (prog nil
     (add-binary-ops binary-ops)
     (add-unary-ops unary-ops)))

(defun install-lisp-number-package ()
  (prog nil
     (install-operators 'lisp-number
                        (list (list 'make #'identity))
                        (list (list 'add #'+)
                              (list 'sub #'-)
                              (list 'mul #'*)
                              (list 'div #'/)))
     'done))

(defun make-lisp-number (n)
  (funcall (get 'make 'lisp-number) n))

(defun install-rational-package ()
  (labels ((numer (x)
             (car x))
           (denom (x)
             (cdr x))
           (make-rat (n d)
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
  (funcall (get 'make 'rational) n d))

(defun install-complex-package ()
  (labels ((make-from-real-imag (x y)
             (funcall (get 'make-from-real-imag 'rectangular) x y))
           (make-from-mag-ang (r a)
             (funcall (get 'make-from-mag-ang 'polar) r a))

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
  (funcall (get 'make-from-real-imag 'complex) x y))

(defun make-complex-from-mag-ang (r a)
  (funcall (get 'make-from-mag-ang 'complex) r a))


; Exercise 2.77

;; First it's dispatched to the 'real-part for complex - this strips off the
;; complex tag, passing the data to the newly defined operators. This then
;; dispatches this data again to 'real-part, now for either 'polar or
;; 'rectangular. The original function returns the correct data.

; Exercise 2.78

(defun type-tag (x)
  (cond ((numberp x) 'lisp-number)
        ((listp x) (car x))
        (t "Unrecognized type -- TYPE-TAG")))

(defun contents (x)
  (cond ((numberp x) x)
        ((listp x) (cdr x))
        (t "Untyped data -- CONTENTS")))

(defun attach-tag (tag data)
  (if (eq tag 'lisp-number)
      data
      (cons tag data)))

; Exercise 2.79

(defgen equ? (x y))

(put 'equ? '(lisp-number lisp-number) #'=)

(put 'equ? '(rational rational) (lambda (x y)
                                  (and (= (numer x) (numer y))
                                       (= (denom x) (denom y)))))

(put 'equ? '(complex complex) (lambda (x y)
                                (and (= (real-part x) (real-part y))
                                     (= (imag-part x) (imag-part y)))))

;; some extra ones for fun ;D

(put 'equ? '(lisp-number rational) (lambda (x y)
                                     (= x (/ (numer y) (denom y)))))

(put 'equ? '(lisp-number complex) (lambda (x y)
                                    (and (= (imag-part y) 0)
                                         (= (real-part y) x))))

(put 'equ? '(rational complex) (lambda (x y)
                                 (and (= (imag-part y) 0)
                                      (= (real-part y) (/ (numer x)
                                                          (denom x))))))

; Exercise 2.80

(defgen =zero? (x))

(put '=zero? '(lisp-number) 'zerop)

(put '=zero? '(rational) (lambda (x)
                           (= (numer x) 0)))

(put '=zero? '(complex) (lambda (x)
                          (and (= (real-part x) 0)
                               (= (imag-part x) 0))))


