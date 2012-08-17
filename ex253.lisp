; Answers for 2-5-3

(load "ex252.lisp")

; had to extract these outside of install-polynomial-package. a term-list
; should probably be its own type providing these operations.
(defun term-list (p)
  (cdr p))

(defun poly-variable (p)
  (car p))

(defun install-polynomial-package ()
  (labels ((tag (p)
             (attach-tag 'polynomial p))
           (make-poly (variable term-list)
             (cons variable term-list))
           (variable? (x)
             (symbolp x))
           (same-variable? (x y)
             (and (variable? x) (variable? y) (eq x y)))
           (add-poly (p1 p2)
             (if (same-variable? (poly-variable p1) (poly-variable p2))
                 (make-poly (poly-variable p1)
                            (add-terms (term-list p1)
                                       (term-list p2)))
                 (error "Polys not in same var ~a, ~a" p1 p2)))
           (sub-poly (p1 p2)
             (if (same-variable? (poly-variable p1) (poly-variable p2))
                 (make-poly (poly-variable p1)
                            (sub-terms (term-list p1)
                                       (term-list p2)))
                 (error "Polys not in same var ~a, ~a" p1 p2)))
           (mul-poly (p1 p2)
             (if (same-variable? (poly-variable p1) (poly-variable p2))
                 (make-poly (poly-variable p1)
                            (mul-terms (term-list p1)
                                       (term-list p2)))
                 (error "Polys not in same var ~a, ~a" p1 p2)))
           (div-poly (p1 p2)
             (if (same-variable? (poly-variable p1) (poly-variable p2))
                 (let ((var (poly-variable p1))
                       (result (div-terms (term-list p1)
                                          (term-list p2))))
                   (mapcar (lambda (term-list)
                             (make-polynomial var term-list))
                           result))
                 (error "Polys not in same var ~a, ~a" p1 p2))))
    (put-generic 'div '(polynomial polynomial) #'div-poly)
    (install-operators 'polynomial
                       (list (list 'make #'make-poly))
                       (list (list 'add #'add-poly)
                             (list 'sub #'sub-poly)
                             (list 'mul #'mul-poly)))))

(defun add-terms (l1 l2)
  (cond ((empty-termlist? l1) l2)
        ((empty-termlist? l2) l1)
        (t
         (let ((t1 (first-term l1))
               (t2 (first-term l2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms l1) l2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms l1 (rest-terms l2))))
                 (t
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms l1)
                              (rest-terms l2)))))))))

(defun mul-terms (l1 l2)
  (if (empty-termlist? l1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term l1) l2)
                 (mul-terms (rest-terms l1) l2))))

(defun mul-term-by-all-terms (t1 l)
  (if (empty-termlist? l)
      (the-empty-termlist)
      (let ((t2 (first-term l)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms l))))))


(defun adjoin-term (term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(defun the-empty-termlist ()
  '())

(defun first-term (term-list)
  (car term-list))

(defun rest-terms (term-list)
  (cdr term-list))

(defun empty-termlist? (term-list)
  (null term-list))

(defun make-term (order coeff)
  (list order coeff))

(defun order (term)
  (car term))

(defun coeff (term)
  (cadr term))

(defun make-polynomial (var terms)
  (funcall (get-generic 'make '(polynomial)) var terms))

(install-polynomial-package)

; Exercise 2.87

(defun zero-termlist? (term-list)
  (every #'=zero? (mapcar #'coeff term-list)))
; The above function breaks data hiding (through using mapcar). It could be
; implemented using first-term and rest-terms to avoid this - or we could
; define a generic iteration function, and build a new map function over it.

(put-generic '=zero? '(polynomial) (lambda (p)
                                     (zero-termlist? (term-list p))))

; Exercise 2.88

;; See above package definition

(defun sub-terms (l1 l2)
  (add-terms l1 (mapcar (lambda (x)
                          (make-term (order x) (negate (coeff x)))) l2)))
; Again, mapcar ...

(defun negate (x)
  (mul -1 x))

(put-generic 'mul '(lisp-number polynomial)
             (lambda (x y)
               (make-polynomial (poly-variable y)
                                (mapcar (lambda (z)
                                          (make-term (order z)
                                                     (mul x (coeff z))))
                                        (term-list y)))))

; This code could be clarified by defining a type for a 'term and 'term-list


; Exercise 2.89

(defun order-front (term-list)
  "Order of the item at the front of the term-list. (This might not be the
  'first' item, which is the first item whose coefficient is not zero.)"
  (1- (length term-list)))

(defun first-term-2 (term-list)
  (if (=zero? (car term-list))
      (first-term-2 (cdr term-list))
      (make-term (order-front term-list)
                 (car term-list))))

(defun drop-while (f seq)
  (if (funcall f (car seq))
      (drop-while f (cdr seq))
      seq))

(defun rest-terms-2 (term-list)
  (labels ((iter (terms)
             (cond ((null terms) '())
                   ((not (=zero? (car terms)))
                    (drop-while #'=zero? (cdr terms)))
                   (t (iter (cdr terms))))))
    (iter term-list)))

; The call to length would be pretty inefficient for a large list of terms (as
; it would take O(n) time to get the first term). If expecting this kind of
; data, should be implemented as a data structure that encodes its length when
; it is created.

(defun zeros (n)
  "Create a list of zeros n elements long."
  (if (= n 0)
      '()
      (cons 0 (zeros (1- n)))))

(defun set-nth (seq n val)
  (cond ((null seq) '())
        ((= n 0) (cons val (cdr seq)))
        (t (cons (car seq) (set-nth (cdr seq) (1- n) val)))))

(defun adjoin-term-2 (term term-list)
  (let ((order-first (order-front term-list))
        (order-term (order term)))
    (if (> order-term order-first)
        (append (list order-term) (zeros (- order-term
                                            order-first 1)) term-list)
        (set-nth term-list (- order-first order-term) (coeff term)))))

;
; Exercise 2.90

;; Tried doing this one, made a massive mess, reverted.

;; todo: retry!


; Exercise 2.91

;; First let me work through the division to make sure I understand it

;         x^5 - 1
;         ------- = x^3 ... 
;         x^2 - 1

;  x^3 * (x^2 - 1) = x^5 - x^3

;         x^3 - 1
;         -------  = x^3 + x ...
;         x^2 - 1

;  (x^3 + x)(x^2 - 1) = x^5 + x^3 - x - x^3 = x^5 - x

;          x - 1
;         -------  = x^3 + x, remainder: x - 1
;         x^2 - 1

; OK ... that makes sense.

(defun div-terms (l1 l2)
  (if (empty-termlist? l1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term l1))
            (t2 (first-term l2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) l1)
            (let* ((new-c (div (coeff t1) (coeff t2)))
                   (new-o (- (order t1) (order t2)))
                   (new-term (make-term new-o new-c)))
              (let ((rest-of-result
                     (div-terms (sub-terms l1
                                           (mul-terms l2
                                                      (adjoin-term
                                                       new-term
                                                       (the-empty-termlist)))) l2)
                     ))
                (list (cons new-term (car rest-of-result))
                      (cadr rest-of-result))
                ))))))

; slightly brain-melting ...

;; Added the poly function to the package def at the top

;; CL-USER> (div (make-polynomial 'x '((5 1) (0 -1)))
;;               (make-polynomial 'x '((2 1) (0 -1))))
;; ((POLYNOMIAL X (3 1) (1 1)) (POLYNOMIAL X (1 1) (0 -1)))



; Exercise 2.92

(defun symbol> (a b)
  "Returns whether a symbol's name occurs alphabetically after another's."
  (not (null (string> (symbol-name a)      ; string> returns 0 instead of t??
                      (symbol-name b)))))

(defun symbol< (a b)
  (symbol> b a))


; First let's make a function that turns this ... 

;;   x^2 (y + 1)

; ... into this ...

;;   y (x^2)  +  1 (x^2)


(defun singleton-termlist (t1)
  (adjoin-term t1 (the-empty-termlist)))

(defun polynomial? (x)
  (and (listp x) (eq (car x) 'polynomial)))

(defun expand-term (t1 var)
  "Given a term of a given variable and a list of terms that are its
   coefficient, expands."
  (labels ((iter (tl2)
             (if (null tl2)
                 (the-empty-termlist)
                 (let ((t2 (first-term tl2)))
                   (adjoin-term (make-term (order t2)
                                           (make-polynomial var
                                                            (singleton-termlist
                                                             (make-term (order t1)
                                                                        1))))
                                (iter (rest-terms tl2)))))))
    (let* ((inner-poly (cdr (coeff t1)))
           (inner-var (poly-variable inner-poly))
           (terms (term-list inner-poly)))
      (make-polynomial inner-var (iter terms)))))

;; CL-USER> (defvar poly (make-term 2 (make-polynomial 'y (adjoin-term (make-term 1 1) (singleton-termlist (make-term 0 1))))))
;; POLY
;; CL-USER> poly
;; (2 (POLYNOMIAL Y (1 1) (0 1)))
;; CL-USER> (expand-term poly 'x)
;; (POLYNOMIAL Y (1 (POLYNOMIAL X (2 1))) (0 (POLYNOMIAL X (2 1))))

;; OK ... thought about it & that doesn't help.

; I want something that converts this ...

;;    z^2 (y^3(x + 1)) + 3z

; Into this ...

;;   z^2 * x * y^3 + z^2 * y^3 * x^0 + 3z

; Then this ...

;;   x(y^3(z^2)) + 1(y^3(z^2) + 1(3z))

;; I'm going to use an intermediate form, which will be collections of variables to given
;; powers and with given (non-polynomial) coefficients.

(defun make-inter-var (var order coeff)
  (list var order coeff))

(defun order-inter-var (x)
  (cadr x))

(defun coeff-inter-var (x)
  (caddr x))

(defun variable-inter-var (x)
  (car x))

(defun join-inter-vars (v1 vl2)
  (mapcar (lambda (v2s)
            (cons v1 v2s)) vl2))

(defgen expand (x))

(put-generic 'expand '(polynomial) 
             (lambda (poly)
               (let ((var (poly-variable poly)))
                 (labels ((iter (terms)
                            (if (null terms)
                                '()
                                (let* ((t1 (first-term terms))
                                       (t1-coeff (coeff t1))
                                       (t1-order (order t1))
                                       (rest (iter (rest-terms terms))))
                                  (if (polynomial? t1-coeff)
                                      (append (join-inter-vars (make-inter-var var t1-order 1)
                                                               (expand t1-coeff))
                                              rest)
                                      (cons (list (make-inter-var var t1-order t1-coeff))
                                            rest))))))
                   (iter (term-list poly))))))

(defparameter +no-expands+ '(lisp-number real rational complex))

(mapcar (lambda (type)
          ; todo - make it retag datatypes like 'rational'
          (put-generic 'expand `(,type) #'list))
        +no-expands+)
  
;; CL-USER> test-poly-2
;; (POLYNOMIAL Z (2 (POLYNOMIAL Y (3 (POLYNOMIAL X (0 1) (1 1))))))
;; CL-USER> (expand test-poly-2)
;; (((Z 2 1) (Y 3 1) (X 0 1)) ((Z 2 1) (Y 3 1) (X 1 1)))

;; CL-USER> test-poly-3
;; (POLYNOMIAL Z (1 3) (2 (POLYNOMIAL Y (3 (POLYNOMIAL X (0 1) (1 1))))))
;; CL-USER> (expand test-poly-3)
;; (((Z 1 3)) ((Z 2 1) (Y 3 1) (X 0 1)) ((Z 2 1) (Y 3 1) (X 1 1)))

; OK ... so that bit was (relatively) easy. The next bit is going to be horrible.

(defun order-expansion (expansion)
  "Order an expansion to the canonical order of the variables (i.e. alpha-order)."
  (mapcar (lambda (vars)
            (sort vars #'symbol< :key #'variable-inter-var))
          expansion))

;; CL-USER> (order-expansion (expand test-poly-3))

;; (((Z 1 3)) ((X 0 1) (Y 3 1) (Z 2 1)) ((X 1 1) (Y 3 1) (Z 2 1)))

; I REALLY need to make a less horrid way to construct these things for testing. In the
; meantime saving this one here so I don't lose it!
(defvar test-poly-3 (make-polynomial
                     'z 
                     (adjoin-term
                      (make-term 1 3)
                      (singleton-termlist 
                       (make-term
                        2
                        (make-polynomial
                         'y
                         (singleton-termlist
                          (make-term
                           3
                           (make-polynomial
                            'x
                            (adjoin-term
                             (make-term 0 1) 
                             (singleton-termlist
                              (make-term 1 1))))))))))))

; WORK IN PROGRESS

; I need a function to normalize expansions, i.e. turn the following:

;; (((Z 1 3)) ((X 0 1) (Y 3 1) (Z 2 1)) ((X 1 1) (Y 3 1) (Z 2 1)))

; into this:

;; (((X 0 1) (Y 0 1) (Z 1 3)) ((X 0 1) (Y 3 1) (Z 2 1)) ((X 1 1) (Y 3 1) (Z 2 1)))

;; (although the first is necessary for the z ... what in the case of a solitary x?
;; it should not really add all the extra zero order expansions ...- think about this

(defun mappend (f seq)
  "Applies f to seq, appending resultant lists."
  (apply #'append (mapcar f seq)))

(defun variable-list (expansion)
  "Given an ordered expansion, returns a full list of variables involved."
  (remove-duplicates (mappend (lambda (var-list)
                                (mapcar #'variable-inter-var var-list))
                              expansion)))

(defun normalize-expansion (expansion)
  (let ((vars (variable-list expansion)))
    (labels ((normalize (var-list)
               (labels ((iter (var-list var-names)
                          (let ((var (car var-list))
                                (var-name (car var-names)))
                            (cond ((null var-list) '())
                                  ((null var-names) (error "Ran out of variable names!"))
                                  ((eq (variable-inter-var var) var-name)
                                   (cons var (iter (cdr var-list) (cdr var-names))))
                                  (t (cons (make-inter-var var-name 0 1)
                                           (iter var-list (cdr var-names))))))))
                 (iter var-list vars))))
      (mapcar #'normalize expansion))))

;; CL-USER> (normalize-expansion (order-expansion (expand test-poly-3)))

;; (((X 0 1) (Y 0 1) (Z 1 3)) ((X 0 1) (Y 3 1) (Z 2 1)) ((X 1 1) (Y 3 1) (Z 2 1)))

(defun canonical-poly (poly)
  (labels (;; Really ugly function. I think the problem is I chose a bad
           ;; representation for the intermediate variable groups. Only one of
           ;; the variables in any given group is ever going to have a
           ;; coefficient set due to the way we have implemented polynomials,
           ;; so in the majority of cases the polynomial is just 1. Would've
           ;; been better represented by lists inside lists, but then we're
           ;; just getting back to the original polynomial representation,
           ;; from which I should perhaps not have veered.
           (reconstruct-poly (vars)
             (if (null vars)
                 (error "Empty varlist.")
                 (let ((v (car vars))
                       (vs (cdr vars)))
                   (make-polynomial (variable-inter-var v)
                                    (singleton-termlist
                                     (make-term (order-inter-var v)
                                                (if (null vs)
                                                    (coeff-inter-var v)
                                                    (reconstruct-poly vs))))))))
           (merge-polys (polys)
             (reduce #'add (cdr polys) :initial-value (car polys))))
    (merge-polys (mapcar #'reconstruct-poly
                         (normalize-expansion (order-expansion (expand poly)))))))

; CL-USER> (canonical-poly test-poly-3)

; (POLYNOMIAL X (1 (POLYNOMIAL Y (3 (POLYNOMIAL Z (2 1)))))
; (0 (POLYNOMIAL Y (3 (POLYNOMIAL Z (2 1))) (0 (POLYNOMIAL Z (1 3))))))


;; ffs, testing this is horrible 

(defun print-poly (poly)
  "Very crappy print poly function to help me test. todo: make this better"
  (let* ((poly (cdr poly))
         (var (poly-variable poly)))
    (labels ((print-term (term)
               (let ((c (coeff term))
                     (o (order term)))
                 (if (and (not (polynomial? c))
                          (not (and (numberp c)
                                    (= c 1))))
                   (princ c))
                 (if (= o 1)
                     (princ var)
                     (if (= o 0)
                         (princ 1)
                         (format t "~a^~a" var o)))
                 (if (polynomial? c)
                     (prog nil
                        (princ "(")
                        (print-poly c)
                        (princ ")")))
                 (format t " + "))))
      (mapcar #'print-term (term-list poly)))))

;; anyway, the long and short of it is canonical poly works. you can change sub-poly, etc.
;; to invoke it on their arguments before adding, subtracting, etc.


; Exercise 2.93

(defun install-rational-package ()
  (labels ((make-rat (n d)
             (let ((reduced (reduce-for-rat n d)))
               (cons (car reduced) (cadr reduced))))
           (add-rat (x y)
             (make-rat (add (mul (numer x) (denom y))
                            (mul (numer y) (denom x)))
                       (mul (denom x) (denom y))))
           (sub-rat (x y)
             (make-rat (sub (mul (numer x) (denom y))
                            (mul (numer y) (denom x)))
                       (mul (denom x) (denom y))))
           (mul-rat (x y)
             (make-rat (mul (numer x) (numer y))
                       (mul (denom x) (denom y))))
           (div-rat (x y)
             (make-rat (mul (numer x) (denom y))
                       (mul (denom x) (numer y)))))
    (install-operators 'rational
                       (list (list 'make #'make-rat))
                       (list (list 'add #'add-rat)
                             (list 'sub #'sub-rat)
                             (list 'mul #'mul-rat)
                             (list 'div #'div-rat)))
    'done))

(install-rational-package)

(defvar p1 (make-polynomial 'x '((2 1) (0 1))))
(defvar p2 (make-polynomial 'x '((3 1) (0 1))))
(defvar rf (make-rational p1 p2))

;; CL-USER> rf
;; (RATIONAL (POLYNOMIAL X (2 1) (0 1)) POLYNOMIAL X (3 1) (0 1))

(defun gcd-terms (a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

; Exercise 2.94

(defun remainder-terms (a b)
  (cadr (div-terms a b)))

(defun gcd-poly (p1 p2)
  (labels ((variable? (x)
             (symbolp x))
           (same-variable? (x y)
             (and (variable? x) (variable? y) (eq x y))))
    (if (same-variable? (poly-variable p1) (poly-variable p2))
        (make-polynomial (poly-variable p1) (gcd-terms (term-list p1)
                                                       (term-list p2)))
        (error "Polys not in same var: ~a, ~a" (poly-variable p1) (poly-variable p2)))))

(defgen greatest-common-divisor (a b))

(put-generic 'greatest-common-divisor '(polynomial polynomial) #'gcd-poly)
(put-generic 'greatest-common-divisor '(lisp-number lisp-number) #'gcd)
(put-generic 'greatest-common-divisor '(real real) #'gcd)

(defvar p3 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(defvar p4 (make-polynomial 'x '((3 1) (1 -1))))


;; CL-USER> (greatest-common-divisor p3 p4)

;; (POLYNOMIAL X (2 -1) (1 1))

; p3:      x^4 - x^3 - 2x^2 + 2x
; p4:      x^3 - x
; gcd:     -x^2 + x

;; factorise ...

; p3:  (-x^2 + x)(-x^2 + 2)

; p4:  (-x^2 + x)(-1 - x)


; Exercise 2.95

(defvar p5 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(defvar p6 (make-polynomial 'x '((2 11) (0 7))))
(defvar p7 (make-polynomial 'x '((1 13) (0 5))))

(defvar q1 (mul p5 p6))
(defvar q2 (mul p5 p7))

;; CL-USER> q1
;; (POLYNOMIAL X (4 11) (3 -22) (2 18) (1 -14) (0 7))
;; CL-USER> q2
;; (POLYNOMIAL X (3 13) (2 -21) (1 3) (0 5))
;; CL-USER> (greatest-common-divisor q1 q2)
;; (POLYNOMIAL X (2 1458/169) (1 -2916/169) (0 1458/169))

;; CL-USER> (trace gcd-terms)
;; (GCD-TERMS)
;; CL-USER> (greatest-common-divisor q1 q2)
;;   0: (GCD-TERMS ((4 11) (3 -22) (2 18) (1 -14) (0 7))
;;                 ((3 13) (2 -21) (1 3) (0 5)))
;;     1: (GCD-TERMS ((3 13) (2 -21) (1 3) (0 5))
;;                   ((2 1458/169) (1 -2916/169) (0 1458/169)))
;;       2: (GCD-TERMS ((2 1458/169) (1 -2916/169) (0 1458/169)) NIL)
;;       2: GCD-TERMS returned ((2 1458/169) (1 -2916/169) (0 1458/169))
;;     1: GCD-TERMS returned ((2 1458/169) (1 -2916/169) (0 1458/169))
;;   0: GCD-TERMS returned ((2 1458/169) (1 -2916/169) (0 1458/169))
;; (POLYNOMIAL X (2 1458/169) (1 -2916/169) (0 1458/169))

;; It's because div-terms divides the coefficients, producing fractions.


; Exercise 2.96

; a)

(defun pseudoremainder-terms (a b)
  (cadr (pseudodiv-terms a b)))

(defun mul-terms-by-const (termlist c)
  (mapcar (lambda (term)
            (make-term (order term)
                       (* c (coeff term))))
          termlist))

(defun pseudodiv-terms (p q)
  (let* ((q1 (first-term q))
         (p1 (first-term p))
         (o1 (order p1))
         (o2 (order q1))
         (c (coeff q1))
         (integerizing-factor (expt c (+ 1 o1 (- o2)))))
    (div-terms (mul-terms-by-const p integerizing-factor) q)))

(defun gcd-terms (a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (pseudoremainder-terms a b))))

;; CL-USER> (greatest-common-divisor q1 q2)

;; (POLYNOMIAL X (2 1458) (1 -2916) (0 1458))


; b)

(defun gcd-coeffs (termlist)
  (let ((coeffs (mapcar #'coeff termlist)))
    (reduce #'gcd (cdr coeffs) :initial-value (car coeffs))))

(defun div-terms-by-const (termlist c)
  (mapcar (lambda (term)
            (make-term (order term)
                       (/ (coeff term) c)))
          termlist))

(defun gcd-terms (a b)
  (if (empty-termlist? b)
      a
      (let ((remainder (pseudoremainder-terms a b)))
        (gcd-terms b (div-terms-by-const remainder (gcd-coeffs remainder))))))


;; CL-USER> (greatest-common-divisor q1 q2)

;; (POLYNOMIAL X (2 1) (1 -2) (0 1))


; Exercise 2.97

(defun quotient-terms (t1 t2)
  (car (div-terms t1 t2)))

(defun reduce-terms (n d)
  (let* ((gcd (gcd-terms n d))
         (c (coeff (first-term gcd)))
         (o1 (max (order (first-term n)) (order (first-term d))))
         (o2 (order (first-term gcd)))
         (integerizing-factor (expt c (+ 1 o1 (- o2))))
         (nn (quotient-terms (mul-terms-by-const n integerizing-factor) gcd))
         (dd (quotient-terms (mul-terms-by-const d integerizing-factor) gcd))
         (gcd-c (gcd (gcd-coeffs nn) (gcd-coeffs dd))))
    (list (div-terms-by-const nn gcd-c)
          (div-terms-by-const dd gcd-c))))

(defun reduce-polys (n d)
  (labels ((variable? (x)
             (symbolp x))
           (same-variable? (x y)
             (and (variable? x) (variable? y) (eq x y))))
  (if (same-variable? (poly-variable n) (poly-variable d))
      (let* ((result (reduce-terms (term-list n)
                                   (term-list d)))
             (tl1 (car result))
             (tl2 (cadr result))
             (var (poly-variable n)))
        (list (make-polynomial var tl1)
              (make-polynomial var tl2)))
      (error "numerator and denominator are not of same variable ~a, ~a" n d))))

;; CL-USER> (reduce-polys (cdr q1) (cdr q2))

;; ((POLYNOMIAL X (2 11) (0 7)) (POLYNOMIAL X (1 13) (0 5)))


; Exercise 2.97

(defun reduce-integers (n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

(defgen reduce-for-rat (n d))

(put-generic 'reduce-for-rat '(polynomial polynomial) #'reduce-polys)
(put-generic 'reduce-for-rat '(lisp-number lisp-number) #'reduce-integers)

(defvar poly1 (make-polynomial 'x '((1 1) (0 1))))
(defvar poly2 (make-polynomial 'x '((3 1) (0 -1))))
(defvar poly3 (make-polynomial 'x '((1 1))))
(defvar poly4 (make-polynomial 'x '((2 1) (0 -1))))

(defvar rf1 (make-rational poly1 poly2))
(defvar rf2 (make-rational poly3 poly4))

;; CL-USER> (add rf1 rf2)
;; (RATIONAL (POLYNOMIAL X (3 1) (2 2) (1 3) (0 1)) POLYNOMIAL X (4 1) (3 1)
;;           (1 -1) (0 -1))


;; hehehe!
