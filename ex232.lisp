; Answers for 2-3-2

(defun deriv (exp var)
  (cond ((numberp exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (t (error "unknown expression type -- DERIV"))))

(defun variable? (x)
  (symbolp x))

(defun same-variable? (x y)
  (and (variable? x) (variable? y) (eq x y)))

(defun make-sum (a1 a2)
  (cond ((=numberp a1 0) a2)
        ((=numberp a2 0) a1)
        ((and (numberp a1) (numberp a2)) (+ a1 a2))
        (t (list '+ a1 a2))))

(defun =numberp (exp num)
  (and (numberp exp) (= exp num)))

(defun make-product (m1 m2)
  (cond ((or (=numberp m1 0) (=numberp m2 0)) 0)
        ((=numberp m1 1) m2)
        ((=numberp m2 1) m1)
        ((and (numberp m1) (numberp m2)) (* m1 m2))
        (t (list '* m1 m2))))

(defun sum? (exp)
  (and (listp exp) (eq (car exp) '+)))

(defun addend (s)
  (cadr s))

(defun augend (s)
  (caddr s))

(defun product? (exp)
  (and (listp exp) (eq (car exp) '*)))

(defun multiplier (p)
  (cadr p))

(defun multiplicand (p)
  (caddr p))

; Exercise 2.56

(defun exponentiation? (exp)
  (and (listp exp) (eq (car exp) '**)))

(defun base (exp)
  (cadr exp))

(defun exponent (exp)
  (caddr exp))

(defun make-exponentiation (base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        ((numberp base) (expt base exponent))
        (t (list '** base exponent))))

(defun deriv-2 (exp var)
  (cond ((numberp exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation (base exp) (1- (exponent exp))))
          (deriv-2 (base exp) var)))
        ((sum? exp)
         (make-sum (deriv-2 (addend exp) var)
                   (deriv-2 (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv-2 (multiplicand exp) var))
          (make-product (deriv-2 (multiplier exp) var)
                        (multiplicand exp))))
        (t (error "unknown expression type -- DERIV"))))

; Exercise 2.57

(defun augend (exp)
  (let ((rest (cddr exp)))
    (if (null (cdr rest))
        (car rest)
        (list* '+ rest))))

(defun multiplicand (exp)
  (let ((rest (cddr exp)))
    (if (null (cdr rest))
        (car rest)
        (list* '* rest))))

;; Could be abstracted into a common pattern?

; Exercise 2.58

; a)

(defun addend (s)
  (car s))

(defun augend (s)
  (caddr s))

(defun multiplier (p)
  (car p))

(defun multiplicand (p)
  (caddr p))

(defun sum? (exp)
  (and (listp exp) (eq (cadr exp) '+)))

(defun product? (exp)
  (and (listp exp) (eq (cadr exp) '*)))

; b)

;; Not given how 'deriv' is defined.

;; i.e.. Is (3 * 4 + 4) a sum or a product? 

