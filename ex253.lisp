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
                 (error "Polys not in same var ~a, ~a" p1 p2))))
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

(defun mul-terms-by-all-terms (t1 l)
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

(put-generic '=zero? '(polynomial) (lambda (p)
                                     (zero-termlist? (term-list p))))

; Exercise 2.88

;; See above package definition

(defun sub-terms (l1 l2)
  (add-terms l1 (mapcar (lambda (x)
                          (make-term (order x) (negate (coeff x)))) l2)))
; The above function breaks data hiding (through using mapcar). It could be
; implemented using first-term and rest-terms to avoid this - or we could
; define a generic iteration function, and build a new map function over it.

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

