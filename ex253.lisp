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

;; FFS D:

