; Answers for 2-5-3

; Exercise 2.90

(load "ex252.lisp")

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

(defun make-polynomial (var terms)
  (funcall (get-generic 'make '(polynomial)) var terms))

(install-polynomial-package)

; Exercise 2.87


(put-generic '=zero? '(polynomial) (lambda (p)
                                     (zero-termlist? (term-list p))))

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

(defun drop-while (f seq)
  (cond ((null seq) '())
        ((funcall f (car seq)) (drop-while f (cdr seq)))
        (t seq)))

(defun zeros (n)
  "Create a list of zeros n elements long."
  (if (= n 0)
      '()
      (cons 0 (zeros (1- n)))))

(defun set-nth (seq n val)
  (cond ((null seq) '())
        ((= n 0) (cons val (cdr seq)))
        (t (cons (car seq) (set-nth (cdr seq) (1- n) val)))))
;
; Exercise 2.90

(defgen order (x))
(defgen coeff (x))

(defun install-term-package ()
  (labels ((make-term (order coeff)
             (list order coeff))
           (order (term)
             (car term))
           (coeff (term)
             (cadr term)))
    (install-operators 'term
                       `((make ,#'make-term))
                       '())
    (put-generic 'order '(term) #'order)
    (put-generic 'coeff '(term) #'coeff)))

(defun make-term (order coeff)
  (funcall (get-generic 'make '(term)) order coeff))

(defgen adjoin-term (x y))
(defgen empty-termlist? (x))
(defgen rest-terms (x))
(defgen first-term (x))
(defgen zero-termlist? (x))

(defun install-sparse-termlist-package ()
  (labels ((tag (term-list)
             (cons 'sparse term-list))
           (adjoin-term (term term-list)
             (let ((term (cons 'term term))) ; I know this is hideous
               (if (=zero? (coeff term))
                   term-list
                   (cons term term-list))))
           (the-empty-termlist ()
             '())
           (empty-termlist? (term-list)
             (null term-list))
           (first-term (term-list)
             (car term-list))
           (rest-terms (term-list)
             (cdr term-list))
           (zero-termlist? (term-list)
             (every #'=zero? (mapcar #'coeff term-list))))

    (install-operators 'sparse
                       `((the-empty-termlist ,#'the-empty-termlist)
                         (rest-terms ,#'rest-terms))
                       '())

    (put-generic 'empty-termlist? '(sparse) #'empty-termlist?)
    (put-generic 'zero-termlist? '(sparse) #'zero-termlist?)
    (put-generic 'first-term '(sparse) #'first-term)
    (put-generic 'adjoin-term '(term sparse) (compose #'tag #'adjoin-term))))

(defun install-dense-termlist-package ()
  (labels ((tag (term-list)
             (cons 'dense term-list))
           (the-empty-termlist ()
             '())
           (empty-termlist? (term-list)
             (null term-list))
           (order-front (term-list)
             (1- (length term-list)))
           (adjoin-term (term term-list)
             (let* ((term (cons 'term term)) ; I know this is hideous
                    (order-first (order-front term-list))
                    (order-term (order term)))
               (if (> order-term order-first)
                   (append (list order-term) (zeros (- order-term
                                                       order-first 1)) term-list)
                   (set-nth term-list (- order-first order-term) (coeff term)))))
           (rest-terms (term-list)
             (labels ((iter (terms)
                        (cond ((null terms) (the-empty-dense-termlist))
                              ((not (=zero? (car terms)))
                               (drop-while #'=zero? (cdr terms)))
                              (t (iter (cdr terms))))))
               (iter term-list)))
           (first-term (term-list)
             (if (=zero? (car term-list))
                 (first-term (cdr term-list))
                 (make-term (order-front term-list)
                            (car term-list))))
           (zero-termlist? (term-list)
             (every #'=zero? (mapcar #'coeff term-list))))

    (install-operators 'dense
                       `((the-empty-termlist ,#'the-empty-termlist)
                         (rest-terms ,#'rest-terms))
                       '())

    (set-parent 'sparse 'dense)

    (put-generic 'raise '(dense)
                 (lambda (t1)
                   (labels ((iter (terms acc)
                              (adjoin-term (first-term terms) acc)))
                     (iter t1 (the-empty-sparse-termlist)))))

    (put-generic 'empty-termlist? '(dense) #'empty-termlist?)
    (put-generic 'zero-termlist? '(dense) #'zero-termlist?)
    (put-generic 'first-term '(dense) #'first-term)
    (put-generic 'adjoin-term '(term dense) (compose #'tag #'adjoin-term))))
             
(defun install-termlist-package ()
  (labels ((tag (term-list)
             (cons 'termlist term-list))
           (adjoin-term-a (term term-list)
             (adjoin-term (cons 'term term) term-list))
           (add-terms (l1 l2)
             (cond ((empty-termlist? l1) l2)
                   ((empty-termlist? l2) l1)
                   (t
                    (let ((t1 (first-term l1))
                          (t2 (first-term l2)))
                      (cond ((> (order t1) (order t2))
                             (adjoin-term-a
                              t1 (add-terms (rest-terms l1) (tag l2))))
                            ((< (order t1) (order t2))
                             (adjoin-term-a
                              t2 (add-terms (tag l1) (rest-terms l2))))
                            (t
                             (adjoin-term-a
                              (make-term (order t1)
                                         (add (coeff t1) (coeff t2)))
                              (add-terms (rest-terms l1)
                                   (rest-terms l2)))))))))
           (sub-terms (l1 l2)
             (add-terms (tag l1) (mapcar (lambda (x)
                                           (make-term (order x) (negate (coeff x)))) l2)))
           (mul-terms (l1 l2)
             (if (empty-termlist? l1)
                 (the-empty-sparse-termlist)
                 (add-terms (mul-term-by-all-terms (first-term l1) (tag l2))
                            (mul-terms (rest-terms l1) (tag l2))))))
 
    (put-generic 'add '(termlist termlist) (compose #'tag #'add-terms))
    (put-generic 'sub '(termlist termlist) #'sub-terms)
    (put-generic 'mul '(termlist termlist) #'mul-terms)
    (put-generic 'adjoin-term '(term termlist) (compose #'tag #'adjoin-term))
    (put-generic 'empty-termlist? '(termlist) #'empty-termlist?)
    (put-generic 'zero-termlist? '(termlist) #'zero-termlist?)
    (put-generic 'first-term '(termlist) #'first-term)))

(defun mul-term-by-all-terms (t1 l)
  (if (empty-termlist? l)
      (the-empty-sparse-termlist)
      (let ((t2 (first-term l)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms l))))))

(defun the-empty-sparse-termlist ()
  (funcall (get-generic 'the-empty-termlist '(sparse))))

(defun the-empty-dense-termlist ()
  (funcall (get-generic 'the-empty-termlist '(dense))))

(install-term-package)
(install-sparse-termlist-package)
(install-dense-termlist-package)
(install-termlist-package)


;; FUCK THIS
