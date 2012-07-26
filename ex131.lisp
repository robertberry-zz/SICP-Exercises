; Exercises for 1.3.1

; this stuff for later exercise
(defun find-divisor (n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (t (find-divisor n (1+ test-divisor)))))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun divides? (a b)
  (= (mod b a) 0))

(defun square (a)
  (* a a))

(defun prime? (x)
  (= (smallest-divisor x) x))




; Identity function
(defun K (x)
  x)

; Exercise 1.29




;;;; need to do this one!!!



; Exercise 1.30

(defun sum (term a next b)
  (labels ((iter (a result)
             (if (> a b)
                 result
                 (iter (funcall next a) (+ result (funcall term a))))))
    (iter a 0)))

; Exercise 1.31

; a)

(defun product (term a next b)
  (if (> a b)
      1
      (* (funcall term a) (product term (funcall next a) next b))))

(defun factorial (n)
  (product #'K 1 #'1+ n))

; b)

(defun product-iter (term a next b)
  (labels ((iter (a result)
             (if (> a b)
                 result
                 (iter (funcall next a) (* result (funcall term a))))))
    (iter a 1)))

; Exercise 1.32

; a)

(defun accumulate (combiner null-value term a next b)
  (if (> a b)
      null-value
      (funcall combiner
               (funcall term a)
               (accumulate combiner null-value term (funcall next a) next b))))

(defun acc-product (term a next b)
  (accumulate #'* 1 term a next b))

(defun acc-sum (term a next b)
  (accumulate #'+ 0 term a next b))

; b)

(defun accumulate-iter (combiner null-value term a next b)
  (labels ((iter (a result)
             (if (> a b)
                 result
                 (iter (funcall next a)
                       (funcall combiner result (funcall term a))))))
    (iter a null-value)))

; Exercise 1.33

(defun filtered-accumulate (combiner null-value filter term a next b)
  (labels ((iter (a result)
             (if (> a b)
                 result
                 (iter (funcall next a)
                       (if (funcall filter a)
                           (funcall combiner result (funcall term a))
                           result)))))
    (iter a null-value)))

; a)

(defun sum-squares-primes (a b)
  (filtered-accumulate #'+
                       0
                       #'prime?
                       #'square
                       (if (evenp a) (1+ a) a)
                       (lambda (x)
                           (+ x 2))
                       b))

; b)

(defun sum-relative-primes (n)
  (filtered-accumulate #'+
                       0
                       (lambda (x)
                         (= (gcd x n) 1))
                       #'K
                       1
                       #'1+
                       n))
