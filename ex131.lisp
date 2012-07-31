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

(defun integral (f a b dx)
  (labels ((add-dx (x)
             (+ x dx)))
    (* (sum f (+ a (/ dx 2.0)) #'add-dx b)
       dx)))

(defun simpsons-rule-integral (f a b n)
  (let ((h (/ (- b a) n)))
    (labels ((y (k)
               (funcall f (+ a (* k h))))
             (iter (k)
               (if (= k n)
                   (y n)
                   (+ (* (cond ((= k 0) 1)
                               ((oddp k) 4)
                               (t 2))
                         (y k))
                      (iter (1+ k))))))
      (if (oddp n)
          (error "n must be an even integer.")
          (* (/ h 3) (iter 0))))))

(defun cube (n)
  (* n n n))

(defun test-integral (dx)
  (format t "~a~%" (integral #'cube 0 1 dx)))

(defun test-simpsons-integral (n)
  (format t "~a~%" (simpsons-rule-integral #'cube 0 1 n)))

;; CL-USER> (test-integral 0.0001)
;; 0.24991691
;; NIL
;; CL-USER> (test-simpsons-integral 100)
;; 1/4      <- seems to be exact here anyway!
;; NIL
;; CL-USER> (test-simpsons-integral 1000)
;; 1/4
;; NIL

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
