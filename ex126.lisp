

; first define this stuff in cl

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

; Exercise 1.21

;CL-USER> (smallest-divisor 199)
;199
;CL-USER> (smallest-divisor 1999)
;1999
;CL-USER> (smallest-divisor 19999)
;7

; Exercise 1.22

; Earlier prime? definition

(defun prime? (x)
  (= (smallest-divisor x) x))

; Scheme primatives not in CL
(defun runtime ()
  (get-internal-real-time))

(defun timed-prime-test (n)
  (format t "~%")
  (format t "~:d" n)
  (start-prime-test n (runtime)))

(defun start-prime-test (n start-time)
  (if (prime? n)
      (let nil
        (report-prime start-time (runtime))
        t)
      nil))

(defun report-prime (start-time end-time)
  (format t " *** ")
  (format t "~:d" (- end-time start-time)))

; My code

(defun search-for-primes (x y)
  (cond ((> x y) nil)
        ((evenp x) (search-for-primes (1+ x) y))
        (t (let ((remaining (search-for-primes (+ x 2) y)))
             (if (prime? x)
                 (cons x remaining)
                 remaining)))))

; misunderstood - should be more like this:

(defun report-primes (x y)
  (cond ((> x y) nil)
        ((evenp x) (report-primes (1+ x) y))
        (t (prog nil
              (timed-prime-test x)
              (report-primes (+ x 2) y)))))

(defun report-n-smallest-above (x n)
  (cond ((= n 0) nil)
        ((evenp x) (report-n-smallest-above (1+ x) n))
        ((timed-prime-test x) (cons x (report-n-smallest-above (+ x 2) (1- n))))
        (t (report-n-smallest-above (+ x 2) n))))

; Unfortunately my computer is too fast for any noticable difference in run
; time between computations.

;; CL-USER> (report-n-smallest-above 1000 3)

;; 1,001
;; 1,003
;; 1,005
;; 1,007
;; 1,009 *** 0
;; 1,011
;; 1,013 *** 0
;; 1,015
;; 1,017
;; 1,019 *** 0
;; (1009 1013 1019)
;; CL-USER> (report-n-smallest-above 10000 3)

;; 10,001
;; 10,003
;; 10,005
;; 10,007 *** 0
;; 10,009 *** 0
;; 10,011
;; 10,013
;; 10,015
;; 10,017
;; 10,019
;; 10,021
;; 10,023
;; 10,025
;; 10,027
;; 10,029
;; 10,031
;; 10,033
;; 10,035
;; 10,037 *** 0
;; (10007 10009 10037)
;; CL-USER> (report-n-smallest-above 100000 3)

;; 100,001
;; 100,003 *** 0
;; 100,005
;; 100,007
;; 100,009
;; 100,011
;; 100,013
;; 100,015
;; 100,017
;; 100,019 *** 0
;; 100,021
;; 100,023
;; 100,025
;; 100,027
;; 100,029
;; 100,031
;; 100,033
;; 100,035
;; 100,037
;; 100,039
;; 100,041
;; 100,043 *** 0
;; (100003 100019 100043)

; Ex 1.23

(defun next (n)
  (if (= n 2)
      3
      (+ 2 n)))

(defun find-divisor-fast (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (t (find-divisor n (next test-divisor)))))

;; can't do the testing for speed differences as it is already 0 D:

; Ex 1.24

;; same issue

; Ex 1.25

;; She is correct, it would work. It's not as fast, though.

;; Do some kind of proof here for that statement ...

; Ex 1.26

;; Because (expmod base (/ exp 2) m) is being calculated twice every time the
;; exponent is even.

;; Do a proof here that this means the process executes in theta(n) time.

;; Ex 1.27

(defun expmod (base exp m)
  (cond ((= exp 0) 1)
        ((evenp exp) (mod (square (expmod base (/ exp 2) m)) m))
        (t (mod (* base (expmod base (- exp 1) m)) m))))

(defun carmichael-test (n)
  (labels ((test (n a)
             (cond ((= n a) t)
                   ((= (expmod a n n) (mod a n)) (test n (1+ a)))
                   (t nil))))
    (if (< n 1)
        (error "Carmichael test only works for positive integers.")
        (test n 1))))

(defparameter *carmichael-numbers*
  (list 561 1105 1729 2465 2821 6601))

; CL-USER> (every (lambda (x) (and (carmichael-test x) (not (prime? x)))) *carmichael-numbers*)
; T

;; Ex 1.28

(defun miller-rabin-test (n a)
  (= (expmod a (- n 1) n) (mod 1 n)))

; need to do this still ... 
