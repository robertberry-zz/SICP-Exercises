; Exercises from 1.2.1

(defparameter *coins* '(50 25 10 5 1))

(defun count-the-ways (amount &optional (coins *coins*))
  (cond ((= amount 0) 1) ; reduced amount to 0 - this is a way to arrange coins
        ((< amount 0) 0) ; deducted too much - not a way to arrange coins
        ((null coins) 0) ; no coins, still amount. not a way  to arrange coins.
        (t (+ (count-the-ways amount (rest coins))
              (count-the-ways (- amount (first coins)) coins)))))

;; work through in detail how the reduction rule
;; applies to the problem of making change for 10 cents using pennies and
;; nickels.

;                                    10 [5 1]
;                           10 [1]                5 [5 1] <-
;                    10 []          5 [1]     
;                      0        5 []       4 [1] ..
;                                0
;                           --------------------------
;                                   5 [5 1] <- 
;                              5 [1]         0 [5 1]
;                                1               1
;

;; `Count-change' generates a tree-recursive process with redundancies
;; similar to those in our first implementation of `fib'.  (It will take
;; quite a while for that 292 to be computed.)  On the other hand, it is
;; not obvious how to design a better algorithm for computing the result,
;; and we leave this problem as a challenge.

; Too challenging for me :P. Will come back to this later.

; Exercise 1.11

; recursive version
(defun f (n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

; iterative version
(defun g (n)
  (labels ((g-iter (left prev pprev ppprev)
             (let ((this-calc (+ prev (* 2 pprev) (* 3 ppprev))))
               (if (= left 0)
                   this-calc
                   (g-iter (- left 1) this-calc prev pprev)))))
    (if (< n 3)
        n
        (g-iter (- n 3) 2 1 0))))

; Exercise 1.12

(defun pascal-row (previous &optional (last-element nil))
  "Given the previous row of a Pascal triangle, computes the next."
  (if (null previous)
      '(1)
      (cons (if (null last-element)
               1
               (+ last-element (car previous)))
           (pascal-row (cdr previous) (car previous)))))

(defun pascal (depth &optional (previous-row nil))
  "Computes the elements of a Pascal triangle up to the given depth."
  (if (= depth 0)
      nil
      (let ((this-row (pascal-row previous-row)))
        (cons this-row
              (pascal (- depth 1) this-row)))))

; Exercise 1.13

;; f(n) = (a ** n) / sqrt(5)
;; a = (1 + sqrt(5)) / 2

;; b = (1 - sqrt(5)) / 2

;; Fib(n) | n == 0 = 0
;;        | n == 1 = 1
;;        | otherwise = fib (n - 1) + fib (n - 2)

;; INDUCTION UGH

;; prove   fib(n) = (a ** n - b ** n) / sqrt(5)

; BASE CASES

; fib(0) = (a ** 0 - b ** 0) / sqrt(5)
;        = (1 - 1) / sqrt(5)
;        = 0

; fib(1) = (a ** 1 - b ** 1) / sqrt(5)
;        = 

; INDUCTIVE CASE

; fib(n + 1) = fib (n) + fib (n - 1)
;            = (a ** n - b ** n) / sqrt(5) + (a ** n - b ** n) / sqrt(5)
