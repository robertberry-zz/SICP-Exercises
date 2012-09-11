; Answers for 4-3-1

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

; Exercise 4.35

(define (an-integer-between n m)
  (if (> n m)
      (amb)
      (amb n (an-integer-between (+ 1 n) m))))

; Exercise 4.36

;; To to the evaluator performing depth-first searches. i.e., i and j will be
;; set to the value of low, then the evaluator will repeatedly attempt the
;; requirement predicate for increasing values of k, never trying any other
;; search branches.


;; We know for a fact, however, that i and j must be lower than k. (because if
;; either were equal to k, any addition would make that value higher than
;; k). We can make use of this by infinitely generating ascending values of k
;; and only looking at possibilities of i and j that could produce k.

(define (a-pythagorean-triple-above low)
  (let ((k (an-integer-starting-from low)))
    (let ((i (an-integer-between low k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))


; Exercise 4.37

;; Yes, it's more efficient, because it only explores all possible values of i
;; and j (where i and j are all numbers between low and high for which j >=
;; i), whereas the initial algorithm explores all possible values of i, j, and
;; k (where i, j, and k are all numbers between low and high for which k >= j
;; >= i).

;; Basically, if n is high-low, the initial algorithm has n^3 possible answers
;; to consider, whereas the new algorithm has n^2 possible answers to
;; consider.
