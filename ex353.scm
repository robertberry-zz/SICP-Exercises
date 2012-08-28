; Answers for 3-5-3

(load "ex352.scm")

(define (average x y)
  (/ (+ x y) 2.0))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; S_(n-1)
        (s1 (stream-ref s 1))           ; S_n
        (s2 (stream-ref s 2)))          ; S_(n+1)
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))


; Exercise 3.63

; Instead of using the same stream for which the precomputed values are
; memoized, the stream is mapping onto another call of 'sqrt-stream', which
; returns a new stream that has to calculate the values anew. This in turn
; will result in another recursive call, etc.

; If delay did not memoize the calculated values it would be similarly
; inefficient.


; Exercise 3.64

(define (abs-diff x y)
  (abs (- x y)))

(define (stream-limit S tolerance)
  (define (iter last remaining)
    (let ((next (stream-car remaining)))
      (if (< (abs-diff last next) tolerance)
          next
          (iter next (stream-cdr remaining)))))
  (iter (stream-car S) (stream-cdr S)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

; Exercise 3.65

(define (natural-logarithm-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (natural-logarithm-summands (+ n 1)))))

(define natural-logarithm-stream
  (partial-sums (natural-logarithm-summands 1)))

; Didn't end up using this one, ignore.
(define (take-while f S)
  (let ((first (stream-car S))
        (rest (stream-cdr S)))
    (if (f first)
        (cons-stream first (take-while rest f))
        the-empty-stream)))

(define (until-converges S tolerance)
  (define (iter S previous)
    (let ((x (stream-car S)))
      (cons-stream x
                   (if (< (abs-diff x previous) tolerance)
                       the-empty-stream
                       (iter (stream-cdr S) x)))))
  (iter (stream-cdr S) (stream-car S)))

(define (length-stream S)
  (define (iter S acc)
    (if (empty-stream? S)
        acc
        (iter (stream-cdr S) (+ acc 1))))
  (iter S 0))

; To three decimal places:

;; 1 ]=> (length-stream (until-converges natural-logarithm-stream 0.001))

;; ;Value: 1000

;; 1 ]=> (length-stream (until-converges (euler-transform natural-logarithm-stream) 0.001))

;; ;Value: 5

;; 1 ]=> (length-stream (until-converges (accelerated-sequence euler-transform natural-logarithm-stream) 0.001))

;; ;Value: 3


(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))


(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


; Exercise 3.66

(define (zip-streams S T)
  (if (or (empty-stream? S) (empty-stream? T))
      the-empty-stream
      (cons-stream (list (stream-car S)
                         (stream-car T))
                   (zip-streams (stream-cdr S)
                                (stream-cdr T)))))

(define (enumerate S)
  (zip-streams integers S))

; OK, so how frequently do pairs beginning with i occur?

; Generally it's 2**i. Interleaves takes one element from list one, then one
; element from list two, etc. The pairs function interleaves the list of all
; pairs beginning with i with the results of calling the pairs function on the
; rest of the lists, with i + 1. This causes a recursive split.

; Some slight shifting occurs, however, because the pairs function itself
; always conses one element on the front of the list (which begins with i)
; before the interleaving. This causes the first to elements to always be from
; the ith list, which means that the second element of the list actually only
; occurs at the distance normally given to the i-1th terms.



; 1: 1, 2, 4, 6, 8, 10 ...
; 2: 3, 5, 9, 13, 15 ...
; 3: 7, 11, 19 ...
; 4: 15 ... 

; The first instance of i is at the 2**i - 1 position.
; The second instance of i is at the (2**i - 1) + (2**[i - 1])
; After that, they are (2**i) apart.

; Which instance of i is given by j-i + 1

; So for the position of a pair (i, j):

(define (position i j)
  (let ((instance-of-i (- j i))
        (i0 (- (expt 2 i) 1))
        (i1-distance (expt 2 (- i 1)))
        (ix-distance (expt 2 i)))
    (cond ((= instance-of-i 0) i0)
          ((= instance-of-i 1) (+ i0 i1-distance))
          (else (+ i0
                   i1-distance
                   (* ix-distance (- instance-of-i 1)))))))

; Sorry for my bad explanation! I need to learn more maths. :<

; Exercise 3.67

(define (pairs-2 s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
     (stream-map (lambda (x) (list x (stream-car t)))
                 (stream-cdr s)))
    (pairs-2 (stream-cdr s) (stream-cdr t)))))

; Exercise 3.68

; It causes an infinite loop, as it immediately recurses into another call of
; pairs, rather than defining a stream with delayed execution.


; Exercise 3.69

(define (triples S T U)
  (let ((s1 (stream-car S))
        (t1 (stream-car T))
        (u1 (stream-car U)))
  (cons-stream
   (list s1 t1 u1)
   (interleave
    (stream-map (lambda (x) (cons s1 x)) (pairs (stream-cdr T) (stream-cdr U)))
    (triples (stream-cdr S) (stream-cdr T) (stream-cdr U))))))

(define pythagorean-triples
  (stream-filter (lambda (triple)
                   (let ((i (car triple))
                         (j (cadr triple))
                         (k (caddr triple)))
                     (= (+ (square i)
                           (square j))
                        (square k)))) (triples integers integers integers)))

; Exercise 3.70

(define (merge-weighted S T weight)
  (cond ((stream-null? S) T)
        ((stream-null? T) S)
        (else
         (let* ((s1 (stream-car S))
                (t1 (stream-car T))
                (weight-s1 (weight s1))
                (weight-t1 (weight t1)))
           (cond ((< weight-s1 weight-t1)
                  (cons-stream s1 (merge-weighted (stream-cdr S) T weight)))
                 ((> weight-s1 weight-t1)
                  (cons-stream t1 (merge-weighted S (stream-cdr T) weight)))
                 (t (cons-stream s1 (cons-stream t1 (merge-weighted (stream-cdr S)
                                                                    (stream-cdr T) weight)))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted 
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

; a)

(define weighted-sum-pairs (weighted-pairs integers integers (lambda (pair)
                                                               (+ (car pair) (cadr pair)))))

; b)

(define (divisible? x n)
  (= (remainder x n) 0))

(define (valid-for-b? pair)
  (define (ok-term? x)
    (not (or (divisible? x 2)
             (divisible? x 3)
             (divisible? x 5))))
  (and (ok-term? (car pair))
       (ok-term? (cadr pair))))

(define (weight-for-b pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* 2 i)
       (* 3 j)
       (* 5 i j))))

(define b-stream (stream-filter valid-for-b?
                                (weighted-pairs integers integers
                                                weight-for-b)))


; Exercise 3.71

(define (ramanujan-weight pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (expt i 3)
       (expt j 3))))

(define (drop-while f s)
  (if (f (stream-car s))
      (drop-while f (stream-cdr s))
      s))

(define (duplicates-by-key s key)
  (define (iter s last)
    (if (= (key (stream-car s)) (key last))
        (cons-stream (list last (stream-car s)) (duplicates-by-key
                                                 (drop-while (lambda (x)
                                                               (= (key x) (key last))) s) key))
        (iter (stream-cdr s) (stream-car s))))
  (iter (stream-cdr s) (stream-car s)))

(define ramanujan-numbers
  (duplicates-by-key (weighted-pairs integers integers ramanujan-weight)
                     ramanujan-weight))

;; 1 ]=> (show 10
;;       (zip-streams ramanujan-numbers
;;                    (stream-map (lambda (pairs)
;;                                  (ramanujan-weight (car pairs))) ramanujan-numbers)))

;; (((1 12) (9 10)) 1729)
;; (((2 16) (9 15)) 4104)
;; (((2 24) (18 20)) 13832)
;; (((10 27) (19 24)) 20683)
;; (((4 32) (18 30)) 32832)
;; (((2 34) (15 33)) 39312)
;; (((9 34) (16 33)) 40033)
;; (((3 36) (27 30)) 46683)
;; (((17 39) (26 36)) 64232)
;; (((12 40) (31 33)) 65728)


; Exercise 3.72

(define (stream-cadr s)
  (stream-car (stream-cdr s)))

(define (stream-caddr s)
  (stream-car (stream-cdr (stream-cdr s))))

(define (stream-cdddr s)
  (stream-cdr (stream-cdr (stream-cdr s))))

(define (triplicates-by-key s key)
  (define (iter s)
    (let ((s1 (stream-car s))
          (s2 (stream-cadr s))
          (s3 (stream-caddr s)))
      (if (= (key s1) (key s2) (key s3))
          (cons-stream (list s1 s2 s3) (iter (drop-while (lambda (x)
                                                           (= (key x)
                                                              (key s1))) (stream-cdddr s))))
          (iter (stream-cdr s)))))
  (iter s))

(define (sum-of-squares pair)
  (+ (expt (car pair) 2)
     (expt (cadr pair) 2)))

(define sum-two-squares-3
  (triplicates-by-key (weighted-pairs integers integers sum-of-squares)
                      sum-of-squares))


;; 1 ]=> (show 10 sum-two-squares-3)

;; ((1 18) (6 17) (10 15))
;; ((5 20) (8 19) (13 16))
;; ((5 25) (11 23) (17 19))
;; ((7 26) (10 25) (14 23))
;; ((2 29) (13 26) (19 22))
;; ((3 29) (11 27) (15 25))
;; ((5 30) (14 27) (21 22))
;; ((1 32) (8 31) (20 25))
;; ((4 33) (9 32) (12 31))
;; ((5 35) (17 31) (25 25))


; ... todo