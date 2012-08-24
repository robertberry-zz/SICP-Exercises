; Answers for 3-5-2

(load "ex351.scm")

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))


(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
       (cons-stream 0
                    (cons-stream 1
                                 (add-streams (stream-cdr fibs)
                                              fibs))))

(define (scale-stream stream factor)
       (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
       (cons-stream
        2
        (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
       (define (iter ps)
         (cond ((> (square (stream-car ps)) n) true)
               ((divisible? n (stream-car ps)) false)
               (else (iter (stream-cdr ps)))))
       (iter primes))

; Exercise 3.53

; s       1   2   4
; s       1   2   4
; s   1   2   4   8 ...

; powers of 2.


; Exercise 3.54

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams factorials integers)))


; Exercise 3.55

(define (partial-sums s)
  (define (iter s acc)
    (let ((sum (+ (stream-car s) acc)))
      (cons-stream sum
                   (iter (stream-cdr s) sum))))
  (iter s 0))

;; This does work but I'm not sure it's elegant or how I'm supposed to do it.

;; Let me try another table ...


;  integers        1   2   3   4   5
;  partial-sums        1   3   6   10
;  partial-sums    1   3   6   10  15

; OK, so it looks like the partial sums is the integer plus the previous
; partial sum. (makes sense!)

(define (partial-sums s)
  (define stream (cons-stream (stream-car s)
                              (add-streams (stream-cdr s) stream)))
  stream)

(define integer-sums (partial-sums integers))

; That's better!


; Exercise 3.56

(define (merge s1 s2)
            (cond ((stream-null? s1) s2)
                  ((stream-null? s2) s1)
                  (else
                   (let ((s1car (stream-car s1))
                         (s2car (stream-car s2)))
                     (cond ((< s1car s2car)
                            (cons-stream s1car (merge (stream-cdr s1) s2)))
                           ((> s1car s2car)
                            (cons-stream s2car (merge s1 (stream-cdr s2))))
                           (else
                            (cons-stream s1car
                                         (merge (stream-cdr s1)
                                                (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))


; Exercise 3.57

; n - 2 additions, as the results are all memoized.

; Without the memoization ... 

(define fibs
       (cons-stream 0
                    (cons-stream 1
                                 (add-streams (stream-cdr fibs)
                                              fibs))))



;   fibs                 0    1   (+ 0 1)    (+ 1 (+ 0 1))  (+ (+ 0 1) (+ 1 (+ 0 1)))
;   fibs                             0            1                (+ 0 1)
;  (stream-cdr fibs)                 1         (+ 0 1)           (+ 1 (+ 0 1))

; And so on ...

; The problem is recalculating previous additions as it re-evaluates the
; streams recursively.


; Exercise 3.58

(define (take n s)
  (if (or (= n 0) (empty-stream? s))
      the-empty-stream
      (cons-stream (stream-car s)
                   (take (- n 1) (stream-cdr s)))))

(define (show n s)
  "Helper function for viewing n elements of a stream."
  (display-stream (take n s)))

(define (expand num den radix)
            (cons-stream
             (quotient (* num radix) den)
             (expand (remainder (* num radix) den) den radix)))

; Gives the division of the first two terms after the decimal point in the
; given radix.

; (expand 1 7 10)

; Returns the division

; produces   (quotient (* 1 10) 7) = 1
;            (quotient (* 3 10) 7) = 4
;            (quotient (* 2 10) 7) = 2
;            (quotient (* 6 10) 7) = 8
;            (quotient (* 4 10) 7) = 5
;            (quotient (* 5 10) 7) = 7
;            (quotient (* 1 10) 7) = 1 ...

; i.e.  1/7 = 0.1428571

; (expand 3 8 10)

; produces   (quotient (* 3 10) 8) = 3
;            (quotient (* 6 10) 8) = 7
;            (quotient (* 4 10) 8) = 5

; i.e.  3/8 = 3.75

; Kind of like long division!


; Exercise 3.59

; a)

(define (div-series s1 s2)
  (stream-map / s1 s2))

(define (integrate-series s)
  (div-series s integers))

; b)

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define (constant-stream constant)
  (define stream (cons-stream constant stream))
  stream)

(define (negate-stream s)
  (mul-streams (constant-stream -1)
               s))

(define cosine-series
  (cons-stream 1 (integrate-series (negate-stream sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))


; Exercise 3.60

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))              ; Multiplies first terms
               (add-streams (mul-series s1 (stream-cdr s2))     ; Multiplies s1 by rest of s2
                            (mul-series (stream-cdr s1) s2))))  ; Multiplies s2 by rest of s1

(define (powers x)
  (define stream
    (cons-stream 1
                 (mul-streams (constant-stream x)
                              stream)))
  stream)

;;;  todo: check works for the cosine + sine test

(define (cosine-test x)
  (let ((xs (powers x)))
    (show 10 (add-streams (mul-streams xs (mul-series sine-series sine-series))
                          (mul-streams xs (mul-series cosine-series cosine-series))))))

; Doesn't seem to...

; todo ...


