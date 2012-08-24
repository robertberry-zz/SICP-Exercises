; Answers to 3-5-1

; In Scheme so we have access to delay (although this could've been
; implemented as a macro in CL I guess. Scheme is nicer though ... )

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream first rest)
     (cons first (delay rest)))))

(define the-empty-stream '())

(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-enumerate-interval low high)
       (if (> low high)
           the-empty-stream
           (cons-stream
            low
            (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
       (cond ((stream-null? stream) the-empty-stream)
             ((pred (stream-car stream))
              (cons-stream (stream-car stream)
                           (stream-filter pred
                                          (stream-cdr stream))))
             (else (stream-filter pred (stream-cdr stream)))))



; Exercise 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

; Exercise 3.51

(define (show x)
  (display-line x)
  x)


; (define x (stream-map show (stream-enumerate-interval 0 10)))

; At this point 0 is printed, as the first value in the map is calculated.

; (stream-ref x 5)

; At this point all of the values from 1 to 5 are printed out, and the
; interpreter says the value of the expression is 5. 0 is not re-printed as
; the result of the function has already been calculated.

; (stream-ref x 7)

; At this point 6 and 7 are printed out. The previous thunks have been
; memoized, so show is not re-evaluated for 1-5. The value of the expression
; is 7.


; Exercise 3.52

; (define sum 0)

; (define (accum x)
;           (set! sum (+ x sum))
;           sum)

; (define seq (stream-map accum (stream-enumerate-interval 1 20)))

; The first value is calculated here, which sets sum to 1.

; (define y (stream-filter even? seq))

; The first value that is even is accumulated by evaluating more of the accum
; stream:

;   sum = (1 + 2)   3
;       = (3 + 3)   6

; Sum is now 6.

; (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
;                                   seq))

; The first value that is divisible by 5 is now calculated by re-evaluating
; the accum stream. The original three calculations have been memoized, so it
; continues calculating at 4:

;   sum = (6 + 4)    10

; This is divisible by 5, so it stops.

; (stream-ref y 7)

; Stream-ref now attempts to find the 8th even number. This processes the y
; stream until 8 even numbers have been found. 6 and 10 are accumulated from
; previously calculated values of seq, then seq continues to process:

;   sum = (10 + 5)    15
;       = (15 + 6)    21
;       = (21 + 7)    28      <-
;       = (28 + 8)    36      <-
;       = (36 + 9)    45
;       = (45 + 10)   55
;       = (55 + 11)   66      <-
;       = (66 + 12)   78      <-
;       = (78 + 13)   91
;       = (91 + 14)   105
;       = (105 + 15)  120     <-
;       = (120 + 16)  136     <-

; At this point, 136 is displayed. This is also the value of sum.

; (display-stream z)

;; Iterates through the previously calculated values for seq, displaying all
;; whose factors include 5: 10, 15, 45, 55, 105, 120

;; Then the calculations for seq continue until enumerated up to 20:

;       = (136 + 17)  153
;       = (153 + 18)  171
;       = (171 + 19)  190    <-
;       = (190 + 20)  210    <-

; sum is now 210.

; Yes, they would differ. If we did not memoize, the accum procedure would run
; again for all of the values previously computed when display-stream
; ran. This would cause all of the values of z to be +104.

