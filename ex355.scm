; Answers for 3-5-5

(load "ex354.scm")

(define (rand-update x)
  (let ((a (expt 2 32))
        (c 1103515245)
        (m 123456))
    (modulo (+ (* a x) c) m)))

(define random-init 137)

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))


; Exercise 3.81

(define (random-numbers-with-init init)
  (define stream
    (cons-stream init
                 (stream-map rand-update stream)))
  stream)

(define (random-generator-stream requests)
  (define (reset? x)
    (eq? (car x) 'reset))
  (define (generate? x)
    (eq? (car x) 'generate))
  (define (reset-value x)
    (cadr x))
  (define (iter requests generator)
    (let ((request (stream-car requests)))
      (cond ((reset? request)
             (iter (stream-cdr requests)
                   (random-numbers-with-init (reset-value request))))
            ((generate? request)
             (cons-stream (stream-car generator)
                          (iter (stream-cdr requests) (stream-cdr generator))))
            (else (error "Request stream contained a value that was not to generate or reset.")))))
  (iter requests (random-numbers-with-init random-init)))

(define (make-reset-request n)
  (list 'reset n))

(define (make-generate-request)
  '(generate))

; Example request stream for testing
(define request-stream
  (stream-map (lambda (x) (if (divisible? x 10)
                              (make-reset-request x)
                              (make-generate-request)))
              integers))


; Exercise 3.82

(define (make-circle-predicate centre-x centre-y radius)
  (lambda (x y)
    (<= (+ (square (- x centre-x)) (square (- y centre-y)))
        (square radius))))


; I know this is stateful but my above rand-update is not random enough to
; work.

(define (scheme-randoms max)
  (define stream
    (cons-stream (random max) (scheme-randoms max)))
  stream)

(define (random-numbers-in-range x y)
  (let ((d (- y x)))
    (stream-map (lambda (n)
                  (+ x n)) (scheme-randoms d))))

(define (estimate-integral P x1 x2 y1 y2)
  (define random-xs
    (random-numbers-in-range x1 x2))
  (define random-ys
    (random-numbers-in-range y1 y2))
  (define random-points
    (stream-map list random-xs random-ys))
  (define experiments
    (stream-map (lambda (pair)
                  (P (car pair) (cadr pair))) random-points))
  (define area
    (* (abs (- x2 x1)) (abs (- y2 y1))))

  (stream-map (lambda (pass-rate)
                (* pass-rate area)) (monte-carlo experiments 0 0)))

(define pi-estimates
  (estimate-integral (make-circle-predicate 0 0 1.0)
                     -1.0 1.0 -1.0 1.0))

