; Answers for 3-5-4

(load "ex352.scm")

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)



; Exercise 3.77

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

; Exercise 3.78

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

; Exercise 3.79

;; todo: I need to look up second-order differential equations. ...

; Exercise 3.80

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define diL (add-streams (scale-stream iL (- (/ r L)))
                             (scale-stream vC (/ 1 L))))
    (define dvC (scale-stream iL (- (/ 1 C))))
    (cons vC iL)))

(define exampleRLC ((RLC 1 1 0.2 0.1) 10 0))

;; 1 ]=> (show 10 (car exampleRLC))

;; 10
;; 10
;; 9.5
;; 8.55
;; 7.220000000000001
;; 5.5955
;; 3.77245
;; 1.8519299999999999
;; -.0651605000000004
;; -1.8831384500000004

;; 1 ]=> (show 10 (cdr exampleRLC))

;; 0
;; 1.
;; 1.9
;; 2.66
;; 3.249
;; 3.6461
;; 3.84104
;; 3.834181
;; 3.6359559
;; 3.2658442599999997

