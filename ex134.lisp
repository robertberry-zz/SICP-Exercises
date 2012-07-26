; Answers to exercises in 1.3.4

;; Initial gunk used elsewhere

(defparameter dx 0.00001)

(defparameter *tolerance* 0.00001)

(defun fixed-point (f first-guess)
  (labels ((close-enough? (v1 v2)
             (< (abs (- v1 v2)) *tolerance*))
           (try (guess)
             (let ((next (funcall f guess)))
               (if (close-enough? guess next)
                   next
                   (try next)))))
    (try first-guess)))

(defun deriv (g)
  (lambda (x)
    (/ (- (funcall g (+ x dx)) (funcall g x))
       dx)))

(defun newton-transform (g)
  (lambda (x)
    (- x (/ (funcall g x) (funcall (deriv g) x)))))

(defun newtons-method (g guess)
  (fixed-point (newton-transform g) guess))

(defun square (x)
  (* x x))

(defun newton-sqrt (x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

; Exercise 1.40

(defun cubic (a b c)
  (lambda (x)
    (+ (expt x 3) (* a (expt x 2)) (* b x) c)))

; Exercise 1.41

; a)

(defun my-double (f)
  (lambda (x)
    (funcall f (funcall f x))))

; b)

;; I imagine it should be octupling inc, which means it should be 13. (as it
;; doubles doubling doubling)

(defun inc (x)
  (1+ x))

;; This is the somewhat unwieldy Common Lisp version
(funcall (funcall (my-double (my-double #'my-double)) #'inc) 5)

;; It actually yields 21. (so it's incremented 16 times) Why?

; Initial double doubles.
; Next application calls double on the initial double, which creates a function that quadruples.
; Next application calls the quadrupling function on itself, which creates a function that ...
; errr ... applies the function given to it 16 times.

;; Ouch brain :(

; Exercise 1.42

(defun compose (f g)
  (lambda (x)
    (funcall f (funcall g x))))

;  CL-USER> (funcall (compose #'square #'inc) 6)
; 49

; Exercise 1.43

(defun repeated (f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;  CL-USER> (funcall (repeated #'square 2) 5)
; 625

; Exercise 1.44

; a)

(defun average (&rest args)
  (/ (apply #'+ args) (length args)))

(defun smooth (f dx)
  (lambda (x)
    (average (funcall f (- x dx))
             (funcall f)
             (funcall f (+ x dx)))))

; b)

(defun n-fold-smoothed (f dx n)
  (lambda (x)
    (repeated (lambda (g)
                (smooth g dx)) n)))

; Exercise 1.45

(defun average-damp (f)
  (lambda (y)
    (average y (funcall f y))))
  
(defun 4th-root (x)
  (fixed-point (funcall (repeated #'average-damp 2)
                        (lambda (y)
                          (/ x (expt y 3))))
               1.0)))

; For experimenting with ... 
(defun create-nth-root-procedure (n number-of-dampings)
  (lambda (x)
    (fixed-point (funcall (repeated #'average-damp number-of-dampings)
                          (lambda (y)
                            (/ x (expt y (- n 1)))))
                 1.0)))


;; CL-USER> (funcall (create-nth-root-procedure 8 2) 32)
;; ; Evaluation aborted on NIL.
;; CL-USER> (funcall (create-nth-root-procedure 8 3) 32)
;; 1.5422108

;; CL-USER> (funcall (create-nth-root-procedure 16 3) 32)
;; ; Evaluation aborted on NIL.
;; CL-USER> (funcall (create-nth-root-procedure 16 4) 32)
;; 1.2418692

;; CL-USER> (funcall (create-nth-root-procedure 32 4) 32)
;; ; Evaluation aborted on NIL.
;; CL-USER> (funcall (create-nth-root-procedure 32 5) 32)
;; 1.114387

;; Sooo ... it looks like we need the log (base 2) of the nth root number of dampings!
  
(defun nth-root (x n)
  (funcall (create-nth-root-procedure n (floor (log n 2)))
           x))

;; Aw yeah ... I am kewl <3

; Exercise 1.46

; a)

(defun iterative-improve (good-enough? improve-guess)
  (labels ((iter (guess)
             (if (funcall good-enough? guess)
                 guess
                 (iter (funcall improve-guess guess)))))
    #'iter))

; b)

(defun my-sqrt (x)
  (funcall (iterative-improve (lambda (guess)
                                (< (abs (- (square guess) x)) 0.001))
                              (lambda (guess)
                                (average guess (/ x guess))))
           1.0))

; c)

;; This is less efficient than the original implementation, though, as it will
;; call 'f' twice, both in the good-enough? procedure and as improve-guess.

(defun fixed-point (f first-guess)
  (let ((tolerance 0.0001))
    (funcall (iterative-improve (lambda (guess)
                                  (< (abs (- guess (funcall f guess))) tolerance))
                                f)
             first-guess)))

;; But it is a nice abstraction :>