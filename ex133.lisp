; Computers let you play with numbers! :D

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

; Exercise 1.35

;; Gawly some mathy stuff
;; TODO

(defparameter *golden-ratio* (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1))

; Exercise 1.36

(defun fixed-point-verbose (f first-guess)
  (labels ((close-enough? (v1 v2)
             (< (abs (- v1 v2)) *tolerance*))
           (try (guess)
             (let ((next (funcall f guess)))
               (format t "~a~%" guess)
               (if (close-enough? guess next)
                   next
                   (try next)))))
    (try first-guess)))

(defun average (x y)
  (/ (+ x y) 2))

(fixed-point-verbose (lambda (x) (/ (log 1000) (log x))) 2)

;; 2
;; 9.965784
;; 3.0044723
;; 6.279196
;; 3.7598507
;; 5.215844
;; 4.182207
;; 4.827765
;; 4.3875937
;; 4.67125
;; 4.481404
;; 4.6053658
;; 4.523085
;; 4.5771146
;; 4.541383
;; 4.5649033
;; 4.5493727
;; 4.5596066
;; 4.552854
;; 4.5573053
;; 4.5543694
;; 4.5563054
;; 4.5550284
;; 4.5558705
;; 4.555315
;; 4.555681
;; 4.55544
;; 4.5555987
;; 4.5554943
;; 4.555563
;; 4.5555177
;; 4.5555477
;; 4.555528
;; 4.555541

(fixed-point-verbose (lambda (x) (average x (/ (log 1000) (log x)))) 2)

;; 2
;; 5.982892
;; 4.9221687
;; 4.6282244
;; 4.5683465
;; 4.5577307
;; 4.55591
;; 4.555599
;; 4.5555468
;; 4.5555325

; Exercise 1.37

; a)

(defun cont-frac (n D k)
  (labels ((term (i)
             (/ (funcall n i)
                (+ (funcall D i) (if (= i k)
                                     0
                                     (term (1+ i)))))))
    (term 1)))

;; About 12 iterations does it ... I stumbled on this almost immediately lol,
;; though it would've been better to write some code to check for you.

; b)

(defun cont-frac-iter (n D k)
  (labels ((iter (i acc)
             (if (= i 1)
                 (/ (funcall n 1) acc)
                 (iter (1- i) (+ (funcall D (1- i)) (/ (funcall n i) acc))))))
    (iter (1- k) (/ (funcall n k) (funcall D k)))))

;; this works but is ugly - should I really be passing in the Kth term as the
;; original accumulator? write-only code too ... todo: refactor

; Exercise 1.38

(defun const (i)
  "Returns a function that takes one argument but always returns the given value."
  (lambda (x) i))

(defparameter *e* (+ 2 (cont-frac (const 1) (lambda (i)
                                              (if (= (mod i 3) 2)
                                                  (* (+ (floor (/ i 3)) 1) 2)
                                                  1)) 10)))

; Exercise 1.39

(defun tan-cf (x k)
  (cont-frac (lambda (i)
               (if (= i 1) x (- (expt x 2))))
             (lambda (i)
               (- (* i 2) 1))
             k))

;; Seems to need a lot of iterations (like 100) to get close ...

