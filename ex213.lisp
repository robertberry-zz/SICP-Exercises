

; Exercise 2.4

(defun my-cons (x y)
  (lambda (m) (m x y)))

(defun my-car (z)
  (funcall z (lambda (p q) p)))

(defun my-cdr (z)
  (funcall z (lambda (p q) q)))

; Exercise 2.5

; a) TODO

;; matheh <: (

;; but basically, 3 and 2 have no common divisor ... both are prime.

;; and here is where I get stuck.

; b)

(defun arith-cons (a b)
  (* (expt 2 a) (expt 3 b)))

(defun arith-divisions (n)
  "Shows how many times n can be divided by 2 or 3."
  (prog nil
     (format t "~a" n)
     (cond ((= n 1) nil)
           ((evenp n) (prog nil
                         (format t " / 2~%")
                         (arith-divisions (/ n 2))))
           (t (prog nil
                 (format t " / 3~%")
                 (arith-divisions (/ n 3)))))))

;; running this basically shows that you repeatedly divide by 2 first, then 3.
;; how many times you divide by each is what the car and cdr must be expressed by.

; helper function
(defun holds-true-for (holds-true? next x)
  "Repeatedly transforms x with next, and counts untils holds-true? returns false."
  (labels ((iter (n acc)
             (if (funcall holds-true? n)
                 (iter (funcall next n) (+ acc 1))
                 acc)))
    (iter x 0)))

(defun arith-car (z)
  (holds-true-for #'evenp (lambda (n) (/ n 2)) z))

; helper function
(defun transform-until (transform done? x)
  "Repeatedly applies transformation to x until done? returns true."
  (if (funcall done? x)
      x
      (transform-until transform done? (funcall transform x))))

(defun arith-cdr (z)
  (holds-true-for (lambda (n) (> n 1))
                  (lambda (n) (/ n 3))
                  (transform-until (lambda (n) (/ n 2)) #'oddp z)))

; Exercise 2.6

(defparameter *zero*
  (lambda (f)
    (lambda (x) x)))

(defun add-1 (n)
  (lambda (f)
    (lambda (x)
      (funcall f (funcall (funcall n f) x)))))

; a)

(defparameter *one*
  (lambda (f)
    (lambda (x)
      (funcall f (funcall (lambda (x) x) x)))))

;; can be simplified by removing the inner funcall

;(defparameter *one*
;  (lambda (f)
;    (lambda (x)
;      (funcall f x))))

(defparameter *two*
  (lambda (f)
    (lambda (x)
      (funcall f (funcall (lambda (x)
                            (funcall f (funcall (lambda (x) x) x)))
                          x)))))

;; again, simplifies by removing the unnecessary identity function .. 

;(defparameter *two*
;  (lambda (f)
;    (lambda (x)
;      (funcall f (funcall f x)))))

; b)

(defun add (m n)
  (lambda (f)
    (lambda (x)
      (funcall (funcall m f) (funcall (funcall n f) x)))))
