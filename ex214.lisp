; Answers for section 2.1.4

(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

; Exercise 2.7

(defun make-interval (a b) (cons a b))

(defun upper-bound (x)
  (cdr x))

(defun lower-bound (x)
  (car x))

; Exercise 2.8

; Produces an interval between the smallest possible value (the lower bound of
; x subtract the higher bound of y) and the highest possible value (the higher
; bound of x subtract the lower bound of y)

(defun sub-interval (x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; Exercise 2.9

(defun width-interval (x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

; a)    w(x1, x2) = (x2 - x1) / 2

;  ok ... so for a + b = c ... 

;  c1 = a1 + b1
;  c2 = a2 + b2
;  w(c1, c2) = ((a2 + b2) - (a1 + b1)) / 2
;            = (a2 + b2 - a1 - b1) / 2
;            = (a2 - a1) + (b2 - b1) / 2
;            = w(a1, a2) + w(b1, b2)

; and for a - b = c

;  c1 = a1 - b2
;  c2 = a2 - b1

;  w(c1, c2) = ((a2 - b1) - (a1 - b2)) / 2
;            = (a2 - b1 - a1 + b2) / 2
;            = ((a2 - a1) + (b2 - b1)) / 2
;            = w(a1, a2) + w(b1, b2)

; So for both, you just add the widths of the terms together. (Which makes
; sense, as you just add more uncertainty!)

; b)

;; give examples? ehhhh?

