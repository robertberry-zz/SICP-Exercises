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

;; TODO!!!


; Exercise 2.10

(defun div-interval-2 (x y)
  (if (or (= (upper-bound y) 0) (= (lower-bound y) 0))
      (error "Cannot divide by an interval that spans 0.")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

; Exercise 2.11

(defun mul-interval-2 (x y)
  (let* ((xl (lower-bound x))
         (xu (upper-bound x))
         (yl (lower-bound y))
         (yu (upper-bound y))
         (xl+ (plusp xl))
         (xu+ (plusp xu))
         (yl+ (plusp yl))
         (yu+ (plusp yu))
         (xl- (not xl+))
         (xu- (not xu+))
         (yl- (not yl+))
         (yu- (not yu+)))
    (cond ((and xl- xu- yl- yu-) (make-interval (* xu yu) (* xl yl)))
          ((and xl- xu+ yl- yu-) (make-interval (* xu yl) (* xl yl)))
          ((and xl- xu- yl- yu+) (make-interval (* xl yu) (* xl yl)))
          ((and xl+ xu+ yl- yu-) (make-interval (* xu yl) (* xl yu)))
          ((and xl- xu- yl+ yu+) (make-interval (* xl yu) (* xu yl)))
          ((and xl+ xu+ yl+ yu+) (make-interval (* xl yl) (* xu yu)))
          ((and xl+ xu+ yl- yu+) (make-interval (* xu yl) (* xu yu)))
          ((and xl- xu+ yl+ yu+) (make-interval (* xl yu) (* xu yu)))
          ((and xl- xu+ yl- yu+) (make-interval (min (* xl yu) (* xu yl))
                                                (max (* xl yl) (* xu yu))))
          (t (error "This should never happen!")))))

;; some tests for this, as it's pretty complex

(defun rand-range (a b)
  (+ a (random (- b a))))

(defun rand-interval ()
  (let ((a (rand-range -100 100))
        (b (rand-range -100 100)))
    (make-interval (min a b) (max a b))))

(defun interval-eq (a b)
  (and (= (upper-bound a) (upper-bound b))
       (= (lower-bound a) (lower-bound b))))

(defun test-mul-interval-2 ()
  (let ((a (rand-interval))
        (b (rand-interval)))
    (if (interval-eq (mul-interval a b)
                     (mul-interval-2 a b))
        (format t "~a~%" "All OK!")
        (format t "~a ~a . ~a~%" "ERROR!" a b))))

(defun redo (f n)
  (if (= n 0)
      nil
      (prog nil
         (funcall f)
         (redo f (- n 1)))))

;; Can test like this: (redo #'test-mul-interval-2 100)

; Exercise 2.12

(defun make-center-width (c w)
  (make-interval (- c w) (+ c w)))

(defun center (i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(defun width (i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;

(defun make-center-percent (c tolerance)
  (let ((width (abs (* (/ tolerance 100.0) c))))
    (make-center-width c width)))

(defun percent (i)
  (* 100.0 (/ (width i) (abs (center i)))))

; Exercise 2.13
; -------------

; JOY OH JOY MORE MATHS

; tolerance(x1, x2) =  0.5 (x2 - x1)  =  x2 - x1
;                      -------------     ------
;                      0.5 (x1 + x2)     x1 + x2  <- this is only ok assuming positive numbers

; 
;

; a = [Ca*(1 - 0.5*Ta), Ca*(1 + 0.5*Ta)]
; b = [Cb*(1 - 0.5*Ta), Ca*(1 + 0.5*Ta)]

; a*b = [(Ca*(1 - 0.5 * Ta))*(Cb*(1-0.5 * Tb)), (Ca*(1 + 0.5 * Ta))*(Cb*(1+0.5 * Tb))

; ... 


; Exercise 2.14

(defun para1 (r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defun para2 (r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;;

;; TODO ...