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

; a = [Ca*(1 - 0.5*Ta), Ca*(1 + 0.5*Ta)]
; b = [Cb*(1 - 0.5*Ta), Ca*(1 + 0.5*Ta)]

; a*b = [(Ca*(1 - 0.5 * Ta))*(Cb*(1-0.5 * Tb)),
;        (Ca*(1 + 0.5 * Ta))*(Cb*(1+0.5 * Tb))]

; lower = ( Ca - 0.5 Ta Ca ) (Cb - 0.5 Tb Cb )
;       =   Ca Cb - 0.5 Ta Ca Cb - 0.5 Ca Tb Cb + 0.25 Ta Ca Tb Cb
;       =   Ca Cb (1 - 0.5 Ta - 0.5 Tb + 0.25 Ta Tb)
;       =   Ca Cb (1 - 0.5 (Ta + Tb) + 0.25 Ta Tb)

; upper = ( Ca + 0.5 Ta Ca ) (Cb + 0.5 Tb Cb)
;       =   Ca Cb + 0.5 Tb Cb Ca + 0.5 Ta Ca Cb + 0.25 Ta Tb Ca Cb
;       =   Ca Cb (1 + 0.5 (Tb + Ta) + 0.25 Ta Tb)

; Tolerance will be half of difference between upper and lower.

; 0.25 Ta Tb will be a tiny number as the tolerances are small anyway, so should be discounted.

; Therefore the tolerance will be roughly the sum of the tolerances of of a and b.

; Exercise 2.14

(defun para1 (r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defun para2 (r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))


(defparameter int-a (make-center-width 1000 5))
(defparameter int-b (make-center-width 100 1))

;; CL-USER> (para1 int-a int-b)
;; (89.06419 . 92.78336)
;; CL-USER> (para2 int-a int-b)
;; (90.04114 . 91.77667)
;; CL-USER> (percent (para1 int-a int-b))
;; 2.0452127
;; CL-USER> (percent (para2 int-a int-b))
;; 0.95454603

; loses accuracy with each division

;; CL-USER> (percent int-a)
;; 0.5
;; CL-USER> (div-interval (make-interval 1 1) (div-interval (make-interval 1 1) int-a))
;; (995.0 . 1004.99994)
;; CL-USER> (percent (div-interval (make-interval 1 1) (div-interval (make-interval 1 1) int-a)))
;; 0.49999696

; that's not the reason, though!

;; CL-USER> (div-interval int-a int-a)
;; (0.9900498 . 1.0100503)

;; So, if you divide the interval by itself, it does not equal the interval
;; for 1 (1-1). Why is this? Because div-interval attempts to figure out the
;; minimum and maximum possible values. The interval represents one resistor
;; whose resistance is somewhere in that interval. But this resistance does
;; not change!

;; But in div interval, to calculate the lower resistance, it would divide the
;; lower part of the interval by the higher. This would never actually happen
;; ... and it means the data becomes noisy. I think.

; Exercise 2.15

;; I answered this above, lol.

; Exercise 2.16

;; The package would need to know when two intervals are the *same*
;; interval. Equivalent intervals are not necessarily the same, however. You
;; might have two resistors whose resistance is between 9-11 ohms, but that
;; does not mean they are the same.

;; This could be kept track via a third variable that identifies the variable
;; in the same way that gensym works.

;; You would then need to rewrite algebraic expressions so as to make sure
;; that when two variables that are the same variable are being manipulated,
;; it only ever performs the operation on the same bound (either the upper or
;; lower).

;; Ok ... so I'll try to verify this by actually attempting to write such a
;; package.

(let ((id 0))
  (defun make-interval-id ()
    (incf id)))
  
(defun make-interval-z (x y)
  (list x y (make-interval-id)))

(defun interval-id (x)
  (caddr x))

(defun same-interval (x y)
  (= (interval-id x) (interval-id y)))

;; This can remain as it is
(defun add-interval-z (x y)
  (make-interval-z (+ (lower-bound-z x) (lower-bound-z y))
                   (+ (upper-bound-z x) (upper-bound-z y))))

(defun lower-bound-z (a)
  (car a))

(defun upper-bound-z (a)
  (cadr a))

(defun mul-interval-z (x y)
  (let ((p1 (* (lower-bound-z x) (lower-bound-z y)))
        (p2 (* (lower-bound-z x) (upper-bound-z y)))
        (p3 (* (upper-bound-z x) (lower-bound-z y)))
        (p4 (* (upper-bound-z x) (upper-bound-z y))))
    (if (same-interval x y)
        (make-interval-z (min p1 p4)
                         (max p1 p4))
        (make-interval-z (min p1 p2 p3 p4)
                         (max p1 p2 p3 p4)))))

(defun div-interval-z (x y)
  (if (same-interval x y)
      (make-interval-z 1 1)
      (mul-interval-z x
                    (make-interval-z (/ 1.0 (upper-bound-z y))
                                     (/ 1.0 (lower-bound-z y))))))


(defun para1-z (r1 r2)
  (div-interval-z (mul-interval-z r1 r2)
                  (add-interval-z r1 r2)))

(defun para2-z (r1 r2)
  (let ((one (make-interval-z 1 1)))
    (div-interval-z one
                    (add-interval-z (div-interval-z one r1)
                                    (div-interval-z one r2)))))

;; CL-USER> (para1-z i j)

;; (2.5 20.0 16)
;; CL-USER> (para2-z i j)
;; (5.0 10.0 24)

;; OK so that completely did not work! Why?

;; The answers are so different it looks like in one it's realized some
;; resistors are the same and in the other it hasn't. Information is being
;; lost at some point. (basically because 1/R1 should be considered the same
;; as 1/R1, whereas at the moment that information is lost as a new ID is
;; generated.)

;; Would we be able to keep this information throughout? You'd have to
;; remember the original IDs and the actions performed to create the new
;; variables. Seems very complex ...

;; Anyway, time to look up the answer as I'm probably way off.

;; <- wow, I'm actually correct about this. As for whether it's doable, the
;; answer doesn't say, but I'm on the right track (re identity) ... 
