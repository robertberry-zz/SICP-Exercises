; Exercises from 1.1.1

(defun f (a b c)
  (apply #'sum-of-squares (cond ((< a b c) (list b c))
                                ((< b a c) (list a c))
                                (t (list a b)))))

(defun a-plus-abs-b (a b)
  (funcall (if (> b 0)
       #'+
       #'-)
   a b))


(defun newton-iter (x guess prev-guess good-enough? improve-guess)
  (if (funcall good-enough? guess prev-guess x)
      guess
      (newton-iter x (funcall improve-guess guess x) guess good-enough? improve-guess)))

(defun const-diff-good-enough? (guess prev-guess x)
  (< (abs (- x (expt guess 2))) 0.001))

(defun fraction-change-good-enough? (guess prev-guess x)
  (if (null prev-guess)
      nil
      (< (/ (abs (- guess prev-guess))
            guess)
         0.001)))

(defun mean (&rest numbers)
  (/ (apply #'+ numbers)
     (length numbers)))

(defun sqrt-improve-guess (guess x)
  (mean guess (/ x guess)))

(defun my-sqrt (x)
  (newton-iter x 1.0 nil #'const-diff-good-enough? #'sqrt-improve-guess))

(defun my-sqrt2 (x)
  (newton-iter x 1.0 nil #'fraction-change-good-enough? #'sqrt-improve-guess))

(defun cbrt-improve-guess (guess x)
  (/ (+ (/ x (expt guess 2)) (* 2 guess)) 3))

(defun my-cbrt (x)
  (newton-iter x 1.0 nil #'fraction-change-good-enough? #'cbrt-improve-guess))

