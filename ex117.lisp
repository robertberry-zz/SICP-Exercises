; Exercises from 1.1.7

(defun mean (&rest numbers)
  (/ (apply #'+ numbers)
     (length numbers)))

(defun my-sqrt (x)
  (labels ((sqrt-iter (guess prev-guess)
           (if (good-enough? guess prev-guess)
               guess
               (sqrt-iter (improve-guess guess) guess)))
         (good-enough? (guess prev-guess)
           (if (null prev-guess)
               nil
               (< (/ (abs (- guess prev-guess))
                     guess)
                  0.001)))
         (improve-guess (guess)
           (mean guess (/ x guess))))
    (sqrt-iter 1.0 nil)))
