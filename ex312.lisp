; Answers for 3-1-2

(defun estimate-pi (trials)
  (sqrt (/ 6 (monte-carlo trials #'cesaro-test))))

(defun rand ()
  (random 10000))

(defun cesaro-test ()
  (= (gcd (rand) (rand)) 1))

(defun monte-carlo (trials experiment)
  (labels ((iter (trials-remaining trials-passed)
             (cond ((= trials-remaining 0)
                    (/ trials-passed trials))
                   ((funcall experiment)
                    (iter (- trials-remaining 1) (1+ trials-passed)))
                   (t
                    (iter (- trials-remaining 1) trials-passed)))))
    (iter trials 0)))

; Exercise 3.5

;;; todo ...
