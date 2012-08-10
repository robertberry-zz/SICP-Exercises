(load "pswtw1.lisp")

; Problem 2

(defun stop-at (n)
  (lambda (your-hand opponent-up-card)
    (< (hand-total your-hand) n)))

; Problem 3

(defun test-strategies (strategy1 strategy2 n-tests)
  (if (= n-tests 0)
      0
      (+ (twenty-one strategy1 strategy2)
         (test-strategies strategy1 strategy2 (1- n-tests)))))

;; CL-USER> (test-strategies (stop-at 16) (stop-at 15) 10)
;; 4
;; CL-USER> (test-strategies (stop-at 16) (stop-at 15) 10)
;; 8
;; CL-USER> (test-strategies (stop-at 16) (stop-at 15) 10)
;; 4
;; CL-USER> (test-strategies (stop-at 16) (stop-at 15) 10)
;; 4
;; CL-USER> (test-strategies (stop-at 16) (stop-at 15) 10)
;; 5

(defun watch-player (strategy)
  (lambda (your-hand opponent-up-card)
    (format t "Player has hand total ~a~%" (hand-total your-hand))
    (let ((hits (funcall strategy your-hand opponent-up-card)))
      (format t "Player ~a!~%" (if hits "hits" "stays"))
      hits)))

;; CL-USER> (twenty-one (watch-player (stop-at 15)) (watch-player (stop-at 16)))
;; Player has hand total 3
;; Player hits!
;; Player has hand total 6
;; Player hits!
;; Player has hand total 9
;; Player hits!
;; Player has hand total 17
;; Player stays!
;; Player has hand total 5
;; Player hits!
;; Player has hand total 11
;; Player hits!
;; Player has hand total 19
;; Player stays!
;; 0
;; CL-USER> (twenty-one (watch-player (stop-at 15)) (watch-player (stop-at 16)))
;; Player has hand total 4
;; Player hits!
;; Player has hand total 9
;; Player hits!
;; Player has hand total 13
;; Player hits!
;; Player has hand total 16
;; Player stays!
;; Player has hand total 1
;; Player hits!
;; Player has hand total 6
;; Player hits!
;; Player has hand total 7
;; Player hits!
;; Player has hand total 10
;; Player hits!
;; Player has hand total 16
;; Player stays!
;; 0

(defun louis (your-hand opponent-up-card)
  (let ((total (hand-total your-hand)))
    (cond ((< total 12) t)
          ((> total 16) nil)
          ((and (= total 12) (< opponent-up-card 4)) t)
          ((and (= total 16) (= opponent-up-card 10)) nil)
          ((> opponent-up-card 6) t)
          (t nil))))

;; CL-USER> (test-strategies #'louis (stop-at 15) 1000)
;; 414
;; CL-USER> (test-strategies #'louis (stop-at 16) 1000)
;; 369
;; CL-USER> (test-strategies #'louis (stop-at 17) 1000)
;; 366

(defun both (strategy1 strategy2)
  (lambda (your-hand opponent-up-card)
    (and (funcall strategy1 your-hand opponent-up-card)
         (funcall strategy2 your-hand opponent-up-card))))

;; CL-USER> (twenty-one (both (stop-at 16) #'hit?) (stop-at 17))

;; Opponent up card 3
;; Your Total: 5
;; Hit? 
;; (y or n) y

;; Opponent up card 3
;; Your Total: 6
;; Hit? 
;; (y or n) y

;; 0
;; CL-USER> (twenty-one (both (stop-at 16) #'hit?) (stop-at 17))

;; Opponent up card 3
;; Your Total: 6
;; Hit? 
;; (y or n) y

;; Opponent up card 3
;; Your Total: 8
;; Hit? 
;; (y or n) y

;; Opponent up card 3
;; Your Total: 15
;; Hit? 
;; (y or n) y

;; 0


;; TODO: Tutorial ex 1 and 2
