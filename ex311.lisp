; Answers for 3-1-1

(defvar *balance* 100)

(defun withdraw (amount)
  (if (>= *balance* amount)
      (setf *balance* (- *balance* amount))
      "Insufficient funds"))

(let ((balance 100))
  (defun new-withdraw (amount)
    (if (>= balance amount)
        (setf balance (- balance amount))
        "Insufficient funds")))

(defun make-withdraw (balance)
  (lambda (amount)
    (if (>= balance amount)
        (setf balance (- balance amount))
        "Insufficient funds")))

(defvar w1 (make-withdraw 100))
(defvar w2 (make-withdraw 200))

;; CL-USER> (funcall w1 10)
;; 90
;; CL-USER> (funcall w2 1)
;; 199
;; CL-USER> (funcall w1 10)
;; 80

(defun make-account (balance)
  (labels ((withdraw (amount)
             (if (>= balance amount)
                 (decf balance amount)
                 "Insufficient funds"))
           (deposit (amount)
             (incf balance amount))
           (dispatch (m)
             (cond ((eq m 'withdraw) #'withdraw)
                   ((eq m 'deposit) #'deposit)
                   (t (error "Unknown request -- MAKE-ACCOUNT ~a" m)))))
    #'dispatch))

(defvar acc (make-account 100))

;; CL-USER> (funcall (funcall acc 'withdraw) 10)
;; 90
;; CL-USER> (funcall (funcall acc 'withdraw) 100)
;; "Insufficient funds"
;; CL-USER> (funcall (funcall acc 'deposit) 400)
;; 490
;; CL-USER> (funcall (funcall acc 'withdraw) 50)
;; 440

; Scheme is nicer :(


; Exercise 3.1

(defun make-accumulator (sum)
  (lambda (n)
    (incf sum n)))

(defvar a (make-accumulator 5))

;; CL-USER> (funcall a 10)
;; 15
;; CL-USER> (funcall a 10)
;; 25


; Exercise 3.2

(defun make-monitored (f)
  (let ((times-called 0))
    (lambda (x)
      (labels ((exec ()
                 (incf times-called)
                 (funcall f x)))
        (if (symbolp x)
            (case x
              (how-many-calls? times-called)
              (reset-count (setf times-called 0))
              (otherwise (exec)))
            (exec))))))

(defvar s (make-monitored #'sqrt))

;; CL-USER> (funcall s 100)
;; 10.0
;; CL-USER> (funcall s 'how-many-calls?)
;; 1
;; CL-USER> (funcall s 100)
;; 10.0
;; CL-USER> (funcall s 100)
;; 10.0
;; CL-USER> (funcall s 'how-many-calls?)
;; 3
;; CL-USER> (funcall s 'reset-count)
;; 0
;; CL-USER> (funcall s 100)
;; 10.0
;; CL-USER> (funcall s 'how-many-calls?)
;; 1


; Exercise 3.3

(defun make-account-2 (balance password)
  (labels ((withdraw (amount)
             (if (>= balance amount)
                 (decf balance amount)
                 "Insufficient funds"))
           (deposit (amount)
             (incf balance amount))
           (dispatch (pw-attempt m)
             (if (eq password pw-attempt)
                 (cond ((eq m 'withdraw) #'withdraw)
                       ((eq m 'deposit) #'deposit)
                       (t (error "Unknown request -- MAKE-ACCOUNT ~a" m)))
                 (lambda (x) "Incorrect password"))))
    #'dispatch))

(defvar acc-2 (make-account-2 100 'checkers))

;; CL-USER> (funcall (funcall acc-2 'draughts 'withdraw) 10)
;; "Incorrect password"
;; CL-USER> (funcall (funcall acc-2 'checkers 'withdraw) 10)
;; 90


; Exercise 3.4

(defun call-the-cops ()
  (lambda (x)
    (format t "--o o--")))

(defun make-account-3 (balance password)
  (let ((incorrect-attempts 0))
    (labels ((withdraw (amount)
               (if (>= balance amount)
                   (decf balance amount)
                   "Insufficient funds"))
             (deposit (amount)
               (incf balance amount))
             (dispatch (pw-attempt m)
               (if (eq password pw-attempt)
                   (cond ((eq m 'withdraw) #'withdraw)
                         ((eq m 'deposit) #'deposit)
                         (t (error "Unknown request -- MAKE-ACCOUNT ~a" m)))
                   (if (> incorrect-attempts 7)
                       (call-the-cops)
                       (let nil
                         (incf incorrect-attempts)
                         (lambda (x) "Incorrect password"))))))
      #'dispatch)))


(defvar acc-3 (make-account-3 100 'checkers))

;; CL-USER> (funcall (funcall acc-3 'checkers 'withdraw) 10)
;; 90
;; CL-USER> (funcall (funcall acc-3 'draughts 'withdraw) 10)
;; "Incorrect password"
;; CL-USER> (funcall (funcall acc-3 'draughts 'withdraw) 10)
;; "Incorrect password"
;; CL-USER> (funcall (funcall acc-3 'draughts 'withdraw) 10)
;; "Incorrect password"
;; CL-USER> (funcall (funcall acc-3 'draughts 'withdraw) 10)
;; "Incorrect password"
;; CL-USER> (funcall (funcall acc-3 'draughts 'withdraw) 10)
;; "Incorrect password"
;; CL-USER> (funcall (funcall acc-3 'draughts 'withdraw) 10)
;; "Incorrect password"
;; CL-USER> (funcall (funcall acc-3 'draughts 'withdraw) 10)
;; "Incorrect password"
;; CL-USER> (funcall (funcall acc-3 'draughts 'withdraw) 10)
;; "Incorrect password"
;; CL-USER> (funcall (funcall acc-3 'draughts 'withdraw) 10)
;; --o o--
