; Answers for 3-1-3

(defun make-account (balance password)
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
                       ((eq m 'check-password) "Correct password")
                       (t (error "Unknown request -- MAKE-ACCOUNT ~a" m)))
                 (lambda (x) "Incorrect password"))))
    #'dispatch))

(defun make-joint (original-account original-password new-password)
  (labels ((joint-account (password m)
             (if (eq password new-password)
                 (funcall original-account original-password m)
                 (lambda (x) "Incorrect password"))))
    (if (string= (funcall original-account
                          original-password
                          'check-password)
                 "Correct password")
        #'joint-account
        (error "Bad password!"))))

(defvar acc1)
(defvar joint-acc)

(setf acc1 (make-account 100 'jimmy))
(setf joint-acc (make-joint acc1 'jimmy 'julia))

;; CL-USER> (funcall (funcall acc1 'jimmy 'withdraw) 10)
;; 90
;; CL-USER> (funcall (funcall acc1 'jimmy 'withdraw) 10)
;; 80
;; CL-USER> (funcall (funcall joint-acc 'jimmy 'withdraw) 10)
;; "Incorrect password"
;; CL-USER> (funcall (funcall joint-acc 'julia 'withdraw) 10)
;; 70


; Exercise 3.8

(let ((history '()))
  (defun f (x)
    (if (null history)
        (let nil
          (setf history (cons x history))
          0)
        (let ((return-value (car history)))
          (setf history '())
          return-value))))

;; CL-USER> (+ (f 1) (f 0))
;; 1
;; CL-USER> (+ (f 0) (f 1))
;; 0

