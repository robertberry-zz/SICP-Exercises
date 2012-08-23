; Answers for 3-4-2

(defun set-cdr! (x y)
  (setf (cdr x) y))

(defun set-car! (x y)
  (setf (car x) y))

; Exercise 3.39

;; 101: P_1 sets `x' to 100 and then P_2 increments
;;      `x' to 101.
;; 121: P_2 increments `x' to 11 and then P_1 sets
;;      `x' to `x' times `x'.
;; 100: P_1 accesses `x' (twice), then P_2 sets
;;      `x' to 11, then P_1 sets `x'.


; Exercise 3.40

; Without serialization:

;; 100:     P_1 accesses x twice (10) then P_2 cubes x (1000) then P_1 sets x to
;;          10 * 10.
;; 1000:    P_2 accesses x thrice (10) then P_1 squares x (100) then P_2 sets x to
;;          10 * 10 * 10.
;; 1000000: P_1 squares 'x' to 100 then P_2 cubes x to 1000000 (or P_2, then P_1)
;; 10000:   P_1 accesses x once (10), then P_2 cubes x (1000), then P_1 accesses
;;          x again (1000) and multiplies this by 10.
;; 100000:  P_2 accesses x once (10), then P_1 squares x (100), then P_1 accesses x
;;          twice more (each 100), then multiplies 10 * 100 * 100.

; With serialization only 1000000 remains, as whatever order the serialized
; processes run in produces the same value. All the other values require the
; processes to be running concurrently.


; Exercise 3.41

; In this case I don't think so, as any time the balance is read it should be
; in a consistent state. This is because it is only set once at the end of a
; process, and it is the only variable that is set.


; Exercise 3.42

; I can't see any reason why there would be any difference in the concurrency.
; Trick question? :<


; Exercise 3.43

; If the processes run sequentially, the exchange operation is atomic, so the
; only operation available is to exchange the balances of two
; accounts. However many times that runs will only ever produce three accounts
; containing the initial values.

; With the original procedure, when exchange is not atomic, the calculation of
; the balance difference can cause problems, as below:

;;    Acc1               Acc2              Acc3               Peter              Paul
;; +--------+         +--------+        +--------+    +----------------------+
;; |  $10   |         |  $20   |        |  $30   |    | balance-diff A3, A1: |
;; +--------+         +--------+        +--------+    |        $20           |
;;                                                    +----------------------+

;;                        +--------------------------------------------------------+
;;                        |                 |                                      |
;;                    +---v----+        +---v----+                        +-----------------+
;;                    |  $30   |        |  $20   |                        | Exchange A2, A3 |
;;                    +--------+        +--------+                        +-----------------+


;;     +------------------------------------+--------------------.
;;     |                                    |         +----------------------+
;; +---v----+                           +---v----+    | withdraw A3, $20.    |
;; |  $30   |                           |  $0    |    | deposit A1, $20      |
;; +--------+                           +--------+    +----------------------+

; The accounts, however, will always sum up to the same value, as the only
; operation that ever occurs is to subtract x from one account and add x to
; another account. As x is always both subtracted and added, the difference
; to the sum total of this operation is always 0.

; I'm not drawing another timing diagram! The reason has already been
; explained earlier in the chapter - the withdraw and deposit functions for an
; individual account can no longer reliably know that the balance hasn't
; changed before the set it. (e.g. one exchange might perform a withdrawal,
; which reads the current balance of A1, after which another exchange
; withdraws $10 from A1, then the first exchange's withdraw completes, setting
; the account's balance to an inconsistent value).


; Exercise 3.44

; No, Louis is wrong. The only inconsistent state is that for a while the
; money is in neither account, which shouldn't be a problem, as it will
; eventually be consistent.

; The essential difference between the procedures is that transfer consists
; completely of atomic procedures that perform actions on accounts. The
; exchange procedure, however, stores within a local state variable a
; calculation based on a state of the accounts that might have changed in the
; meantime, meaning that the procedure cannot always perform what it is
; supposed to do.


; Exercise 3.45

; serialized-exchange uses the deposit and withdraw procedures. If it is
; itself serialized then when it tries to run either of those procedures they
; will not run, as they are in the same set of procedures that cannot run in
; parallel as the serialized-exchange procedure itself. This will cause the
; system to lock.



; Exercise 3.46


;;      Cell                    test-and-set! 1                test-and-set! 2

;;  +-----------+         +-------------------------+    +-------------------------+
;;  | (false)   |         | if (car cell) <- false  |    | if (car cell) <- false  |
;;  +-----------+         +-------------------------+    +-------------------------+
;;                                     |                              |
;;                                     v                              |
;;  +-----------+         +----------------------+                    |
;;  | (true)    |<--------| set-car! cell true   |                    |
;;  +-----------+         +----------------------+                    |
;;                              (returns false)                       |
;;                                                                    v
;;                                                       +--------------------------+
;; (already true)<---------------------------------------| set-car! cell true       |
;;                                                       +--------------------------+
;;                                                                 (returns false)


; Exercise 3.47

; a)

(defun n-applications (n f)
  "Returns a list of of n applications of f."
  (if (= n 0)
      '()
      (cons (funcall f) (n-applications (1- n) f))))

(defun cycle! (seq)
  "Turn a list into a cycle (loop the rear back to the front)."
  (let ((front (car seq))
        (rear (last-pair seq)))
    (set-cdr! rear front)
    seq))

; aborted attempt:
;; (defun make-semaphore (n)
;;   (let* ((next-lock (make-mutex))
;;          (mutexes (cycle! (n-applications n #'make-mutex))))
;;     (labels ((get-next ()
;;                (funcall next-lock 'acquire)
;;                (let ((next-mutex (car mutexes)))
;;                  (setf mutexes (cdr mutexes))
;;                  (funcall next-lock 'release)
;;                  next-mutex))
;;              (the-semaphore (m)
;;                (case m
;;                  (acquire

(defun make-semaphore (n)
  (let ((lock (make-mutex))
        (waiting (make-queue))
        (acquired 0))
    (labels ((acquire ()
               (funcall lock 'acquire)
               (if (>= acquired n)
                   (let (waiting-lock (make-mutex))
                     (funcall waiting-lock 'acquire) ; initial acquire to make it unavailable
                     (insert-queue! waiting waiting-lock)
                     (funcall lock 'release)
                     (funcall waiting-lock 'acquire) ; now will wait until released
                     (funcall lock 'acquire)))
               (incf acquired)
               (funcall lock 'release))
             (release ()
               (funcall lock 'acquire)
               (decf acquired)
               (unless (empty-queue? waiting)
                 (let (first (front-queue waiting))
                   (funcall first 'release)
                   (delete-queue! queue)))
               (funcall lock 'release))
             (the-semaphore (m)
               (case m
                 (acquire (acquire))
                 (release (release)))))
      #'the-semaphore)))

; b)

(defun make-semaphore-2 (n)
  (let ((clear-cell '(nil))
        (cells (cycle! (n-applications n (lambda () nil)))))
    (labels ((acquire ()
               (labels ((iter (cells)
                          (if (test-and-set! cells)
                              (iter (cdr cells))))) ; cycles through cells till one is free
                 (iter cells)))
             (release ()
               (if (test-and-set! clear-cell)  ; as clear provides no information as to whether
                   (release)                   ; the cell was initially set, we need to force this
                   (labels ((iter (cells)      ; operation to be sequential with another cell.
                              (if (car cells)
                                  (clear! (car cells))
                                  (iter (cdr cells)))))
                     (iter cells)
                     (clear! clear-cell))))
             (the-semaphore (m)
               (case m
                 (acquire (acquire))
                 (release (release)))))
      #'the-semaphore)))

; These are pretty dumb, wasteful solutions. I thought as much so looked up
; better answers at
; http://wqzhang.wordpress.com/2009/08/04/sicp-exercise-3-47/


; Exercise 3.48

;; It imposes an ordering on the access to the accounts. This means if two
;; processes need access to the same two accounts, they both first attempt to
;; get a lock on the first account. One will get the lock, the other will wait
;; (meaning the second account remains free).

(let* ((id-serializer (make-serializer))
       (next-id 0)
       (get-id (funcall id-serializer (lambda ()
                                        (let ((next next-id))
                                          (incf next-id)
                                          next)))))
  (defun make-account-and-serializer (balance)
    (let ((balance-serializer (make-serializer))
          (account-id (get-id)))
      (labels ((withdraw (amount)
                 (if (>= balance amount)
                     (let nil
                       (setf balance (- balance amount))
                       balance)
                     "Insufficient funds"))
               (deposit (amount)
                 (setf balance (+ balance amount))
                 balance)
               (dispatch (m)
                 (case m
                   (withdraw #'withdraw)
                   (deposit #'deposit)
                   (balance #'balance)
                   (serializer balance-serializer)
                   (id account-id)
                   (otherwise (error "Unknown request -- MAKE-ACCOUNT ~a"
                                     m)))))
        #'dispatch))))

(defun serialized-exchange (account1 account2)
  (let* ((reversed (> (funcall account1 'id) (funcall account2 'id)))
         (account1 (if reversed account2 account1))
         (account2 (if reversed account1 account2)))
    (let ((serializer1 (funcall account1 'serializer))
          (serializer2 (funcall account2 'serializer)))
      (funcall (funcall serializer1 (funcall serializer 2))
               account1
               account2))))

; Exercise 3.49

; Let's say there are linked shared resources, credit cards and bank
; accounts. A credit card might be disocciated or associated with an account,
; so to access one from the other you should first acquire a lock.

; Let's say there's a process that given a credit card needs to automatically
; deduct balance from a bank account. There's another process that given a
; bank account needs to automatically disable credit cards.

; Process one gets the lock for the credit card. Process two gets the lock for
; the bank account. Process one finds the bank account out from the credit
; card but it can't get the lock because process two has it. Process two finds
; the credit card out from the account, but can't get the lock because process
; one has it.

; Convoluted example I know D:

