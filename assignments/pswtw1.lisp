(load "common.lisp") 

(defun twenty-one (player-strategy house-strategy)
  (let ((house-initial-hand (make-new-hand (deal))))
    (let ((player-hand
           (play-hand player-strategy
                      (make-new-hand (deal))
                      (hand-up-card house-initial-hand))))
      (if (> (hand-total player-hand) 21)
          0
          (let ((house-hand
                 (play-hand house-strategy
                            house-initial-hand
                            (hand-up-card player-hand))))
            (cond ((> (hand-total house-hand) 21)
                   1)
                  ((> (hand-total player-hand)
                      (hand-total house-hand))
                   1)
                  (t 0)))))))

(defun play-hand (strategy my-hand opponent-up-card)
  (cond ((> (hand-total my-hand) 21) my-hand)
        ((funcall strategy my-hand opponent-up-card)
         (play-hand strategy
                    (hand-add-card my-hand (deal))
                    opponent-up-card))
        (t my-hand)))

(defun deal ()
  (1+ (random 10)))

(defun make-new-hand (first-card)
  (make-hand first-card first-card))

(defun make-hand (up-card total)
  (cons up-card total))

(defun hand-up-card (hand)
  (car hand))

(defun hand-total (hand)
  (cdr hand))

(defun hand-add-card (hand new-card)
  (make-hand (hand-up-card hand)
             (+ new-card (hand-total hand))))

(defun hit? (your-hand opponent-up-card)
  (newline)
  (princ "Opponent up card ")
  (princ opponent-up-card)
  (newline)
  (princ "Your Total: ")
  (princ (hand-total your-hand))
  (newline)
  (princ "Hit? ")
  (user-says-y?))

(defun user-says-y? ()
  (y-or-n-p))

