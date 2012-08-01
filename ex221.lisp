; Exercises for 2-2-1

; Exercise 2.17

(defun last-pair (seq)
  (labels ((iter (seq)
             (if (null (cdr seq))
                 (car seq)
                 (iter (cdr seq)))))
    (if (null seq)
        (error "Empty sequence.")
        (iter seq))))

; Exercise 2.18

(defun my-reverse (seq)
  (labels ((iter (seq acc)
             (if (null seq)
                 acc
                 (iter (cdr seq) (cons (car seq) acc)))))
    (iter seq '())))

; Exercise 2.19

; a)

(defparameter us-coins (list 50 25 10 5 1))
;; Lol 0.5? When was this written? 
(defparameter uk-coins (list 100 50 20 10 5 2 1 0.5))

(defun cc (amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (t (+ (cc amount
                  (except-first-denomination coin-values))
              (cc (- amount
                     (first-denomination coin-values))
                  coin-values)))))

(defun first-denomination (coin-values)
  (car coin-values))

(defun except-first-denomination (coin-values)
  (cdr coin-values))

(defun no-more? (coin-values)
  (null coin-values))

; b) 

; No.

; Exercise 2.20

(defun same-parity (x &rest xs)
  (remove-if-not (if (oddp x) #'oddp #'evenp) xs))

;; Though, I'm probably supposed to do it without remove-if-not.

(defun same-parity-2 (x &rest xs)
  (labels ((iter (keep? seq)
             (cond ((null seq) '())
                   ((funcall keep? (car seq))
                    (cons (car seq) (iter keep? (cdr seq))))
                   (t (iter keep? (cdr seq))))))
    (iter (if (oddp x) #'oddp #'evenp) xs)))

; Exercise 2.21

(defun square (x)
  (* x x))

(defun square-list (items)
  (if (null items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(defun square-list-2 (items)
  (mapcar #'square items))

; Exercise 2.22

; a) Because he is consing onto the answer list, which puts the answer onto
;    the front of the list, rather than the back.

; b) Because of two reasons:
;    i) You are attempting to cons onto the answer, which is not a list. You
;       cannot push something to the front of a number.
;    ii) Even if you put the answer in a list, you would only be consing a
;        reference to the current answer list at that point to the start - it
;        would not 'join' the lists, but successively nest the answers, like so:
;
; (for answers a, b, c ... )
;
;   ((((nil a) b) c) d ... )

; Exercise 2.23

;; A really easy implementation of for-each is just in terms of map. (as it is
;; still essentially mapping, just discarding the results.)

(defun for-each (f seq)
  (prog nil
     (mapcar f seq)
     t))

;; A recursive solution is as follows:

(defun for-each-2 (f seq)
  (if (null seq)
      t
      (prog nil
         (funcall f (car seq))
         (for-each-2 f (cdr seq)))))

;; This doesn't actually return True, it returns nil. I think this is
;; something to do with how prog works. TODO: Look this up and find out.

;; This can easily be worked around with a labels iter, however.
