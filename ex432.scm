; Answers for 4-3-2


; Exercise 4.38


;; todo ...



; Exercise 4.39

;; It won't have a major effect (if any) on speed, as the major bottleneck to
;; the procedure is the sheer number of possibilities it must consider
;; (5^5). The order of the requirements will not change that, only how quickly
;; a given possibility is rejected.

;; If you were going to re-order them you would do so by descending order of
;; how many possibilities a given requirement will reject that have not
;; already been rejected by any of the previous requirements.

;; Given that, the ordering is already pretty good.

;; The number of possibilities originally is 5^5 - 3125.

;; The number of distinct orderings is 5! (120) - so the first requirement
;; rejects 3005 of the possibilities.

;; Of those remaining, Baker is on floor 5 for all distinct permutations of
;; the other 4 (i.e. 4!) - this rejects 24 further possibilities, so we're
;; down to 96.

;; Cooper is on floor 5 again for all distinct permutations of the other
;; 4. But we must now consider subtracting all of the permutations already
;; dismissed by Baker. i.e., all the permutations for which Cooper is on floor
;; 1 and Baker is on floor 5. This is 3! (6). So now we dismiss another 24-6
;; (18) possibilities, down to 78 ... etc.

;; Got a bit carried away there :D LAWL



; Exercise 4.40

;; 3125 before, 120 after.

(define (multiple-dwelling)
  (let ((fletcher (amb 1 2 3 4 5)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))       
    (let ((cooper (amb 1 2 3 4 5)))               
      (require (not (= fletcher cooper)))
      (require (not (= cooper 1)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((baker (amb 1 2 3 4 5)))
        (require (not (= baker fletcher)))
        (require (not (= baker cooper)))
        (require (not (= baker 5)))
        (let ((miller (amb 1 2 3 4 5)))
          (require (> miller cooper))
          (require (not (= miller fletcher)))
          (require (not (= miller baker)))
          (let ((smith (amb 1 2 3 4 5)))
            (require (not (= (abs (- smith fletcher)) 1)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))


; Exercise 4.41

(define (insertions seq x)
  "List of every possible insertion of x into seq."
  (if (null? seq)
      (list (list x))
      (cons (cons x seq)
            (map (lambda (rest)
                   (cons (car seq) rest))
                 (insertions (cdr seq) x)))))

;; 1 ]=> (insertions '(1 2 3 4 5) 'a)

;; ;Value: insertions

;; 1 ]=> 
;; ;Value 13: ((a 1 2 3 4 5) (1 a 2 3 4 5) (1 2 a 3 4 5) (1 2 3 a 4 5) (1 2 3 4 a 5) (1 2 3 4 5 a))

(define (mappend f seq)
  "Map f to seq, then append results into one list."
  (reduce append '() (map f seq)))

(define (permutations seq)
  "List all permutations of seq."
  (if (null? seq)
      '(())
      (let ((perms-rest (permutations (cdr seq)))
            (first (car seq)))
        (mappend (lambda (seq)
                   (insertions seq first)) perms-rest))))

(define (remove-if f seq)
  "Sequence containing elements of seq for which f is not true."
  (if (null? seq)
      '()
      (if (f (car seq))
          (remove-if f (cdr seq))
          (cons (car seq) (remove-if f (cdr seq))))))

(define (caddddr seq)
  (car (cdr (cdddr seq))))

(define (multiple-dwelling)
  (define (describe-permutation perm)
    (list (list 'baker (car perm))
          (list 'cooper (cadr perm))
          (list 'fletcher (caddr perm))
          (list 'miller (cadddr perm))
          (list 'smith (caddddr perm))))
  (let ((positions-permutations (permutations '(1 2 3 4 5))))
    (map describe-permutation
         (remove-if (lambda (p)
                      (let ((baker (car p))
                            (cooper (cadr p))
                            (fletcher (caddr p))
                            (miller (cadddr p))
                            (smith (caddddr p)))
                        (or (= baker 5)
                            (= cooper 1)
                            (= fletcher 5)
                            (= fletcher 1)
                            (> cooper miller)
                            (= (abs (- smith fletcher)) 1)
                            (= (abs (- fletcher cooper)) 1))))
                    positions-permutations))))

;; 1 ]=> (multiple-dwelling)

;; ;Value 29: (((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1)))


; Exercise 4.42

(define (xor a b)
  "Exclusive or."
  (and (not (and a b))
       (not (and (not a) (not b)))))

(define (liars)
  (define (describe-permutation perm)
    (list (list 'betty (car perm))
          (list 'ethel (cadr perm))
          (list 'joan (caddr perm))
          (list 'kitty (cadddr perm))
          (list 'mary (caddddr perm))))
  (let ((positions-permutations (permutations '(1 2 3 4 5))))
    (map describe-permutation
         (filter (lambda (p)
                   (let ((betty (car p))
                         (ethel (cadr p))
                         (joan (caddr p))
                         (kitty (cadddr p))
                         (mary (caddddr p)))
                     (and (xor (= kitty 2) (= betty 3))
                          (xor (= ethel 1) (= joan 2))
                          (xor (= joan 3) (= ethel 5))
                          (xor (= kitty 2) (= mary 4))
                          (xor (= mary 4) (= betty 1)))))
                 positions-permutations))))

;; 1 ]=> (liars)

;; ;Value 33: (((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4)))

