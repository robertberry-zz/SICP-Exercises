; Answers for 4-3-2

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

; Exercise 4.38

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; ;;; Starting a new problem 
;; ;;; Amb-Eval value:
;; ((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; ((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; ((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; ((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; There are no more values of
;; (multiple-dwelling)


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
            (require (not (= smith fletcher)))
            (require (not (= smith cooper)))
            (require (not (= smith baker)))
            (require (not (= smith miller)))
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


; Exercise 4.43

;; what we can logically extract from the passage:

;; barnacle's yacht = gabrielle
;; moore's yacht = lorna
;; hall's yacht = rosalind
;; downing's yacht = melissa
;; parker's yacht = mary

;; dr parker's daughter = gabrielle's father's yacht

;; implies, dr parker's daughter is not gabrielle, as all the yachts are named
;; after other's daughters.

;; barnacle's daughter = melissa
;; moore's daughter /= lorna
;; hall's daughter /= rosalind
;; dr parker's daughter /= gabrielle
;; dr parker's daughter /= mary

(define (yachts)
  (define (yacht owner)
    (cond 
      ((eq? owner 'barnacle) 'gabrielle)
      ((eq? owner 'moore) 'lorna)
      ((eq? owner 'hall) 'rosalind)
      ((eq? owner 'downing) 'melissa)
      ((eq? owner 'parker) 'mary)))
  (let ((barnacle 'melissa))
    (let ((moore 'mary))
      (let ((parker (amb 'lorna 'rosalind)))
        (let ((hall (amb 'gabrielle 'mary 'lorna)))
          (require (not (eq? hall parker)))
          (let ((downing (amb 'gabrielle 'lorna 'rosalind)))
            (define (father daughter)
              (cond
                ((eq? daughter barnacle) 'barnacle)
                ((eq? daughter moore) 'moore)
                ((eq? daughter hall) 'hall)
                ((eq? daughter downing) 'downing)
                ((eq? daughter parker) 'parker)))
            (require (not (eq? downing parker)))
            (require (not (eq? downing hall)))
            (require (eq? parker (yacht (father 'gabrielle))))
            (list (list 'barnacle barnacle)
                  (list 'moore moore)
                  (list 'hall hall)
                  (list 'downing downing)
                  (list 'parker parker))))))))

;; ;;; Amb-Eval input:
;; (yachts)

;; ;;; Starting a new problem 
;; ;;; Amb-Eval value:
;; ((barnacle melissa) (moore mary) (hall gabrielle) (downing lorna) (parker rosalind))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; There are no more values of
;; (yachts)

(define (yachts)
  (define (yacht owner)
    (cond 
      ((eq? owner 'barnacle) 'gabrielle)
      ((eq? owner 'moore) 'lorna)
      ((eq? owner 'hall) 'rosalind)
      ((eq? owner 'downing) 'melissa)
      ((eq? owner 'parker) 'mary)))
  (let ((barnacle 'melissa))
    (let ((parker (amb 'lorna 'rosalind)))
      (let ((moore (amb 'rosalind 'gabrielle 'mary)))
        (require (not (eq? parker moore)))
        (let ((hall (amb 'gabrielle 'mary 'lorna)))
          (require (not (eq? hall moore)))
          (require (not (eq? hall parker)))
          (let ((downing (amb 'gabrielle 'mary 'lorna 'rosalind)))
            (define (father daughter)
              (cond
                ((eq? daughter barnacle) 'barnacle)
                ((eq? daughter moore) 'moore)
                ((eq? daughter hall) 'hall)
                ((eq? daughter downing) 'downing)
                ((eq? daughter parker) 'parker)))
            (require (not (eq? downing parker)))
            (require (not (eq? downing moore)))
            (require (not (eq? downing hall)))
            (require (eq? parker (yacht (father 'gabrielle))))
            (list (list 'barnacle barnacle)
                  (list 'moore moore)
                  (list 'hall hall)
                  (list 'downing downing)
                  (list 'parker parker))))))))

;; ;;; Amb-Eval input:
;; (yachts)

;; ;;; Starting a new problem 
;; ;;; Amb-Eval value:
;; ((barnacle melissa) (moore gabrielle) (hall mary) (downing rosalind) (parker lorna))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; ((barnacle melissa) (moore mary) (hall gabrielle) (downing lorna) (parker rosalind))


;; so, two answers.

; Exercise 4.44

(define (eight-queens)
  (define (a-position-in-column col)
    (list col (an-integer-between 1 8)))
  (define (column piece)
    (car piece))
  (define (row piece)
    (car (cdr piece)))      ;; this version of Scheme lacks cadr
  (define (horizontal-to a b)
    (= (row a) (row b)))
  (define (vertical-to a b)
    (= (column a) (column b)))
  (define (diagonal-to a b)
    (= (abs (- (column a) (column b)))
       (abs (- (row a) (row b)))))
  (define (attacks a b)
    (cond ((horizontal-to a b) true)    ;; this version of Scheme lacks or
          ((vertical-to a b) true)
          ((diagonal-to a b) true)
          (else false)))
  (define (for-each f seq)
    (if (null? seq)
        'ok
        (begin
          (f (car seq))
          (for-each f (cdr seq)))))
  (define (iter column acc)
    (if (= column 9)
        acc
        (let ((piece (a-position-in-column column)))
          (for-each (lambda (other-piece)
                      (require (not (attacks piece other-piece)))) acc)
          (iter (+ column 1) (cons piece acc)))))
  (iter 1 '()))

;; following is for printing out answers, but use in a full Scheme
;; evaluator. amb lacks display, etc.

(define (draw-eight-queens-solution pieces)
  (define (n-times n f)
    (if (= n 0)
        'ok
        (begin
          (f)
          (n-times (- n 1) f))))
  (define (draw-line)
    (display "+")
    (n-times 8 (lambda ()
                 (display "---+"))))
  (define (queen-at col row)
    (let iter ((pieces pieces))
      (if (null? pieces)
          false
          (let ((piece (car pieces)))
            (if (and (= (car piece) col) (= (cadr piece) row))
                true
                (iter (cdr pieces)))))))
  (define (draw-row row)
    (define (iter col)
      (if (= col 9)
          'ok
          (begin
            (display " ")
            (if (queen-at col row)
                (display "Q")
                (display " "))
            (display " |")
            (iter (+ col 1)))))
    (display "|")
    (iter 1))
  (define (iter row)
    (if (= row 9)
        'ok
        (begin
          (draw-line)
          (newline)
          (draw-row row)
          (newline)
          (iter (+ row 1)))))
  (iter 1)
  (draw-line))

;; 1 ]=> (draw-eight-queens-solution '((8 4) (7 2) (6 7) (5 3) (4 6) (3 8) (2 5) (1 1)))
;; +---+---+---+---+---+---+---+---+
;; | Q |   |   |   |   |   |   |   |
;; +---+---+---+---+---+---+---+---+
;; |   |   |   |   |   |   | Q |   |
;; +---+---+---+---+---+---+---+---+
;; |   |   |   |   | Q |   |   |   |
;; +---+---+---+---+---+---+---+---+
;; |   |   |   |   |   |   |   | Q |
;; +---+---+---+---+---+---+---+---+
;; |   | Q |   |   |   |   |   |   |
;; +---+---+---+---+---+---+---+---+
;; |   |   |   | Q |   |   |   |   |
;; +---+---+---+---+---+---+---+---+
;; |   |   |   |   |   | Q |   |   |
;; +---+---+---+---+---+---+---+---+
;; |   |   | Q |   |   |   |   |   |
;; +---+---+---+---+---+---+---+---+


; Exercise 4.45

;; (sentence
;;  (simple-noun-phrase
;;   (article the)
;;   (noun professor))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb-phrase
;;     (verb lectures)
;;     (prep-phrase (prep to)
;;                  (simple-noun-phrase (article the)
;;                                      (noun student))))
;;    (prep-phrase (prep in)
;;                 (simple-noun-phrase (article the)
;;                                     (noun class))))
;;   (prep-phrase (prep with)
;;                (simple-noun-phrase (article the)
;;                                    (noun cat)))))

;; In this sentence the professor is lecturing
;;  * to the student
;;  * in the class
;;  * with the cat  <- i.e., the cat is lecturing with him.

;; (sentence
;;  (simple-noun-phrase
;;   (article the)
;;   (noun professor))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb lectures)
;;    (prep-phrase (prep to)
;;                 (simple-noun-phrase
;;                  (article the)
;;                  (noun student))))
;;   (prep-phrase (prep in)
;;                (noun-phrase (simple-noun-phrase (article the)
;;                                                 (noun class))
;;                             (prep-phrase (prep with)
;;                                          (simple-noun-phrase (article the)
;;                                                              (noun cat)))))))

;; In this sentence the professor is lecturing
;;   * to the student
;;   * in the class with the cat   <- i.e., the specific class containing the cat.

;; (sentence
;;  (simple-noun-phrase (article the)
;;                      (noun professor))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb lectures)
;;    (prep-phrase (prep to)
;;                 (noun-phrase
;;                  (simple-noun-phrase (article the)
;;                                      (noun student))
;;                  (prep-phrase (prep in)
;;                               (simple-noun-phrase (article the)
;;                                                   (noun class))))))
;;   (prep-phrase (prep with)
;;                (simple-noun-phrase (article the)
;;                                    (noun cat)))))

;; In this sentence the professor is lecturing
;;    * to the student in the class   <- the specific student that is in the class
;;    * with the cat                  <- the cat is helping lecture

;; (sentence
;;  (simple-noun-phrase
;;   (article the)
;;   (noun professor))
;;  (verb-phrase
;;   (verb lectures)
;;   (prep-phrase
;;    (prep to)
;;    (noun-phrase
;;     (noun-phrase
;;      (simple-noun-phrase (article the)
;;                          (noun student))
;;      (prep-phrase (prep in)
;;                   (simple-noun-phrase (article the)
;;                                       (noun class))))
;;     (prep-phrase (prep with)
;;                  (simple-noun-phrase (article the)
;;                                      (noun cat)))))))

;; In this sentence the professor is lecturing to the student

;; * in the class   <- the student is in the class
;; * with the cat   <- the student is with the cat

;; (sentence
;;  (simple-noun-phrase (article the)
;;                      (noun professor))
;;  (verb-phrase
;;   (verb lectures)
;;   (prep-phrase
;;    (prep to)
;;    (noun-phrase
;;     (simple-noun-phrase
;;      (article the)
;;      (noun student))
;;     (prep-phrase
;;      (prep in)
;;      (noun-phrase
;;       (simple-noun-phrase
;;        (article the)
;;        (noun class))
;;       (prep-phrase
;;        (prep with)
;;        (simple-noun-phrase
;;         (article the)
;;         (noun cat)))))))))

;; In this sentence the professor is lecturing to the student

;; * in the class with the cat  <- i.e., the specific class containing the cat


; Exercise 4.46

;; Because the definitions in the grammar try the simplest definitions first,
;; followed by potentially endlessly more complicated definitions. e.g.,
;; parse-verb-phrase's 'rightmost' argument to amb would actually be a verb
;; followed by an infinite number of prepositional phrases. The program would
;; get stuck in an infinite loop attempting to find the rightmost argument.


; Exercise 4.47

;; Yes, it should work OK.

;; If you change the ordering, then it will infinitely recurse, as every first
;; option for parse-verb-phrase itself invokes parse-verb-phrase.


; Exercise 4.48

(define *unparsed* '())

(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define prepositions '(prep for to in by with))

(define conjunctions '(conjunction and but))

(define adverbs '(adverb quickly softly loudly quietly))

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-word adverbs)))))
  (maybe-extend (parse-word verbs)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-simple-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-sentence)
  (define (maybe-extend sentence)
    (amb sentence
         (maybe-extend (list 'sentence
                             sentence
                             (parse-word conjunctions)
                             (parse-simple-sentence)))))
  (maybe-extend (parse-simple-sentence)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))


; Exercise 4.49

(define (parse-word word-list)
  (list (car word-list) (an-element-of (cdr word-list))))

;; ;;; Amb-Eval input:
;; (parse '())

;; ;;; Starting a new problem 
;; ;;; Amb-Eval value:
;; (sentence (simple-noun-phrase (article the) (noun student)) (verb studies))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; (sentence (sentence (simple-noun-phrase (article the) (noun student)) (verb studies)) (conjunction and) (sentence (simple-noun-phrase (article the) (noun student)) (verb studies)))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; (sentence (sentence (sentence (simple-noun-phrase (article the) (noun student)) (verb studies)) (conjunction and) (sentence (simple-noun-phrase (article the) (noun student)) (verb studies))) (conjunction and) (sentence (simple-noun-phrase (article the) (noun student)) (verb studies)))

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; (sentence (sentence (sentence (sentence (simple-noun-phrase (article the) (noun student)) (verb studies)) (conjunction and) (sentence (simple-noun-phrase (article the) (noun student)) (verb studies))) (conjunction and) (sentence (simple-noun-phrase (article the) (noun student)) (verb studies))) (conjunction and) (sentence (simple-noun-phrase (article the) (noun student)) (verb studies)))


;; etc. Gets stuck in recursing between different kinds of compound sentences.

