; Answers for 4-4-1

;; Exercise 4.55

;; 1.

(supervisor ?name (BitDiddle Ben))

;; 2.

(job ?name (accounting . ?job))

;; 3.

(address ?name (Slumerville . ?address))


;; Exercise 4.56

;; a)

(and (supervisor ?name (BitDiddle Ben))
     (address ?name ?address))

;; b)

(and (salary (BitDiddle Ben) ?ben-salary)
     (salary ?name ?salary)
     (lisp-value < ?salary ?ben-salary))

;; c)

(and (supervisor ?name ?super-name)
     (not (job ?super-name (computer . ?super-job))))


;; Exercise 4.57

(rule (replaces ?x ?y)
      (and (job ?x ?x-job)
           (job ?y ?y-job)
           (not (same ?x ?y))
           (or (same ?x-job ?y-job)
               (can-do-job ?x-job ?y-job))))

;; a)

(replaces ?x (Fect Cy D))

;; b) 

(and (replaces ?x ?y)
     (salary ?x ?salary-x)
     (salary ?y ?salary-y)
     (lisp-value < ?salary-x ?salary-y))


;; Exercise 4.58

(rule (big-shot ?person ?division)
      (and (job ?person (?division . ?job))
           (not (and (supervisor ?person ?super)
                     (job ?super (?division . ?super-job))))))


;; Exercise 4.59

;; a)

(meeting ?type (Friday ?time))

;; b) 

(rule (division ?x ?division)
      (job ?x (?division . ?job)))

(rule (meeting-time ?person ?day-and-time)
      (and (division ?person ?division)
           (or (meeting ?division ?day-and-time)
               (meeting whole-company ?day-and-time))))

;; c)

(meeting-time (Hacker Alyssa P) (Wednesday . ?time))


;; Exercise 4.60

;; It happens logically because if a lives near b, b lives near a. It happens
;; in terms of the query system because there is a correct state when ?a is
;; assigned the first person and ?b the second, and a correct state when ?a is
;; assigned the second person and ?b the first.

;; There isn't using the given rule. You would need to write a new rule that
;; imposes ordering, probably using some function through lisp-value. Then you
;; could write

(and (lives-near ?a ?b)
     (symbol< ?a ?b))


;; Exercise 4.61

(1 next-to (2 3) in (1 (2 3) 4))
((2 3) next-to 4 in (1 (2 3) 4))

(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))


;; Exercise 4.62

(rule (last-pair (?x) (?x)))

(rule (last-pair (?x . ?xs) ?y)
      (last-pair ?xs ?y))

;; Yes they work correctly. The last one has infinitely many answers though,
;; so causes an infinite loop.

;; Exercise 4.63

(rule (grandson ?x ?y)
      (and (son ?x ?z)
           (son ?z ?y)))

(rule (son ?husband ?child)
      (and (wife ?husband ?wife)
           (son ?wife ?child)))

;; ;;; Query input:
;; (grandson Cain ?x)

;; ;;; Query results:
;; (grandson cain irad)

;; ;;; Query input:
;; (son Lamech ?x)

;; ;;; Query results:
;; (son lamech jubal)
;; (son lamech jabal)

;; ;;; Query input:
;; (grandson Methushael ?x)

;; ;;; Query results:
;; (grandson methushael jubal)
;; (grandson methushael jabal)
