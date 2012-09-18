; Answers for 4-4-3

;; Exercise 4.64

;; Because the first clause of the and recursively calls outranked-by without
;; setting ?middle-manager. This in turn attempts to apply the rule to
;; everyone in the system including the original staff-person?, which will in
;; turn recursively call this rule, etc.

;; Previously this did not happen because the (supervisor ?staff-person
;; ?middle-manager?) rule first limited the frames to check only whether
;; supervisors of ?staff-person were outranked-by ?boss.


;; Exercise 4.65

;; Because the body of the rule matches for Warbucks Oliver four times.

;; To be more specific, Warbucks Oliver supervises four supervisors.


;; Exercise 4.66

;; Some queries will allow the same result to be returned twice, e.g.

(sum ?amount
     (and (wheel ?x)
          (salary ?x ?amount)))

;; This will cause the sum to be incorrect.

;; He could have his accumulation function first filter the stream to
;; eliminate duplicates. (This should be OK to do as if you are performing
;; accumulation operations the stream ought to be finite.)


;; Exercise 4.67

;; Any time a pattern or rule is evaluated with a given frame, the pattern and
;; the frame should be pushed onto the history stack. Also the evaluator needs
;; to check back through the history stack to make sure there is no
;; pattern-frame combination that matches the current evaluation. Once a given
;; pattern (with frame) is resolved, it needs to be popped from the stack.


;; Exercise 4.68

(rule (reverse () ()))

(rule (reverse (?x . ?xs) ?ys)
      (and (reverse ?xs ?reverse-xs)
           (append-to-form ?reverse-xs (?x) ?ys)))

;; Causes an infinite loop if x is variable. If you switch the clauses around
;; it does the same if the y is variable instead. Not sure if it's possible to
;; implement this without one causing infinite loops.


;; Exercise 4.69

(rule (grandson-list (grandson)))

(rule (grandson-list (great . ?rest))
      (grandson-list ?rest))

(rule ((grandson) ?x ?y)
      (and (son ?x ?z)
           (son ?z ?y)))

(rule ((great . ?rel) ?x ?y)
      (and (son ?z ?y)
           (?rel ?x ?z)
           (grandson-list ?rel)))

; works for both - this is because there is a limited number of son
; relationships that can be generated from the database, and these are checked
; before looking at ?rel more closely.