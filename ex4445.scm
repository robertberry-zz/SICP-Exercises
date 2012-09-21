; Answers for 4-4-4-5

; Exercise 4.70

;; The second argument to cons-stream is delayed. When it is evaluated
;; THE-ASSERTIONS will no longer refer to its original value (hence why it is
;; stored in the let variable), but instead to the new form of
;; THE-ASSERTIONS. You would now have an infinite stream all composed of the
;; newly added rule.

