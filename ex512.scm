; Answers for 5-1-2

; See ex512_square_root.svg

; Exercise 5.3

; Simplified model

(controller
 (assign g (const 1))
 test-good-enough
 (test (op good-enough?) (reg x) (reg g))
 (branch (label square-root-done))
 (assign g (op improve) (reg x) (reg g))
 (goto (label test-good-enough))
 square-root-done)

; Full model

(controller
 (assign g (const 1))
 test-good-enough
 (assign s (op square) (reg g))
 (assign d (op -) (reg s) (reg x))
 (assign d (abs d))
 (test (op <) (reg d) (const 0.001))
 (branch (label square-root-done))
 improve-guess
 (assign h (op /) (reg x) (reg g))
 (assign g (op average) (reg g) (reg h))
 (goto (label test-good-enough))
 square-root-done)

;; Could be expanded further, if you wanted to be more specific about square and average:

(controller
 (assign g (const 1))
 test-good-enough
 (assign s (op *) (reg g) (reg g))
 (assign d (op -) (reg s) (reg x))
 (assign d (abs d))
 (test (op <) (reg d) (const 0.001))
 (branch (label square-root-done))
 improve-guess
 (assign h (op /) (reg x) (reg g))
 (assign t (op +) (reg g) (reg h))
 (assign g (op /) (reg t) (const 2))
 (goto (label test-good-enough))
 square-root-done)

