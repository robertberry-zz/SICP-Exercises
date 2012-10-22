; Answers for 5-1-4

; See the graphviz visualisations to go with these (ex5_exponent.dot, .ps,
; ex5_exponent_iter.dot, .ps)

; Exercise 5.4

; a)

(controller
 (assign continue (label expt-done))
 expt-loop
 (test (op =) (reg n) (const 0))
 (branch (label base-case))
 ;; set up to compute expt for n - 1
 (save continue)
 (assign continue (label after-expt))
 (assign n (op -) (reg n) (const 1))
 (goto (label expt-loop))
 after-expt
 ;; multiply answer from previous expt
 (restore continue)
 (assign val
         (op *) (reg val) (reg b))
 (goto (reg continue))
 base-case
 (assign val (const 1))
 (goto (reg continue))
expt-done)

;; To check this works, I did a mental run through:

;; b = 2
;; n = 3

;; continue = expt-done
;; push: [expt-done]
;; continue = after-expt
;; n = 2
;; push: [expt-done : after-expt]
;; continue = after-expt
;; n = 1
;; push: [expt-done : after-expt : after-expt]
;; continue = after-expt
;; n = 0
;; val = 1
;; pop: [expt-done : after-expt]
;; continue = after-expt
;; val = 2
;; pop: [expt-done]
;; continue = after-expt
;; val = 4
;; pop: []
;; continue = expt-done
;; val = 8

;; Looks correct!

; b)

;; Note: I may have incorrectly simplified this machine (is this
;; tail-recursive optimisation?). The other method would be to pointlessly
;; save the after-expt and expt-done labels to the stack. after-expt itself
;; would simply pop another label from the stack and continue until you
;; reached expt-done.

(controller
 (assign product (const 1))
 expt-iter
 (test (op =) (reg n) (const 0))
 (branch (label expt-done))
 (assign counter (op -) (reg counter) (const 1))
 (assign product (op *) (reg b) (reg product))
 (goto (label expt-iter))
expt-done)


; Exercise 5.5

;; Factorial:

;; n = 3

;; continue = fact-done
;; push: [fact-done]
;; push: [fact-done : 3]
;; n = 2
;; continue = after-fact
;; push: [fact-done : 3 : after-fact]
;; push: [fact-done : 3 : after-fact : 2]
;; n = 1
;; continue = after-fact
;; val = 1
;; pop: [fact-done : 3 : after-fact]
;; n = 2
;; pop: [fact-done : 3]
;; continue = after-fact
;; val = 2
;; pop: [fact-done]
;; n = 3
;; pop: []
;; continue = fact-done
;; val = 6

;; Fibonacci:

;; n = 2

;; continue = fib-done
;; push: [fib-done]
;; continue = afterfib-n-1
;; push: [fib-done : 2]
;; n = 1
;; val = 1
;; pop: [fib-done]
;; n = 2
;; pop: []
;; continue = fib-done
;; n = 0
;; push: [fib-done]
;; continue = afterfib-n-2
;; push: [fib-done : 1]
;; val = 0
;; n = 0
;; pop: [fib-done]
;; val = 1
;; pop: []
;; continue = fib-done
;; val = 1


; Exercise 5.6

;; Within after-fib-n-1 - continue is popped from and pushed to the stack
;; without its being used in between.
