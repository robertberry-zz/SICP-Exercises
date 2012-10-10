; Answers for 5-1-1

; Exercise 5.2

(controller
 (assign p (const 1))
 (assign c (const 1))
 test-c-n
 (test (op >) (reg c) (reg n))
 (branch (label factorial-done))
 (assign p (op *) (reg c) (reg p))
 (assign c (op +) (reg c) (const 1))
 (goto (label test-c-n))
 factorial-done)
