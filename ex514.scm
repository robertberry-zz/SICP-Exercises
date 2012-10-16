; Answers for 5-1-4

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
 (save n)
 (assign n (op -) (reg n) (const 1))
 (goto (label expt-loop))
 after-expt
 ;; multiply answer from previous expt
 ;; tbc
)
