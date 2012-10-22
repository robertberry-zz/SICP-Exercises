; Answers for 5-2-0

; Exercise 5.7

(define (assert= x y)
  (if (= x y)
      true
      (error "Assertion error - values should be equal" x y)))

(define expt-machine
  (make-machine
   '(continue n b val)
   (list (list '= =)
         (list '* *)
         (list '- -))
   '(controller
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
     expt-done)))

(define (test-expt-machine)
  (set-register-contents! expt-machine 'b 2)
  (set-register-contents! expt-machine 'n 3)
  (start expt-machine)
  (assert= (get-register-contents expt-machine 'val)
           8))

(define expt-iter-machine
  (make-machine
   '(n b counter product)
   (list (list '= =)
         (list '* *)
         (list '- -))
   '(controller
     (assign counter (reg n))
     (assign product (const 1))
     expt-iter
     (test (op =) (reg counter) (const 0))
     (branch (label expt-done))
     (assign counter (op -) (reg counter) (const 1))
     (assign product (op *) (reg b) (reg product))
     (goto (label expt-iter))
     expt-done)))

(define (test-expt-iter-machine)
  (set-register-contents! expt-iter-machine 'b 2)
  (set-register-contents! expt-iter-machine 'n 3)
  (start expt-iter-machine)
  (assert= (get-register-contents expt-iter-machine 'product)
           8))

