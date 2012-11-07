; Answers for 5-2-3

; Exercise 5.9

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (label-exp? e)
                    (error "Machine operations may not be used on labels" exp)
                    (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

;; for testing

(define bad-machine
  (make-machine
   '(r)
   (list (list '= =))
   '(controller
     (test (op =) (reg r) (label done))
     done)))

;; can be defined before. now if you try to define, throws error:

;; 1 ]=> (define bad-machine
;;   (make-machine
;;    '(r)
;;    (list (list '= =))
;;    '(controller
;;      (test (op =) (reg r) (label done))
;;      done)))

;; ;Machine operations may not be used on labels ((op =) (reg r) (label done))

; Exercise 5.10

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'move)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'compare)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'if-true)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'jump)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'push)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'pop)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'do)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

; borrowed from Haskell

(define (init seq)
  "Sequence without its last element."
  (if (null? seq)
      (error "Sequence is empty.")
      (let ((front (car seq))
            (rest (cdr seq)))
        (if (null? rest)
            '()
            (cons front (init rest))))))

(define (last seq)
  "Last element of sequence."
  (if (null? seq)
      (error "Sequence is empty.")
      (let ((front (car seq))
            (rest (cdr seq)))
        (if (null? rest)
            front
            (last rest)))))
            
(define (assign-reg-name assign-instruction)
  (last (cdr assign-instruction)))

(define (assign-value-exp assign-instruction)
  (init (cdr assign-instruction)))

(define expt-machine
  (make-machine
   '(continue n b val)
   (list (list '= =)
         (list '* *)
         (list '- -))
   '(controller
     (move (label expt-done) continue)
     expt-loop
     (compare (op =) (reg n) (const 0))
     (if-true (label base-case))
     ;; set up to compute expt for n - 1
     (push continue)
     (move (label after-expt) continue)
     (move (op -) (reg n) (const 1) n)
     (jump (label expt-loop))
     after-expt
     ;; multiply answer from previous expt
     (pop continue)
     (move (op *) (reg val) (reg b) val)
     (jump (reg continue))
     base-case
     (move (const 1) val)
     (jump (reg continue))
     expt-done)))

(define (test-expt-machine)
  (set-register-contents! expt-machine 'b 2)
  (set-register-contents! expt-machine 'n 3)
  (start expt-machine)
  (= (get-register-contents expt-machine 'val)
     8))

; Exercise 5.11

; a)

(controller
 (assign continue (label fib-done))
 fib-loop
 (test (op <) (reg n) (const 2))
 (branch (label immediate-answer))
 (save continue)
 (assign continue (label afterfib-n-1))
 (save n)                           
 (assign n (op -) (reg n) (const 1))
 (goto (label fib-loop))            
 afterfib-n-1                       
 (restore n)
 (assign n (op -) (reg n) (const 2))
 (assign continue (label afterfib-n-2))
 (save val)                  
 (goto (label fib-loop))
 afterfib-n-2                
; (assign n (reg val))        <- no longer swap n and val
; (restore val)                  then restore old version of val.
 (restore n)              ; <- instead just load old version of val into n
 (restore continue)
 (assign val                 
         (op +) (reg val) (reg n))
 (goto (reg continue))       
 immediate-answer
 (assign val (reg n))        
 (goto (reg continue))
 fib-done)

; run through for fib 3 to make sure it works

;; n := 3

;; continue := fib-done
;; fib-done :: []
;; continue := afterfib-n-1
;; 3 :: fib-done :: []
;; n := 2
;; afterfib-n-1 :: 3 :: fib-done :: []
;; continue := afterfib-n-1
;; 2 :: afterfib-n-1 :: 3 :: fib-done :: []
;; n := 1
;; val := 1
;; n := 2 (afterfib-n-1 :: 3 :: fib-done :: [])
;; n := 0
;; continue := afterfib-n-2
;; 1 :: afterfib-n-1 :: 3 :: fib-done :: []
;; val := 0
;; n := 1 (afterfib-n-1 :: 3 :: fib-done :: [])
;; continue := after-fib-n-1 (3 :: fib-done :: [])
;; val := 1
;; n := 3 (fib-done :: [])
;; n := 1
;; continue := after-fib-n-2
;; 1 :: fib-done :: []
;; val := 1
;; n := 1 (fib-done :: [])
;; continue := fib-done ([])
;; val := 2

;; Now a programmatic test:

(define simple-fib
  (make-machine
   '(continue n val)
   (list (list '< <)
         (list '- -)
         (list '+ +))
   '(controller
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)                           
     (assign n (op -) (reg n) (const 1))
     (goto (label fib-loop))            
     afterfib-n-1                       
     (restore n)
     (assign n (op -) (reg n) (const 2))
     (assign continue (label afterfib-n-2))
     (save val)                  
     (goto (label fib-loop))
     afterfib-n-2                
     (restore n)
     (restore continue)
     (assign val                 
             (op +) (reg val) (reg n))
     (goto (reg continue))       
     immediate-answer
     (assign val (reg n))        
     (goto (reg continue))
     fib-done)))

(define (test-simple-fib)
  (set-register-contents! simple-fib 'n 5)
  (start simple-fib)
  (= (get-register-contents simple-fib 'val)
     5))

; b)

(define (make-register-snapshot register reg-name)
  (list register (get-contents register) reg-name))

(define (snapshot-register snapshot)
  (car snapshot))

(define (snapshot-value snapshot)
  (cadr snapshot))

(define (snapshot-register-name snapshot)
  (caddr snapshot))

(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine
                           reg-name)))
    (lambda ()
      (push stack (make-register-snapshot reg reg-name))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (lambda ()
      (let ((snapshot (pop stack)))
        (if (eq? (snapshot-register snapshot) reg)
            (begin
              (set-contents! reg (snapshot-value snapshot))
              (advance-pc pc))
            (error "Attempting to restore value from another register"
                   reg-name
                   (snapshot-register-name snapshot)))))))

;; 1 ]=> (test-simple-fib)

;; ;Attempting to restore value from another register n val

; c)

;; Many of the below procedures are edited to stop passing the stack around (as there
;; are now several stacks).

(define (chain-calls f g)
  "Given function f and function g, returns a new function that invokes f and
then g."
  (lambda ()
    (f)
    (g)))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (the-instruction-sequence '()))
    (let* ((initialize-stacks
            (lambda ()
              'stacks-allocated))
           (the-ops
            (list (list 'initialize-stacks (lambda ()
                                             (initialize-stacks)))))
           (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (let ((stack (make-stack)))
              (set! register-table
                    (cons (list name (make-register name) stack)
                          register-table))
              (set! initialize-stacks
                    (chain-calls (lambda ()
                                   (stack 'initialize)) initialize-stacks))
              'register-allocated)))
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (lookup-stack name)
        (let ((val (assoc name register-table)))
          (if val
              (let ((stack (caddr val)))
                (if (null? stack)
                    (error "No stack for register:" name)
                    stack))
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'get-stack) lookup-stack)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (get-stack machine register)
  "The stack for the given register from the given machine."
  ((machine 'get-stack) register))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag ops)))
     insts)))

(define (make-execution-procedure inst labels machine
                                  pc flag ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(define (make-save inst machine pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name))
         (stack (get-stack machine reg-name)))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name))
         (stack (get-stack machine reg-name)))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))


; Exercise 5.12

(define (comparing f before?)
  "Given a key function and a function for determining whether an element comes
before another, returns a comparison function for comparing two objects
via that key."
  (lambda (n m)
    (before? (f n) (f m))))

(define (instruction-type instruction)
  "Symbol denoting the type of the instruction (e.g. 'goto)."
  (car instruction))

(define (break p seq)
  "Applied to predicate p and seq, returns tuple where first element is longest
prefix of seq for which all elements do not satisfy p and second element is the
remainder of the list."
  (define (iter seq acc)
    (cond ((null? seq)
           (list (reverse acc) '()))
          ((p (car seq))
           (list (reverse acc) seq))
          (else
           (iter (cdr seq) (cons (car seq) acc)))))
  (iter seq '()))

(define (remove-duplicates seq)
  "Seq without consecutive duplicates."
  (define (iter seq acc)
    (if (null? seq)
        (reverse acc)
        (let ((first (car seq))
              (rest (cdr seq)))
          (if (and (not (null? acc))
                   (equal? first (car acc)))
              (iter rest acc)
              (iter rest (cons first acc))))))
  (iter seq '()))

(define (partition seq key before?)
  "Partition seq into a list of tuples, where the first element is the key for
that subgroup and the second element is a list of all the elements of seq
belonging to that subgroup."
  (let ((seq (sort seq (comparing key before?))))
    (if (null? seq)
        '()
        (let* ((x (car seq))
               (fx (key x))
               (break-tuple (break (lambda (y)
                                     (not (equal? fx (key y)))) seq))
               (x-group (car break-tuple))
               (rest (cadr break-tuple)))
          (cons (list fx x-group)
                (partition rest key before?))))))

(define (without-labels instructions)
  "Instructions list without labels."
  (filter list? instructions))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (sorted-instructions '())
        (entry-point-registers '())
        (stack-linked-registers '())
        (register-sources '())
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (instructions-for type)
        (let ((instructions (assoc type sorted-instructions)))
          (cadr instructions)))
      (define (analyze-instruction-sequence instructions)
        (define (register-list registers)
          (remove-duplicates (sort registers symbol<?)))
        (define (analyze-gotos)
          (define (collect-registers gotos acc)
            (if (null? gotos)
                (register-list acc)
                (let ((first (car gotos))
                      (rest (cdr gotos)))
                  (let ((dest (goto-dest first)))
                    (if (register-exp? dest)
                        (collect-registers rest (cons (register-exp-reg dest) acc))
                        (collect-registers rest acc))))))
          (set! entry-point-registers
                (collect-registers (instructions-for 'goto)
                                   '())))
        (define (analyze-stack-ops)
          (set! stack-linked-registers
                (register-list
                 (map stack-inst-reg-name
                      (concatenate (list (instructions-for 'save)
                                         (instructions-for 'restore)))))))
        (define (analyze-register-sources)
          (set! register-sources
                (map (lambda (group)
                       (cons (car group)
                             (map assign-value-exp (cadr group))))
                     (partition (instructions-for 'assign)
                                assign-reg-name
                                symbol<?))))
        (set! sorted-instructions
              (partition (remove-duplicates
                          (without-labels instructions))
                         instruction-type
                         symbol<?))
        (analyze-gotos)
        (analyze-stack-ops)
        (analyze-register-sources))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'analyze-instruction-sequence)
               analyze-instruction-sequence)
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'instructions) sorted-instructions)
              ((eq? message 'entry-point-registers) entry-point-registers)
              ((eq? message 'stack-registers) stack-linked-registers)
              ((eq? message 'register-sources) register-sources)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'analyze-instruction-sequence)
     controller-text)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define fibonacci-machine
  (make-machine
   '(n val continue)
   (list (list '< <)
         (list '- -)
         (list '+ +))
   '(controller
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute _Fib_(n - 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)                           ; save old value of `n'
     (assign n (op -) (reg n) (const 1)); clobber `n' to n - 1
     (goto (label fib-loop))            ; perform recursive call
     afterfib-n-1                         ; upon return, `val' contains _Fib_(n - 1)
     (restore n)
     (restore continue)
     ;; set up to compute _Fib_(n - 2)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)                         ; save _Fib_(n - 1)
     (goto (label fib-loop))
     afterfib-n-2                         ; upon return, `val' contains _Fib_(n - 2)
     (assign n (reg val))               ; `n' now contains _Fib_(n - 2)
     (restore val)                      ; `val' now contains _Fib_(n - 1)
     (restore continue)
     (assign val                        ;  _Fib_(n - 1) +  _Fib_(n - 2)
             (op +) (reg val) (reg n))
     (goto (reg continue))              ; return to caller, answer is in `val'
     immediate-answer
     (assign val (reg n))               ; base case:  _Fib_(n) = n
     (goto (reg continue))
     fib-done)))

; Exercise 5.13

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (register-exists? name)
        (assoc name register-table))
      (define (lookup-register name)
        (begin
          (if (not (register-exists? name))
              (allocate-register name))
          (let ((val (assoc name register-table)))
            (cadr val))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define fibonacci-machine
  (make-machine
   (list (list '< <)
         (list '- -)
         (list '+ +))
   '(controller
     (assign continue (label fib-done))
     fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute _Fib_(n - 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)                           ; save old value of `n'
     (assign n (op -) (reg n) (const 1)); clobber `n' to n - 1
     (goto (label fib-loop))            ; perform recursive call
     afterfib-n-1                         ; upon return, `val' contains _Fib_(n - 1)
     (restore n)
     (restore continue)
     ;; set up to compute _Fib_(n - 2)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)                         ; save _Fib_(n - 1)
     (goto (label fib-loop))
     afterfib-n-2                         ; upon return, `val' contains _Fib_(n - 2)
     (assign n (reg val))               ; `n' now contains _Fib_(n - 2)
     (restore val)                      ; `val' now contains _Fib_(n - 1)
     (restore continue)
     (assign val                        ;  _Fib_(n - 1) +  _Fib_(n - 2)
             (op +) (reg val) (reg n))
     (goto (reg continue))              ; return to caller, answer is in `val'
     immediate-answer
     (assign val (reg n))               ; base case:  _Fib_(n) = n
     (goto (reg continue))
     fib-done)))
