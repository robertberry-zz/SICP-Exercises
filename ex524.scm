; Answers for 5-2-4

; Exercise 5.14

(define factorial-machine
  (make-machine
   '(continue n val)
   (list (list '= =)
         (list '- -)
         (list '* *)
         (list 'print display)
         (list 'read (lambda ()
                       (newline)
                       (read))))
   '(controller
     fact-machine-loop
     (assign n (op read))
     (assign continue (label fact-done))     ; set up final return address
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;; Set up for the recursive call by saving `n' and `continue'.
     ;; Set up `continue' so that the computation will continue
     ;; at `after-fact' when the subroutine returns.
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))   ; `val' now contains n(n - 1)!
     (goto (reg continue))                   ; return to caller
     base-case
     (assign val (const 1))                  ; base case: 1! = 1
     (goto (reg continue))                   ; return to caller
     fact-done
     (perform (op print) (reg val))
     (perform (op print-stack-statistics))
     (goto (label fact-machine-loop))
     )))

;; 2
;; 2
;; (total-pushes = 2 maximum-depth = 2)
;; 3
;; 6
;; (total-pushes = 6 maximum-depth = 4)
;; 4
;; 24
;; (total-pushes = 12 maximum-depth = 6)
;; 5
;; 120
;; (total-pushes = 20 maximum-depth = 8)
;; 6
;; 720
;; (total-pushes = 30 maximum-depth = 10)
;; 7
;; 5040
;; (total-pushes = 42 maximum-depth = 12)
;; 8
;; 40320
;; (total-pushes = 56 maximum-depth = 14)
;; 9
;; 362880
;; (total-pushes = 72 maximum-depth = 16)
;; 10
;; 3628800
;; (total-pushes = 90 maximum-depth = 18)


;; n                      2     3    4    5
;; n^2                    4     9    16   25
;; total-pushes           2     6    12   20
;; maximum-depth          2     4    6    8


;; total-pushes = n^2 - n
;; maximum-depth = 2n - 2


; Exercise 5.15

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (instruction-count 0)
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'reset-instruction-count
                       (lambda ()
                         ; -1 so that the increment for this instruction brings
                         ; it back to 0
                         (set! instruction-count -1)))
                 (list 'print-instruction-count
                       (lambda ()
                         (newline)
                         (display instruction-count)))
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
                (set! instruction-count (+ instruction-count 1))
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

(define factorial-machine
  (make-machine
   '(continue n val)
   (list (list '= =)
         (list '- -)
         (list '* *)
         (list 'print display)
         (list 'read (lambda ()
                       (newline)
                       (read))))
   '(controller
     fact-machine-loop
     (perform (op reset-instruction-count))
     (assign n (op read))
     (assign continue (label fact-done))     ; set up final return address
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;; Set up for the recursive call by saving `n' and `continue'.
     ;; Set up `continue' so that the computation will continue
     ;; at `after-fact' when the subroutine returns.
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))   ; `val' now contains n(n - 1)!
     (goto (reg continue))                   ; return to caller
     base-case
     (assign val (const 1))                  ; base case: 1! = 1
     (goto (reg continue))                   ; return to caller
     fact-done
     (perform (op print) (reg val))
     (perform (op print-instruction-count))
     (goto (label fact-machine-loop))
     )))


; Exercise 5.16

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (trace false)
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
              (let ((inst (car insts)))
                (if trace
                    (begin
                      (newline)
                      (display (instruction-text inst))))
                ((instruction-execution-proc inst))
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
              ((eq? message 'trace-on)
               (set! trace true))
              ((eq? message 'trace-off)
               (set! trace false))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (activate-trace machine)
  (machine 'trace-on))

(define (deactivate-trace machine)
  (machine 'trace-off))


; Exercise 5.17

;; Going to store the label with the instruction entry now

(define (make-instruction text label)
  (list text label '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-label inst)
  (cadr inst))

(define (set-cadr! x y)
  (set-car! (cdr x) y))

(define (set-instruction-label! inst text)
  (set-cadr! inst text))

(define (instruction-execution-proc inst)
  (caddr inst))

(define (set-caddr! x y)
  (set-car! (cddr x) y))

(define (set-instruction-execution-proc! inst proc)
  (set-caddr! inst proc))

;; Need to change extract-labels so it adds this information when creating the
;; instruction entries

(define (label-name label)
  (car label))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               ; extract-labels works backwards, so we can modify the last
               ; instruction so as to contain the label information
               (begin
                 (if (not (null? insts))
                     (set-instruction-label! (car insts) next-inst))
                 (receive 
                     insts
                     (cons (make-label-entry next-inst
                                             insts)
                           labels)))
               (receive (cons (make-instruction next-inst false)
                              insts)
                   labels)))))))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (instruction-count 0)
        (trace false)
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
              (let ((inst (car insts)))
                (if trace
                    (begin
                      (newline)
                      (let ((label (instruction-label inst)))
                        (if label
                            (begin
                              (display label)
                              (newline)))
                        (display (instruction-text inst)))))
                ((instruction-execution-proc inst))
                (set! instruction-count (+ instruction-count 1))
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
              ((eq? message 'trace-on)
               (set! trace true))
              ((eq? message 'trace-off)
               (set! trace false))
              ((eq? message 'instruction-count)
               instruction-count)
              ((eq? message 'reset-instruction-count)
               (set! instruction-count 0))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))


; Exercise 5.18

(define (make-register name)
  (let ((trace false)
        (contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'trace-on) (set! trace true))
            ((eq? message 'trace-off) (set! trace false))
            ((eq? message 'set)
             (lambda (value)
               (if trace
                   (begin
                     (newline)
                     (display name)
                     (display ": ")
                     (display contents)
                     (display " -> ")
                     (display value)))
               (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (activate-register-trace machine register)
  ((get-register machine register) 'trace-on))

(define (deactivate-register-trace machine register)
  ((get-register machine register) 'trace-off))


; Exercise 5.19

(define (make-breakpoint label offset)
  (cons label offset))

(define (breakpoint-label breakpoint)
  (car breakpoint))

(define (breakpoint-offset breakpoint)
  (cdr breakpoint))

;; Going to store the breakpoint with the instruction now

(define (make-instruction text label)
  (list text label '() '()))

(define (set-cadddr! x y)
  (set-car! (cdddr x) y))

(define (set-instruction-breakpoint! inst breakpoint)
  (set-cadddr! inst breakpoint))

(define (instruction-breakpoint inst)
  (cadddr inst))

(define (without list elem)
  "List without elem."
  (if (null? list)
      '()
      (if (equal? (car list) elem)
          (without (cdr list) elem)
          (cons (car list) (without (cdr list) elem)))))

(define (nth n seq)
  "nth element of sequence (0-indexed)"
  (define (iter i xs)
    (if (null? xs)
        (error "nth applied to too small a list" n seq)
        (if (= i 0)
            (car xs)
            (iter (- i 1) (cdr xs)))))
  (if (< n 0)
      (error "nth only accepts positive integers" n)
      (iter n seq)))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (instruction-count 0)
        (trace false)
        (breakpoints '())
        (label-index '())
        (resuming false)
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
      (define (set-label-index! labels)
        (set! label-index labels))
      (define (instruction-from-label label offset)
        (nth offset (lookup-label label-index label)))
      (define (breakpoint-instruction breakpoint)
        (instruction-from-label
         (breakpoint-label breakpoint)
         (breakpoint-offset breakpoint)))
      (define (delete-breakpoint! label n)
        (let ((breakpoint (make-breakpoint label n)))
          (let ((instruction (breakpoint-instruction breakpoint)))
            (set-instruction-breakpoint! instruction '())
            (set! breakpoints (without breakpoints breakpoint)))))
      (define (add-breakpoint! label n)
        (let ((breakpoint (make-breakpoint label n)))
          (let ((instruction (breakpoint-instruction breakpoint)))
            (set-instruction-breakpoint! instruction breakpoint)
            ; add to the breakpoints index
            (set! breakpoints (cons breakpoint breakpoints)))))
      (define (reset-breakpoints!)
        (map (lambda (breakpoint)
               (delete-breakpoint! (breakpoint-label breakpoint)
                                   (breakpoint-offset breakpoint)))
             breakpoints))
      (define (print-trace inst)
        (newline)
        (let ((label (instruction-label inst)))
          (if label
              (begin
                (display label)
                (newline)))
          (display (instruction-text inst))))
      (define (print-breakpoint breakpoint)
        (newline)
        (display "Stopping at breakpoint: ")
        (display (breakpoint-label breakpoint))
        (display " ")
        (display (breakpoint-offset breakpoint)))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (let* ((inst (car insts))
                     (breakpoint (instruction-breakpoint inst)))
                (define (execute-instruction)
                  (if trace (print-trace inst))
                  ((instruction-execution-proc inst))
                  (set! instruction-count (+ instruction-count 1))
                  (execute))
                (if (not (null? breakpoint))
                    (if resuming
                        (begin
                          (set! resuming false)
                          (execute-instruction))
                        (print-breakpoint breakpoint))
                    (execute-instruction))))))
      (define (resume)
        (set! resuming true)
        (execute))
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
              ((eq? message 'trace-on)
               (set! trace true))
              ((eq? message 'trace-off)
               (set! trace false))
              ((eq? message 'instruction-count)
               instruction-count)
              ((eq? message 'set-label-index!)
               set-label-index!)
              ((eq? message 'set-breakpoint)
               add-breakpoint!)
              ((eq? message 'cancel-breakpoint)
               delete-breakpoint!)
              ((eq? message 'reset-breakpoints)
               (reset-breakpoints!))
              ((eq? message 'resume)
               (resume))
              ((eq? message 'reset-instruction-count)
               (set! instruction-count 0))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (set-label-index! machine labels)
  ((machine 'set-label-index!) labels))

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    (set-label-index! machine labels)
                    insts)))

(define (set-breakpoint machine label offset)
  ((machine 'set-breakpoint) label offset))

(define (cancel-breakpoint machine label offset)
  ((machine 'set-breakpoint) label offset))

(define (cancel-all-breakpoints machine)
  (machine 'reset-breakpoints))

(define (proceed-machine machine)
  (machine 'resume))
