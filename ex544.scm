; Answers for 5-4-4

; Exercise 5.26

;; a) 10

;; b) 

;; 1 = 64
;; 2 = 99
;; 3 = 134
;; 4 = 169
;; 5 = 204

;; diff of 35 between each

;; so

;; 35n + 29


; Exercise 5.27

;; For the recursive factorial:

;; | n | pushes | depth |
;; |---+--------+-------|
;; | 1 |     16 |     8 |
;; | 2 |     48 |    13 |
;; | 3 |     80 |    18 |
;; | 4 |    112 |    23 |
;; | 5 |    144 |    28 |
;; | 6 |    176 |    33 |

;; So:

;; |           | Maximum depth | Number of pushes |
;; |-----------+---------------+------------------|
;; | Recursive | 5n + 3        | 32n - 16         |
;; | Iterative | 10            | 35n + 29         |


; Exercise 5.28

(define eceval-no-tail
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
  '(
;;SECTION 5.4.4
read-eval-print-loop
  (perform (op initialize-stack))
  (perform
   (op prompt-for-input) (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label eval-dispatch))
print-result
;;**following instruction optional -- if use it, need monitored stack
  (perform (op print-stack-statistics))
  (perform
   (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

unknown-expression-type
  (assign val (const unknown-expression-type-error))
  (goto (label signal-error))

unknown-procedure-type
  (restore continue)
  (assign val (const unknown-procedure-type-error))
  (goto (label signal-error))

signal-error
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

;;SECTION 5.4.1
eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))
ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))
ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure)
              (reg unev) (reg exp) (reg env))
  (goto (reg continue))

ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))
ev-appl-did-operator
  (restore unev)
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)
ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))
ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))
ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))
apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))  
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))

primitive-apply
  (assign val (op apply-primitive-procedure)
              (reg proc)
              (reg argl))
  (restore continue)
  (goto (reg continue))

compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment)
              (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))

;;;SECTION 5.4.2
ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))

ev-sequence
  (test (op no-more-exps?) (reg unev))
  (branch (label ev-sequence-end))
  (assign exp (op first-exp) (reg unev))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence-end
  (restore continue)
  (goto (reg continue))

;;;SECTION 5.4.3

ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label eval-dispatch))
ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))
ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))

ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch))
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op set-variable-value!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))

ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch))
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))
   )))

;; Iterative:
   
;; |  n | pushes | depth |
;; |----+--------+-------|
;; |  1 |     70 |    17 |
;; |  2 |    107 |    20 |
;; |  3 |    144 |    23 |
;; |  4 |    181 |    26 |
;; |  5 |    218 |    29 |
;; |  6 |    255 |    32 |
;; |  7 |    292 |    35 |
;; |  8 |    329 |    38 |
;; |  9 |    366 |    41 |
;; | 10 |    403 |    44 |

;; Recursive:

;; |  n | pushes | depth |
;; |----+--------+-------|
;; |  1 |     18 |    11 |
;; |  2 |     52 |    19 |
;; |  3 |     86 |    27 |
;; |  4 |    120 |    35 |
;; |  5 |    154 |    43 |
;; |  6 |    188 |    51 |
;; |  7 |    222 |    59 |
;; |  8 |    256 |    67 |
;; |  9 |    290 |    75 |
;; | 10 |    324 |    83 |

;; |           | pushes   | depth   |
;; |-----------+----------+---------|
;; | Iterative | 37n + 33 | 3n + 14 |
;; | Recursive | 34n - 16 | 8n + 3  |


; Exercise 5.29

;; |  n | pushes | depth |
;; |----+--------+-------|
;; |  1 |     16 |     8 |
;; |  2 |     72 |    13 |
;; |  3 |    128 |    18 |
;; |  4 |    240 |    23 |
;; |  5 |    408 |    28 |
;; |  6 |    688 |    33 |
;; |  7 |   1136 |    38 |
;; |  8 |   1864 |    43 |
;; |  9 |   3040 |    48 |
;; | 10 |   4944 |    53 |

;; a)

;; 5n + 3

;; | n | S(n) | S(n - 1) | S(n - 2) | S(n - 1) + S(n - 2) |
;; |---+------+----------+----------+---------------------|
;; | 3 |  128 |       72 |       16 |                  88 |
;; | 4 |  240 |      128 |       72 |                 200 |
;; | 5 |  408 |      240 |      128 |                 368 |
;; | 6 |  688 |      408 |      240 |                 648 |
;; | 7 | 1136 |      688 |      408 |                1096 |

;; S(n) = S(n - 1) + S(n - 2) + 40

;; (The reason being it has to do all the number of pushes required to
;; calculate fib for n - 1 and n - 2 and then do some constant work in adding
;; those values together, returning from the function, etc.)

;; (k = 40)

;; |  n | pushes | fib (n + 1) | pushes / fib (n + 1) |     diff |
;; |----+--------+-------------+----------------------+----------|
;; |  1 |     16 |           1 |                   16 |          |
;; |  2 |     72 |           2 |                   36 |       20 |
;; |  3 |    128 |           3 |            42.666667 | 6.666667 |
;; |  4 |    240 |           5 |                   48 | 5.333333 |
;; |  5 |    408 |           8 |                   51 |        3 |
;; |  6 |    688 |          13 |            52.923077 | 1.923077 |
;; |  7 |   1136 |          21 |            54.095238 | 1.172161 |
;; |  8 |   1864 |          34 |            54.823529 | 0.728291 |
;; |  9 |   3040 |          55 |            55.272727 | 0.449198 |
;; | 10 |   4944 |          89 |            55.550562 | 0.277835 |

;; #+TBLFM: $3='(fib (+ (string-to-number $1) 1))::$4=$2/$3::$5=$4 - @-1$-1

;; todo ...

;; TBH I have no idea how to express this in terms of fib(n + 1). Will have to
;; come back to it when I do.


; Exercise 5.30

; a)