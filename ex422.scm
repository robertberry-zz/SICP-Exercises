; Answers for 4-2-2

; Exercise 4.27

;; (define w (id (id 10)))

;; ;;; L-Eval input:
;; count
;; ;;; L-Eval value:
;; 0

; At this point count is 1 because when w is defined, id is evaluated with its
; delayed argument of (id 10). (id 10) is held in a thunk (not forced) so only
; the initial id is actually evaluated, updating the count to 1.

;; ;;; L-Eval input:
;; w
;; ;;; L-Eval value:
;; 10

; The REPL forces evaluation of the (id 10) thunk, so 10 is now shown.

;; ;;; L-Eval input:
;; count
;; ;;; L-Eval value:
;; 2

; Count is now 2 because id was called a second time to evaluate the thunk.


; Exercise 4.28

;; Whenever higher-order procedures are being used, e.g.

;; (define (map f xs)
;;   (if (null? xs)
;;       '()
;;       (cons (f (car xs))
;;             (map f (cdr xs)))))

;; (map (if some-parameter some-function some-other-function) seq)

;; Here when (f (car xs)) is evaluated, f will be a thunk. It needs to be
;; forced before it can be applied to any arguments.


; Example 4.29

;; (define (make-list-of x n)
;;   "Create a list of n x's."
;;   (if (= n 0)
;;       '()
;;       (cons x (make-list-of x (- n 1)))))

;; (define seq (make-list-of (computationally-expensive-function) some-large-number))

; then as cons would have produced thunks itself, evaluate the sequence by
; typing 'seq' into the REPL.

; computationally-expensive-function will be called n times, when it only
; needed to be called once.

;; ;;; L-Eval input:
;; (square (id 10))
;; ;;; L-Eval value:
;; 100

;; ;;; L-Eval input:
;; count
;; ;;; L-Eval value:
;; 2

; As x is evaluated twice within 'square', the thunk will be evaluated twice.


; Example 4.30

; a)

;; Ben is right because the procedure he has passed to for-each passes the
;; thunk of (car items) to 'display', which, being a primitive, forces its
;; argument.

;; In a more general sense, for-each works because the first statement in its
;; sequence is a function call which is being used for its side-effects rather
;; than its value. (Thus the fact its return value is never evaluated is not
;; really a problem by definition.)

; b)

;; (p1 1) will result in the same value as with the applicative-order
;; evaluator '(1 2). This is because the first statement is, again, being used
;; for its side-effect rather than its return value.

;; (p2 1), however, will return 1. This is because in the inner p procedure e
;; is never forced, so the thunk of (set! x (cons x '(2))) will never be
;; evaluated, and x is just returned in its initial state. In a general sense,
;; it's because the argument to the function is never itself forced.

;; With the proposed change both will return '(1 2).

; c)

;; Because forcing the return value from (proc (car items)) could never change
;; the behaviour of the program. This is itself due to the fact that the only
;; thunk being passed to the proc is '(car items), which itself has no side
;; effects. If proc returned x and it was forced, it would make no difference
;; to the behaviour of anything, as this value is then thrown away anyway.

; d)

;; Errr ... 

;; I don't like the idea of introducing side-effects into a language with lazy
;; evaluation - it seems difficult to reason about when the side effects will
;; actually occur. (Hence why all the lazily-evaluated languages I can think
;; of are functional.) I would not allow side-effects in a language like
;; this. That would also invalidate the need for sequences, as functions would
;; only ever be used for their return values.


; Exercise 4.31

(cd "lib")
(load "ch4-mceval.scm")

(define (actual-value exp env)
  (force-it (eval exp env)))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;; thunks

(define (delay-it exp env)
  (list 'thunk exp env false))

(define (delay-memoize-it exp env)
  (list 'thunk exp env true))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
(define (thunk-memoize? thunk) (cadddr thunk))

;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

;; memoizing version of force-it

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (if (thunk-memoize? obj)
               (begin
                 (set-car! obj 'evaluated-thunk)
                 (set-car! (cdr obj) result)
                 (set-cdr! (cdr obj) '())))
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'newline newline)
        (list 'display display)
;;      more primitives
        ))


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (let ((params (procedure-parameters procedure)))
           (eval-sequence
            (procedure-body procedure)
            (extend-environment
             (map parameter-name params)
             (arguments-for-apply (map parameter-type params) arguments env)
             (procedure-environment procedure)))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (parameter-name param)
  (if (list? param)
      (car param)
      param))

(define (parameter-type param)
  (cond ((not (list? param)) 'evaluated)
        ((eq? (cadr param) 'lazy) 'delayed)
        ((eq? (cadr param) 'lazy-memo) 'memoized)))

(define (arguments-for-apply parameter-types arguments env)
  "Given a list of parameter types and a list of arguments, either evaluates
the arguments in the given env or stores them in thunks with the given env as
appropriate."
  (map (lambda (type arg)
         ((case type
            ('evaluated eval)
            ('delayed delay-it)
            ('memoized delay-memoize-it)) arg env))
       parameter-types
       arguments))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;(define the-global-environment (setup-environment))
;;(driver-loop)


;; 1 ]=> (driver-loop)


;; ;;; L-Eval input:
;; (define (when condition (statement lazy-memo))
;;   (if condition statement false))

;; ;;; L-Eval value:
;; ok

;; ;;; L-Eval input:
;; (define x 2)

;; ;;; L-Eval value:
;; ok

;; ;;; L-Eval input:
;; (when false (set! x 3))

;; ;;; L-Eval value:
;; #f

;; ;;; L-Eval input:
;; x

;; ;;; L-Eval value:
;; 2

;; ;;; L-Eval input:
;; (when true (set! x 3))

;; ;;; L-Eval value:
;; ok

;; ;;; L-Eval input:
;; x

;; ;;; L-Eval value:
;; 3

;; ;;; L-Eval input:
