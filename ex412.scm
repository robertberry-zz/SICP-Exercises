; Answers for 4-1-2

(load "ex411.scm")

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no `else' clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

; Exercise 4.2

; a) The procedure application clause is a kind of 'catch all', it just
;    interprets any list as the application of a procedure, so (define x y)
;    will attempt to look up a 'define' procedure to apply to x and y, which
;    will signal an error.

; b) Wat

; (define (application? exp) (tagged-list? exp 'call))
; (define (operator exp) (cadr exp))
; (define (operands exp) (cddr exp))


; Exercise 4.3

(load "generics.scm")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else (let ((proc (get 'eval (car exp))))
                (display proc)
                (cond (proc (proc exp env))
                      ((application? exp) (apply (eval (operator exp) env)
                                                 (list-of-values (operands exp) env)))
                      (else
                       (error "Unknown expression type -- EVAL" exp)))))))

(put 'eval 'quote (lambda (exp env)
                    (text-of-quotation exp)))

(put 'eval 'define eval-definition)
(put 'eval 'set! eval-assignment)
(put 'eval 'if eval-if)

(put 'eval 'lambda (lambda (exp env)
                     (make-procedure (lambda-parameters exp)
                                     (lambda-body exp)
                                     env)))

(put 'eval 'begin (lambda (exp env)
                    (eval-sequence (begin-actions exp) env)))

(put 'eval 'cond (lambda (exp env)
                   (eval (cond->if exp) env)))

; Exercise 4.4

; a)

(define (and-expressions exp)
  (cdr exp))

(define (first-expression exps)
  (car exps))

(define (rest-expressions exps)
  (cdr exps))

(define (true? x)
  (not (eq? x 'false)))

(define (false? x)
  (eq? x 'false))

(define (eval-and exp env)
  (define (iter exprs)
    (let ((first (eval (first-expression exprs) env))
          (rest (rest-expressions exprs)))
      (if (false? first)
          false
          (if (null? rest)
              first
              (iter rest)))))
  (iter (and-expressions exp)))

(define (or-expressions exp)
  (cdr exp))

(define (eval-or exp env)
  (define (iter exprs)
    (let ((first (eval (first-expression exprs env)))
          (rest (rest-expressions exprs)))
      (if (true? first)
          first
          (if (null? rest)
              false
              (iter rest)))))
  (iter (or-expressions exp)))

(put 'eval 'and eval-and)
(put 'eval 'or eval-or)

; b)

(define (and->if exp)
  (expand-and-expressions (and-expressions exp)))

(define (expand-and-expressions expressions)
  (let ((first (first-expression expressions))
        (rest (rest-expressions expressions)))
    (if (null? rest)
        first
        (make-if first
                 (expand-and-expressions rest)
                 'false))))

;; 1 ]=> (expand-and-expressions '(a b c))

;; ;Value 16: (if a (if b c false) false)

(define (or->if exp)
  (expand-or-expressions (or-expressions exp)))

; or is much trickier to implement, as it needs to save the result of the if
; conditional to return, which means storing it in a variable. You do not,
; however, want to shadow any variables already set in the environment. I am
; using 'generate-unique-symbol' for this. The implementation would have to
; prevent the user from creating symbols whose name begin with gensym- to
; guarantee that these symbols are unique.

(define next-unique-symbol 1)

(define (generate-unique-symbol)
  (let ((sym (symbol 'gensym- next-unique-symbol)))
    (set! next-unique-symbol (+ 1 next-unique-symbol))
    sym))

(define (make-application operator operands)
  (cons operator operands))

; Creates a lambda expression that is the same as using 'let' on one variable
(define (make-let-lambda var-name expr body)
  (make-application (make-lambda (list var-name) (list body)) (list expr)))

; e.g.

;; 1 ]=> (make-let-lambda 'bob '(+ 4 5) '(print bob))

;; ;Value 13: ((lambda (bob) (print bob)) (+ 4 5))

(define (expand-or-expressions expressions)
  (let ((first (first-expression expressions))
        (rest (rest-expressions expressions)))
    (if (null? rest)
        first
        (let ((val-expr (generate-unique-symbol)))
          (make-let-lambda val-expr
                           first
                           (make-if val-expr
                                    val-expr
                                    (expand-or-expressions rest)))))))

;; 1 ]=> (expand-or-expressions '(a b c))

;; Value 17: ((lambda (gensym-1) (if gensym-1 gensym-1 ((lambda (gensym-2)
;;           (if gensym-2 gensym-2 c)) b))) a)


; Exercise 4.5

(define (recipient-action? action)
  (eq? (car action) '=>))

(define (recipient-proc action)
  (cadr action))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no `else' clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (let ((actions (cond-actions first)))
              (if (recipient-action? actions)
                  (let ((val-expr (generate-unique-symbol)))
                    (make-let-lambda val-expr
                                     (cond-predicate first)
                                     (make-if val-expr
                                              (make-application (recipient-proc actions)
                                                                (list val-expr))
                                              (expand-clauses rest))))
                  (make-if (cond-predicate first)
                           (sequence->exp (cond-actions first))
                           (expand-clauses rest))))))))

;; 1 ]=> (expand-clauses '(((assoc 'b '((a 1) (b 1))) => cadr) (else false)))

;; ;Value 21: ((lambda (gensym-2) (if gensym-2 (cadr gensym-2) false))
;;  (assoc (quote b) (quote ((a 1) (b 1)))))


; Exercise 4.6

(define (let-clauses exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))

(define (let->combination exp)
  (expand-let (let-clauses exp) (let-body exp)))

(define (let-clause-variable clause)
  (car clause))

(define (let-clause-expression clause)
  (cadr clause))

(define (expand-let clauses body)
  (let ((vars (map let-clause-variable clauses))
        (exprs (map let-clause-expression clauses)))
    (make-application (make-lambda vars body)
                      exprs)))

;; 1 ]=> (let->combination '(let ((x 1) (y (+ 2 3))) (+ x y)))

;; ;Value 32: ((lambda (x y) (+ x y)) 1 (+ 2 3))

(put 'eval 'let (lambda (exp env)
                  (eval (let->combination exp) env)))

; Exercise 4.7

(define (make-let clauses body)
  (append (list 'let clauses) body))

(define (let*->nested-lets exp)
  (expand-let* (let-clauses exp) (let-body exp)))

(define (expand-let* clauses body)
  (let ((first (car clauses))
        (rest (cdr clauses)))
    (if (null? rest)
        (make-let (list first)
                  body)
        (make-let (list first)
                  (list (expand-let* (cdr clauses) body))))))

;; 1 ]=> (let*->nested-lets '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z)))

;; ;Value 33: (let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z))))

; It can be written as a series of nested lets because each let creates a new
; environment in which to evaluate its body which has its variables
; available. The body then contains the next let expression, etc.

; It's sufficient to add to eval, as eval is recursively called, which then
; expands out the lets, and then finally evaluates the lambdas.

(put 'eval 'let* (lambda (exp env)
                   (eval (let*->nested-lets exp) env)))

; Exercise 4.8

(define (named-let? exp)
  (symbol? (cadr exp)))

(define (named-let-name exp)
  (cadr exp))

(define (named-let-clauses exp)
  (caddr exp))

(define (named-let-body exp)
  (cadddr exp))

(define (let->combination exp)
  (if (named-let? exp)
      (expand-named-let (named-let-name exp)
                        (named-let-clauses exp)
                        (named-let-body exp))
      (expand-let (let-clauses exp) (let-body exp))))

(define (make-let-proc proc-name vars proc-body let-body)
  (make-let (list (list proc-name (make-lambda vars proc-body)))
            let-body))

(define (expand-named-let proc-name clauses body)
  (let ((vars (map let-clause-variable clauses))
        (initial-values (map let-clause-expression clauses)))
    (make-let-proc proc-name vars body
                   (make-application proc-name initial-values))))

;; 1 ]=> (let->combination '(let fib-iter ((a 1) (b 0) (count n)) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))

;; ;Value 14: (let ((fib-iter (lambda (a b count) (if (= count 0) b (fib-iter (+ a b) a (- count 1)))))) (fib-iter 1 0 n))


; Exercise 4.9

; (while <expr> <body>)
;
; while <expr> is true evaluates <body> repeatedly

; e.g.

; (let ((n 0))
;   (while (< n 10)
;     (set! n (+ n 1))
;     (print n)))

; The result of the expression is 'ok.

(define (while-expr exp)
  (cadr exp))

(define (while-body exp)
  (cddr exp))

(define (while->combination exp)
  (let ((body-sym (generate-unique-symbol)))
    (make-let-proc body-sym '()
                   (list (make-if (while-expr exp)
                                  (make-begin (append (while-body exp)
                                                      (list (make-application body-sym '()))))
                                  'ok))
                   (make-application body-sym '()))))


;; 1 ]=> (while->combination '(while (< n 10) (+ n 1) (print n)))

;; ;Value 27: (let ((gensym-1 (lambda () (if (< n 10) (begin (+ n 1) (print n) (gensym-1)) ok)))) (gensym-1))


; (until <expr> <body>)

; while <expr> is false evaluates <body> repeatedly

(define (make-while expr body)
  (list 'while expr body))

(define (until-expr exp)
  (cadr exp))

(define (until-body exp)
  (cddr exp))

(define (make-not expr)
  (list 'not expr))

(define (until->combination exp)
  (make-while (make-not (until-expr exp)) (until-body exp)))

;; 1 ]=> (until->combination '(until (= n 10) (+ n 1) (print n)))

;; ;Value 30: (while (not (= n 10)) ((+ n 1) (print n)))

(put 'eval 'while (lambda (exp env)
                    (eval (while->combination exp) env)))

(put 'eval 'until (lambda (exp env)
                    (eval (until->combination exp) env)))

; Exercise 4.10

;; todo: do this later, no point now as we don't have the environment data
;; structures with which to test any changes
