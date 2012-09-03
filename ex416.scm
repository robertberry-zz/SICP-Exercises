; Answers for 4-1-6

(load "ex415.scm")

; Exercise 4.16

; a)

(define (binding-unassigned? binding)
  (eq? (binding-value binding) '*unassigned*))

(define (lookup-variable-value var env)
  (let ((binding (find-binding var env)))
    (cond ((not binding) (error "Unbound variable" var))
          ((binding-unassigned? binding) (error "Binding unassigned" var))
          (else (binding-value binding)))))

; b)

(define (separate seq f)
  "Separate a sequence into a pair of sequences based on whether elements
match the predicate f."
  (let iter ((seq seq)
             (left '())
             (right '()))
    (if (null? seq)
        (list (reverse left) (reverse right))
        (let ((first (car seq))
              (rest (cdr seq)))
          (if (f first)
              (iter rest (cons first left) right)
              (iter rest left (cons first right)))))))

;; 1 ]=> (separate '(1 2 3 4 5 6 7 8 9 10) odd?)

;; ;Value 13: ((1 3 5 7 9) (2 4 6 8 10))

(define (make-set variable value)
  (list 'set! variable value))

(define (make-unassigned-let variables body)
  (make-let (map (lambda (var) (list var ''*unassigned*)) variables)
            body))

(define (make-simultaneous-definition-let variables values body)
  (make-unassigned-let variables
                       (append (map make-set variables values)
                               body)))

(define (scan-out-defines body)
  (let* ((separated (separate body definition?))
         (definitions (car separated))
         (body (cadr separated)))
    (if (null? definitions)
        body
        (let* ((definition-variables (map definition-variable definitions))
               (definition-values (map definition-value definitions)))
          (list (make-simultaneous-definition-let definition-variables
                                                  definition-values
                                                  (append set-definitions body)))))))

;; 1 ]=> (scan-out-defines '((define x 1) (define (f a) (+ x a)) (define y (f x)) (f (* x 3))))

;; ;Value 21: (let ((x *unassigned*) (f *unassigned*) (y *unassigned*)) (set! x 1) (set! f (lambda (a) (+ x a))) (set! y (f x)) (f (* x 3)))


; c)

; Make procedure is the better place, as procedures are defined only once but
; evaluated potentially multiple times.

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))


; Exercise 4.17

;; I don't think it's really appropriate to draw environment diagrams, as in
;; the initial there is only one frame (that created when executing the outer
;; lambda) and in the latter there is only one extra frame (that created when
;; executing the let), whose parent is the former.

;; This would not make any difference in a correct program, because procedure
;; body definitions should not rely on unassigned variables that are being
;; simultaneously defined. (In the case of the former, this may instead find a
;; variable with the same name in an outer frame, which would cause incorrect
;; behaviour.)

;; To implement the simultaneous scope without constructing an extra frame
;; would require procedures to carry information with them about which
;; variables are defined within the body of the procedure. Then, apply would
;; need to make sure these are initially set to *unassigned* in the
;; environment created for the procedure. The defines in the procedure body
;; would need to be replaced with set!s instead, that are before any other
;; statements in the body.


; Exercise 4.18

;; This procedure will not work, because (stream-map f y) depends on y, which
;; is still *unassigned* at the time b is assigned.

;; It does work in the original expansion, because y is set before dy.


; Exercise 4.19

;; Ben's answer is incorrect by definition (not simultaneous). Eva's answer
;; seems the most sensible but it looks like in Scheme it's Alyssa's answer
;; that is implemented. (Having now read the footnote this all makes sense.)

;; To make definitions behave as Eva prefers is difficult. You would need to
;; scan the bodies of variables definitions to find out what variables they
;; require to be assigned. (So in this case b's requirements would be '(a) and
;; a's requirements would be '()). You would then need to order definitions
;; based on their dependencies, i.e. any definition that depends on another
;; should appear after that definition.

;; To form the definition requirements lists is fairly complicated, however,
;; as the scanner would need to be aware of various rules within Scheme for
;; variable shadowing. e.g., in the following expression,

;; (define a (let ((b 4)) (+ b 1)))

;; this should not specify b as a requirement.


; Exercise 4.20

; a)

(define (transform-eval tag f)
  (put 'eval tag (lambda (exp env)
                   (eval (f exp) env))))

(define (letrec->combination exp)
  (expand-letrec (let-clauses exp) (let-body exp)))

(define (expand-letrec clauses body)
  (let ((variables (map let-clause-variable clauses))
        (expressions (map let-clause-expression clauses)))
    (make-simultaneous-definition-let variables expressions body)))

(transform-eval 'letrec letrec->combination)

; b)

;;              +------------------------------------------------------------------+
;;  global  --> | f: ---+
;;  env         |       |
;;              +-------|----------------------------------------------^-----------+
;;                      v                ^                             |
;;                  .--- ---.            |                             |     (f 5)
;;                  | 0 | 0-+------------+                         +---+--------+
;;                  `-|-^---'                                      | x: 5       |<-- E1
;;                    |                                            +------------+
;;                    v                                               ^
;;             parameters: x                                          |      (letrec ...)
;;             body: (letrec ...                                   +--+------------------+
;;                                                                 | even?: *unassigned* |<-- E2
;;                                                                 | odd?: *unassigned*  |
;;                                                                 +---------------------+
;;         after E2 is created, even? and odd? are assigned with set!    ^
;;          both are like this, with the env being E2                    |
;;                                                  |       +---+---+    |
;;                                                  +-->    | 0 | 0 +----+
;;                                                          +-|-+---+        
;;                                                            |              
;;                                                            v              
;;                                                     parameters: n         
;;                                                      body: (if (= n 0) ...
                                                     
;; <REST OF BODY OF 'F'> is then evaluated in E2. Note that the even? and odd?
;; procedures have E2 as their own environments, so when they are evaluated they
;; have access to one another and themselves.
                                                     
                                                     


;;              +------------------------------------------------------------------+
;;  global  --> | f: ---+
;;  env         |       |
;;              +-------|----------------------------------------------^-----------+
;;                      v                ^                             |
;;                  .--- ---.            |                             |     (f 5)
;;                  | 0 | 0-+------------+                         +---+--------+
;;                  `-|-^---'                           +--------->| x: 5       |<-- E1
;;                    |                                 |          +------------+
;;                    v                                 |             ^
;;             parameters: x                            |             |      (let (even? ...)
;;             body: (letrec ...      odd? and even?    |          +--+------------------+   
;;                                           +---+---+  |          | even?: (lambda (..) |<-- E2
;;                                           | 0 | 0 +--+          | odd?: (lambda (..)  |
;;                                           +-|-+---+             +---------------------+
;;                                             |        
;;                                             v        
;;                                         parameters: n
                                                 
;; <REST OF BODY OF 'F'> is then evaluated in E2. Note that odd? and even?'s
;; environments are E1, in which the definitions of odd? and even? do not
;; exist. This causes recursive calls to fail.


; Exercise 4.12

; a)

((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (fib n)
      (if (or (= n 0) (= n 1))
          n
          (+ (fib fib (- n 1)) (fib fib (- n 2)))))))
 10)

; b)

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

