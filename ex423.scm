; Answers for 4-2-3


; Exercise 4.32

; One example of a major advantage has already been given - as the initial
; element is delayed, streams can be recursively computed from the first
; element (so long as one of the streams' first elements are not dependent on
; other dependent streams to be computed - i.e., an initially supplied value).

; Another major advantage is you can traverse the list without having to
; perform the computations. e.g., in the initial form of streams to compute
; the length of the list would require computing every element in the list, as
; every time you retrieve the cdr of the stream, the car of that cdr is
; computed. In this version, no elements of the stream would be computed.

; Another major advantage is that the first element is not always computed. A
; given stream may not actually be used within the running of a particular
; instance of a program, so it can be wasteful to compute the first
; element. e.g., if you wrote a function that given a file-path returned a
; stream containing the lines of the file, in the initial version this stream
; would always have to open that file (which uses a file-handle and is a
; relatively slow operation) in order to read the first line. In the new
; version, the file would only actually be opened if the first line were ever
; accessed.



; Exercise 4.33

(cd "lib")
(load "ch4-leval.scm")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (quotation exp env))
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

(define (quotation exp env)
  (let ((text (text-of-quotation exp)))
    (if (list? text)
        (eval (quoted-list->consing text) env)
        text)))

(define (quoted-list->consing seq)
  (if (null? seq)
      '(list)
      (let ((first (car seq))
            (rest (cdr seq)))
        (cons 'cons
              (cons (if (list? first)
                        (quoted-list->consing first)
                        (list 'quote (car seq)))
                    (list (quoted-list->consing rest)))))))

(define the-global-environment (setup-environment))

(define (global-eval exp)
  (eval exp the-global-environment))

(global-eval '(define (cons x y)
                (lambda (m) (m x y))))

(global-eval '(define (car z)
                (z (lambda (p q) p))))

(global-eval '(define (cdr z)
                (z (lambda (p q) q))))

;(driver-loop)


; Exercise 4.34

;; I think you have to just print the lists (even though they may be
;; infinite). It's now very difficult to distinguish between fixed-length and
;; infinite-length lists. A decent REPL provides an escape sequence to cancel
;; printing of such structures anyway.

;; It's a bit more difficult to print, though, as knowing they might be
;; infinite, you cannot simply convert to a list and then use the in-built
;; display functions.

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((consing? exp) (make-delayed-pair (delay-it (cons-first exp) env)
                                           (delay-it (cons-second exp) env)))
        ((quoted? exp) (quotation exp env))
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
        ((application? exp)             ; clause from book
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (consing? exp)
  (tagged-list? exp 'cons))

(define (cons-first exp)
  (cadr exp))

(define (cons-second exp)
  (caddr exp))

(define (make-delayed-pair first second)
  (list 'delayed-pair first second))

(define (delayed-pair? exp)
  (tagged-list? exp 'delayed-pair))

(define (delayed-pair-car pair)
  (cadr pair))

(define (delayed-pair-cdr pair)
  (caddr pair))

(define primitive-procedures
  (list (list 'car delayed-pair-car)
        (list 'cdr delayed-pair-cdr)
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

(define the-global-environment (setup-environment))

(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((delayed-pair? object)
         (display-sequence object))
        (else (display object))))

(define (display-sequence seq)
  (define (iter pair)
    (let ((first (force-it (delayed-pair-car pair)))
          (rest (force-it (delayed-pair-cdr pair))))
      (user-print first)
      (if (not (null? rest))
          (begin
            (display " ")
            (if (delayed-pair? rest)
                (iter rest)
                (user-print rest))))))
  (if (null? seq)
      (display "()")
      (begin
        (display "(")
        (iter seq)
        (display ")"))))


;; ;;; L-Eval input:
;; (define x '(1 2 3 4))

;; ;;; L-Eval value:
;; ok

;; ;;; L-Eval input:
;; x

;; ;;; L-Eval value:
;; (1 2 3 4)

;; ;;; L-Eval input:
;; (define ones (cons 1 ones))

;; ;;; L-Eval value:
;; ok

;; ;;; L-Eval input:
;; (car ones)

;; ;;; L-Eval value:
;; 1

;; ;;; L-Eval input:
;; (car (cdr ones))

;; ;;; L-Eval value:
;; 1

;; ;;; L-Eval input:
;; ones

;; ;;; L-Eval value:
;; (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

; etc.