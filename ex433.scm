; Answers for 4-3-3

;(cd "lib")
;(load "ch4-ambeval.scm")

; Exercise 4.50

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (segment-about-pivot seq n)
  "Given a sequence and index n, returns a list of all elements up to the
element at nth position, the element at nth position, and the rest."
  (let iter ((seq seq)
             (acc '())
             (n n))
    (if (null? seq)
        (error "Tried to segment about pivot beyond end of list" n)
        (if (= n 0)
            (list (reverse acc) (car seq) (cdr seq))
            (iter (cdr seq) (cons (car seq) acc) (- n 1))))))

(define (shuffled seq)
  "Sequence with its elements in random new positions. (This works but is
very inefficient. Lack of experience with algorithms showing again.)"
  (let iter ((seq seq)
             (n (length seq)))
    (if (<= n 1)
        seq
        (let* ((k (random n))
               (segmented (segment-about-pivot seq k))
               (left (car segmented))
               (x (cadr segmented))
               (right (caddr segmented)))
          (cons x (iter (append left right) (- n 1)))))))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next (shuffled cprocs))))) ;; shuffling here so that the ambs are reshuffled
                                      ;; every time the amb expression is evaluated

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) 
        ((amb? exp) (analyze-amb exp))                
        ((ramb? exp) (analyze-ramb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))


;; ;;; Amb-Eval input:
;; (ramb 4 9 10 12 5)

;; ;;; Starting a new problem 
;; ;;; Amb-Eval value:
;; 5

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; 10

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; 9

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; 12

;; ;;; Amb-Eval input:
;; try-again

;; ;;; Amb-Eval value:
;; 4

;; ;;; Amb-Eval input:
;; try-again

;; ;;; There are no more values of
;; (ramb 4 9 10 12 5)


;; It helps by making better use of the recursive nature of the grammar. Ramb
;; will randomly keep repeatedly extending constructs, causing it to more
;; quickly develop complicated sentences. It also can be used to randomize the
;; order in which words are picked from the word lists; otherwise they always
;; come out in the same order, causing sentences to contain 'the student
;; studies', etc., repeatedly.



; Exercise 4.51

(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))

(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) 
        ((amb? exp) (analyze-amb exp))                
        ((ramb? exp) (analyze-ramb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

;; If we had used set! then count would always be 1 (because any failure or
;; try-again would revert the set!)


; Exercise 4.52

(define (if-fail? exp)
  (tagged-list? exp 'if-fail))

(define (if-fail-expression exp)
  (cadr exp))

(define (if-fail-alternative exp)
  (caddr exp))

(define (analyze-if-fail exp)
  (let ((expr (analyze (if-fail-expression exp)))
        (alternative (analyze (if-fail-alternative exp))))
    (lambda (env succeed fail)
      (expr env
            succeed
            (lambda ()
              (alternative env (lambda (val fail)
                                 (succeed val fail)) fail))))))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) 
        ((amb? exp) (analyze-amb exp))                
        ((ramb? exp) (analyze-ramb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))


; Exercise 4.53

;; Returns the list of all the prime sum pairs (in reverse order). This is
;; because permanent-set! incrementally adds the results to pairs, which is
;; returned by if-fail once the (amb) for the last pair is evaluated.


; Exercise 4.54

(define (require? exp)
  (tagged-list? exp 'require))

(define (require-predicate exp)
  (cadr exp))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((require? exp) (analyze-require exp))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) 
        ((amb? exp) (analyze-amb exp))                
        ((ramb? exp) (analyze-ramb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not pred-value)
                   (fail)
                   (succeed 'ok fail2)))
             fail))))
