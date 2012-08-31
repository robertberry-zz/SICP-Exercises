; Answers for 4-1-3

(load "ex412.scm")

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


; Exercise 4.11

(define (make-binding var val)
  (cons var val))

(define (make-frame vars vals)
  (map make-binding vars vals))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (set-binding! binding val)
  (set-cdr! binding val))

(define (insert-list! seq x)
  (let ((front (car seq)))
    (set-car! seq x)
    (set-cdr! seq (cons front (cdr seq)))))

(define (add-binding-to-frame! binding frame)
  (insert-list! frame binding))

(define (find-if f seq)
  (if (null? seq)
      false
      (let ((first (car seq)))
        (if (f first)
            first
            (find-if f (cdr seq))))))

(define (some f seq)
  (if (null? seq)
      false
      (let ((fx (f (car seq))))
        (if fx
            fx
            (some f (cdr seq))))))

(define (binding-has-var var)
  (lambda (binding)
    (eq? (binding-variable binding) var)))

(define (find-binding-in-frame var frame)
  (find-if (binding-has-var var) frame))

(define (find-binding var env)
  (some (lambda (frame) (find-binding-in-frame var frame)) env))

(define (lookup-variable-value var env)
  (let ((binding (find-binding var env)))
    (if binding
        (binding-value binding)
        (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (let ((binding (find-binding var env)))
    (if binding
        (set-binding! binding val)
        (error "Unbound variable -- SET!" var))))

(define (define-variable! var val env)
  (let* ((frame (first-frame env))
         (binding (find-binding-in-frame var frame)))
    (if binding
        (set-binding! binding val)
        (add-binding-to-frame! (make-binding var val) frame))))

; Exercise 4.12

;; Oops, already did that.

; Exercise 4.13

;; Interesting question. I doubt the safety of allowing a user to remove
;; variable bindings from enclosing environments, as it could effectively
;; break other pieces of code. Imagine a procedure that removed the binding
;; for '+ for example.

;; On the other hand, it's confusing that you could unbind a variable and then
;; find it is still bound, only further down the environment chain. But then
;; perhaps this is the only possible use for this function - you're not going
;; to refer to a variable again after it has been unbound otherwise, as it
;; would signal an error, so what's the point?

;; So I'm going to implement it for the first frame only.

(define (remove-if f seq)
  (if (null? seq)
      '()
      (let ((x (car seq)))
        (if (f x)
            (cdr seq)
            (cons x (remove-if f (cdr seq)))))))

(define (make-unbound! var env)
  (set-car! env (remove-if (binding-has-var var) (first-frame env))))
