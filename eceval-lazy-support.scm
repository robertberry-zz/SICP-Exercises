; Functions from the lazy evaluator used to support the eceval lazy evaluator
; machine

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (set-thunk-value! thunk value)
  (set-car! thunk 'evaluated-thunk)
  (set-car! (cdr thunk) value)  ; replace `exp' with its value
  (set-cdr! (cdr thunk) '()))   ; forget unneeded `env'

