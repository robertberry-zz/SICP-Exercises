; Answers for 5-3-1

;; Exercise 5.20

;;         +---+---+
;; y --->  | * | * |
;;         +-|-+-|-+
;;       2   |   |
;;           |   |
;;           +---'
;;           |
;;           |
;;           V
;;         +---+---+     +---+
;; x --->  | * | * +---->| 2 |
;;         +-|-+---+     +---+
;;       1   |
;;           V
;;         +---+
;;         | 1 |
;;         +---+


;;    Index   0    1    2    3    4    5    6    7    8    ...
;;          +----+----+----+----+----+----+----+----+----+----
;; the-cars |    | n1 | p1 |    |    |    |    |    |    | ...
;;          +----+----+----+----+----+----+----+----+----+----
;; the-cdrs |    | n2 | p1 |    |    |    |    |    |    | ...
;;          +----+----+----+----+----+----+----+----+----+----

;;    free: 3
;;    x:    p1
;;    y:    p2


;; Exercise 5.21

;; a)

;; (define (count-leaves tree)
;;   (cond ((null? tree) 0)
;;         ((not (pair? tree)) 1)
;;         (else (+ (count-leaves (car tree))
;;                  (count-leaves (cdr tree))))))

(define count-leaves-machine
  (make-machine
   '(continue tree val val2)
   (list (list '+ +)
         (list 'null? null?)
         (list 'pair? pair?)
         (list 'eq? eq?)
         (list 'car car)
         (list 'cdr cdr))
   '(controller
     (assign continue (label count-done))

     count-loop
     (test (op null?) (reg tree))
     (branch (label null-tree))
     (test (op pair?) (reg tree))
     (branch (label pair))
     (goto (label atom))

     pair
     (save continue)
     (save tree)
     (assign continue (label after-count-car))
     (assign tree (op car) (reg tree))
     (goto (label count-loop))

     after-count-car
     (restore tree)
     (save val)
     (assign continue (label after-count-cdr))
     (assign tree (op cdr) (reg tree))
     (goto (label count-loop))

     after-count-cdr
     (assign val2 (reg val))
     (restore val)
     (assign val (op +) (reg val) (reg val2))
     (restore continue)
     (goto (reg continue))

     null-tree
     (assign val (const 0))
     (goto (reg continue))

     atom
     (assign val (const 1))
     (goto (reg continue))

     count-done)))

(define (count-leaves seq)
  (set-register-contents! count-leaves-machine 'tree seq)
  (start count-leaves-machine)
  (get-register-contents count-leaves-machine 'val))

;; b)

;; (define (count-leaves tree)
;;   (define (count-iter tree n)
;;     (cond ((null? tree) n)
;;           ((not (pair? tree)) (+ n 1))
;;           (else (count-iter (cdr tree)
;;                             (count-iter (car tree) n)))))
;;   (count-iter tree 0))

(define count-leaves-iter-machine
  (make-machine
   '(continue tree val n)
   (list (list '+ +)
         (list 'null? null?)
         (list 'pair? pair?)
         (list 'eq? eq?)
         (list 'car car)
         (list 'cdr cdr))
   '(controller
     (assign continue (label count-done))
     (assign n (const 0))

     count-loop
     (test (op null?) (reg tree))
     (branch (label null-tree))
     (test (op pair?) (reg tree))
     (branch (label pair))
     (goto (label atom))

     pair
     (save continue)
     (save tree)
     (assign continue (label after-count-car))
     (assign tree (op car) (reg tree))
     (goto (label count-loop))

     after-count-car
     (restore tree)
     (assign n (reg val))
     (assign continue (label after-count-cdr))
     (assign tree (op cdr) (reg tree))
     (goto (label count-loop))

     after-count-cdr
     (restore continue)
     (goto (reg continue))

     null-tree
     (assign val (reg n))
     (goto (reg continue))

     atom
     (assign val (op +) (reg n) (const 1))
     (goto (reg continue))

     count-done)))

(define (count-leaves-iter seq)
  (set-register-contents! count-leaves-iter-machine 'tree seq)
  (start count-leaves-iter-machine)
  (get-register-contents count-leaves-iter-machine 'val))


; Exercise 5.22

; a)

;; (define (append x y)
;;   (if (null? x)
;;       y
;;       (cons (car x) (append (cdr x) y))))

(define append-machine
  (make-machine
   '(continue x y val car-x)
      (list
       (list '+ +)
       (list 'null? null?)
       (list 'pair? pair?)
       (list 'eq? eq?)
       (list 'car car)
       (list 'cons cons)
       (list 'cdr cdr))
   '(controller
     (assign continue (label append-done))

     append-loop
     (test (op null?) (reg x))
     (branch (label null-x))
     (save continue)
     (save x)
     (assign x (op cdr) (reg x))
     (assign continue (label consing))
     (goto (label append-loop))

     consing
     (restore x)
     (restore continue)
     (assign car-x (op car) (reg x))
     (assign val (op cons) (reg car-x) (reg val))
     (goto (reg continue))

     null-x
     (assign val (reg y))
     (goto (reg continue))

     append-done)))

(define (append x y)
  (set-register-contents! append-machine 'x x)
  (set-register-contents! append-machine 'y y)
  (start append-machine)
  (get-register-contents append-machine 'val))

; b)

;; (define (append! x y)
;;   (set-cdr! (last-pair x) y)
;;   x)

;; (define (last-pair x)
;;   (if (null? (cdr x))
;;       x
;;       (last-pair (cdr x))))

(define append!-machine
  (make-machine
   '(x y cdr-x)
   (list
    (list 'null? null?)
    (list 'cons cons)
    (list 'cdr cdr)
    (list 'set-cdr! set-cdr!))
   '(controller
     last-pair-loop
     (assign cdr-x (op cdr) (reg x))
     (test (op null?) (reg cdr-x))
     (branch (label last-pair))
     (assign x (op cdr) (reg x))
     (goto (label last-pair-loop))

     last-pair
     (perform (op set-cdr!) (reg x) (reg y))
     )))

(define (append! x y)
  (set-register-contents! append!-machine 'x x)
  (set-register-contents! append!-machine 'y y)
  (start append!-machine))

