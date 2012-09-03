; Answers for 4-2-1

; Exercise 4.25

;; It will crash due to an infinite loop. One of the arguments to unless is a
;; call to factorial, which results in this.

;; In normal order, it works.


; Exercise 4.26

; a)

(load "ex412.scm")

(define (unless-predicate exp)
  (cadr exp))

(define (unless-consequent exp) (caddr exp))

(define (unless-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (unless->if exp)
  (make-if (unless-predicate exp)
           (unless-alternative exp)
           (unless-consequent exp)))

;; 1 ]=> (unless->if '(unless (> x 4) (+ 4 5)))

;; ;Value 11: (if (> x 4) false (+ 4 5))

; b)

;; It would be useful to have unless available as a procedure for use with
;; higher level procedures. e.g., one could map unless across three
;; lists. This would produce a list of elements from the second and third
;; lists based on the 'truthiness' of elements in the first list.
.

