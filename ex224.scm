; Have to use Scheme for this one. :<

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 0)))

; Exercise 2.44

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; Fiddling around

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;; this is a nicer definition than what they give!
(define flipped-pairs
  (square-of-four identity flip-vert identity flip-vert))

;; Maybe this definition, however, is a bit confusing ... 
(define (square-limit painter n)
  ((square-of-four flip-horiz
                   identity
                   rotate180
                   flip-vert) (corner-split painter n)))

; Exercise 2.45

;; Was unsure about the name of the procedures to use here. I used 'stack'
;; because that's the function that appends the smaller split versions of the
;; original to the original. I used 'join' because that's the function that
;; joins the two smaller pictures in each level of recursion together
(define (split stack join)
  (define (split-iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-iter painter (- n 1))))
          (stack painter (join smaller smaller)))))
  split-iter)

