#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

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

; Exercise 2.46

(define (my-make-vect x y)
  (list (x y)))

(define (my-xcor-vect v)
  (car v))

(define (my-ycor-vect v)
  (cadr v))

(define (my-add-vect v u)
  (make-vect (+ (my-xcor-vect v)
                (my-xcor-vect u))
             (+ (my-ycor-vect v)
                (my-ycor-vect u))))

(define (my-sub-vect v u)
  (make-vect (- (my-xcor-vect v)
                (my-xcor-vect u))
             (- (my-ycor-vect v)
                (my-ycor-vect u))))

(define (my-scale-vect x v)
  (make-vect (* (my-xcor-vect v) x)
             (* (my-ycor-vect v) x)))

; Exercise 2.47

(define (make-frame-1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-1 f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))

(define (origin-frame-2 f)
  (car f))

(define (edge1-frame-2 f)
  (cadr f))

(define (edge2-frame-2 f)
  (cddr f))

; Exercise 2.48

(define (my-make-segment start end)
  (list start end))

(define (my-start-segment segment)
  (car segment))

(define (my-end-segment segment)
  (cadr segment))

; Exercise 2.49

; a)

(define outline-segments (list (make-segment (make-vect 0 0) (make-vect 0.99 0))
                               (make-segment (make-vect 0.99 0) (make-vect 0.99 0.99))
                               (make-segment (make-vect 0.99 0.99) (make-vect 0 0.99))
                               (make-segment (make-vect 0 0.99) (make-vect 0 0))))

(define outline
  (segments->painter outline-segments))

; b)

(define x
  (segments->painter (list (make-segment (make-vect 0 0) (make-vect 0.99 0.99))
                           (make-segment (make-vect 0 0.99) (make-vect 0.99 0)))))

; c)

(define diamond
  (segments->painter (list (make-segment (make-vect 0 0.5) (make-vect 0.5 0.99))
                           (make-segment (make-vect 0.5 0.99) (make-vect 0.99 0.5))
                           (make-segment (make-vect 0.99 0.5) (make-vect 0.5 0))
                           (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))))

; d) 

;; Too verbose a syntax, adding helper function
(define (make-segs segs)
  (segments->painter (map (lambda (pair)
                            (let ((start (car pair))
                                  (end (cadr pair)))
                              (make-segment (apply make-vect start) (apply make-vect end))))
                          segs)))

(define wave
  (make-segs '(((0.0 0.6) (0.2 0.4))
               ((0.0 0.8) (0.2 0.6))
               ((0.2 0.4) (0.3 0.6))
               ((0.2 0.6) (0.3 0.7))
               ((0.3 0.6) (0.4 0.5))
               ((0.4 0.5) (0.2 0.0))
               ((0.3 0.7) (0.4 0.7))
               ((0.4 0.7) (0.35 0.85))
               ((0.35 0.85) (0.4 1.0))
               ((0.6 1.0) (0.65 0.85))
               ((0.65 0.85) (0.6 0.7))
               ((0.6 0.7) (0.7 0.7))
               ((0.7 0.7) (1.0 0.4))
               ((0.45 0.0) (0.5 0.3))
               ((0.5 0.3) (0.55 0.0))
               ((0.75 0.0) (0.65 0.45))
               ((0.65 0.45) (1.0 0.2))
               )))

; Exercise 2.50

(define my-flip-horiz
  (transform-painter (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define my-rotate180
  (transform-painter (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define my-rotate270
  (transform-painter (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; Exercise 2.51

(define (below-1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        ((paint-bottom painter1) frame)
        ((paint-top painter2) frame)))))

(define (below-2 painter1 painter2)
  (rotate270 (beside (rotate90 painter2) (rotate90 painter1))))

; Exercise 2.52

; a)

(define smile
  (make-segs '(((0.45 0.8) (0.5 0.75))
               ((0.5 0.75) (0.55 0.8)))))

(define (overlay painter1 painter2)
  (lambda (frame)
    (painter1 frame)
    (painter2 frame)))

(define wave-2 (overlay wave smile))

; b)

(define right-split (split beside below))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner))))))

; c)

;; I'm not sure exactly what is expected, as it seems like they already all
;; look out from each corner of the square ...

;; So I'll just do a random new pattern.

(define (square-limit-2 painter n)
  ((square-of-four flip-horiz
                   flip-vert
                   flip-horiz
                   flip-vert) (corner-split painter n)))
