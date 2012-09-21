; Answers for 4-4-4-8

; Exercise 4.71

;; (define (simple-query query-pattern frame-stream)
;;   (stream-flatmap
;;    (lambda (frame)
;;      (stream-append-delayed
;;       (find-assertions query-pattern frame)
;;       (delay (apply-rules query-pattern frame))))
;;    frame-stream))

;; (define (disjoin disjuncts frame-stream)
;;   (if (empty-disjunction? disjuncts)
;;       the-empty-stream
;;       (interleave-delayed
;;        (qeval (first-disjunct disjuncts) frame-stream)
;;        (delay (disjoin (rest-disjuncts disjuncts)
;;                        frame-stream)))))



;; For disjoin it recursively calls itself, so if the stream is infinite it
;; will never end. This is not true for simple-query, however.

;; is it something to do with rules potentially infinitely recursing on
;; themselves?

;; todo ...


; Exercise 4.72

;; Because streams might potentially be infinite, in which case it's better to
;; interleave so as to produce items from all matched assertions and rules
;; rather than just the first.


; Exercise 4.73

;; It has a recursive call to flatten-stream for the rest of the stream. If
;; the stream is infinite, this will never complete.


; Exercise 4.74

;; a)

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (complement f)
  "The complement of f (assumes f takes only one argument)."
  (lambda (x)
    (not (f x))))

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (complement stream-null?) stream)))

;; b)

;; No, it shouldn't do. Interleave doesn't change the ordering - it always
;; takes an element from the initial stream, then from the second, then front
;; the initial ...

;; But when it encounters that the stream is null the second time it tries to

;; take from the initial, it just returns all of the second stream. This is
;; the same ordering as would be produced by simple-flatten.


; Exercise 4.75

(define (unique-query query)
  (car query))

(define (singleton-stream? stream)
  "Whether stream contains only one element."
  (and (not (stream-null? stream))
       (stream-null? (stream-cdr stream))))

(define (uniquely-asserted query frame-stream)
  (let ((query (unique-query query)))
    (stream-flatmap
     (lambda (frame)
       (let ((new-framestream (qeval query (singleton-stream frame))))
         (if (singleton-stream? new-framestream)
             new-framestream
             the-empty-stream)))
     frame-stream)))

(put 'unique 'qeval uniquely-asserted)

;; ;;; Query input:
;; (and (job ?x ?xjob) (unique (supervisor ?y ?x)))

;; ;;; Query results:
;; (and (job (scrooge eben) (accounting chief accountant)) (unique (supervisor (cratchet robert) (scrooge eben))))
;; (and (job (hacker alyssa p) (computer programmer)) (unique (supervisor (reasoner louis) (hacker alyssa p))))


; Exercise 4.76

(define (first-binding frame)
  (car frame))

(define (rest-bindings frame)
  (cdr frames))

(define (merge-frames f1 f2)
  "Merges frames f1 and f2. Returns 'failed if incompatible."
  (cond ((eq? f1 'failed) 'failed)
        ((null? f2) f1)
        (else
         (let ((binding (first-binding f2)))
           (merge-frames 
            (unify-match (binding-variable binding)
                         (binding-value binding)
                         f1)
            (rest-bindings f2))))))

(define (stream-combinations . streams)
  "Distinct combinations of elements chosen from an arbitrary number of
streams. e.g. for streams X, Y and Z, produces a stream containing (X_i, Y_j,
Z_k) for all i < size(X), j < size(Y), and k < size(Y)."
  (if (null? streams)
      (singleton-stream '())
      (let ((first (car streams))
            (rest (apply stream-combinations (cdr streams))))
        (stream-flatmap
         (lambda (acc)
           (stream-map
            (lambda (x)
              (cons x acc)) first))
         rest))))

(define (stream-cross-map-filter f keep? . streams)
  "Maps f against first two streams. Filters the resultant stream to only
contain elements for which keep? returns T. The resultant stream is then
stream-cross-map-filtered against the remaining streams."
  (define (iter acc seq)
    (if (null? seq)
        acc
        (iter (stream-filter keep?
                             (stream-map (lambda (pair)
                                           (apply f pair))
                                         (stream-combinations acc
                                                              (car seq))))
              (cdr seq))))
  (if (< (length streams) 2)
      (error "Cannot perform cross-map on fewer than 2 streams.")
      (iter (car streams) (cdr streams))))

(define (conjoin conjuncts frame-stream)
  (define (not-failed? frame-stream)
    (not (eq? frame-stream 'failed)))
  (let ((evaluated-conjuncts        
         (map (lambda (conjunct)      ; Breaks data abstraction, sorry.
                (qeval conjunct frame-stream)) conjuncts)))
    (apply stream-cross-map-filter
           merge-frames
           not-failed?
           evaluated-conjuncts)))

(put 'and 'qeval conjoin)


; Exercise 4.77

     ;; *Exercise 4.77:* In section *Note 4-4-3:: we saw that `not' and
     ;; `lisp-value' can cause the query language to give "wrong" answers
     ;; if these filtering operations are applied to frames in which
     ;; variables are unbound.  Devise a way to fix this shortcoming.  One
     ;; idea is to perform the filtering in a "delayed" manner by
     ;; appending to the frame a "promise" to filter that is fulfilled
     ;; only when enough variables have been bound to make the operation
     ;; possible.  We could wait to perform filtering until all other
     ;; operations have been performed.  However, for efficiency's sake, we
     ;; would like to perform filtering as soon as possible so as to cut
     ;; down on the number of intermediate frames generated.

; I wish they'd used better data-hiding for the frame. Need to redefine a few
; things:

(define the-empty-frame '(() ()))

; This has to use the new definition of an empty frame

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q (singleton-stream the-empty-frame))))
           (query-driver-loop)))))

(define (make-frame bindings filters)
  (list bindings filters))

(define (frame-bindings frame)
  (car frame))

(define (frame-filters frame)
  (cadr frame))

(define (add-filter-to-frame filter frame)
  "Adds the filter to the given frame."
  (make-frame (frame-bindings frame)
              (cons filter (frame-filters frame))))

; Need to redefine these

(define (binding-in-frame variable frame)
  (assoc variable (frame-bindings frame)))

(define (extend variable value frame)
  (make-frame (cons (make-binding variable value) (frame-bindings frame))
              (frame-filters frame)))

; and this ...

(define (merge-frames f1 f2)
  "Merges frames f1 and f2. Returns 'failed if incompatible."
  (cond ((eq? f1 'failed) 'failed)
        ((null? f2) f1)
        (else
         (let ((f2-bindings (frame-bindings f2)))
           (let ((binding (first-binding f2-bindings)))
             (merge-frames 
              (unify-match (binding-variable binding)
                           (binding-value binding)
                           f1)
              (make-frame (rest-bindings f2-bindings) (frame-filters f2))))))))


; Now the actual code to implement saving and recalling the filters:

(define (lisp-value-variables exp)
  (filter var? exp))

(define (make-filter dependencies promise)
  (cons dependencies promise))

(define (filter-dependencies filter)
  (car filter))

(define (filter-promise filter)
  (cdr filter))

;; (define (negate operands frame-stream)
;;   (stream-flatmap
;;    (lambda (frame)
;;      (if (stream-null? (qeval (negated-query operands)
;;                               (singleton-stream frame)))
;;          (singleton-stream frame)
;;          the-empty-stream))
;;    frame-stream))

;; (define (lisp-value call frame-stream)
;;   (stream-flatmap
;;    (lambda (frame)
;;      (if (execute
;;           (instantiate
;;            call
;;            frame
;;            (lambda (v f)
;;              (error "Unknown pat var -- LISP-VALUE" v))))
;;          (singleton-stream frame)
;;          the-empty-stream))
;;    frame-stream))

;; (put 'not 'qeval negate)
;; (put 'lisp-value 'qeval lisp-value)

