; Answers for 5-2-2

; Exercise 5.8

;; a will be 3, as the first here will be the first label in the lookup table.

;; It would be inefficient to check the whole list for duplicates for every
;; label added - O(n^2/2). Better would be to form a sorted list of label
;; names at the end of extracting labels, then check sequentially for
;; duplicates - O(n log n + n); n log n for the sort, n for the scan.

;; Alternatively, you could scan the list of label names, checking whether
;; they are in a set (implemented as a hash table); if not, add to the set; if
;; so, signal error. As hash-table look up is constant, this would be O(n).

(define (repeated-value sequence)
  (define (iter last sequence)
    (if (null? sequence)
        false
        (let ((first (car sequence))
              (rest (cdr sequence)))
          (if (equal? first last)
              first
              (iter first rest)))))
  (if (null? sequence)
      false
      (iter (car sequence) (cdr sequence))))

(define (extract-labels text receive)
  (define (label-names labels)
    (map (lambda (label)
           (symbol->string (car label))) labels))
  (define (extract text receive)
    "Original extract procedure"
    (if (null? text)
        (receive '() '())
        (extract (cdr text)
                 (lambda (insts labels)
                   (let ((next-inst (car text)))
                     (if (symbol? next-inst)
                         (receive insts
                             (cons (make-label-entry next-inst
                                                     insts)
                                   labels))
                         (receive (cons (make-instruction next-inst)
                                        insts)
                             labels)))))))
  (extract text (lambda (insts labels)
                  (let ((repeated
                         (repeated-value (sort (label-names labels) string<?))))
                    (if repeated
                        (error "Label redefined" repeated)
                        (receive insts labels))))))
